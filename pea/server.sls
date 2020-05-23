;; PEA Server.
;;
;; Written by Akce 2020.
;;
;; Server design notes:
;;  * Navigates via root playlist.
;;  * Playlists are hierarchical.
;;  * Media formats are handled via format specific drivers:
;;    - libmpv for audio (and video on desktop)
;;    - omxplayer for video on rpi
;;    - uade123 for amiga mod files
;;  * State changes are "broadcast" via UDP multicast and include:
;;    - track change (including all tag info)
;;    - current directory / playlist change
;;    - play state change
;;    - current timestamp
;;  * One control unicast TCP socket accepts client connections to:
;;    - query server on current state and accept response
;;    - query playlists
;;    - change server state and accept response
;;    ? client connections to be short lived. ie, command / response / close.
(library
  (pea server)
  (export
    make-pea-server)
  (import
    (rnrs)
    (only (chezscheme) display-condition)
    (pea player)
    (pea playlist)
    (only (pea util) arg command define-enum my input-port-ready? read-trim-right)
    (rename
      (pea util)
      (write-now %write-now))
    (pea vfs)
    ;; 3rd party libs.
    (ev)
    (socket extended)
    )

  (define write-now
    (lambda (input port)
      (case (command input)
        [(POS)
         (when (= (arg input) 0)
           (display "> ")(display '(POS *))(newline))]
        [else
          (display "> ")(display input)(newline)])
      (%write-now input port)))

  (define-record-type model
    (fields
      [immutable	vfs]
      [immutable	cursor])
    (protocol
      (lambda (new)
        (lambda (root-playlist-path vfs-state-file)
          (let ([vfs (make-vfs root-playlist-path)])
            (new vfs (make-cursor vfs vfs-state-file)))))))

  (define make-pea-server
    (lambda (root-playlist-path state-file ctrl-node ctrl-service mcast-node mcast-service)
      (my
        [controller (make-controller
                      (make-model root-playlist-path state-file)
                      (socket->port
                        (connect-client-socket
                          mcast-node mcast-service
                          (address-family inet) (socket-domain datagram)
                          (address-info all numerichost)
                          (ip-protocol udp))))]
        [ctrl-sock	(connect-server-socket
                          ctrl-node ctrl-service
                          (address-family inet) (socket-domain stream)
                          (address-info all numerichost passive) (ip-protocol tcp))])

      ;; Handle stdin commands. ie, server exit.
      (ev-io 0 (evmask 'READ) (make-stdin-reader controller))

      ;; Create the Control socket event watcher.
      (ev-io (socket-fd ctrl-sock) (evmask 'READ) (make-control-server controller ctrl-sock))

      ;; Create the media player.
      (controller `(set-player! ,(make-player controller)))

      ;; return the runner!
      (lambda ()
        (ev-run))))

  ;; [proc] make-control-server: create a socket listener function for use as an ev-io watcher.
  ;; It's only purpose is to create client watchers as they connect.
  (define make-control-server
    (lambda (controller ctrl-sock)
      (lambda (w revent)
        ;; This function is called when there's read activity on the Control socket.
        ;; ie, when a client is ready to connect.
        ;; HMMM add abstraction for all of client-* to (socket extended)?
        (let ([client-sock (socket-accept ctrl-sock)])
          ;; Create the client connect event watcher.
          (ev-io (socket-fd client-sock) (evmask 'READ) (make-control-client controller client-sock))
          ;; TODO show client address.
          (display "new client ")(display (socket-fd client-sock))(newline)))))

  ;; [proc] make-control-client: makes a control client function suitable for use as an ev-io watcher.
  (define make-control-client
    (lambda (controller client-sock)
      ;; The control client will read commands, action them, and respond.
      (my
        [client-port	(socket->port client-sock)])
      ;; Return a welcome message.
      (write-now '(AHOJ "Control connection established, welcome to PEA! :)") client-port)
      ;; Send current state, so UIs can immediately draw themselves.
      (write-now (controller 'state?) client-port)
      (write-now (controller 'vfs?) client-port)

      (lambda (w revent)
        ;; WARNING: Using 'read' here assumes that clients are well behaved and send fully formed sexprs.
        ;; WARNING: A partial datum will cause read to block the thread as it waits for a complete message.
        (let loop ([input (read-trim-right client-port)])
          ;;(display "client incoming: ")(write input)(newline)
          ;;(flush-output-port (current-output-port))
          (cond
            [(eof-object? input)
             ;; TODO show client address.
             (display "close client: ")(display (ev-io-fd-get w))(newline)
             (ev-io-stop w)
             (socket-shutdown client-sock (shutdown-method read write))
             (socket-close client-sock)
             (close-port client-port)]
            [else
              ;; process command.
              ;;(display "client command: ")(display input)(newline)
              (guard
                ;; TODO coding errors need to drop into debugger rather than report and continue.
                (e [else
                     ;; TODO define condition->string such that string quotes are escaped..
                     ;; TODO then do some proper server logging..
                     (display-condition e)
                     ;; TODO and then define a client message type.
                     (display-condition e client-port)
                     (flush-output-port client-port)])
                (let ([msg (controller input)])
                  (when msg
                    (write-now msg client-port)))
                ;; drain input port.
                (when (input-port-ready? client-port)
                  (loop (read-trim-right client-port))))])))))

  ;; PEA states follow a simple pattern:
  ;; - client requests player enter a state,
  ;; - player callback informs on new state.
  ;;
  ;; Allowed state transitions:
  ;; (PAUSED | PLAYING) + STOP -> STOPPED
  ;; (STOPPED) + PLAY -> PLAYING
  ;; (PLAYING) + PAUSE -> PAUSED
  ;;
  ;; Announcements are for upcoming video tracks in continuous play only:
  ;; (PLAYING + STOP) -> ANNOUNCING
  ;; (ANNOUNCING) + STOP -> STOPPED
  ;; (ANNOUNCING) + announce-delay timeout -> PLAY
  ;; Announcement state allows to see the upcoming video trackname (and optionally cancel)
  ;; before playback starts and the screen is filled/obscured with video.
  ;;
  ;; Using define-enum here to help catch mis-typed symbols, but probably not needed in the long term.
  (define-enum pea-state
    ;; Intermediate, client requested states. These are internal and not multicast.
    [PAUSE	'PAUSE]		; Client requested pause.
    [PLAY	'PLAY]		; Client requested play.
    [STOP	'STOP]		; Client requested stop.
    [ANNOUNCING 'ANNOUNCING]	; Announce the upcoming video track.
				; Play next after delay unless cancelled via stop!.
    ;; End player states.
    [PAUSED	'PAUSED]	; Player state PAUSED.
    [PLAYING	'PLAYING]	; Player state PLAYING.
    [STOPPED	'STOPPED]	; Played state STOPPED.
    )

  ;; [proc] make-controller: creates the main controller function.
  ;; [return] controller function.
  ;;
  ;; 'model' is the global pea data.
  ;; 'mcast' is the multicast *text* port through which global messages are sent.
  ;;
  ;; eg,
  ;; > (define controller (make-controller pea-model multicast-port))
  ;;
  ;; The controller handles state transitions and drives the media player.
  ;;
  ;; It's also responsible for sending all multicast messages. These multicast messages usually
  ;; indicate changes to global state and/or informational messages that all clients should be aware of.
  ;;
  ;; The returned controller function will take one input argument.
  ;; ie, the command and its arguments (if any).
  ;;
  ;; The format for this input must be a list or a symbol.
  ;;
  ;; eg, To start play:
  ;; > (controller 'play!)
  ;;
  ;; Unit commands may also be passed in within a list:
  ;; > (controller '(play!))
  ;;
  ;; A command with argument, cursor move:
  ;; > (controller '(move! 1))
  (define make-controller
    (lambda (model mcast)
      (my
        ;; accessors for convenience.
        [cursor	(model-cursor model)]
        [vfs	(model-vfs model)]
        ;; player driver. ie, handles play, pause, stop etc...
        [player	#f]
        ;; timer used to announce upcoming video before playing.
        [announce-timer #f]	; HMMM always create the timer, and just re-arm as needed?
        [announce-delay 5]
        ;; assume initial pea state is stopped.
        [state	(pea-state STOPPED)]
        ;; cache the current track info.
        [track-tags	'()]
        [track-pos	#f]
        [track-length	#f]
        )

      ;; [proc] ack-mcast: Multicasts message and returns ACK to caller (ie, client).
      (define ack-mcast
        (lambda (msg)
          (write-now msg mcast)
          'ACK))

      ;; [proc] state-set!: Changes PEA state, and multicasts it.
      ;; This is called only from the player callback messages as these are end states
      ;; (not intermediate request states) and as such need to be multicast to listening clients.
      (define state-set!
        (lambda (new-state)
          (unless (eq? state new-state)
            (set! state new-state)
            (write-now (make-state-info) mcast))))

      ;; [proc] make-state-info:
      ;; [return] (STATE ...)
      ;; Do not send track record as scheme readers need to understand (import) the track record def.
      (define make-state-info
        (lambda ()
          (case state
            [(ANNOUNCING)
             `(STATE ,state ,announce-delay ,(cursor-index cursor))]
            [else
              `(STATE ,state
                 ;; Current track pos/length/tags. Can be #f if STOPPED.
                 ,track-pos
                 ,track-length
                 ,track-tags
                 )])))

      ;; Playlist state: vpath/index/title/type
      (define make-vfs-info
        (lambda ()
          (let ([t (current-track)])
            `(VFS
               ,(vfs-vpath vfs)
               ,(cursor-index cursor)
               ,(track-title t)
               ,(track-type t)))))

      ;; [proc] current-track: return the track under the cursor.
      (define current-track
        (lambda ()
          (list-ref (vfs-tracks vfs) (cursor-index cursor))))

      ;; [proc] goto-next-media: goto the next media track.
      ;; [return] next track or #f.
      (define goto-next-media!
        (lambda ()
          (define (next! pos)
            (cond
              [pos
                (let ([t (current-track)])
                  (case (track-type t)
                    [(AUDIO VIDEO)
                     t]
                    [else
                      (next! (cursor-move! cursor 1))]
                    ))]
              [else
                #f]
              ))
          (let ([i (next! (cursor-move! cursor 1))])
            (when i
              (ack-mcast (make-vfs-info)))
            i)))

      (define announce-track
        (lambda ()
          (state-set! (pea-state ANNOUNCING))
          (set! announce-timer
            (ev-timer announce-delay 0
                      (lambda (timer i)
                        (set! announce-timer #f)
                        (controller 'play!))))
          'ACK))

      ;; [proc] play-another: play the next media track in the current playlist.
      ;; [return] result of (controller play!) or #f if nothing left in playlist.
      (define play-another
        (lambda ()
          (case state
            [(PLAYING)	; current mode is to keep playing, so try and move to the next track.
             (let ([t (goto-next-media!)])
               (cond
                 [t
                  (case (track-type t)
                    [(VIDEO)
                     ;; For upcoming VIDEO tracks, announce the title, and set a timer before playing.
                     ;; This allows for cancellation of continuous play and also a chance to see
                     ;; the name of what's coming up (in the case where the UI is on the same display).
                     (announce-track)]
                    [else
                      ;; Otherwise, play immediately.
                      (set! state (pea-state STOPPED))	; set STOPPED as play! requires it.
                      (controller 'play!)]
                    )]
                 [else
                   ;; Nothing more in the playlist.
                   ;; Move cursor back to first track before stopping...
                   (unless (= (cursor-index cursor) 0)
                     ;; ...but only when there's more than 1 item in the playlist.
                     (cursor-set! cursor 0)
                     (ack-mcast (make-vfs-info)))
                   #f]
               ))]
            [else
              #f]
            )))

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; The controller function itself.
      ;; It's really just a state transition table.
      (define (controller input)
        (case (command input)
          [(POS)
           (when (= 0 (arg input))
             (display "< ")(display '(POS *))(newline))]
          [else
            (display "< ")(display input)(newline)])
        ;; List these commands in alphabetic order within their groupings.
        (case (command input)
          ;;;; Player commands.
          [(play!)
           ;; For now, only allow PLAY from ANNOUNCING, STOPPED or STOP states.
           ;; The STOP state occurs when move! is selected during PLAYING.
           (case state
             [(ANNOUNCING STOPPED STOP)
              (set! state (pea-state PLAY))
              ;; TODO check that play actually works.
              (player
                'play!
                (track-join-path
                  (vfs-current vfs)
                  (current-track)))
              (cursor-save cursor)
              'ACK]
             [else
               `(DOH "Can only play! from ANNOUNCING/STOPPED states. Current state" ,state)])]
          [(stop!)
           (case state
             [(ANNOUNCING)
              (when announce-timer
                (ev-timer-stop announce-timer)
                (set! announce-timer #f))
              (state-set! (pea-state STOPPED))
              'ACK]
             [(PLAY PLAYING PAUSING PAUSED)
              (set! state (pea-state STOP))
              (player 'stop!)
              'ACK]
             [else
               `(DOH "Cannot stop! from current state" ,state)])]
          [(toggle!)	; as in toggle pause.
           (case state
             [(PAUSED PLAYING)
               (player 'toggle!)
               'ACK]
             [else
              `(DOH "Cannot toggle! pause from current state" ,state)])]
          [(seek!)
           (case state
             [(PAUSED PLAYING)
              (apply player input)
              'ACK]
             [else
              `(DOH "Cannot seek! from current state" ,state)])]

          ;;;; VFS navigation.
          [(enter!)
           (vfs-enter! vfs (cursor-index cursor))
           (cursor-sync! cursor)
           (cursor-save cursor)
           (ack-mcast (make-vfs-info))]
          [(move!)
           ;; Only signal if there was a change of position.
           (let ([new-pos (cursor-move! cursor (arg input))])
             (cond
               [new-pos
                 (ack-mcast (make-vfs-info))
                 (case state
                   [(PLAYING)
                    ;; Moving selection while playing will attempt to play the newly selected track.
                    ;; NB: This doesn't work for mpv video as that driver has to guess the reason
                    ;; for entering IDLE state. For now, it assumes user stop! rather than move!.
                    (controller 'stop!)
                    (case (track-type (current-track))
                      [(AUDIO)
                       (controller 'play!)]
                      [(VIDEO)
                       (announce-track)]
                      [else
                        'ACK])]
                   [else
                     'ACK])]
               [else
                 '(DOH "move! cursor unchanged")]
               ))]
          [(pop!)
           (vfs-pop! vfs)
           (cursor-sync! cursor)
           (ack-mcast (make-vfs-info))]
          [(root!)
           (vfs-root! vfs)
           (cursor-sync! cursor)
           (ack-mcast (make-vfs-info))]
          [(refresh!)	; reload current vlist.
           ;; This is a brute-force reload.
           ;; It should (at some point) mcast only when there's changes.
           (vfs-rebuild! vfs)
           (ack-mcast (make-vfs-info))]
          ;; Server quit. Note the double exclamations: this is an important command!!
          [(quit!!)
           (ev-break (evbreak 'ALL))
           (ack-mcast '(BYE "server end: goodbye"))]

          ;;;; Client query commands.
          [(len?)
           track-length]
          [(pos?)
           track-pos]
          [(state?)
           (make-state-info)]
          [(tags?)
           track-tags]
          [(tracks?)
           (make-track-list vfs)]
          [(vfs?)
           (make-vfs-info)]

          ;;;; Player change state commands. Cache and multicast them.
          [(STOPPED)
           ;; Reset track cache data.
           (set! track-tags '())
           (set! track-length #f)
           (set! track-pos #f)
           (unless (play-another)
             (state-set! (pea-state STOPPED)))]
          [(PLAYING)
           (state-set! (pea-state PLAYING))]
          [(PAUSED)
           (state-set! (pea-state PAUSED))]
          [(UNPAUSED)
           (state-set! (pea-state PLAYING))]

          ;;;; Player informational messages. Cache and multicast them.
          [(LEN)
           (set! track-length (arg input))
           (write-now input mcast)]
          [(POS)
           (set! track-pos (arg input))
           (write-now input mcast)]
          [(TAGS)
           (set! track-tags (arg input))
           (write-now input mcast)]
          ;; mpv debug message.
          [(MPV)
           (write-now input mcast)]

          ;; HMMM both player and controller need to refer to each other. Not sure I like it this way..
          [(set-player!)
           (set! player (arg input))]

          ;; HMMM add a help command?
          [else
            'eh?]))

      ;; AHOJ Notifies any watching clients that this server is now up.
      (write-now '(AHOJ "pea: i live again...") mcast)

      controller))

  ;; [proc] make-ui-track-list: return current playlist tracks with info that a UI would find useful.
  ;; ie,
  ;; '(TRACKS (("title-string" . TYPE) ...))
  (define make-track-list
    (lambda (vfs)
      `(TRACKS
         ,(map
            (lambda (t)
              (cons (track-title t) (track-type t)))
            (vfs-tracks vfs)))))

  (define make-stdin-reader
    (lambda (controller)
      (lambda (w revent)
        (let ([in (read-trim-right (current-input-port))])
          (cond
            [(eof-object? in)
             (ev-io-stop w)
             ;; translate EOF to quit command.
             (controller 'quit!!)]
            [else
              ;; Allow server console control commands.
              ;; TODO use same exception handling from make-control-client.
              (controller in)]
            )))))
  )
