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
    (only (pea util) arg command define-enum my input-port-ready? read-trim-right write-now)
    (pea vfs)
    ;; 3rd party libs.
    (ev)
    (socket extended)
    )

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
      ;; TODO unicast current state, so UIs can immediately draw themselves.

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
  ;; Using define-enum here to help catch mis-typed symbols, but probably not needed in the long term.
  (define-enum pea-state
    ;; Intermediate, client requested states. These are internal and not multicast.
    [PAUSE	'PAUSE]		; Client requested pause.
    [PLAY	'PLAY]		; Client requested play.
    [STOP	'STOP]		; Client requested stop.
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
            (write-now `(STATE ,new-state) mcast))))

      ;; [proc] make-info: make current i (cursor index position track-info).
      ;; [return] (I state index vpath current-track-title current-track-type)
      ;; Do not send track record as scheme readers need to understand (import) the track record def.
      (define make-info
        (lambda ()
          (let* ([i (cursor-index cursor)]
                 [t (list-ref (vfs-tracks vfs) i)])
            `(I
               ,state
               ,i
               ,(vfs-vpath vfs)
               ,track-tags
               ,track-pos
               ,track-length
               (,(track-title t) ,(track-type t))))))

      ;; [proc] play-another: attempt to play the next song in the current playlist.
      ;; [return] result of (controller play!) or #f if nothing left in playlist.
      (define play-another
        (lambda ()
          (define (start-play)
            (set! state (pea-state STOPPED))	; set STOPPED as play! currently requires it.
            (controller 'play!))
          (case state
            [(PLAYING)	; current mode is to keep playing, so try and move to the next track.
             (case (controller '(move! 1))
               [(ACK)
                (let ([t (list-ref (vfs-tracks vfs) (cursor-index cursor))])
                  (case (track-type t)
                    [(VIDEO)
                     ;; For upcoming VIDEO tracks, announce the title, and set a timer before playing.
                     ;; This allows for cancellation of continuous play.
                     ;; HMMM should there be an ANNOUNCE state?
                     (write-now `(ANNOUNCE ,announce-delay "Upcoming video" ,(track-title t)) mcast)
                     (set! announce-timer
                       (ev-timer announce-delay 0
                                 (lambda (timer i)
                                   (set! announce-timer #f)
                                   (start-play))))
                     ]
                    [else
                      ;; Otherwise, play immediately.
                      (start-play)]))]
               [else
                 ;; Nothing more in the playlist.
                 ;; HMMM should cursor move to position 0 before stopping?
                 #f])]
            [else
              #f])))

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; The controller function itself.
      ;; It's really just a state transition table.
      (define (controller input)
        ;; List these commands in alphabetic order within their groupings.
        (case (command input)
          ;;;; Player commands.
          [(play!)
           ;; For now, only allow PLAY from STOPPED state. In future, play might be able to interrupt
           ;; any state to re-start or resume play or play from a new cursor position.
           (case state
             [(STOPPED)
              (set! state (pea-state PLAY))
              ;; TODO check that play actually works.
              (player
                'play!
                (track-join-path
                  (vfs-current vfs)
                  (list-ref (vfs-tracks vfs) (cursor-index cursor))))
              (cursor-save cursor)
              'ACK]
             [else
               `(DOH "Can only play! from STOPPED state. Current state" ,state)])]
          [(stop!)
           (case state
             [(PLAY PLAYING PAUSING PAUSED)
              (set! state (pea-state STOP))
              (when announce-timer
                (ev-timer-stop announce-timer)
                (set! announce-timer #f))
              (player 'stop!)
              'ACK]
             [else
               `(DOH "Cannot stop! from current state" ,state)])]
          [(toggle!)	; as in toggle pause.
           (case state
             [(STOP STOPPED)
              `(DOH "Cannot toggle! pause from current state" ,state)]
             [else
               (player 'toggle!)
               'ACK])]

          ;;;; VFS navigation.
          [(enter!)
           ;; Ensure enter! then sync!
           (let* ([vpath (vfs-enter! vfs (cursor-index cursor))]
                  [pos (cursor-sync! cursor)])
             (cursor-save cursor)
             (ack-mcast `(VPATH ,pos ,vpath)))]
          [(move!)
           ;; Only signal if there was a change of position.
           (let ([new-pos (cursor-move! cursor (arg input))])
             (if new-pos
                 (ack-mcast (make-info))
                 '(DOH "move! cursor unchanged")))]
          [(pop!)
           ;; Ensure pop! then sync!
           (let* ([vpath (vfs-pop! vfs)]
                  [pos (cursor-sync! cursor)])
             (ack-mcast `(VPATH ,pos ,vpath)))]
          [(root!)
           ;; Ensure root! then sync!
           (let* ([vpath (vfs-root! vfs)]
                  [pos (cursor-sync! cursor)])
             (ack-mcast `(VPATH ,pos ,vpath)))]
          ;; Server quit. Note the double exclamations: this is an important command!!
          [(quit!!)
           (ev-break (evbreak 'ALL))
           (ack-mcast '(BYE "server end: goodbye"))]

          ;;;; Client query commands.
          [(info)		; includes player state, vpath and track.
           (make-info)]
          [(tracks)
           (make-ui-track-list vfs)]

          ;;;; Player change state commands. Multicast them.
          [(STOPPED)
           (unless (play-another)
             (state-set! (pea-state STOPPED))
             ;; Reset track cache data.
             (set! track-tags #f)
             (set! track-length #f)
             (set! track-pos #f))]
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
  (define make-ui-track-list
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
              (let ([msg (controller in)])
                (when msg
                  (write-now msg (current-output-port))))])))))
  )
