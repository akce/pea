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
    init
    run
    )
  (import
    (rnrs)
    (only (chezscheme) display-condition)
    (pea player)
    (pea playlist)
    (only (pea util) define-enum my)
    (pea vfs)
    ;; 3rd party libs.
    (ev)
    (socket extended)
    )

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
  (define-enum state
    [PAUSE	'PAUSE]		; Client requested pause.
    [PLAY	'PLAY]		; Client requested play.
    [STOP	'STOP]		; Client requested stop.
    [PAUSED	'PAUSED]	; Player state PAUSED.
    [PLAYING	'PLAYING]	; Player state PLAYING.
    [STOPPED	'STOPPED]	; Played state STOPPED.
    )

  (define-record-type pea
    (fields
      [immutable	vfs]
      [immutable	cursor]
      [immutable	ctrl]		; control socket.
      [immutable	mcast-sock]	; multi-cast socket.
      [immutable	mcast-port]	; multi-cast port.
      [mutable		state]		; pea state. ie, (state *).
      )
    (protocol
      (lambda (new)
        (lambda (vfs cursor ctrl mcast)
          (let ([mcast-port (socket->port mcast)])
            (new vfs cursor ctrl mcast mcast-port (state STOPPED)))))))

  (define init
    (lambda (root-playlist-path state-file ctrl-node ctrl-service mcast-node mcast-service)
      (my
        [vfs	(make-vfs root-playlist-path)]
        [cursor	(make-cursor vfs state-file)]
        [ctrl	(connect-server-socket
                  ctrl-node ctrl-service
                  (address-family inet) (socket-domain stream)
                  (address-info all numerichost passive) (ip-protocol tcp))]
        [mcast	(connect-client-socket
                  mcast-node mcast-service
                  (address-family inet) (socket-domain datagram)
                  (address-info all numerichost)
                  (ip-protocol udp))]
        [pea	(make-pea vfs cursor ctrl mcast)])

      ;; Handle stdin commands. ie, server exit.
      (ev-io 0 (evmask 'READ) (make-stdin-reader pea))

      ;; Create the Control socket event watcher.
      (ev-io (socket-fd (pea-ctrl pea)) (evmask 'READ) (make-control-server pea))

      (init-player (make-player-event-handler pea))
      pea))

  (define make-control-server
    (lambda (pea)
      (lambda (w revent)
        ;; This function is called when there's read activity on the Control socket.
        ;; ie, when a client is ready to connect.
        ;; TODO add abstraction for all of client-* to (socket extended)?
        (let ([client-sock (socket-accept (pea-ctrl pea))])
          ;; Create the client socket event watcher.
          (ev-io (socket-fd client-sock) (evmask 'READ) (make-control-client pea client-sock))
          ;; TODO show client address.
          (display "new client ")(display (socket-fd client-sock))(newline)))))

  (define make-control-client
    (lambda (pea client-sock)
      ;; The control client will read commands, action them, and respond.
      (my
        [client-port		(socket->port client-sock)]
        [input-processor	(make-input-processor pea)])
      ;; Return a welcome message.
      (write-now '(MESSAGE "Welcome to PEA! :)") client-port)

      (lambda (w revent)
        ;; WARNING: Using 'read' here assumes that clients are well behaved and send fully formed sexprs.
        ;; WARNING: A partial datum will cause read to block the thread as it waits for a complete message.
        (let ([input (read client-port)])
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
              (display "client command: ")(display input)(newline)
              (guard
                (e [else
                     ;; TODO define condition->string such that string quotes are escaped..
                     ;; TODO then do some proper server logging..
                     (display-condition e)
                     ;; TODO and then define a client message type.
                     (display-condition e client-port)])
                (write (input-processor input) client-port))
              (flush-output-port client-port)])))))

  (define make-input-processor
    (lambda (pea)
      (my
        [cursor	(pea-cursor pea)]
        [vfs	(pea-vfs pea)]
        [mcast	(pea-mcast-port pea)])

      (lambda (input)
        (my
          ;; sanitise 1st arg (if any) of input.
          [command (lambda ()
                     (cond
                       [(null? input)
                        'EMPTY]
                       [(list? input)
                        (car input)]
                       [else
                         input]))]
          [arg (lambda ()
                 (cadr input))])

        ;; List these commands in alphabetic order within their groupings.
        (case (command)
          ;;;; Player commands.
          [(play!)
           ;; For now, only allow PLAY from STOPPED state. In future, play might be able to interrupt
           ;; any state to re-start or resume play or play from a new cursor position.
           (case (pea-state pea)
             [(STOPPED)
              (pea-state-set! pea (state PLAY))
              (play (list-ref (vfs-tracks vfs) (cursor-index cursor))
                    (vfs-current vfs))
              (cursor-save cursor)])]
          [(stop!)
           (case (pea-state pea)
             [(PLAY PLAYING PAUSING PAUSED)
              (pea-state-set! pea (state STOP))
              (stop)])]
          [(toggle!)	; as in toggle pause.
           (case (pea-state pea)
             [(STOP STOPPED)
              (if #f #f)]
             [else
               (toggle-pause)])]
          ;;;; VFS navigation.
          [(enter!)
           ;; Ensure enter! then sync!
           (let* ([vpath (vfs-enter! vfs (cursor-index cursor))]
                  [pos (cursor-sync! cursor)])
             (write-now `(VPATH ,pos ,vpath) mcast))
           (cursor-save cursor)]
          [(move!)
           ;; Only signal if there was a change of position.
           (let ([new-pos (cursor-move! cursor (arg))])
             (if new-pos
                 (write-now `(POS ,new-pos) mcast)))]
          [(pop!)
           ;; Ensure pop! then sync!
           (let* ([vpath (vfs-pop! vfs)]
                  [pos (cursor-sync! cursor)])
             (write-now `(VPATH ,pos ,vpath) mcast))]
          [(pos)
           (cursor-index cursor)]
          [(root!)
           ;; Ensure root! then sync!
           (let* ([vpath (vfs-root! vfs)]
                  [pos (cursor-sync! cursor)])
             (write-now `(VPATH ,pos ,vpath) mcast))]
          [(tracks)
           (make-ui-track-list vfs)]
          [(vpath)	; get current vpath
           (vfs-vpath vfs)]
          ;; TODO add a help command?
          [else
            'eh?]))))

  ;; [proc] make-ui-track-list: return current playlist tracks with info that a UI would find useful.
  ;; ie,
  ;; '(TRACKS (("title-string" . TYPE) ...))
  (define make-ui-track-list
    (lambda (vfs)
      (list 'TRACKS
        (map
          (lambda (t)
            (cons (track-title t) (track-type t)))
          (vfs-tracks vfs)))))

  (define run
    (lambda (pea)
      (let ([msg '(MESSAGE "pea: i live again...")])
        (display (cadr msg))(newline)
        (write-now msg (pea-mcast-port pea)))
      (ev-run)))

  (define make-stdin-reader
    (lambda (pea)
      (lambda (w revent)
        (let ([in (read (current-input-port))])
          (cond
            [(eof-object? in)
             (let ([msg '(MESSAGE "server end: goodbye")])
               (display (cadr msg))(newline)
               (write-now msg (pea-mcast-port pea)))
             (ev-io-stop w)
             (ev-break (evbreak 'ALL))]
             ;; TODO add some server console control commands?
             )))))

  ;; [proc] socket->port: shortcut for creating a text port from a binary socket
  (define socket->port
    (lambda (sock)
      (transcoded-port (socket-input/output-port sock) (native-transcoder))))

  ;; [proc] make-player-event-handler: player state change callback.
  (define make-player-event-handler
    (lambda (pea)
      (lambda (msg)
        ;; Certain messages, eg state changes, require action within pea and may need to be translated.
        (let ([xmsg (case (car msg)
                      [(STOPPED)
                       (pea-state-set! pea (state STOPPED))
                       '(STATE STOPPED)]
                      [(PLAYING)
                       (if (eq? (pea-state pea) (state PLAYING))
                           #f
                           (begin
                             (pea-state-set! pea (state PLAYING))
                             '(STATE PLAYING)))]
                      [(PAUSED)
                       (if (eq? (pea-state pea) (state PAUSED))
                           #f
                           (begin
                             (pea-state-set! pea (state PAUSED))
                             '(STATE PAUSED)))]
                      [(UNPAUSED)
                       (if (eq? (pea-state pea) (state PLAYING))
                           #f
                           (begin
                             (pea-state-set! pea (state PLAYING))
                             '(STATE PLAYING)))]
                      [else
                        msg])])
          (when xmsg
            (display xmsg)(newline)
            (write-now xmsg (pea-mcast-port pea)))))))

  ;; [proc] write-now: write message and flush port.
  ;; If I had a dollar for the number of times i've forgotten to flush after write....
  (define write-now
    (lambda (msg port)
      (write msg port)
      (flush-output-port port)))
  )
