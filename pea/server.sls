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
    (pea playlist)
    (pea vfs)
    ;; 3rd party libs.
    (ev)
    (socket extended)
    )

  (define-record-type pea
    (fields
      [immutable	vfs]
      [immutable	cursor]
      [immutable	ctrl]		; control socket.
      #;[immutable	mcast]		; multi-cast socket.
      )
    (protocol
      (lambda (new)
        (lambda (vfs cursor ctrl #;mcast)
          (new vfs cursor ctrl)))))

  (define init
    (lambda (root-playlist-path state-file ctrl-node ctrl-service mcast-node mcast-service)
      (let* ([vfs	(make-vfs root-playlist-path)]
             [cursor	(make-cursor vfs state-file)]
             [pea	(make-pea vfs cursor
                          (connect-server-socket
                            ctrl-node ctrl-service
                            (address-family inet) (socket-domain stream)
                            (address-info all numerichost passive) (ip-protocol tcp)))])

        ;; Handle stdin commands. ie, server exit.
        (ev-io 0 (evmask 'READ) stdin-reader)

        ;;;;; Create the Control socket event watcher.
        (ev-io (socket-fd (pea-ctrl pea)) (evmask 'READ) (make-control-server pea))
        )))

  (define make-control-server
    (lambda (pea)
      (lambda (w revent)
        ;; This function is called when there's read activity on the Control socket.
        ;; ie, when a client is ready to connect.
        ;; TODO add abstraction for all of client-* to (socket extended)?
        (let ([client-sock (socket-accept (pea-ctrl pea))])
          ;;;; Create the client socket event watcher.
          (ev-io (socket-fd client-sock) (evmask 'READ) (make-control-client pea client-sock))
          ;; TODO show client address.
          (display "new client ")(display (socket-fd client-sock))(newline)))))

  (define make-control-client
    (lambda (pea client-sock)
      ;; The control client will read commands, action them, and respond.
      (define client-port (transcoded-port (socket-input/output-port client-sock) (native-transcoder)))
      (lambda (w revent)
        ;;;; WARNING: Using 'read' here assumes that clients are well behaved and send fully formed sexprs.
        ;;;; WARNING: A partial datum will cause read to block the thread as it waits for a complete message.
        (let ([cmd (read client-port)])
          (cond
            [(eof-object? cmd)
             ;; TODO show client address.
             (display "close client: ")(display (ev-io-fd-get w))(newline)
             (ev-io-stop w)
             (socket-shutdown client-sock (shutdown-method read write))
             (socket-close client-sock)
             (close-port client-port)]
            [else
              ;; process command.
              (display "client command: ")(display cmd)(newline)
              (write (do-command cmd pea) client-port)
              (flush-output-port client-port)])))))

  (define do-command
    (lambda (cmd pea)
      (local
        [arg (lambda ()
               (cadr cmd))]
        [cursor (pea-cursor pea)]
        [vfs (pea-vfs pea)])
      (cond
        [(null? cmd)
         #f]
        [else
          (case (car cmd)
            [(cursor-index)
             (cursor-index cursor)]
            [(cursor-move!)
             (cursor-move! cursor (arg))]
            [(vfs-enter!)
             (vfs-enter! vfs (cursor-index cursor))
             (cursor-sync! cursor)
             (cursor-save cursor)]
            [(vfs-pop!)
             (vfs-pop! vfs)
             (cursor-sync! cursor)]
            [(vfs-root!)
             (vfs-root! vfs)
             (cursor-sync! cursor)]
            [(vfs-tracks)
             (make-ui-track-list vfs)]
            [(vfs-vpath)	; get current vpath
             (vfs-vpath vfs)]
            ;; TODO add a help command?
            )])))

  ;; [proc] make-ui-track-list: return current playlist tracks with info that a UI would find useful.
  ;; ie,
  ;; '(TRACKS (("title-string" . TYPE) ...))
  (define make-ui-track-list
    (lambda (vfs)
      (list 'TRACKS
        (vector->list
          (playlist-map
            (lambda (t)
              (cons (track-title t) (track-type t)))
            (playlist-tracks (vfs-playlist vfs)))))))

  (define run
    (lambda ()
      (ev-run)))

  (define stdin-reader
    (lambda (w revent)
      (let ([in (read (current-input-port))])
        (cond
          [(eof-object? in)
           (display "server end: goodbye")(newline)
           (ev-io-stop w)
           (ev-break (evbreak 'ALL))]
          ;; TODO add some server console control commands?
          ))))

  ;; [syntax] local: short-hand for batch defines.
  (define-syntax local
    (syntax-rules ()
      [(_ (name val) ...)
       (begin
         (define name val) ...)]))
  )
