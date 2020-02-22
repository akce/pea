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

  (define init
    (lambda (root-playlist-path state-file ctrl-node ctrl-service mcast-node mcast-service)
      (local
        [vfs (make-vfs root-playlist-path)]
        [cursor (make-cursor vfs state-file)]
        [ctrl-sock (connect-server-socket
                     ctrl-node ctrl-service
                     (address-family inet) (socket-domain stream)
                     (address-info all numerichost passive) (ip-protocol tcp))])
      ;;;;; Create the Control socket event watcher.
      (ev-io (socket-fd ctrl-sock) (evmask 'READ)
        (lambda (w revent)
          ;; This function is called when there's read activity on the Control socket.
          ;; ie, when a client is ready to connect.
          ;; TODO add abstraction for all of client-* to (socket extended)?
          (let* ([client-sock (socket-accept ctrl-sock)]
                 [client-fd (socket-fd client-sock)]
                 [client-port (transcoded-port (socket-input/output-port client-sock) (native-transcoder))])
            ;;;; Create the client socket event watcher.
            ;; The client function will read client commands, action them, and respond.
            (ev-io client-fd (evmask 'READ)
              (lambda (cw crevent)
                ;;;; WARNING: Using 'read' here assumes that clients are well behaved and send fully formed sexprs.
                ;;;; WARNING: A partial datum will cause read to block the thread as it waits for a complete message.
                (let ([cmd (read client-port)])
                  (cond
                    [(eof-object? cmd)
                     (display "close client: ")(display client-fd)(newline)
                     (ev-io-stop cw)]
                    [else
                      ;; process command.
                      (display "client command: ")(display cmd)(newline)
                      (write (do-command cmd vfs cursor) client-port)
                      (flush-output-port client-port)]))))
            (display "new client ")(display client-fd)(newline))))))

  (define do-command
    (lambda (cmd vfs cursor)
      (define arg
        (lambda ()
          (cadr cmd)))
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

  ;; [syntax] local: short-hand for batch defines.
  (define-syntax local
    (syntax-rules ()
      [(_ (name val) ...)
       (begin
         (define name val) ...)]))
  )
