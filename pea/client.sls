;; PEA common client code.
(library
  (pea client)
  (export
    make-pea-client
    seconds->string
    )
  (import
    (rnrs)
    (only (chezscheme) format)
    (only (pea util) arg command my input-port-ready? read-trim-right write-now)
    (ev)
    (socket extended)
    )

  (define-record-type model
    (fields
      ;; PEA server details, mostly if the client needs to reconnect.
      [immutable	ctrl-node]
      [immutable	ctrl-service]
      [immutable	mcast-node]
      [immutable	mcast-service]

      ;; VFS/playlist.
      [mutable		vpath]
      [mutable		cursor]
      [mutable		title]
      [mutable		type]

      ;; Track/playback.
      [mutable		state]
      [mutable		pos]
      [mutable		duration]
      [mutable		tags]

      [mutable		tracks]
      )
    (protocol
      (lambda (new)
        (lambda (ctrl-node ctrl-service mcast-node mcast-service)
          (new ctrl-node ctrl-service mcast-node mcast-service
               ;; vfs/playlist
               #f #f #f #f
               ;; track/playback
               #f #f #f '()
               ;; tracks
               #f)))))

  (define make-pea-client
    (lambda (ctrl-node ctrl-service mcast-node mcast-service)
      (my
        [model		(make-model ctrl-node ctrl-service mcast-node mcast-service)]
        [ctrl-sock	(connect-client-socket
                          ctrl-node ctrl-service
                          (address-family inet) (socket-domain stream)
                          (address-info v4mapped addrconfig) (ip-protocol tcp))]
        [mcast-sock	(connect-server-socket
                          mcast-node mcast-service
                          (address-family inet) (socket-domain datagram)
                          (address-info addrconfig numericserv)
                          (ip-protocol udp))]
        [ctrl-port	(socket->port ctrl-sock)]
        [mcast-port	(socket->port mcast-sock)]
        [ctrl-handler	(make-msg-handler model control-msg-handler)]
        [mcast-handler	(make-msg-handler model mcast-msg-handler)]
        [ui-handler	(make-ui-handler model ctrl-port ctrl-handler mcast-handler)])

      (mcast-add-membership mcast-sock mcast-node)

      ;; Watch for PEA control port responses.
      ;; Messages on the control port will only contain PEA server responses to ui commands.
      (ev-io (socket-fd ctrl-sock) (evmask 'READ)
             (make-watcher ctrl-handler ctrl-port ctrl-sock 'pea-control-client))

      ;; Watch for PEA multicast global status messages etc.
      (ev-io (socket-fd mcast-sock) (evmask 'READ)
             (make-watcher mcast-handler mcast-port mcast-sock 'pea-mcast-client))

      ui-handler))

  ;; handle messages from the ui.
  (define make-ui-handler
    (lambda (model ctrl-port . handlers)
      (define client #f)
      (lambda (input)
        (case (command input)
          [(client-set!)
           ;; HMMM maybe use a separate 'debug-client-set! command.
           (set! client (arg input))
           (for-each
             (lambda (h)
               (h input))
             handlers)
           'ACK]
          [(cached-cursor?)
           (model-cursor model)]
          [(cached-len?)
           (model-duration model)]
          [(cached-pos?)
           (model-pos model)]
          [(cached-state?)
           (model-state model)]
          [(cached-tags?)
           (model-tags model)]
          [(cached-tracks?)
           (model-tracks model)]
          [(cached-type?)
           (model-type model)]
          [(cached-vpath?)
           (model-vpath model)]
          [else
            ;; TODO verify command.
            (when client
              (client `(client-command ,input)))
            (write-now input ctrl-port)
            #f]))))

  (define make-msg-handler
    (lambda (model msg-handler)
      (my
        [client	#f])
      (lambda (input)
        (let ([client-msg
                (case (command input)
                  [(client-set!)
                   (set! client (arg input))
                   #f]
                  [else
                    (msg-handler model input)])])
          (when (and client client-msg)
            (client client-msg))))))

  (define cache-message-data
    (lambda (model msg)
      (case (command msg)
        [(POS)
         (model-pos-set! model (arg msg))]
        [(LEN)
         (model-duration-set! model (arg msg))]
        [(STATE)
         (model-state-set! model (state-info-state msg))
         (model-pos-set! model (state-info-track-pos msg))
         (model-duration-set! model (state-info-track-length msg))
         (model-tags-set! model (state-info-track-tags msg))]
        [(TAGS)
         (model-tags-set! model (arg msg))]
        [(TRACKS)
         (model-tracks-set! model (arg msg))]
        [(VFS)
         (model-vpath-set! model (vfs-info-vpath msg))
         (model-cursor-set! model (vfs-info-cursor msg))
         (model-title-set! model (vfs-info-track-title msg))
         (model-type-set! model (vfs-info-track-type msg))]
        )))

  (define mcast-msg-handler
    (lambda (model msg)
      (cache-message-data model msg)
      `(mcast-message ,msg)))

  (define control-msg-handler
    (lambda (model msg)
      (cache-message-data model msg)
      `(control-message ,msg)))

  ;; generic event watcher function.
  ;; 'read' data will be passed onto the handler.
  (define make-watcher
    (lambda (handler port sock error-id)
      (lambda (w revent)
        (let loop ([data (read-trim-right port)])
          (cond
            [(eof-object? data)
             (ev-io-stop w)
             (socket-shutdown sock (shutdown-method read write))
             (socket-close sock)
             (close-port port)
             ;; TODO try re-open at intervals.
             (error error-id "remote socket closed")]
            [else
              (handler data)
              ;; drain input port.
              (when (input-port-ready? port)
                (loop (read-trim-right port)))])))))

  ;;;; Client util functions.
  ;; These could be useful to multiple clients so could be moved into a (client util) lib.

  (define state-info-state
    (lambda (i)
      (list-ref i 1)))
  (define state-info-track-pos
    (lambda (i)
      (list-ref i 2)))
  (define state-info-track-length
    (lambda (i)
      (list-ref i 3)))
  (define state-info-track-tags
    (lambda (i)
      (list-ref i 4)))
  (define vfs-info-vpath
    (lambda (i)
      (list-ref i 1)))
  (define vfs-info-cursor
    (lambda (i)
      (list-ref i 2)))
  (define vfs-info-track-title
    (lambda (i)
      (list-ref i 3)))
  (define vfs-info-track-type
    (lambda (i)
      (list-ref i 4)))

  (define pad-num
    (lambda (num)
      (format "~2,'0d" num)))

  (define seconds->string
    (lambda (seconds)
      (my
        [mins (div seconds 60)]
        [hours (div mins 60)])
      (string-append (pad-num hours) ":" (pad-num (mod mins 60)) ":" (pad-num (mod seconds 60)))))
  )
