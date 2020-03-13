;; PEA common client code.
(library
  (pea client)
  (export
    make-pea-client)
  (import
    (rnrs)
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
      [mutable		tracks]

      ;; Track/playback.
      [mutable		state]
      [mutable		pos]
      [mutable		duration]
      [mutable		title]
      [mutable		tags]
      [mutable		last-msg])
    (protocol
      (lambda (new)
        (lambda (ctrl-node ctrl-service mcast-node mcast-service)
          (new ctrl-node ctrl-service mcast-node mcast-service
               #f #f #f
               #f #f #f #f #f #f)))))

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
      (lambda (input)
        (case (command input)
          [(client-set!)
           (for-each
             (lambda (h)
               (h input))
             handlers)
           'ACK]
          [(len)
           (model-duration model)]
          [(pos)
           (model-pos model)]
          [(tags)
           (model-tags model)]
          [else
            ;; TODO verify command.
            ;;(display "ui->server: ")(write input)(newline)
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

  (define mcast-msg-handler
    (lambda (model msg)
      ;;(display "mcast recv: ")(write msg)(newline)
      (case (command msg)
        [(POS)
         (model-pos-set! model (arg msg))]
        [(LEN)
         (model-duration-set! model (arg msg))]
        [(STATE)
         (model-state-set! model (arg msg))]
        [(TAGS)
         (model-tags-set! model (arg msg))]
        #;[else
          msg]
        )
    msg))

  (define control-msg-handler
    (lambda (model msg)
      (case (command msg)
        [else
          msg]
        )))

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
  )
