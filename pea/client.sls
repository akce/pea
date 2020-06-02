;; PEA common client code.
(library
  (pea client)
  (export
    make-pea-client
    seconds->string
    pad-num
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
        (lambda ()
          (new
            ;; vfs/playlist
            #f #f #f #f
            ;; track/playback
            #f #f #f '()
            ;; tracks
            #f)))))

  (define make-pea-client
    (lambda (mcast-node mcast-service mcast-ui-callback)
      ;; Store config in the model and connect the multicast socket.
      ;; There shouldn't be a problem on the multicast, it'll be used to watch
      ;; for pea servers coming online (via their ahoj message).
      ;; It'll be up to the UI if they want a control channel.
      (my
        [model		(make-model)]
        [mcast-sock	(connect-server-socket
                          mcast-node mcast-service
                          (address-family inet) (socket-domain datagram)
                          (address-info addrconfig numericserv)
                          (ip-protocol udp))]
        [ui-handler	(make-ui-handler model)])
      (mcast-add-membership mcast-sock mcast-node)

      ;; Watch for PEA multicast global status messages etc.
      (ev-io (socket-file-descriptor mcast-sock) (evmask 'READ)
        (make-mcast-ev-watcher
          mcast-sock
          (lambda (peer input)
            (cache-message-data model input)
            (mcast-ui-callback peer input))))

      ui-handler))

  ;; handle messages from the ui.
  (define make-ui-handler
    (lambda (model)
      (my
        [cc-watcher	#f]
        [ctrl-sock	#f]
        [ctrl-port	#f])

      (define connect-control
        (lambda (ctrl-node ctrl-service ctrl-ui-callback)
          ;; HMMM cache ctrl-node/ctrl-service to allow for a reconnect command?
          (set! ctrl-sock
            (connect-client-socket
              ctrl-node ctrl-service
              (address-family inet) (socket-domain stream)
              (address-info v4mapped addrconfig) (ip-protocol tcp)))
          (when cc-watcher
            (cc-watcher `(debug (connect-control ,ctrl-node ,ctrl-service ,ctrl-ui-callback))))
          (cond
            [ctrl-sock
              (set! ctrl-port (socket->port ctrl-sock))

              ;; Watch for PEA control port responses.
              ;; Messages on the control port will only contain PEA server responses to ui commands.
              (ev-io (socket-file-descriptor ctrl-sock) (evmask 'READ)
                     (make-control-ev-watcher
                       ctrl-sock ctrl-port
                       (lambda (input)
                         (cache-message-data model input)
                         (ctrl-ui-callback input))))
              'ACK]
            [else
                ;; TODO handle failure. ie, client waits for AHOJ message from mcast.
                'DOH])
          ))

      (lambda (input)
        (case (command input)
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
          [(make-control-connection!)
           (connect-control (list-ref input 1) (list-ref input 2) (list-ref input 3))]
          [(set-client-command-watcher!)
           ;; Lets interested client UIs see commands sent to server.
           (set! cc-watcher (arg input))
           'ACK]
          [else
            ;; TODO verify 'input' command.
            (when cc-watcher
              (cc-watcher `(client-command ,input)))
            ;; HMMM have a specific send command so that unknown/malformed messages aren't sent?
            ;; HMMM it'd also save some processing power to put this before the set-*! messages.
            (when ctrl-port
              (write-now input ctrl-port))
            #f]))))

  ;; extract and save useful data received from pea-server.
  (define cache-message-data
    (lambda (model msg)
      (case (command msg)
        [(POS)
         (model-pos-set! model (arg msg))]
        [(LEN)
         (model-duration-set! model (arg msg))]
        [(STATE)
         (model-state-set! model (state-info-state msg))
         (case (model-state model)
           [(ANNOUNCING)
             (model-pos-set! model #f)
             (model-duration-set! model #f)
             (model-tags-set! model '())]
           [else
             (model-pos-set! model (state-info-track-pos msg))
             (model-duration-set! model (state-info-track-length msg))
             (model-tags-set! model (state-info-track-tags msg))])]
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

  ;; Read scheme datums from socket and pass onto handler function.
  (define make-mcast-ev-watcher
    (lambda (sock handler)
      (lambda (w revent)
        (let ([pkt (socket-recvfrom sock 1024)])
          ;; Check that source is one we're interested in.
          (define peer (socket-recvfrom-peerinfo pkt (name-info nofqdn numericserv)))
          (socket-recvfrom-free pkt)
          ;; PEA multicast packets only contain one datum so there's no need to drain the port.
          (handler
            peer
            (read
              (open-string-input-port (bytevector->string (car pkt) (native-transcoder)))))))))

  ;; Read scheme datums from socket and pass onto handler function.
  (define make-control-ev-watcher
    (lambda (sock port handler)
      (lambda (w revent)
        (let loop ([data (read-trim-right port)])
          (cond
            [(eof-object? data)
             ;; Remote has closed, so some of this is probably unnecessary but isn't harmful either.
             (socket-shutdown sock (shutdown-method read write))
             (socket-close sock)
             (close-port port)
             ;; Notify UI of server end. It's up to them whether to reconnect or exit.
             (handler '(BYE "pea server closed connection"))
             ;; TODO cleanup ev-io watcher things.
             (ev-io-stop w)
             ]
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
