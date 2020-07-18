;; PEA common client code.
(library
  (pea client)
  (export
    make-pea-client
    seconds->string
    pad-num
    ;; Model of pea server state.
    make-model
    model-vpath model-cursor model-title model-type model-state model-pos model-duration model-tags model-tracks
    cache-message-info
    ;; Server message accessors.
    state-info-state state-info-track-pos state-info-track-length state-info-track-tags
    vfs-info-vpath vfs-info-cursor vfs-info-track-title vfs-info-track-type
    )
  (import
    (rnrs)
    (only (chezscheme) format)
    (only (pea util) arg command my input-port-ready? read-trim-right string-startswith? write-now)
    (ev)
    (socket extended)
    )

  (define make-pea-client
    (lambda (mcast-node mcast-service hostname mcast-ui-callback)
      ;; Only connect to the multicast socket.
      ;; There shouldn't be a problem on the multicast, it'll be used to watch
      ;; for pea servers coming online (via their ahoj message).
      ;; It'll be up to the UI if they want a control channel.
      (my
        [mcast-sock	(connect-server-socket
                          mcast-node mcast-service
                          (address-family inet) (socket-domain datagram)
                          (address-info addrconfig numericserv)
                          (ip-protocol udp))]
        [ui-handler	(make-ui-handler)]
        [process-pkt?	(make-packet-filter hostname mcast-ui-callback)])
      (mcast-add-membership mcast-sock mcast-node)

      ;; Watch for PEA multicast messages from hostname only.
      (ev-io (socket-file-descriptor mcast-sock) (evmask 'READ)
             (lambda (w revent)
               (let ([pkt (socket-recvfrom mcast-sock 1024)])
                 ;; Filter packets here so that unwanted ones are not cached. This will need to be revised
                 ;; if clients want the option of changing the host they're connected to at runtime.
                 (when (and (not (eof-object? pkt))
                            (process-pkt? pkt))
                   ;; PEA multicast datagrams only contain one datum so there's no need to drain the port.
                   (mcast-ui-callback
                     (read (open-string-input-port (bytevector->string (car pkt) (native-transcoder)))))))))

      ui-handler))

  ;; [proc] make-packet-filter: create a predicate that matches hostname sockaddr.
  ;; This caches the sockaddr for hostname and for all ignored hosts with the idea that a bytevector compare
  ;; is cheaper than hostname lookups.
  ;; This assumes that the number of pea servers multicasting in the LAN will be low.
  ;; The ui-callback is only here for debug messages and can be removed if they're no longer desired.
  (define make-packet-filter
    (lambda (hostname ui-callback)
      (define host-sockaddr #f)
      (define seen-sockaddr '())
      (lambda (pkt)
        (cond
          [host-sockaddr
            (bytevector=? host-sockaddr (cdr pkt))]
          [(memp (lambda (sa)
                   (bytevector=? sa (cdr pkt))) seen-sockaddr)
           ;; discard addresses that we've seen that aren't the host.
           #f]
          [else
            (let ([peer (getnameinfo (cdr pkt) (name-info nofqdn numericserv))])
              (cond
                ;; Only checking the start of the string as getnameinfo will still return the localnet suffix.
                ;; eg, "atlantis.localnet".
                ;; This is despite the nofqdn flag...
                [(string-startswith? (car peer) hostname)
                 (set! host-sockaddr (cdr pkt))
                 (ui-callback `(debug (hostname found ,hostname ,(car peer) ,host-sockaddr)))
                 #t]
                [else
                  (ui-callback `(debug (hostname ignore ,hostname ,(car peer) ,host-sockaddr)))
                  (set! seen-sockaddr (cons (cdr pkt) seen-sockaddr))
                  #f]))]))))

  ;; handle messages from the ui.
  (define make-ui-handler
    (lambda ()
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
                     (make-control-ev-watcher ctrl-sock ctrl-port ctrl-ui-callback))
              'ACK]
            [else
                ;; TODO handle failure. ie, client waits for AHOJ message from mcast.
                'DOH])
          ))

      (lambda (input)
        (case (command input)
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

  ;; [proc] cache-message-info: Extract and save useful info received from pea-server.
  (define cache-message-info
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
         ;; Only update vpath cache value if it has changed. ie, don't store a #f value.
         (cond
           [(vfs-info-vpath msg) =>
            (lambda (vp)
              (model-vpath-set! model vp))])
         (model-cursor-set! model (vfs-info-cursor msg))
         (model-title-set! model (vfs-info-track-title msg))
         (model-type-set! model (vfs-info-track-type msg))]
        )))

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
