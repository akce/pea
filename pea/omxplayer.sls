(library (pea omxplayer)
  (export
    make-omxplayer)
  (import
    (rnrs)
    (ev)
    (only (chezscheme) input-port-ready? open-process-ports port-file-descriptor)
    )

  (define make-omxplayer
    (case-lambda
      [(controller)
       (make-omxplayer controller "/usr/bin/omxplayer" "both")]
      [(controller omxbin audio-dev)
       (define base-command (string-append "exec " omxbin " -o " audio-dev " "))
       (define state 'STOPPED)
       (define omx-stdin #f)

       (define make-omxwatcher
         (lambda (from-stdout)
           (ev-io
             (port-file-descriptor from-stdout)
             (evmask 'READ)
             (let ([last-pos -1])
               (lambda (w revent)
                 (let ([x (get-line from-stdout)])
                   ;; omxplayer doesn't output position as it's playing so we're only interested in exit.
                   (when (eof-object? x)
                     (ev-io-stop w)
                     (set! omx-stdin #f)
                     (set! state 'STOPPED)
                     (controller state))))))))

       (define omx-set!
         (lambda (key)
           (write key omx-stdin)
           (flush-output-port omx-stdin)))

       (lambda input
         (case (car input)
           [(play!)
            (when (eq? state 'STOPPED)
              (let-values
                ([(to-stdin from-stdout from-stderr process-id)
                  (open-process-ports (string-append base-command (quote-string (cadr input)))
                                      (buffer-mode block) (native-transcoder))])
                (set! state 'PLAYING)
                (make-omxwatcher from-stdout)
                (controller state)))]
           [(toggle!)
            (case state
              [(PLAYING)
               (set! state 'PAUSED)]
              [(PAUSED)
               (set! state 'PLAYING)]
              [else
                (error 'omxplayer "Can only toggle! in playing or paused states. Not when: " state)])
            (omx-set! "p")
            ;; omxplayer gives no indication of changed state so assume success and return to controller.
            (controller state)]
           [(stop!)
            ;; have omx-watcher notify the controller when stopped.
            (omx-set! "q")]))]))

  (define quote-string
    (lambda (str)
      (string-append "\"" str "\"")))
  )
