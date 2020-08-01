;; Executable player wrapper.
(library (pea exewrap)
  (export
    make-exeplayer)
  (import
    (rnrs)
    (ev)
    (only (chezscheme) input-port-ready? open-process-ports port-file-descriptor)
    (only (pea util) my read-trim-right string-join write-now)
    )

  (define make-exeplayer
    (lambda (controller binary . args)
      (my
        [exe-command (apply string-join " " "exec" binary args)]
        [stdin #f]
        [stderr #f])

      (define make-proc-watcher
         (lambda (from-stdout)
           (ev-io
             (port-file-descriptor from-stdout)
             (evmask 'READ)
             (let ([last-pos -1])
               (lambda (w revent)
                 (let ([msg (read-trim-right from-stdout)])
                   (cond
                     [(eof-object? msg)
                      (ev-io-stop w)
                      (set! stdin #f)]
                     [else
                       (controller msg)])))))))

       (define send-command
         (lambda (msg)
           (write-now msg stdin)))

       (define start-player
         (lambda ()
           (let-values
             ([(to-stdin from-stdout from-stderr process-id)
               (open-process-ports exe-command (buffer-mode block) (native-transcoder))])
             (set! stdin to-stdin)
             (set! stderr from-stderr)
             (make-proc-watcher from-stdout))))

       ;; Start immediately, that way players can register themselves with the controller.
       (start-player)
       (lambda msg
         (cond
           [stdin
            (send-command msg)]
           [else
             (error #f exe-command (if (input-port-ready? stderr)
                                       (get-line stderr)
                                       "Amiga mod player is down."))]))))
  )
