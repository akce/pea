#! /usr/bin/chez-scheme --program

#;(suppress-greeting #t)
#;(debug-on-exception #t)

(import
  (rnrs)
  (only (chezscheme) command-line-arguments)
  (socket extended)
  (pea server))

;; TODO load config.

(define service "49000")
(define mcast-node "224.0.0.49")
(define ctrl-node #f)

(define default-playlist "root.m3u")
(define default-statefile "pea-state.scm")

(define display-params
  (lambda ()
    (display "root playlist @ ")(display default-playlist)(newline)
    (display "state savefile @ ")(display default-statefile)(newline)
    (display "pea server control @ ")(display ctrl-node)(display ":")(display service)(newline)
    (display "pea multicast @ ")(display mcast-node)(display ":")(display service)(newline)))

;; Enable SO_REUSEADDR for all created sockets. See socket(7).
(create-socket-reuseaddr #t)

;; TODO this should be data driven...
(let loop ([args (command-line-arguments)])
  (cond
    [(null? args)
     (display-params)
     ((make-pea-server default-playlist default-statefile ctrl-node service mcast-node service))]
    [(string=? "--help" (car args))
     (display "RTS for now..")(newline)]
    [(string=? "--root" (car args))
      (set! default-playlist (cadr args))
      (loop (cddr args))]
    [else
      (display "skipping unknown arg ")(display (car args))(newline)
      (loop (cdr args))]
     ))
