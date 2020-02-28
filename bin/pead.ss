#! /usr/bin/chez-scheme --script

(suppress-greeting #t)
(debug-on-exception #t)

(import
  (rnrs)
  (pea server))

;; TODO load config.

(define service "49000")
(define mcast-node "224.0.0.49")
(define ctrl-node #f)

(define default-playlist "root.m3u")
(define default-statefile "pea-state.scm")

(display "root playlist @ ")(display default-playlist)(newline)
(display "state savefile @ ")(display default-statefile)(newline)
(display "pea server control @ ")(display ctrl-node)(display ":")(display service)(newline)
(display "pea multicast @ ")(display mcast-node)(display ":")(display service)(newline)
((make-pea-server default-playlist default-statefile ctrl-node service mcast-node service))
