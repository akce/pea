#! /usr/bin/scheme --script

(suppress-greeting #t)
(debug-on-exception #t)

(import
  (chezscheme)
  (pea server))

;; TODO load config.

(define service "49000")
(define mcast-node "224.0.0.49")
(define ctrl-node #f)

(define default-playlist "root.m3u")

(display "root playlist @ ")(display default-playlist)(newline)
(display "pea server control @ ")(display ctrl-node)(display ":")(display service)(newline)
(display "pea multicast @ ")(display mcast-node)(display ":")(display service)(newline)
(init default-playlist ctrl-node service mcast-node service)
(run)
