#! /usr/bin/chez-scheme --script

(suppress-greeting #t)
(debug-on-exception #t)

(import
  (rnrs)
  (only (chezscheme) iota)
  (ev)
  (pea client)
  (only (pea util) arg command my object->string read-trim-right safe-substring string-join write-now)
  (socket extended))

;; eg, server command aliases.
(define ui->server-command
  (lambda (input)
    (case (command input)
      [(a)
       (case (model-type model)
         [(AMIGA AUDIO VIDEO)
          'play!]
         [(DIR M3U PLS)
          'enter!])]
      [(ls)
       'tracks?]
      [else
        input])))

(define format-tracks
  (lambda (tracks)
    (apply string-join "\n"
           (map
             (lambda (i t)
               (string-append (pad-num i) " " (object->string t)))
             (iota (length tracks)) tracks))))

(define format-tags
  (lambda (ts)
    (apply string-join "\n"
             (map
               (lambda (t)
                 (string-append (car t) "\t" (cdr t)))
               ts))))

;; [proc] dress-up: converts result into something nice to display.
(define dress-up
  (lambda (msg)
    (case (command msg)
      [(LEN POS)
       (if (arg msg)
           `(,(command msg) ,(arg msg) ,(seconds->string (arg msg)))
           msg)]
      #;[(TAGS)
       ;;(display "TAGS:")(newline)
       (format-tags (arg msg))]
      [(TRACKS)
       (format-tracks (arg msg))]
      [else
        (object->string msg)]
      )))

(define mcast-msg-watcher
  (lambda (msg)
    (cache-message-info model msg)
    (case (command msg)
      [(POS)
       (if #f #f)]
      [else
        (display "* ")(display (dress-up msg))(newline)])
    ))

(define control-msg-watcher
  (lambda (msg)
    (cache-message-info model msg)
    (display "< ")(display (dress-up msg))(newline)))

(define outgoing-message-watcher
  (lambda (msg)
    (display "> ")(display (object->string msg))(newline)))

(my
  [model (make-model)]
  [controller #f]
  [service "49000"]
  [mcast-node "224.0.0.49"]
  [ctrl-node (gethostname)])

;; Enable SO_REUSEADDR for all created sockets. See socket(7).
(create-socket-reuseaddr #t)

(unless (null? (command-line-arguments))
  (set! ctrl-node (car (command-line-arguments))))

(set! controller
  (make-pea-client mcast-node service mcast-msg-watcher
                   ;; Include mcast-msg-watcher in packet filter to get packet filter debug messages.
                   (make-packet-filter ctrl-node mcast-msg-watcher)))
(controller `(set-client-command-watcher! ,outgoing-message-watcher))
(controller `(make-control-connection! ,ctrl-node ,service ,control-msg-watcher))

;; Handle stdin commands.
(ev-io 0 (evmask 'READ)
  (lambda (w revent)
    (let ([in (read-trim-right (current-input-port))])
      (cond
        [(eof-object? in)
         (ev-io-stop w)
         (ev-break (evbreak 'ALL))]
        [else
          (case (command in)
            [(model-vpath model-cursor model-title model-type
              model-state model-pos model-duration model-tags
              model-tracks)
             (write ((eval (command in)) model))(newline)]
            [else
              (let ([res (controller (ui->server-command in))])
                (when res
                  (write res)
                  (newline)))])]))))

(ev-run)
