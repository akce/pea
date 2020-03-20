#! /usr/bin/chez-scheme --script

(suppress-greeting #t)
(debug-on-exception #t)

(import
  (rnrs)
  (pea client)
  (only (pea util) arg command string-join write-now)
  (ev)
  )

;; TODO load config.

(define service "49000")
(define mcast-node "224.0.0.49")
(define ctrl-node "localhost")

(define controller (make-pea-client ctrl-node service mcast-node service))

;; Handle stdin commands.
(ev-io 0 (evmask 'READ)
  (lambda (w revent)
    (let ([in (read (current-input-port))])
      (cond
        [(eof-object? in)
         (ev-io-stop w)
         (ev-break (evbreak 'ALL))]
        [else
          (let ([res (controller (ui->server-command in))])
            (when res
              (display (dress-up in res))
              (newline)))]
      ))))

;; eg, server command aliases.
(define ui->server-command
  (lambda (input)
    (case (command input)
      [(i)
       'info]
      [(ls)
       'tracks]
      [else
        input])))

(define server-msg-handler
  (lambda (input)
    (case (command input)
      [(AHOJ)
       ;; TODO query current server state.
       (display (cadr input))
       (newline)]
      ;; Hide POS message. These can be sent out every second and quickly overwhelm the display.
      [(POS LEN TAGS STATE)
       (if #f #f)]
      [(TRACKS)
       (let loop ([i 1] [ts (arg input)])
         (cond
           [(null? ts)
            (if #f #f)]
           [else
             (display
               (string-append (pad-num i) " " (symbol->string (cdr (car ts))) " " (car (car ts))))
             (newline)
             (loop (+ i 1) (cdr ts))]))]
      [else
        (write-now input (current-output-port))])))

;; [proc] dress-up: converts result into something nice to display.
(define dress-up
  (lambda (msg result)
    (case (command msg)
      [(len pos)
       (seconds->string result)]
      [(tags)
       (apply string-join "\n"
              (map
                (lambda (t)
                  (string-append (car t) "\t" (cdr t)))
                result))]
      [else
        result]
      )))

(controller `(client-set! ,server-msg-handler))

;; use libedit for input and completion?

(ev-run)
