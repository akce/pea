#! /usr/bin/chez-scheme --script

;; peace: the PEA Curses Experience.
;; An ncurses based UI for pea.

(suppress-greeting #t)
(debug-on-exception #t)

(import
  (rnrs)
  (only (chezscheme) list-tail)
  (ev)
  (ncurses)
  (pea client)
  (pea util)
  )

(define init-curses
  (lambda ()
    (setlocale LC_ALL "")
    (initscr)
    (keypad stdscr #t)
    (noecho)
    (cbreak)
    (start-color)
    (curs-set 0)
    (use-default-colors)))

;; TODO load config.

(define service "49000")
(define mcast-node "224.0.0.49")
(define ctrl-node "localhost")
(define debug #f)

#;(define audio-code (char->integer #\♫))
(define audio-code (char->integer #\a))
(define video-code (char->integer #\v))
(define folder-code (char->integer #\/))
(define unknown-code (char->integer #\?))

(define app-name "The System v3.0")

(define draw-border
  (lambda ()
    (box stdscr ACS_VLINE ACS_HLINE)
    (mvaddch (- LINES 1) (- COLS (string-length app-name) 3) ACS_RTEE)
    (addstr app-name)
    (addch ACS_LTEE)))

(define list-view
  (lambda (lst p h)
    (cond
      [(or (not lst)
           (null? lst))
       (values '() #f)]
      [(< p h)
       (values lst p)]
      [else
        (list-view (list-tail lst h) (- p h) h)])))

(define get-filetype-char
  (lambda (type)
    (case type
      [(AUDIO)		audio-code]
      [(VIDEO)		video-code]
      [(DIR M3U PLS)	folder-code]
      [else		unknown-code])))

(define draw-list
  (lambda (lst p y x w h)
    (let-values ([(slst sp) (list-view lst (if p p -1) h)])
      (cond
        [(null? slst)
         ;; blank the list panel.
         (let loop ([i 0])
           (when (< i h)
             ;; TODO write a blank line.
             (loop (+ i 1))))]
        [else
          (let loop ([ts slst] [i 0])
            (cond
              [(null? ts)
               (if #f #f)]
              [else
                ;; TODO this is playlist specific. Need to abstract this bit away..
                (mvaddch (+ i y) x (get-filetype-char (cdr (car ts))))
                (when (= sp i)
                  (attr-on A_REVERSE))
                (mvaddstr (+ i y) (+ x 2) (safe-substring (car (car ts)) 0 (- w 2)))
                (when (= sp i)
                  (attr-off A_REVERSE))
                (loop (cdr ts) (+ i 1))]))]))
    ))

(define object->string
  (lambda (obj)
    (call-with-string-output-port
      (lambda (p)
        (write obj p)))))

(define safe-substring
  (lambda (str start end)
    (guard (e [else str])
      (substring str start end))))

(define main
  (lambda ()
    (define controller (make-pea-client ctrl-node service mcast-node service))

    ;; Display the object (or as much as will fit) on the last line of the screen.
    (define draw-msg
      (lambda (msg)
        (let ([y (- LINES 2)]
              [msg-str (safe-substring (object->string msg) 0 (- COLS 2))])
          (mvaddstr y 1 msg-str)
          (clrtoeol)
          (mvaddch y (- COLS 1) ACS_VLINE))))

    (define draw-state
      (lambda ()
        (mvaddch 1 2
                 (case (controller 'cached-state?)
                   [(PLAYING)	ACS_GEQUAL]
                   [(PAUSED)	ACS_NEQUAL]
                   [(STOPPED)	ACS_DIAMOND]
                   [else	(char->integer #\?)]))))

    (define draw-tags
      (lambda ()
        (define tags (controller 'cached-tags?))
        (if #f #f)
        ))

    (define draw-timer
      (lambda ()
        (define pos (controller 'cached-pos?))
        (define len (controller 'cached-len?))
        (mvaddstr 1 4 (if pos
                          (seconds->string pos)
                          "-"))
        (when len
          (addstr " / ")
          (addstr (seconds->string len)))))

    (define draw-tracks
      (lambda ()
        (define pos (controller 'cached-cursor?))
        (define tracks (controller 'cached-tracks?))
        (draw-list tracks pos 1 (div COLS 2) (- (div COLS 2) 2) (- LINES 2))))

    (define draw-vfs
      (lambda ()
        (define vpath (controller 'cached-vpath?))
        (draw-border)
        (if vpath
            (begin
              (mvaddch 0 1 ACS_RTEE)
              (addstr (safe-substring (apply string-join "/" vpath) 0 (- COLS 5)))
              (addch ACS_LTEE)
              ;; TODO draw ACS_HLINE to eol.
            )
            ;; TODO clear vpath.
            )))

    (define server-msg-handler
      (lambda (msg)
        (case (command msg)
          [(LEN POS)
           (draw-timer)]
          [(STATE)
           (draw-state)
           (draw-timer)
           ]
          [(TAGS)
           (draw-tags)]
          [(TRACKS)
           (draw-tracks)]
          [(VFS)
           (draw-vfs)
           (controller 'tracks?)]
          )
        (when debug
          (draw-msg msg))
        (refresh)))

    (define char->pea-command
      (lambda (ch controller)
        ;; TODO modify keymap based on pea state.
        (case ch
          [(#\newline)
           (case (controller 'cached-type?)
             [(DIR M3U PLS)
              'enter!]
             [(AUDIO VIDEO)
              'play!])]
          [(#\h)
           'pop!]
          [(#\H)
           'root!]
          [(#\t #\space)
           'toggle!]
          [(#\s #\q)
           'stop!]
          [(#\j)
           '(move! 1)]
          [(#\k)
           '(move! -1)]
          [(#\J)
           '(move! 10)]
          [(#\K)
           '(move! -10)]
          [else
            #f])))

    (define display-pea-result
      (lambda (res)
        (if #f #f)))

    ;; Handle stdin commands.
    (ev-io 0 (evmask 'READ)
           (lambda (w revent)
             (let ([ch (integer->char (getch))])
               (cond
                 [(char=? ch #\x)
                  (ev-io-stop w)
                  (ev-break (evbreak 'ALL))]
                 [else
                   (let ([cmd (char->pea-command ch controller)])
                     ;; TODO display unknown key error?
                     (when cmd
                       (let ([res (controller cmd)])
                         (when res
                           (display-pea-result res)))))]))))

    (controller `(client-set! ,server-msg-handler))
    (ev-run)))

(init-curses)
(draw-border)
(refresh)

(guard (e [else
            (endwin)
            (raise e)])
  (main)
  (endwin))

