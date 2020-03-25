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

(define controls-window #f)
(define playlist-window #f)
(define timer-window #f)

(define app-name "The System v3.0")

(define draw-border
  (lambda ()
    (box stdscr ACS_VLINE ACS_HLINE)
    (mvwaddch stdscr (- LINES 1) (- COLS (string-length app-name) 3) ACS_RTEE)
    (waddstr stdscr app-name)
    (waddch stdscr ACS_LTEE)
    (wnoutrefresh stdscr)))

(define create-windows
  (lambda ()
    (set! controls-window (newwin 1 2 1 1))
    (set! timer-window (newwin 1 19 1 4))
    (set! playlist-window
      (newwin (- LINES 2) (- (div COLS 2) 2) 1 (div COLS 2)))
    ))

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
  (lambda (win lst pos)
    (define w (- (getmaxx win) 2))
    (werase win)
    (let-values ([(slst sp) (list-view lst (if pos pos -1) (getmaxy win))])
      (let loop ([ts slst] [i 0])
        (cond
          [(null? ts)
           (wnoutrefresh win)]
          [else
            ;; TODO this is playlist specific. Need to abstract this bit away..
            (mvwaddch win i 0 (get-filetype-char (cdr (car ts))))
            (when (= sp i)
              (wattr-on win A_REVERSE))
            (mvwaddstr win i 2
                       (safe-substring (car (car ts)) 0 w))
            (when (= sp i)
              (wattr-off win A_REVERSE))
            (loop (cdr ts) (+ i 1))])))
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
          (mvwaddstr stdscr y 1 msg-str)
          (wclrtoeol stdscr)
          (mvwaddch stdscr y (- COLS 1) ACS_VLINE)
          (wnoutrefresh stdscr))))

    (define draw-controls
      (lambda ()
        (mvwaddstr controls-window
                   0 0
                   (case (controller 'cached-state?)
                     [(PLAYING)	"|>"]
                     [(PAUSED)	"||"]
                     [(STOPPED)	"[]"]
                     [else	"??"]))
        (wnoutrefresh controls-window)))

    (define draw-tags
      (lambda ()
        (define tags (controller 'cached-tags?))
        (wnoutrefresh stdscr)
        ))

    (define draw-timer
      (lambda ()
        (define pos (controller 'cached-pos?))
        (define len (controller 'cached-len?))
        (werase timer-window)
        (mvwaddstr timer-window
                   0 0 (if pos
                          (seconds->string pos)
                          "-"))
        (when len
          (waddstr timer-window " / ")
          (waddstr timer-window (seconds->string len)))
        (wnoutrefresh timer-window)))

    (define draw-tracks
      (lambda ()
        (draw-list
          playlist-window
          (controller 'cached-tracks?)
          (controller 'cached-cursor?))))

    (define draw-vfs
      (lambda ()
        (define vpath (controller 'cached-vpath?))
        (if vpath
            (begin
              (mvwaddch stdscr 0 1 ACS_RTEE)
              (waddstr stdscr (safe-substring (apply string-join "/" vpath) 0 (- COLS 5)))
              (waddch stdscr ACS_LTEE)
              (whline stdscr ACS_HLINE (- COLS 1 (getcurx stdscr))))
            (mvwhline stdscr 0 1 ACS_HLINE (- COLS 2))
            )
        (wnoutrefresh stdscr)))

    (define server-msg-handler
      (lambda (msg)
        (case (command msg)
          [(LEN POS)
           (draw-timer)]
          [(STATE)
           (draw-controls)
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
        (doupdate)))

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
(create-windows)
(draw-border)
(doupdate)

(guard (e [else
            (endwin)
            (raise e)])
  (main)
  (endwin))

