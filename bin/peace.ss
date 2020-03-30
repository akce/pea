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

;; TODO load config.

(define service "49000")
(define mcast-node "224.0.0.49")
(define ctrl-node "localhost")

(define init-curses
  (lambda ()
    (define app-name "The System v3.0")
    (define app-border
      (lambda ()
        (box stdscr ACS_VLINE ACS_HLINE)
        (mvwaddch stdscr (- LINES 1) (- COLS (string-length app-name) 3) ACS_RTEE)
        (waddstr stdscr app-name)
        (waddch stdscr ACS_LTEE)
        (wnoutrefresh stdscr)))

    (setlocale LC_ALL "")
    (initscr)
    (keypad stdscr #t)
    (noecho)
    (cbreak)
    (start-color)
    (curs-set 0)
    (use-default-colors)

    (app-border)))

#;(define audio-code (char->integer #\♫))
(define audio-code (char->integer #\a))
(define video-code (char->integer #\v))
(define folder-code (char->integer #\/))
(define unknown-code (char->integer #\?))

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

(define make-protocol-view
  (lambda (controller)
    ;; Display the object (or as much as will fit) on the last line of the screen.
    (lambda (msg)
      (let ([y (- LINES 2)]
            [msg-str (safe-substring (object->string msg) 0 (- COLS 2))])
        (mvwaddstr stdscr y 1 msg-str)
        (wclrtoeol stdscr)
        (mvwaddch stdscr y (- COLS 1) ACS_VLINE)
        (wnoutrefresh stdscr)))))

(define make-player-view
  (lambda (controller)
    (my
      [controls-window #f]
      [playlist-window #f]
      [tags-window #f]
      [timer-window #f])

    (define (draw-controls)
      (mvwaddstr controls-window
                 0 0
                 (case (controller 'cached-state?)
                   [(PLAYING)	"|>"]
                   [(PAUSED)	"||"]
                   [(STOPPED)	"[]"]
                   [else	"??"]))
      (wnoutrefresh controls-window))

    (define (draw-tags)
      (let ([tags (controller 'cached-tags?)])
        (werase tags-window)
        (unless (null? tags)
          (let* ([label-len
                   (+ 1
                      (apply max
                             (map
                               (lambda (x)
                                 (string-length (car x)))
                               tags)))]
                 [tag-len (- (getmaxx tags-window) label-len)])
            (let loop ([ts tags] [i 0])
              (unless (null? ts)
                (mvwaddstr tags-window i 0 (caar ts))
                (mvwaddstr tags-window i label-len
                           (safe-substring (cdar ts) 0 tag-len))
                (loop (cdr ts) (+ i 1))))))
        (wnoutrefresh tags-window)))

    (define (draw-timer)
      (let ([pos (controller 'cached-pos?)]
            [len (controller 'cached-len?)])
        (werase timer-window)
        (mvwaddstr timer-window
                   0 0 (if pos
                           (seconds->string pos)
                           "-"))
        (when len
          (waddstr timer-window " / ")
          (waddstr timer-window (seconds->string len)))
        (wnoutrefresh timer-window)))

    (define (draw-tracks)
      (draw-list
        playlist-window
        (controller 'cached-tracks?)
        (controller 'cached-cursor?)))

    (define (draw-vfs)
      (cond
        [(controller 'cached-vpath?) =>
         (lambda (vpath)
           (mvwaddch stdscr 0 1 ACS_RTEE)
           (waddstr stdscr (safe-substring (apply string-join "/" vpath) 0 (- COLS 5)))
           (waddch stdscr ACS_LTEE)
           (whline stdscr ACS_HLINE (- COLS 1 (getcurx stdscr))))]
        [else
          (mvwhline stdscr 0 1 ACS_HLINE (- COLS 2))])
         (wnoutrefresh stdscr))

    (define (char->pea-command ch)
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
          #f]))

    (case-lambda
      [(msg)
       (case msg
         [(create)
          (set! controls-window (newwin 1 2 1 1))
          (set! timer-window (newwin 1 19 1 4))
          (set! tags-window (newwin (- LINES 3) (- (div COLS 2) 2) 2 1))
          (set! playlist-window
            (newwin (- LINES 2) (- (div COLS 2) 2) 1 (div COLS 2)))]
         [(destroy)
          (for-each
            delwin
            `(,controls-window ,playlist-window ,tags-window ,timer-window))])]
      [(msg arg)
       (case msg
         [(handle-char)
          (char->pea-command arg)]
         [(server-msg)
          (cond
            [(list? arg)
             (case (car arg)
               [(LEN POS)
                (draw-timer)]
               [(STATE)
                (draw-controls)
                (draw-timer)
                (draw-tags)
                ]
               [(TAGS)
                (draw-tags)]
               [(TRACKS)
                (draw-tracks)]
               [(VFS)
                (draw-vfs)
                (controller 'tracks?)]
               )])
          (doupdate)]
         )])
      ))

(define main
  (lambda ()
    (my
      [controller (make-pea-client ctrl-node service mcast-node service)]
      [player-view (make-player-view controller)]
      [protocol-view (make-protocol-view controller)]
      [current-view player-view])

    (define handle-global-key
      (lambda (ch)
        (case ch
          [(#\x)
           #;(ev-io-stop w)
           (ev-break (evbreak 'ALL))]
          [(16)		; ctrl-p
           (unless (eq? current-view protocol-view)
             (current-view 'destroy)
             (set! current-view protocol-view)
             (current-view 'create))]
          )
        (doupdate)))

    ;; Handle stdin commands.
    (ev-io 0 (evmask 'READ)
           (lambda (w revent)
             (let ([ch (integer->char (getch))])
               (cond
                 [(current-view 'handle-char ch) =>
                  (lambda (cmd)
                    (controller cmd))]
                 [else
                   (handle-global-key ch)]
                 ))))

    ;; Register the server message handler with the controller.
    (controller
      `(client-set!
         ,(lambda (msg)
            (current-view 'server-msg msg))))

    (current-view 'create)
    (doupdate)
    (ev-run)))

(init-curses)

(guard (e [else
            (endwin)
            (raise e)])
  (main)
  (endwin))

