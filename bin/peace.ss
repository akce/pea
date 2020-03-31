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
  (case-lambda
    [(win lst item-renderer)
     (draw-list win lst #f item-renderer #f)]
    [(win lst pos item-renderer selected-item-renderer)
     (define w (- (getmaxx win) 2))
     (werase win)
     (let-values ([(slst sp) (list-view lst (if pos pos -1) (getmaxy win))])
       (let loop ([ts slst] [i 0])
         (cond
           [(null? ts)
            (wnoutrefresh win)]
           [else
             ((if (= sp i) selected-item-renderer item-renderer)
              (car ts) i w)
             (loop (cdr ts) (+ i 1))])))]
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
            (draw-list tags-window tags
                       (lambda (item i w)
                         (mvwaddstr tags-window i 0 (car item))
                         (mvwaddstr tags-window i label-len
                                    (safe-substring (cdr item) 0 tag-len))))))
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
      (define (draw-item item i w)
        (mvwaddch playlist-window i 0 (get-filetype-char (cdr item)))
        (mvwaddstr playlist-window i 2
                   (safe-substring (car item) 0 w))
        )
      (define (draw-selected-item item i w)
        (mvwaddch playlist-window i 0 (get-filetype-char (cdr item)))
        (wattr-on playlist-window A_REVERSE)
        (mvwaddstr playlist-window i 2
                   (safe-substring (car item) 0 w))
        (wattr-off playlist-window A_REVERSE))
      (draw-list
        playlist-window
        (controller 'cached-tracks?)
        (controller 'cached-cursor?)
        draw-item draw-selected-item))

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
        [(#\ą)		; right arrow
         '(seek! 30)]
        [(#\Ą)		; left arrow
         '(seek! -30)]
        [(#\ă)		; up arrow
         '(seek! 600)]
        [(#\Ă)		; down arrow
         '(seek! -600)]
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

