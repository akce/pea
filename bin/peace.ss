#! /usr/bin/chez-scheme --program

;; peace: the PEA Curses Experience.
;; An ncurses based UI for pea.

#;(suppress-greeting #t)
#;(debug-on-exception #t)

(import
  (rnrs)
  (only (chezscheme) command-line-arguments list-tail)
  (ev)
  (ncurses)
  (pea client)
  (pea util)
  (socket extended)
  )

;; TODO load config.

(define service "49000")
(define mcast-node "224.0.0.49")
(define ctrl-node (gethostname))

(define draw-app-border
  (lambda ()
    (define app-name "The System v3.0")
    (box stdscr ACS_VLINE ACS_HLINE)
    (mvwaddch stdscr (- LINES 1) (- COLS (string-length app-name) 3) ACS_RTEE)
    (waddstr stdscr app-name)
    (waddch stdscr ACS_LTEE)
    (wnoutrefresh stdscr)))

(define init-curses
  (lambda ()

    (setlocale LC_ALL "")
    (initscr)
    (keypad stdscr #t)
    (noecho)
    (cbreak)
    (start-color)
    (curs-set 0)
    (use-default-colors)

    (draw-app-border)))

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
      [(AMIGA AUDIO)	audio-code]
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

(define make-debug-view
  (lambda (controller)
    (my
      [last-pos #f]
      [msg-list '()]
      [global-window #f]
      [msg-window #f]
      )
    ;; Display the object (or as much as will fit) on the line of the screen.
    (define (draw-msg item i w)
      (mvwaddstr msg-window i 0
                 (safe-substring (object->string item) 0 w)))

    (define (draw-global-window)
      (werase global-window)
      (when last-pos
        (mvwaddstr
          global-window 0 0
          (safe-substring (object->string last-pos) 0 (getmaxx global-window))))
      (wnoutrefresh global-window))

    (define (draw-msg-window)
      (draw-list msg-window msg-list (- (length msg-list) 1) draw-msg draw-msg))

    (case-lambda
      [(msg)
       (case msg
         [(create)
          (set! global-window (newwin 1 (- COLS 2) 1 1))
          (set! msg-window (newwin
                             (- LINES (getmaxy global-window) 2) (- COLS 2)
                             2 1))
          (draw-global-window)
          (draw-msg-window)]
         [(destroy)
          (for-each
            delwin
            `(,global-window ,msg-window))
          (set! global-window #f)
          (set! msg-window #f)])]
      [(msg arg)
       (case msg
         [(client-command control-message mcast-message debug)
          (case (command arg)
            [(POS)
             ;; pos is special in that a great many of them are sent.
             (set! last-pos arg)
             (when global-window
               (draw-global-window))
             ]
            [else
              (set! msg-list `(,@msg-list ,arg))
              (when msg-window
                (draw-msg-window))]
            )]
         [(handle-char)
          #f]
         )])
    ))

(define make-player-view
  (lambda (controller)
    (my
      [controls-window #f]
      [playlist-window #f]
      [tags-window #f]
      [timer-window #f])

    (define (draw-controls-window)
      (mvwaddstr controls-window
                 0 0
                 (case (controller 'cached-state?)
                   [(PLAYING)		"|>"]
                   [(PAUSED)		"||"]
                   [(STOPPED)		"[]"]
                   [(ANNOUNCING)	"**"]
                   [else		"??"]))
      (wnoutrefresh controls-window))

    (define (draw-tags-window)
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

    (define (draw-timer-window)
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

    (define (draw-playlist-window)
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

    (define (draw-vfs-window)
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
           [(AMIGA AUDIO VIDEO)
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
        [(#\r #\R)
         '(refresh!)]
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
            (newwin (- LINES 2) (- (div COLS 2) 2) 1 (div COLS 2)))
          (draw-controls-window)
          (draw-timer-window)
          (draw-tags-window)
          (draw-playlist-window)
          (draw-vfs-window)
          ]
         [(destroy)
          (for-each
            delwin
            `(,controls-window ,playlist-window ,tags-window ,timer-window))])]
      [(msg arg)
       (case msg
         [(handle-char)
          (case (controller 'cached-state?)
            [(ANNOUNCING)
             'stop!]
            [else
              (char->pea-command arg)])]
         [(mcast-message control-message)
          (cond
            [(list? arg)
             (case (car arg)
               [(LEN POS)
                (draw-timer-window)]
               [(STATE)
                (draw-controls-window)
                (draw-timer-window)
                (draw-tags-window)
                ]
               [(TAGS)
                (draw-tags-window)]
               [(TRACKS)
                (draw-playlist-window)]
               [(VFS)
                (draw-vfs-window)
                (cond
                  [(vfs-info-vpath arg) =>
                   (lambda (vp)
                     (controller 'tracks?))]
                  [else
                    (draw-playlist-window)])])]
            #;[else
              ;; Otherwise arg is a singleton, most likely ACK.
              ;; HMMM Maybe everything (including ACKs) should be in a list?
              (if #f #f)]
          )]
         )])
      ))

(define main
  (lambda (mcast-node mcast-service ctrl-node ctrl-service)

    (define mcast-msg-watcher
      (lambda (msg)
        ;; TODO The display should show that we're waiting for a control connection.
        (debug-view 'mcast-message msg)
        (when (eq? (command msg) 'AHOJ)
          (connect-control))
        ;; TODO review this 'source-tag (input) format.
        (current-view 'mcast-message msg)
        (doupdate)))

    (define control-msg-watcher
      (lambda (msg)
        (debug-view 'control-message msg)
        (current-view 'control-message msg)
        (doupdate)))

    (define connect-control
      (lambda ()
        (controller `(make-control-connection! ,ctrl-node ,ctrl-service ,control-msg-watcher))))

    (my
      [controller (make-pea-client mcast-node mcast-service ctrl-node mcast-msg-watcher)]
      [player-view (make-player-view controller)]
      [debug-view (make-debug-view controller)]
      [current-view player-view])

    (define set-current-view!
      (lambda (new-view)
        (unless (eq? current-view new-view)
          (current-view 'destroy)
          (set! current-view new-view)
          (erase)
          (draw-app-border)
          (current-view 'create))))

    (define handle-global-key
      (lambda (ch)
        (case ch
          [(#\x)
           #;(ev-io-stop w)
           (ev-break (evbreak 'ALL))]
          [(#\D)
           (set-current-view! debug-view)]
          [(#\P)
           (set-current-view! player-view)]
          [(#\ƚ)	; KEY_RESIZE
           (current-view 'destroy)
           (erase)
           (draw-app-border)
           (current-view 'create)]
          )))

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
                 )
               (doupdate))))

    ;; Set our own SIGWINCH handler so as to fake a libev stdin/READ event on window resize.
    (ev-signal 28	; 28 = SIGWINCH on Linux.
      (lambda (w rev)
        (endwin)
        (refresh)
        (ungetch KEY_RESIZE)
        (ev-feed-fd-event 0 (evmask 'READ))))

    ;; Watch outgoing commands. Allows the debug view to see everything.
    (controller
      `(set-client-command-watcher!
         ,(lambda (msg)
            (apply debug-view msg)
            (doupdate))))

    (current-view 'create)
    (doupdate)
    (connect-control)
    (ev-run)))

(unless (null? (command-line-arguments))
  (set! ctrl-node (car (command-line-arguments))))

(init-curses)

(guard (e [else
            (endwin)
            (raise e)])
  (main mcast-node service ctrl-node service)
  (endwin))

