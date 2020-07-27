#! /usr/bin/chez-scheme --program

;; peace: the PEA Curses Experience.
;; An ncurses based UI for pea.

#;(suppress-greeting #t)
#;(debug-on-exception #t)

(import
  (rnrs)
  (only (chezscheme) command-line-arguments getenv list-tail meta-cond)
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

(meta-cond
  [(getenv "WINDOWID")
   (define set-xterm-title!
     (lambda (title-string)
       (write-char #\x1b)
       (display "]0;")
       (display title-string)
       (write-char #\x7)
       (flush-output-port (current-output-port))))
   (define draw-xterm-title
     (lambda (model)
       (set-xterm-title!
         (string-append
           (model-state->string model)
           " "
           (get-title-string model)
           " - PEACE"))))]
  [else
    (define draw-xterm-title
      (lambda (_)
        (if #f #f)))])

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

(define make-config-view
  (lambda (controller)
    (my
      [current-audio-device #f]
      [current-audio-device-pos #f]
      [cursor-pos #f]
      [dev-list '()]
      [audio-devices-window #f])

    ;; Audio device list format: '((("name" . "value") ("description" . "value)) (.......))
    ;; Use description->value.
    (define audio-device->string cdadr)
    ;; Display the object (or as much as will fit) on the line of the screen.
    (define (draw-audio-device item i w)
      (mvwaddstr audio-devices-window i 0 (safe-substring (audio-device->string item) 0 w)))
    (define (draw-selected-audio-device item i w)
      (wattr-on audio-devices-window A_REVERSE)
      (mvwaddstr audio-devices-window i 0 (safe-substring (audio-device->string item) 0 w))
      (wattr-off audio-devices-window A_REVERSE))

    (define (draw-audio-devices-window)
      (draw-list audio-devices-window dev-list cursor-pos draw-audio-device draw-selected-audio-device))
    (define (find-device-index devname)
      (indexp (lambda (x)
                (string-ci=? devname (cdar x)))
              dev-list))
    (define (find-device-name index)
      (cdar (list-ref dev-list index)))
    (define (cursor-move dir)
      (let ([len (length dev-list)]
            [newpos (+ cursor-pos dir)])
        (unless (or
                  (< newpos 0)
                  (>= newpos len))
          (set! cursor-pos newpos)
          (draw-audio-devices-window))
        #f))
    (define (save-config)
      (if (= cursor-pos current-audio-device-pos)
          #f
          (controller `(mpv-audio-device-set! ,(find-device-name cursor-pos)))))
    (case-lambda
      [(view-command)
       (case view-command
         [(create)
          (set! audio-devices-window (newwin (- LINES 2) (- COLS 2) 1 1))
          (werase audio-devices-window)
          (controller '(mpv-audio-device))]
         [(destroy)
          (for-each
            delwin
            `(,audio-devices-window))
          (set! cursor-pos #f)
          (set! audio-devices-window #f)])]
      [(view-command msg)
       (case view-command
         [(control-message)
          (case (command msg)
            [(MPV)
             (case (list-ref msg 1)
               [(audio-device)
                (set! current-audio-device (list-ref msg 2))
                (set! dev-list (list-ref msg 3))
                (set! current-audio-device-pos (find-device-index current-audio-device))
                (set! cursor-pos current-audio-device-pos)
                (draw-audio-devices-window)]
               [(audio-device-changed)
                (set! current-audio-device (list-ref msg 2))
                (set! current-audio-device-pos (find-device-index current-audio-device))
                (set! cursor-pos current-audio-device-pos)
                (draw-audio-devices-window)])
             ])]
         [(handle-char)
          (case msg
            [(#\j)
             (cursor-move 1)]
            [(#\k)
             (cursor-move -1)]
            [(#\newline)
             (save-config)]
            [else
              #f])])])
    ))

(define make-debug-view
  (lambda ()
    (my
      [last-pos #f]
      [msg-list '()]
      [global-window #f]
      [msg-window #f]
      )
    ;; Display the object (or as much as will fit) on the line of the screen.
    (define (draw-msg item i w)
      (mvwaddstr msg-window i 0
                 (safe-substring (string-append
                                   (case (car item)
                                     [(store-control-message) "< "]
                                     [(store-mcast-message) "* "]
                                     [(store-outgoing-message) "> "]
                                     [else "? "])
                                   (object->string (cdr item))) 0 w)))

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
      [(view-command)
       (case view-command
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
      [(view-command msg)
       (case view-command
         [(store-control-message store-mcast-message store-outgoing-message)
          (case (command msg)
            [(POS)	; pos is special in that a great many of them are sent.
             (set! last-pos msg)]
            [else	; append the message.
              ;; TODO limit the number of stored messages.
              (set! msg-list `(,@msg-list ,(cons view-command msg)))])]
         [(client-command control-message mcast-message debug)
          (case (command msg)
            [(POS)	(draw-global-window)]
            [else	(draw-msg-window)])]
         [(handle-char)
          #f])])
    ))

(define model-state->string
  (lambda (model)
    (case (model-state model)
      [(PLAYING)	"|>"]
      [(PAUSED)		"||"]
      [(STOPPED)	"[]"]
      [(ANNOUNCING)	"**"]
      [else		"??"])))

(define make-player-view
  (lambda (controller model)
    (my
      [controls-window #f]
      [playlist-window #f]
      [tags-window #f]
      [timer-window #f])

    (define (draw-controls-window)
      (mvwaddstr controls-window 0 0 (model-state->string model))
      (wnoutrefresh controls-window))

    (define (draw-tags-window)
      (let ([tags (model-tags model)])
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
      (let ([pos (model-pos model)]
            [len (model-duration model)])
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
        (model-tracks model)
        (model-cursor model)
        draw-item draw-selected-item))

    (define (draw-vfs-window)
      (cond
        [(model-vpath model) =>
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
         (case (model-type model)
           [(DIR M3U PLS)
            'enter!]
           [(AMIGA AUDIO VIDEO)
            'play!])]
        [(#\0)
         '(set! audio-device "alsa/default:CARD=Headset")]
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
      [(view-command msg)
       (case view-command
         [(handle-char)
          (case (model-state model)
            [(ANNOUNCING)
             'stop!]
            [else
              (char->pea-command msg)])]
         [(mcast-message control-message)
          (cond
            [(list? msg)
             (case (car msg)
               [(LEN POS)
                (draw-timer-window)]
               [(STATE)
                (draw-controls-window)
                (draw-timer-window)
                (draw-tags-window)
                (draw-xterm-title model)
                ]
               [(TAGS)
                (draw-tags-window)
                (draw-xterm-title model)
                ]
               [(TRACKS)
                (draw-playlist-window)]
               [(VFS)
                (draw-vfs-window)
                (draw-xterm-title model)
                (cond
                  [(vfs-info-vpath msg) =>
                   (lambda (vp)
                     (controller 'tracks?))]
                  [else
                    (draw-playlist-window)])])]
            #;[else
              ;; Otherwise msg is a singleton, most likely ACK.
              ;; HMMM Maybe everything (including ACKs) should be in a list?
              (if #f #f)]
          )]
         )])
      ))

(define main
  (lambda (mcast-node mcast-service ctrl-node ctrl-service)

    ;; This main function will initialise the display, create an mcast message watcher and
    ;; then attempt to create a control connection to pead.
    ;; On success, the mcast watcher function is changed to one that will process all messages from
    ;; requested server.
    ;; Otherwise, the mcast watcher will continue to monitor for pead ahoj (server wakeup) messages.

    ;; [proc] try-connect-control: tries to create a control channel to pead.
    ;; On success, the mcast message watcher function will be set to decode received
    ;; messages and update display as usual.
    ;; On failure, the mcast message watcher remains unchanged and will continue scanning
    ;; mcast messages for a server ahoj message.
    (define try-connect-control
      (lambda ()
        (guard (e [else #f])
          (controller `(make-control-connection! ,ctrl-node ,ctrl-service ,control-msg-watcher))
          (set! mcast-msg-watcher-function connected-mcast-msg-watcher))))

    ;; [proc] ahoj-connect: looks for an ahoj message and then initiates a control connection.
    (define ahoj-connect
      (lambda (msg)
        (when (eq? (command msg) 'AHOJ)
          ;; TODO The display should show connect failures.
          (try-connect-control))))

    ;; [proc] connected-mcast-msg-watcher: Handles all mcast messages received.
    (define connected-mcast-msg-watcher
      (lambda (msg)
        ;; TODO review this 'source-tag (input) format.
        (current-view 'mcast-message msg)))

    ;; [proc] mcast-msg-watcher: the actual mcast message watcher function.
    ;; It performs common operations but delegates actual message processing to mcast-msg-watcher-function.
    (define mcast-msg-watcher
      (lambda (msg)
        (debug-view 'store-mcast-message msg)
        (cache-message-info model msg)
        (mcast-msg-watcher-function msg)
        (doupdate)))

    ;; [proc] control-msg-watcher: Handles unicast messages received on the control channel.
    (define control-msg-watcher
      (lambda (msg)
        (debug-view 'store-control-message msg)
        (cache-message-info model msg)
        (current-view 'control-message msg)
        (doupdate)))

    (my
      [mcast-msg-watcher-function ahoj-connect]
      [model (make-model)]
      [controller (make-pea-client mcast-node mcast-service mcast-msg-watcher
                                   ;; Include mcast-msg-watcher in packet filter to get packet filter debug messages.
                                   (make-packet-filter ctrl-node mcast-msg-watcher))]
      [player-view (make-player-view controller model)]
      [config-view (make-config-view controller)]
      [debug-view (make-debug-view)]
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
          [(#\C)
           (set-current-view! config-view)]
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
            (debug-view 'store-outgoing-message msg)
            (apply current-view msg)
            (doupdate))))

    (current-view 'create)
    (doupdate)
    (try-connect-control)
    (ev-run)))

;; Enable SO_REUSEADDR for all created sockets. See socket(7).
(create-socket-reuseaddr #t)

(unless (null? (command-line-arguments))
  (set! ctrl-node (car (command-line-arguments))))

(init-curses)

(guard (e [else
            (endwin)
            (raise e)])
  (main mcast-node service ctrl-node service)
  (endwin))

