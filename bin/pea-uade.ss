#! /usr/bin/chez-scheme --script

;; This is a player that reads and writes pea commands via stdin and stdout. 
;; It's done as a separate process, rather than as a loaded library so that faults
;; will not affect the pea server and so that it can be loaded and unloaded on demand.

(import
  (rnrs)
  (only (chezscheme) display-condition flonum->fixnum)
  (alsa pcm)
  (ev)
  (pea util)
  (uade))

(my
  [device "default"]
  [buffer/frames #f]
  [period/frames #f]
  [buffer-empty/percent 60])	; Let alsa audio buffer/frames get this empty before refilling.

(define configure-hw-params
  (lambda (handle sampling-rate)
    (let ([hwp (snd-pcm-hw-params-mallocz)])
      (snd-pcm-hw-params-any handle hwp)
      (snd-pcm-hw-params-set-access handle hwp 'rw-interleaved)
      (snd-pcm-hw-params-set-format handle hwp 's16-le)
      (snd-pcm-hw-params-set-channels handle hwp *uade-channels*)
      (snd-pcm-hw-params-set-rate-near handle hwp sampling-rate)
      (snd-pcm-hw-params-set-buffer-time-near handle hwp 500000)
      (snd-pcm-hw-params-set-period-time-near handle hwp 100000)
      (set! buffer/frames (snd-pcm-hw-params-get-buffer-size hwp))
      (set! period/frames (snd-pcm-hw-params-get-period-size hwp))
      (snd-pcm-hw-params handle hwp)
      (snd-pcm-hw-params-free hwp))))

(define configure-sw-params
  (lambda (handle)
    (let ([swp (snd-pcm-sw-params-mallocz)])
      (snd-pcm-sw-params-current handle swp)
      ;; Disable auto-play by setting auto-start position to greater than the buffer.
      ;; This means an snd-pcm-start call is needed to begin play.
      (snd-pcm-sw-params-set-start-threshold handle swp (+ buffer/frames 1))
      (snd-pcm-sw-params handle swp)
      (snd-pcm-sw-params-free swp))))

(define (main)
  (let ([handle (snd-pcm-open device (snd-pcm-stream 'playback) (snd-open-mode 'nonblock))]
        [uade-state (uade-new-state)])

    (configure-hw-params handle (uade-get-sampling-rate uade-state))
    (configure-sw-params handle)

    ;; setup frame buffer and event watchers.
    (let* ([framebuf (uade-malloc/frames buffer/frames)]
           ;; Calculate the libev timer wait-time (between 0 and 1) based on buffer size, song frame rate,
           ;; and how empty we want the buffer before refilling.
           [wait-time
            (real->flonum
              (/
                (* buffer/frames (/ buffer-empty/percent 100))
                (uade-get-sampling-rate uade-state)))])
      (my
        [play-watcher #f]
        [stdin-watcher #f]
        [last-pos #f]
        [bytes/second (* *uade-bytes/frame* (uade-get-sampling-rate uade-state))])

      ;; [proc] load-frames: loads frames into framebuf.
      ;; [return] the actual number of frames loaded into framebuf.
      ;; It helps juggle between the:
      ;; - max local memory buffer size,
      ;; - max available frame space in alsa-lib ring buffer,
      ;; - actual amount of frames loadable from the music file.
      (define load-frames
        (lambda ()
          (let ([max-frames (fxmin buffer/frames (snd-pcm-avail-update handle))])
            (uade-read/frames uade-state framebuf max-frames))))


      (define current-pos/seconds
        (lambda ()
          (flonum->fixnum
            (real->flonum
              (/ (uade-song-info-subsong-bytes (uade-get-song-info uade-state))
                 bytes/second)))))

      (define play-frame
        (lambda (watcher revents)
          ;; we can write more frames to the buffer.
          (let ([frames-read (load-frames)])
            (cond
              [(> frames-read 0)
               ;; TODO check for underruns.
               (snd-pcm-writei handle framebuf frames-read)
               (let ([cp (current-pos/seconds)])
                 (unless (eqv? cp last-pos)
                   (write-now `(POS ,cp) (current-output-port))
                   (set! last-pos cp)))]
              [else
                (stop-command)]
              ))))

    (define play-command
      (lambda (modfile)
        ;; ALSA sets pcm state to SETUP after a file has stopped.
        ;; snd-pcm-prepare resets so this app can play another file.
        (case (snd-pcm-state handle)
          [(setup)
           (snd-pcm-prepare handle)])
        (write-now `(PLAYING) (current-output-port))
        (unless play-watcher
          (uade-play uade-state modfile)
          (write-now `(LEN ,(flonum->fixnum (uade-song-info-duration (uade-get-song-info uade-state)))) (current-output-port))
          ;; Prime the alsa ring buffer before play proper.
          (snd-pcm-writei handle framebuf (load-frames))
          ;; Begin play.
          (set! play-watcher (ev-timer wait-time wait-time play-frame))
          (snd-pcm-start handle))))

    (define stop-command
      (lambda ()
        (when play-watcher
          (ev-timer-stop play-watcher)
          (set! play-watcher #f)
          (set! last-pos #f)
          (snd-pcm-drain handle)
          (uade-stop uade-state)
          (write-now '(STOPPED) (current-output-port)))))

    ;; stdin is where we'll receive pea player commands.
    (set! stdin-watcher
      (ev-io 0 (evmask 'READ)
             (lambda (w rev)
               (let ([msg (read-trim-right)])
                 (case (command msg)
                   [(play!)
                    (play-command (arg msg))]
                   [(stop!)
                    (stop-command)]
                   [(quit!!)
                    (stop-command)
                    (ev-break (evbreak 'ALL))]
                   [else
                     (write-now `(DOH "unknown command" ,msg) (current-output-port))]
                   )))))

    (ev-run)
    (uade-free framebuf))
  (uade-cleanup-state uade-state)
  (snd-pcm-close handle)))

(guard
  (e [else
       (write-now '(STOPPED) (current-output-port))
       ;; Encapsulate exception in a DOH message.
       (let-values ([(port getter) (open-string-output-port)])
         (display-condition e port)
         (write-now `(DOH "uadeplayer" ,(getter)) (current-output-port)))])
  (main))
