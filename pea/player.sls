(library (pea player)
  (export
    make-player)
  (import
    (rnrs)
    (only (chezscheme) machine-type)
    (pea exewrap)
    (pea omxplayer)
    (pea path)
    (pea playlist)
    (pea util)
    (mpv)
    (mpv ev)
    )

  (define make-player
    (lambda (ui-controller player-controller)
      ;; mpv is always used for AUDIO, and for VIDEO on non-raspberry pi machines.
      (my
        [mpv-player (mpv-init ui-controller player-controller)]
        [audio-player mpv-player]
        ;; FIXME Note the call to pea-uade, it really should exist in a libexec dir and should support both
        ;; FIXME script (.ss) or compiled versions of the program..
        [amiga-player (make-exeplayer player-controller "pea-uade")]
        [current-player #f])

      (define video-player
        (case (machine-type)
          ;; assume that ARM is a Raspberry PI.
          [(arm32le)
           (set-video-extensions! '("mp4" "mkv" "avi" "m4v"))
           (make-omxplayer player-controller)]
          [else
            ;; Same extensions as omxplayer, but also webm.
            (set-video-extensions! '("mp4" "mkv" "webm" "avi" "m4v"))
            mpv-player]))

      (set-amiga-extensions! '("mod" "hip" "hipc"))
      (set-audio-extensions! '("mp3" "flac" "aac" "m4a" "wv" "wav" "ogg"))

      (lambda input
        (case (car input)
          [(play!)
           ;; arg[1] == fully resolvable path to track.
           (let ([track (cadr input)])
             (set! current-player
               (case (track-type track)
                 [(AUDIO)
                  audio-player]
                 [(VIDEO)
                  video-player]
                 [(AMIGA)
                  amiga-player]
                 [else
                   (error 'player "cannot play! unsupported track type" track)]))
             (current-player 'play! (uri->string (track-uri track))))]
          [else
           (apply current-player input)]))))

  (define mpv-init
    (lambda (ui-controller player-controller)
      (mpv-create)
      (mpv-set-property "audio-display" #f)	; disable mpv embedded coverart display. 
      ;; Turn on keyboard input for videos.
      (mpv-set-option/flag "input-default-bindings" #t)
      (mpv-set-option/flag "input-vo-keyboard" #t)
      (mpv-set-option/flag "osc" #t)
      (mpv-initialize)
      ;; Rebind default problem keys. Namely, 'q' must *not* shutdown. Stop instead.
      (mpv-command "keybind" "q" "stop")
      ;; Default 's' action is to take a screenshot, but pea can't guarantee write access. Stop instead.
      (mpv-command "keybind" "s" "stop")
      (register-mpv-event-handler (make-mpv-event-handler ui-controller player-controller))
      (lambda input
        (case (car input)
          [(play!)
           (mpv-command "loadfile" (cadr input))]
          [(seek!)
           (mpv-command "seek" (number->string (cadr input)))]
          [(toggle!)
           (mpv-command "cycle" "pause")]
          [(stop!)
           (mpv-command "stop")]
          [(get-audio-extensions)
           '()]
          [(get-video-extensions)
           '()]))))

  (define make-mpv-event-handler
    (lambda (ui-controller player-controller)
      ;; Define our own property event IDs for things that need to be watched.
      (define tags-id 1)
      (define time-pos-id 2)
      (define pause-id 3)
      ;; last-pos & duration are used to guess if 'stop' was selected during video playback.
      ;; This is a workaround for mpv not generating a specific user-selected-stop event and my
      ;; not being able to find a specific event to watch for.
      ;; NB: eof-reached does not trigger as pea needs in keep-open mode and the end-file event
      ;; NB: seems to behave more like a file-closed event.
      ;; An alternate, and much more involved solution, would be to render the video into a pea controlled
      ;; window and have pea handle key input itself. But that's a lot more work...
      (define last-pos 0)
      (define duration #f)

      ;; handler: translate MPV relevant events to PEA events then return via callback function.
      (lambda (event)
        (define eid (mpv-event-id event))
        (cond
          [(mpv-property-event? event)
           (let ([pid (mpv-event-reply-userdata event)])
             (cond
               [(= pid time-pos-id)
                ;; #f time-pos is still occassionally seen at end of track playback.
                ;; Only pass real position values to the controller.
                (let ([pos (mpv-property-event-value event)])
                  (when pos
                    (set! last-pos pos)
                    (player-controller `(POS ,pos))))]
               [(= pid tags-id)
                (player-controller `(TAGS ,(mpv-property-event-value event)))]
               [(= pid pause-id)
                (player-controller
                  (if (mpv-property-event-value event)
                      '(PAUSED)
                      '(PLAYING)))]
               [else
                 (player-controller
                   `(MPV "property-change"
                         ,(mpv-property-event-name event)
                         ,(mpv-property-event-value event)))]))]
          [(= eid (mpv-event-type end-file))
           (mpv-unobserve-property pause-id)
           (mpv-unobserve-property tags-id)
           (mpv-unobserve-property time-pos-id)]
          [(= eid (mpv-event-type idle))
           ;; guess the reason for going idle (stopping).
           (player-controller `(MPV ,last-pos ,duration))
           (when (and
                   (number? duration)
                   ;; Sometimes the last second won't get a time-pos event so lower duration by one.
                   ;; This means that pressing stop in the last second won't stop continuous play.
                   (< last-pos (- duration 1)))
             ;; Assume that play was manually stopped so generate a fake stop! command otherwise
             ;; continuous play would just keep going.
             ;; This is safe to do as mpv-stop won't send another idle at this point.
             ;; NB: Checking current-pos against total-duration doesn't take into account a user move!
             ;; operation so this could do with a rethink at some point.
             ;; However, this only affects mpv video tracks, so acceptable for now.
             (ui-controller 'stop!))
           (player-controller 'STOPPED)]
          [(= eid (mpv-event-type playback-restart))
           (player-controller '(PLAYING))]
          [(= eid (mpv-event-type file-loaded))
           ;; reset duration.
           ;; NB: streams like ice-cast radio stations don't have duration so set them to #f.
           (set! duration
             (guard (e [else #f])
               (mpv-get-property/long "duration")))
           (mpv-observe-property tags-id "metadata" (mpv-format node))
           (mpv-observe-property pause-id "pause" (mpv-format flag))
           (mpv-observe-property time-pos-id "time-pos" (mpv-format int64))
           (player-controller `(LEN ,duration))]
          ;; ignore these events.
          [(or
             (= eid (mpv-event-type start-file))
             (= eid (mpv-event-type audio-reconfig))
             (= eid (mpv-event-type video-reconfig))
             (= eid (mpv-event-deprecated pause))
             (= eid (mpv-event-deprecated unpause))
             (= eid (mpv-event-deprecated tracks-changed))
             (= eid (mpv-event-deprecated metadata-update))
             )
           (if #f #f)]
          ;; log unknown events.
          [else
            (player-controller `(MPV ,eid ,(mpv-event-name eid)))])
        )))
  )
