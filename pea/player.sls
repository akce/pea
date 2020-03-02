(library (pea player)
  (export
    make-player)
  (import
    (rnrs)
    (only (chezscheme) machine-type)
    (pea omxplayer)
    (pea path)
    (pea playlist)
    (pea util)
    (mpv)
    (mpv ev)
    )

  (define make-player
    (lambda (controller)
      ;; mpv is always used for AUDIO, and for VIDEO on non-raspberry pi machines.
      (define mpv-player (mpv-init controller))
      (define audio-player mpv-player)

      (define video-player
        (case (machine-type)
          ;; assume that ARM is a Raspberry PI.
          [(arm32le)
           (make-omxplayer controller)]
          [else
            mpv-player]))

      (define current-player #f)

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
                 [else
                   (error 'player "cannot play! unsupported track type" track)]))
             (current-player 'play! (uri->string (track-uri track))))]
          [else
           (apply current-player input)]))))

  (define mpv-init
    (lambda (controller)
      (mpv-create)
      (mpv-set-property "audio-display" #f)	; disable mpv embedded coverart display. 
      ;; Turn on keyboard input for videos.
      (mpv-set-option/string "input-default-bindings" "yes")
      (mpv-set-option/string "input-vo-keyboard" "yes")
      (mpv-set-option/flag "osc" #t)
      (mpv-initialize)
      ;; Rebind default problem keys. Namely, 'q' must *not* shutdown. Stop instead.
      (mpv-command "keybind" "q" "stop")
      ;; Default 's' action is to take a screenshot, but pea can't guarantee write access. Stop instead.
      (mpv-command "keybind" "s" "stop")
      (register-mpv-event-handler (make-mpv-event-handler controller))
      (lambda input
        (case (car input)
          [(play!)
           (mpv-command "loadfile" (cadr input))]
          [(toggle!)
           (mpv-command "cycle" "pause")]
          [(stop!)
           (mpv-command "stop")]))))

  (define make-mpv-event-handler
    (lambda (controller)
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
                    (controller `(POS ,pos))))]
               [(= pid tags-id)
                (controller `(TAGS ,(mpv-property-event-value event)))]
               [(= pid pause-id)
                (controller (if (mpv-property-event-value event)
                                '(PAUSED)
                                '(PLAYING)))]
               [else
                 `(MPV "property-change"
                       ,(mpv-property-event-name event)
                       ,(mpv-property-event-value event))]))]
          [(= eid (mpv-event-type end-file))
           (mpv-unobserve-property pause-id)
           (mpv-unobserve-property tags-id)
           (mpv-unobserve-property time-pos-id)]
          [(= eid (mpv-event-type idle))
           ;; guess the reason for going idle (stopping).
           (controller `(MPV ,last-pos ,duration))
           (when (and
                   (number? duration)
                   ;; Sometimes the last second won't get a time-pos event so lower duration by one.
                   ;; This means that pressing stop in the last second won't stop continuous play.
                   (< last-pos (- duration 1)))
             ;; Assume that play was manually stopped so generate a fake stop! command otherwise
             ;; continuous play would just keep going.
             ;; This is safe to do as mpv-stop won't send another idle at this point.
             (controller 'stop!))
           (controller 'STOPPED)]
          [(= eid (mpv-event-type playback-restart))
           (controller '(PLAYING))]
          [(= eid (mpv-event-type file-loaded))
           ;; reset duration.
           ;; NB: streams like ice-cast radio stations don't have duration so set them to #f.
           (set! duration
             (guard (e [else #f])
               (mpv-get-property/long "duration")))
           (mpv-observe-property tags-id "metadata" (mpv-format node))
           (mpv-observe-property pause-id "pause" (mpv-format flag))
           (mpv-observe-property time-pos-id "time-pos" (mpv-format int64))
           (controller `(LEN ,duration))]
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
            (controller `(MPV ,eid ,(mpv-event-name eid)))])
        )))
  )
