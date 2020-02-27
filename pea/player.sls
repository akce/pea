(library (pea player)
  (export
    init-player
    play
    toggle-pause
    stop

    mpv-init
    mpv-play
    )
  (import
    (rnrs)
    (pea path)
    (pea playlist)
    (pea util)
    (mpv)
    (mpv ev)
    )

  (define init-player
    (lambda (cb)
      (mpv-init cb)))

  (define play
    (lambda (track parent)
      (my
        [full-path	(track-join-path parent track)])
      (case (track-type track)
        [(AUDIO VIDEO)
         (mpv-play (uri->string (track-uri full-path)))]
        [else
          #f]
        )
      ))

  (define toggle-pause
    (lambda ()
      (mpv-toggle-pause)))

  (define stop
    (lambda ()
      (mpv-stop)))

  (define mpv-init
    (lambda (cb)
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
      (register-mpv-event-handler (make-mpv-event-handler cb))
      ))

  (define mpv-play
    (lambda (file-or-url)
      (mpv-command "loadfile" file-or-url)))

  (define mpv-toggle-pause
    (lambda ()
      (mpv-command "cycle" "pause")))

  (define mpv-stop
    (lambda ()
      (mpv-command "stop")))

  (define make-mpv-event-handler
    (lambda (cb)
      ;; Define our own property event IDs for things that need to be watched.
      (define tags-id 1)
      (define time-pos-id 2)
      (define pause-id 3)

      ;; handler: translate MPV relevant events to PEA events then return via callback function.
      (lambda (event)
        (define eid (mpv-event-id event))
        (cond
          [(mpv-property-event? event)
           (let ([pid (mpv-event-reply-userdata event)])
             (cond
               [(= pid time-pos-id)
                (cb `(POS ,(mpv-property-event-value event)))]
               [(= pid tags-id)
                (cb `(TAGS ,(mpv-property-event-value event)))]
               [(= pid pause-id)
                (cb (if (mpv-property-event-value event)
                        '(PAUSED)
                        '(PLAYING)))]
               [else
                 (display "property-change: ")(display (mpv-property-event-name event))
                 (display " -> ")
                 (display (mpv-property-event-value event))(newline)]))]
          [(= eid (mpv-event-type end-file))
           (mpv-unobserve-property pause-id)
           (mpv-unobserve-property tags-id)
           (mpv-unobserve-property time-pos-id)]
          [(= eid (mpv-event-type idle))
           (cb '(STOPPED))]
          [(= eid (mpv-event-type playback-restart))
           (cb '(PLAYING))]
          [(= eid (mpv-event-type file-loaded))
           (mpv-observe-property tags-id "metadata" (mpv-format node))
           (mpv-observe-property pause-id "pause" (mpv-format flag))
           (mpv-observe-property time-pos-id "time-pos" (mpv-format int64))]
          ;; ignore these events.
          [(or
             (= eid (mpv-event-type start-file))
             (= eid (mpv-event-type audio-reconfig))
             (= eid (mpv-event-deprecated pause))
             (= eid (mpv-event-deprecated unpause))
             (= eid (mpv-event-deprecated tracks-changed))
             (= eid (mpv-event-deprecated metadata-update))
             )
           (if #f #f)]
          ;; log unknown events.
          [else
            (cb `(MPV ,eid ,(mpv-event-name eid)))])
        )))
  )
