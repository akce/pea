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
      (register-mpv-event-handler
        (lambda (eid)
          ;; Translate MPV relevant events to PEA events then send to callback function.
          (cond
            [(= eid (mpv-event-type metadata-update))
             (cb `(TAGS ,(mpv-get-property/node "metadata")))]
            [(= eid (mpv-event-type idle))
             (cb '(STOPPED))]
            [(or
               (= eid (mpv-event-type playback-restart))
               (= eid 13))		; deprecated: MPV_EVENT_UNPAUSE
             (cb '(PLAYING))]
            ;; TODO get pause/unpause info via mpv_observe_property as these events are deprecated.
            [(= eid 12)		; deprecated: MPV_EVENT_PAUSE
             (cb '(PAUSED))]
            ;; ignore these events.
            [(or
               (= eid (mpv-event-type start-file))
               (= eid (mpv-event-type end-file))
               (= eid (mpv-event-type audio-reconfig))
               (= eid (mpv-event-type file-loaded))
               (= eid 9)	; deprecated: MPV_EVENT_TRACKS_CHANGED
               )
             (if #f #f)]
            ;; log unknown events.
            [else
              (cb `(MPV ,eid ,(mpv-event-name eid)))])
          ))
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
  )
