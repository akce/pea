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
      (mpv-initialize)
      (register-mpv-event-handler
        (lambda (eid)
          ;; Translate MPV relevant events to PEA events then send to callback function.
          (cond
            [(= eid MPV_EVENT_METADATA_UPDATE)
             (cb `(TAGS ,(mpv-get-property/node "metadata")))]
            [(= eid MPV_EVENT_IDLE)
             (cb '(STOPPED))]
            [(or
               (= eid MPV_EVENT_PLAYBACK_RESTART)
               (= eid 13))		; deprecated: MPV_EVENT_UNPAUSE
             (cb '(PLAYING))]
            ;; TODO get pause/unpause info via mpv_observe_property as these events are deprecated.
            [(= eid 12)		; deprecated: MPV_EVENT_PAUSE
             (cb '(PAUSED))]
            ;; ignore these events.
            [(or
               (= eid MPV_EVENT_START_FILE)
               (= eid MPV_EVENT_END_FILE)
               (= eid MPV_EVENT_AUDIO_RECONFIG)
               (= eid MPV_EVENT_FILE_LOADED)
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
