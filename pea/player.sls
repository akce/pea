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
        ;; FIXME Note the call to pea-uade.ss, it really should exist in a pea/libexec dir.
        [amiga-player (make-exeplayer player-controller "pea-uade.ss")]
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

      (set-audio-extensions! '("mp3" "flac" "opus" "aac" "m4a" "wv" "wav" "ogg"))

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
          [(mpv-volume mpv-volume-adjust! mpv-toggle-mute! mpv-set-volume! mpv-volume-max! mpv-audio-device mpv-audio-device-set!)
           (apply mpv-player input)]
          [else
           (apply current-player input)]))))

  (define mpv-init
    (lambda (ui-controller player-controller)
      ;; Default mpv value for volume-max is 130.0, set to 100 as it:
      ;; - makes volume calculations simpler
      ;; - reduces chance of mpv softvol distortion at higher values
      ;; - and leaves it up to setting a good alsa PCM value system-wide.
      ;; In future, we may add a native alsa mixer config (either direct or via amixer).
      (define max-volume 100.0)
      (define volume-limit
        (lambda (value)
          (min max-volume (max 0 value))))
      (mpv-create)
      (mpv-set-property "audio-display" #f)	; disable mpv embedded coverart display. 
      ;; Turn on keyboard input for videos.
      (mpv-set-option "input-default-bindings" #t)
      (mpv-set-option "input-vo-keyboard" #t)
      (mpv-set-option "osc" #t)
      (mpv-set-option "user-agent" "PEA (Play 'Em All)/3.0")
      (mpv-initialize)
      (mpv-set-property/double "volume-max" max-volume)
      (mpv-set-property/double "volume" 50.0)
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
          [(mpv-toggle-mute!)
           (mpv-command "cycle" "mute")
           'ACK]
          [(mpv-volume-adjust!)		; arg +/- adjust%
           (mpv-set-property/double "volume" (volume-limit (+ (mpv-get-property/node "volume") (cadr input))))
           'ACK]
          [(mpv-volume)		; returns current volume percent.
           (make-volume-message)]
          [(mpv-set-volume!)	; arg absolute-new-volume%
           ;; Set new volume level.
           (mpv-set-property/double "volume" (volume-limit (cadr input)))
           'ACK]
          [(mpv-volume-max!)
           ;; This is more of an internal, tinker value and shouldn't really need to change.
           (mpv-set-property/double "volume-max" (cadr input))
           'ACK]
          [(mpv-audio-device)
           `(MPV audio-device ,(mpv-get-property/string "audio-device") ,(mpv-get-property/node "audio-device-list"))]
          [(mpv-audio-device-set!)
           (unless (mpv-get-property/flag "core-idle")
             ;; Changing the audio-device resets the audio output layer in mpv.
             ;; For now stop and let the user manually start play again.
             ;; Pause/unpause may work, but requires a couple of manual toggles before it resumes in my experiments.
             ;; One cheat way could be to reload the file and auto seek to current position.
             ;; Or maybe the reload isn't needed? ie, stop!, playlist-play-index, seek to last pos..
             ;; See mpv/player/audio.c:reload_audio_output()
             (ui-controller 'stop!))
           (mpv-set-property "audio-device" (cadr input))
           'ACK]
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
      (define audio-device-id 4)
      (define volume-id 5)
      (define mute-id 6)

      (mpv-observe-property audio-device-id "audio-device" (mpv-format string))
      (mpv-observe-property volume-id "volume" (mpv-format int64))
      (mpv-observe-property mute-id "mute" (mpv-format flag))
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
                (cond
                  [(mpv-property-event-value event) => (lambda (pos)
                                                         (player-controller `(POS ,pos)))])]
               [(= pid tags-id)
                (player-controller `(TAGS ,(mpv-property-event-value event)))]
               [(= pid pause-id)
                (player-controller
                  (if (mpv-property-event-value event)
                      '(PAUSED)
                      '(PLAYING)))]
               [(= pid audio-device-id)
                (player-controller
                  `(MPV audio-device-changed ,(mpv-get-property/string "audio-device") #;core-idle #;,(mpv-get-property/flag "core-idle") #;,(mpv-get-property/node "playlist")))
                #;(when (> (length (mpv-get-property/node "playlist")) 0)
                  (mpv-set-property "pause" #f))
                #;(unless (mpv-get-property/flag "core-idle")
                  #;(mpv-command "playlist-play-index" "0")
                  (mpv-command "playlist-play-index" "0"))]
               [(or (= pid volume-id) (= pid mute-id))
                (player-controller (make-volume-message))]
               [else
                 (player-controller
                   `(MPV property-change
                         ,(mpv-property-event-name event)
                         ,(mpv-property-event-value event)))]))]
          [(= eid (mpv-event-type end-file))
           (cond
             [(= (mpv-event-end-file-reason (mpv-event-data event)) (mpv-file-end-reason stop))
              (ui-controller 'stop!)])
           (mpv-unobserve-property pause-id)
           (mpv-unobserve-property tags-id)
           (mpv-unobserve-property time-pos-id)]
          [(= eid (mpv-event-type idle))
           (player-controller 'STOPPED)]
          [(= eid (mpv-event-type playback-restart))
           (player-controller '(PLAYING))]
          [(= eid (mpv-event-type file-loaded))
           (mpv-observe-property tags-id "metadata" (mpv-format node))
           (mpv-observe-property pause-id "pause" (mpv-format flag))
           (mpv-observe-property time-pos-id "time-pos" (mpv-format int64))
           ;; Streaming media like icecast radio stations don't have duration so set them to #f.
           (let ([duration
                   (guard (e [else #f])
                     (mpv-get-property/long "duration"))])
             (player-controller `(LEN ,duration)))]
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
            (player-controller `(MPV unknown-event-id ,eid ,(mpv-event-name eid)))])
        )))

  (define make-volume-message
    (case-lambda
      [()
       (make-volume-message (mpv-get-property/long "volume"))]
      [(volume)
       `(VOL ,volume ,(mpv-get-property/node "mute"))]))
  )
