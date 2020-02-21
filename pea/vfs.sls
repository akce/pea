;; PEA virtual filesystem (vfs).
;;
;; The vfs maps a playlist (including sub-playlists) to a virtual filesystem.
(library
  (pea vfs)
  (export
    make-vfs vfs? vfs-playlist
    vfs-enter! vfs-pop! vfs-root!
    vfs-path vfs-vpath
    vfs-crumbs->path
    vfs-tracks->paths
    (rename
      (hashtable? vfs-state?)
      (hashtable-keys vfs-state-vpaths))
    vfs-state-make vfs-state-set! vfs-state-get-track-index vfs-state-get-track-label vfs-state-read vfs-state-save)
  (import
    (rnrs)
    (pea path)
    (pea playlist)
    (pea util))

  (define sep "/")

  (define-record-type vfs
    (fields
      [mutable	crumbs]		; list of tracks leading to current playlist.
				; NB: crumbs are stored as a LIFO stack. eg, crumbs[0] is the current track.
				; NB: this makes it very easy to push and pop playlists.
      [mutable	playlist]	; cache of current playlist. TODO may not be needed.
      )
    (protocol
      (lambda (new)
        ;; create and initialise vfs with root playlist path.
        (lambda (root-playlist-path)
          (let* ([root-track (make-track root-playlist-path sep)]
                 [pl (make-playlist root-track)])
            (playlist-read pl)
            (new (list root-track) pl))))))

  ;;;; vfs-pl: vfs playlist storage.

  (define vfs-vpath
    (lambda (vfs)
      (reverse-map track-title (vfs-crumbs vfs))))

  (define vfs-path
    (lambda (vfs)
      (vfs-crumbs->path (vfs-crumbs vfs))))

  ;; [proc] vfs-tracks->paths: return a vector of real paths for each track.
  (define vfs-tracks->paths
    (lambda (vfs)
      ;; TODO recalculating the base vfs-crumbs path is wasteful, so look at optimising at some point.
      ;; TODO the issue is the mix of track and uri types...
      ;; TODO maybe all this should be calculated in vfs-enter! and cached in the vfs record?
      (playlist-map
        (lambda (t)
          (vfs-crumbs->path (cons t (vfs-crumbs vfs))))
        (playlist-tracks (vfs-playlist vfs)))))

  ;; [proc] vfs-crumbs->path: convert a list of crumbs to a path string.
  ;; NB: using crumbs, which are a reversed list of tracks.
  (define vfs-crumbs->path
    (lambda (crumbs)
      ;; Strip playlist files from leading directories (but not from the final
      ;; crumb!) when building the path.
      (define (uri-drop-playlist t)
        (case (track-type t)
          [(DIR)
            (track-uri t)]
          [(M3U PLS)
            (uri-strip-file (track-uri t))]))

      (uri-build-path (reverse
                        (cons
                          (track-uri (car crumbs))
                          (map uri-drop-playlist (cdr crumbs)))))))

  ;; [proc] vfs-enter!: sets the track at index as current.
  ;; [return]: vpath for the new current playlist. #f on failure.
  ;; NOTE: The track type must be one of (M3U PLS).
  (define vfs-enter!
    (lambda (vfs index)
      (let* ([parent-pl (vfs-playlist vfs)]
             [t (track-ref parent-pl index)])
        (case (track-type t)
          [(DIR M3U PLS)
            ;;;; Enter the list.
            (vfs-rebuild! vfs (cons t (vfs-crumbs vfs)))]
          [else
            ;; track is not a list item.
            #f]))))

  ;; [proc] vfs-pop!: back to parent playlist.
  (define vfs-pop!
    (lambda (vfs)
      (let ([tail (cdr (vfs-crumbs vfs))])
        (cond
          [(null? tail)		; don't remove the root crumb.
           #f]
          [else
            (vfs-rebuild! vfs tail)]))))

  ;; [proc] vfs-root!: back to root playlist.
  (define vfs-root!
    (lambda (vfs)
      (vfs-rebuild! vfs (list (car (reverse (vfs-crumbs vfs)))))))

  ;; [proc] vfs-rebuild!: rebuild playlist using new set of crumbs.
  (define vfs-rebuild!
    (lambda (vfs crumbs)
      (vfs-crumbs-set! vfs crumbs)
      ;; TODO revisit the relationship between playlist-path and crumbs.
      ;; TODO for now, explicitly marking the playlist-path track-title as unused.
      (vfs-playlist-set! vfs (make-playlist (make-track (vfs-path vfs) 'UNUSED)))
      (playlist-read (vfs-playlist vfs))))

  ;;;; vfs-state
  ;; The vfs-state object is a very thin wrapper around a hashtable.
  ;; Key: vpath => '(track-index . track-label)

  ;; [proc] vfs-state-make: Create an empty vfs-state object.
  (define vfs-state-make
    (lambda ()
      (make-hashtable string-hash string-ci=?)))

  ;; [proc] vfs-state-set!: Set the current track index/label for virtual path.
  (define vfs-state-set!
    (lambda (vfs-state vpath track-index track-label)
      (hashtable-set! vfs-state vpath (cons track-index track-label))))

  ;; [proc] vfs-state-get-track-index: Get the last track-index for vpath.
  (define vfs-state-get-track-index
    (lambda (vfs-state vpath)
      (car (hashtable-ref vfs-state vpath '(#f . #f)))))

  ;; [proc] vfs-state-get-track-label: Get the last track-label for vpath.
  (define vfs-state-get-track-label
    (lambda (vfs-state vpath)
      (cdr (hashtable-ref vfs-state vpath '(#f . #f)))))

  ;; [proc] vfs-state-read: Loads a vfs-state object from file.
  (define vfs-state-read
    (lambda (path)
      (call-with-input-file
        path
        (lambda (fp)
          (let ([vs (vfs-state-make)])
            (vector-for-each
              (lambda (record)
                (vfs-state-set! vs (car record) (caadr record) (cdadr record)))
              (read fp))
            vs)))))

  ;; [proc] vfs-state-save: Serialises a vfs-state object to file.
  (define vfs-state-save
    (lambda (vfs-state path)
      ;; R6RS call-with-output-file doesn't allow overwrite so use open-file-output-port instead.
      ;; TODO? Write to tmpfile and then overwrite path if successful.
      (let ([outp (open-file-output-port path (file-options no-fail) (buffer-mode block) (make-transcoder (utf-8-codec)))])
        (display ";; Autogenerated PEA state file. Edit only if you know what you're doing. -*- scheme -*-" outp)
        (newline outp)
        ;; Consider using pretty-print or store as individual items in state file.
        ;; Would make it easier to hand modify if each item was on its own line.
        (write
          (vector-map
            (lambda (k)
              (list k (hashtable-ref vfs-state k '(#f . #f))))
            (hashtable-keys vfs-state))
          outp)
        (newline outp)
        ;; Split the modeline to stop vim from using it when editing this actual source file.
        ;; Modeline is typically scanned in the bottom 5 lines. See :help mls in vim.
        (display ";; v" outp)(display "im:set ft=scheme" outp)
        (newline outp)
        ;; TODO? enclose this in a dynamic-wind.
        (close-port outp)))))
