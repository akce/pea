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

    make-cursor cursor? cursor-index
    cursor-sync!
    cursor-move! cursor-set!
    cursor-save
    )
  (import
    (rnrs)
    (only (rnrs mutable-pairs) set-cdr!)
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

  ;;;; Cursor: pointer/selector of vfs tracks.
  (define-record-type cursor
    (fields
      [immutable	vfs]		; The vfs the cursor watches.
      [immutable	filepath]	; Path to storage file.
      [mutable		index]		; Selected track index or key.
      [mutable		alist]		; vpath-hash -> index mapping.
      )
    (protocol
      (lambda (new)
        ;; TODO disable filestore if filepath is false?
        (lambda (vfs filepath)
          (let ([alist (cursor-load filepath)])
            (new vfs filepath (get-pos-from-alist (vpath-hash (vfs-vpath vfs)) alist) alist))))))

  ;; [proc] cursor-sync!: update cursor based on vfs state change.
  (define cursor-sync!
    (lambda (cursor)
      ;; Calling cursor-set! rather than cursor-index-set! accounts for possible playlist length
      ;; changes between syncs.
      (cursor-set!
        cursor
        (get-pos-from-alist (cursor-vpath-hash cursor) (cursor-alist cursor)))))

  ;; [proc] cursor-move!: moves cursor position by offset amount.
  ;; Use a negative offset to go up the list, positive to go down.
  ;; An error is raised if movement is attempted on an empty vfs playlist.
  (define cursor-move!
    (lambda (cursor offset)
      (cursor-set! cursor (+ (cursor-index cursor) offset))))

  ;; [proc] cursor-set!: sets absolute position of cursor.
  ;; cursor position will be changed to fit within playlist bounds.
  ;; error is thrown if the vfs playlist is empty.
  (define cursor-set!
    (lambda (cursor pos)
      ;; Using 'setter' to avoid multiple calls to 'cursor-playlist-length'.
      ;; Could use nested conditionals but this is the first time i've used named lets this way.
      (define pl-len (cursor-playlist-length cursor))
      (let setter ([i pos])
        (cond
          [(= pl-len 0)
           (error 'cursor-set "attempt to set position on empty playlist." i)]
          [(< i 0)
           (setter 0)]
          [(>= i pl-len)
           (setter (- pl-len 1))]
          [(= i (cursor-index cursor))	; position unchanged (do nothing).
           (if #f #f)]
          [else		; cursor pos/i is within bounds.
            (cursor-index-set! cursor i)
            (cursor-update-alist! cursor i)]))))

  ;; [proc] cursor-save: Writes a cursor-alist to file.
  (define cursor-save
    (lambda (cursor)
      ;; R6RS call-with-output-file doesn't allow overwrite so use open-file-output-port instead.
      ;; TODO? Write to tmpfile and then overwrite path if successful.
      (let ([outp (open-file-output-port
                    (cursor-filepath cursor)
                    (file-options no-fail)
                    (buffer-mode block)
                    (make-transcoder (utf-8-codec)))])
        (display ";; Autogenerated PEA state file. Edit only if you know what you're doing. -*- scheme -*-" outp)
        (newline outp)
        ;; Consider using pretty-print or store as individual items in state file.
        ;; Would make it easier to hand modify if each item was on its own line.
        (write (cursor-alist cursor) outp)
        (newline outp)
        ;; Split the modeline to stop vim from using it when editing this actual source file.
        ;; Modeline is typically scanned in the bottom 5 lines. See :help mls in vim.
        (display ";; v" outp)(display "im:set ft=scheme" outp)
        (newline outp)
        ;; TODO? enclose this in a dynamic-wind.
        (close-port outp))))

  ;;;; internal/private cursor helper functions.

  ;; [proc] cursor-load: Loads cursor-alist from file.
  (define cursor-load
    (lambda (path)
      (if (file-exists? path)
          (call-with-input-file
            path
            (lambda (fp)
              (read fp)))
          '())))

  ;; [proc] get-pos-from-alist: retrieves the cached position from stored list.
  (define get-pos-from-alist
    (lambda (vhash alist)
      (cond
        [(assq vhash alist) => cdr]
        [else 0])))

  ;; [proc] vpath-hash: create a hash for the vpath.
  (define vpath-hash
    (lambda (vpath)
      (string-hash (apply string-join "" vpath))))

  ;; [proc] cursor-update-alist!: updates the saved position for the cursor.
  ;; This is also does a bit of housekeeping in that zero positions are not stored and actively removed.
  ;; The rationale is that zero is the default position, so there's no point in saving and searching through them.
  ;; TODO add a timestamp so unused entries are culled?
  (define cursor-update-alist!
    (lambda (cursor pos)
      (let ([vhash (cursor-vpath-hash cursor)])
        (cond
          [(= pos 0)				; pos 0 is the default so no need to store in alist.
           (cursor-alist-set!
             cursor
             (remp
               (lambda (al)
                 (= (car al) vhash))		; remove old entry.
               (cursor-alist cursor)))]
          [(assq vhash (cursor-alist cursor))	; update existing entry.
           => (lambda (x)
                (set-cdr! x pos))]
          [else					; add new entry.
            (cursor-alist-set! cursor (cons (cons vhash pos) (cursor-alist cursor)))]))))

  ;; accessor shortcut.
  (define cursor-vpath-hash
    (lambda (cursor)
      (vpath-hash (vfs-vpath (cursor-vfs cursor)))))

  ;; accessor shortcut.
  (define cursor-playlist-length
    (lambda (cursor)
      (vector-length (playlist-tracks (vfs-playlist (cursor-vfs cursor))))))
  )
