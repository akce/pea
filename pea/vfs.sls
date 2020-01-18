;; PEA virtual filesystem (vfs).
;;
;; The vfs maps a playlist (including sub-playlists) to a virtual filesystem.
(library
  (pea vfs)
  (export
    vfs-make vfs? vfs-playlist vfs-vpath
    (rename
      (hashtable? vfs-state?)
      (hashtable-keys vfs-state-vpaths))
    vfs-state-make vfs-state-set! vfs-state-get-track-index vfs-state-get-track-label vfs-state-read vfs-state-save)
  (import
    (rnrs)
    (pea playlist))

  (define-record-type vfs
    (fields playlist vpath))

  ;; [proc] vfs-make: create and initialise vfs with root playlist-path.
  (define vfs-make
    (lambda (playlist-path)
      (make-vfs (playlist-read playlist-path) "/")))

  ;; [proc] vfs-add: load contents at playlist-path and store at vpath.
  ;; Acts as a refresh if vpath is already loaded.
  (define vfs-add
    (lambda (vfs playlist-path vpath)
      #f))

  ;; [proc] vfs-refresh: reload playlist contents at vpath.
  (define vfs-refresh
    (lambda (vfs vpath)
      #f))

  ;; [proc] vfs-display-list: list of display labels for vfs contents at vpath.
  (define vfs-display-list
    (lambda (vfs vpath)
      '()))

  ;; [proc] vfs-path-list: list of real world paths for contents at vpath.
  (define vfs-path-list
    (lambda (vfs vpath)
      '()))

  ;; [proc] vfs-track-path: real world path for vfs track at vpath.
  (define vfs-track-path
    (lambda (vfs vpath track-index)
      #f))

  ;; [proc] vfs-load!: internally load playlist referenced by vpath.
  (define vfs-load!
    (lambda (vfs vpath)
      #f))

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
