;; Playlist routines.
;;
;; TODO do playlist format types as custom port type? Access via get-track etc.
;;
;; Written by Akce 2020.
;; SPDX-License-Identifier: Unlicense
(library
  (pea playlist)
  (export
    pls-read
    )
  (import
    (rnrs)
    (irregex))

  ;; pls track.
  (define-record-type track
    (fields title number path))

  (define slurp
    (lambda (path)
      (let ([f (open-file-input-port
                 path
                 (file-options no-create)
                 (buffer-mode line)
                 (make-transcoder (utf-8-codec)))])
        (let loop ([line (get-line f)] [lines '()])
          (cond
            [(eof-object? line)
             (close-input-port f)
             (reverse lines)]
            [else
              (loop (get-line f) (cons line lines))])))))

  ;; PLS audio/x-scpls
  ;; Follows m$ ini conventions with [playlist] as the section name and entries:
  ;; - Version=\d+
  ;; - NumberOfEntries=\d+
  ;; - File\d+=.*
  ;; Optional entries:
  ;; - Title\d+=.*
  ;; - Length\d+=-?\d+
  (define pls-read
    (lambda (path)
      (pls-strip-lines (slurp path))))

  (define pls-strip-comments
    (let ([p (irregex '(w/nocase (submatch (* (~ #\; #\#)))))])
      (lambda (line)
        (irregex-match-substring (irregex-search p line) 1))))

  (define pls-strip-space
    (let ([p (irregex '(w/nocase (* space ) (submatch (* nonl)) (* space)))])
      (lambda (line)
        (irregex-match-substring (irregex-search p line) 1))))

  ;; strips comments and surrounding whitespace.
  (define pls-strip
    (lambda (line)
      (pls-strip-space (pls-strip-comments line))))

  (define pls-strip-lines
    (lambda (lines)
      (let loop ([ls lines] [acc '()])
      (cond
        [(null? ls) (reverse acc)]
        [else
          (let ([stripped (pls-strip (car ls))])
            (loop (cdr ls)
                  (if (string=? "" stripped)
                      acc
                      (cons stripped acc))))]))))
  )

