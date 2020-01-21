#! /usr/bin/env scheme-script

(import
 (rnrs)
 (srfi :64 testing)
 (pea playlist))     ; module under test

;;;; utils.

;; [proc] rm-f: as sh 'rm -f'.
(define rm-f
  (lambda (fname)
    (guard (x [else #f])
      (delete-file fname))))

;; [proc] write-file: write all lines to filename.
;; newlines are appended to each line.
(define write-file
  (lambda (fn lines)
    (rm-f fn)
    (call-with-output-file
      fn
      (lambda (p)
        (for-each
          (lambda (line)
            (write line p)(newline p))
          lines)))))

;;;; Tests.

(test-begin "playlist")

(define m3u-files-only
  '("file1.mp3"
    "file2.mp3"))

;; Test that m3u is loaded as a playlist.
(define pm3u-ext "pl-test-ext.m3u")
(write-file pm3u-ext m3u-files-only)

(define p (playlist-read pm3u-ext))
(test-assert "m3u ext is playlist" (playlist? p))

;; Test that m3u8 is loaded as a playlist.
(define pm3u8-ext "pl-test-ext.m3u8")
(write-file pm3u8-ext m3u-files-only)
(set! p (playlist-read pm3u8-ext))
(test-assert "m3u ext is playlist" (playlist? p))

(define ppls-ext "pl-test.pls")
(define pls-simple
  '("[playlist]"
    "Version=2"
    "NumberOfFiles=1"
    "File1=file1.mp3"
    "Title1=File 1"))

(write-file ppls-ext pls-simple)
(set! p (playlist-read ppls-ext))
(test-assert "pls ext is playlist" (playlist? p))

(test-end "playlist")
