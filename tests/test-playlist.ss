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
        (vector-for-each
          (lambda (line)
            (display line p)(newline p))
          lines)))))

;;;; Tests.

(test-begin "playlist")

(define files-only
  '#("file1.mp3"
     "file2.mp3"))

;; Test that m3u is loaded as a playlist.
(define pm3u-ext (playlist-make "pl-test-ext.m3u"))
(test-assert "m3u ext is playlist" (playlist? pm3u-ext))
(test-eq "new m3u ext is empty" '#() (playlist-tracks pm3u-ext))

(write-file (playlist-path pm3u-ext) files-only)
(playlist-read pm3u-ext)
(test-equal "loaded m3u ext track paths" files-only (vector-map track-path (playlist-tracks pm3u-ext)))
(test-equal "loaded m3u ext track titles" '#(#f #f) (vector-map track-title (playlist-tracks pm3u-ext)))

;; Test that m3u8 is loaded as a playlist.
(define pm3u8-ext (playlist-make "pl-test-ext.m3u8"))
(test-assert "m3u8 ext is playlist" (playlist? pm3u8-ext))

(write-file (playlist-path pm3u8-ext) files-only)
(playlist-read pm3u8-ext)
(test-equal "loaded m3u8 ext track paths" files-only (vector-map track-path (playlist-tracks pm3u8-ext)))
(test-equal "loaded m3u8 ext track titles" '#(#f #f) (vector-map track-title (playlist-tracks pm3u8-ext)))

(define ppls-ext (playlist-make "pl-test.pls"))
(test-assert "pls ext is playlist" (playlist? ppls-ext))

(define pls-files-only
  '#("[playlist]"
     "Version=2"
     "NumberOfFiles=2"
     "File1=file1.mp3"
     "File2=file2.mp3"))

(write-file (playlist-path ppls-ext) pls-files-only)
(test-assert "pls ext is playlist" (playlist? ppls-ext))
(test-eq "new pls ext is empty" '#() (playlist-tracks ppls-ext))
(playlist-read ppls-ext)
(test-equal "loaded pls ext track paths" files-only (vector-map track-path (playlist-tracks ppls-ext)))
(test-equal "loaded pls ext track titles" '#(#f #f) (vector-map track-title (playlist-tracks ppls-ext)))

(test-end "playlist")
