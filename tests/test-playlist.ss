#! /usr/bin/env scheme-script

(import
 (rnrs)
 (srfi :64 testing)
 (pea path)
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

;;;; Format tests.

(test-begin "playlist")

(define files-only
  '#("file1.mp3"
     "file2.mp3"))

;; Test that m3u is loaded as a playlist.
(define pm3u-ext (make-playlist "pl-test-ext.m3u"))
(test-assert "m3u ext is playlist" (playlist? pm3u-ext))
(test-eq "new m3u ext is empty" '#() (playlist-tracks pm3u-ext))

;; Some map helper functions.
(define track-path
  (lambda (t)
    (uri->string (track-uri t))))

(define playlist-track-title
  (lambda (pl index)
    (track-title (track-ref pl index))))

(write-file (playlist-path pm3u-ext) files-only)
(playlist-read pm3u-ext)
(test-equal "loaded m3u ext track paths" files-only (vector-map track-path (playlist-tracks pm3u-ext)))
(test-equal "loaded m3u ext track titles" '#("file1" "file2") (vector-map track-title (playlist-tracks pm3u-ext)))
(test-equal "m3u empty track title index 0" "file1" (playlist-track-title pm3u-ext 0))
(test-equal "m3u empty track title index 1" "file2" (playlist-track-title pm3u-ext 1))

;; Test that m3u8 is loaded as a playlist.
(define pm3u8-ext (make-playlist "pl-test-ext.m3u8"))
(test-assert "m3u8 ext is playlist" (playlist? pm3u8-ext))

(write-file (playlist-path pm3u8-ext) files-only)
(playlist-read pm3u8-ext)
(test-equal "loaded m3u8 ext track paths" files-only (vector-map track-path (playlist-tracks pm3u8-ext)))
(test-equal "loaded m3u8 ext track titles" '#("file1" "file2") (vector-map track-title (playlist-tracks pm3u8-ext)))

(define ppls-ext (make-playlist "pl-test.pls"))
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
(test-equal "loaded pls ext track titles" '#("file1" "file2") (vector-map track-title (playlist-tracks ppls-ext)))
(test-equal "pls empty track title index 0" "file1" (playlist-track-title ppls-ext 0))
(test-equal "pls empty track title index 1" "file2" (playlist-track-title ppls-ext 1))

;;;; Generic playlist tests.
;; Using m3u as the base.

(define root-m3u-content
  '#("#EXTINF:-1,Url path test"
     "http://home-pi.localnet/list.m3u"
     "#EXTINF:,Absolute root path"
     "/home/pea/media"
     "#EXTINF:,Absolute root mp3 file"
     "/home/pea/media/file 1.mp3"
     ;; TODO relative path from root playlist
     ))

(define pl-root (make-playlist "test-root.m3u"))
(write-file (playlist-path pl-root) root-m3u-content)
(playlist-read pl-root)

(test-assert "track? and track-ref" (track? (track-ref pl-root 0)))
(test-equal "track http://m3u is audio" 'AUDIO (track-type (track-ref pl-root 0)))
(test-equal "track dir is list" 'LIST (track-type (track-ref pl-root 1)))
(test-equal "track type is audio" 'AUDIO (track-type (track-ref pl-root 2)))

(test-end "playlist")
