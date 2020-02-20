#! /usr/bin/env scheme-script

(import
 (rnrs)
 (srfi :64 testing)
 (irregex)
 (pea path))     ; module under test

(test-begin "path")

;; A test uri copied from RFC3986.
(define rfc-uri (make-uri "http://www.ics.uci.edu/pub/ietf/uri/#Related"))

(test-assert "rfc url" (uri-url? rfc-uri))

(test-equal "rfc scheme" "http" (uri-scheme rfc-uri))
(test-equal "rfc auth" "www.ics.uci.edu" (uri-authority rfc-uri))
(test-equal "rfc path" "/pub/ietf/uri/" (uri-path rfc-uri))
(test-equal "rfc query" #f (uri-query rfc-uri))
(test-equal "rfc fragment" "Related" (uri-fragment rfc-uri))

(define rel-path (make-uri "../dir/filename.mp3"))
(test-equal "rel url" #f (uri-url? rel-path))
(test-equal "rel path" "../dir/filename.mp3" (uri-path rel-path))
(test-equal "rel absolute?" #f (uri-absolute? rel-path))

(define abs-path (make-uri "/home/user/media/filename.mp4"))
(test-equal "abs url" #f (uri-url? abs-path))
(test-equal "abs path" "/home/user/media/filename.mp4" (uri-path abs-path))
(test-assert "abs absolute?" (uri-absolute? abs-path))

;;; Media types tests.
(define media-list '("a.mp3" "a.flac" "a.aac" "a.wv" "a.wav" "a.ogg"
                     "a.mp4" "a.mkv" "a.avi" "a.m4v"
                     "a" "a.m3u" "a.m3u8" "a.pls"
                     "a.txt" "a.log" "a.webm"))
(define uri-list (map make-uri media-list))
(test-equal "media types" '(AUDIO AUDIO AUDIO AUDIO AUDIO AUDIO
                                  VIDEO VIDEO VIDEO VIDEO
                                  DIR LIST LIST LIST
                                  #f #f #f)
            (map uri-media-type uri-list))

(test-equal "ALL urls are AUDIO" '(AUDIO AUDIO AUDIO)
            (map uri-media-type
                 (map make-uri
                      '("http://localhost/" "file:///home/playlist"
                        ;; NOTE for now that also includes URLs to video files!
                        "http://127.0.0.1:80/a.mkv"))))

;;; Real path construction tests.
(test-equal "single relative path" "a" (uri-build-path (map make-uri '("a"))))
(test-equal "single absolute dir" "/a" (uri-build-path (map make-uri '("/a"))))
(test-equal "single absolute url" "file:///" (uri-build-path (map make-uri '("file:///"))))
(test-equal "a + /b" "/b" (uri-build-path (map make-uri '("a" "/b"))))
(test-equal "a + http://localhost/" "http://localhost/" (uri-build-path (map make-uri '("a" "http://localhost/"))))
(test-equal "/b + a" "/b/a" (uri-build-path (map make-uri '("/b" "a"))))
(test-skip "/b/ + a" "/b/a" (uri-build-path (map make-uri '("/b/" "a"))))
(test-equal "http://localhost + a" "http://localhost/a" (uri-build-path (map make-uri '("http://localhost" "a"))))
(test-skip "http://localhost/ + a" "http://localhost/a" (uri-build-path (map make-uri '("http://localhost/" "a"))))
(test-equal "a + /b + c" "/b/c" (uri-build-path (map make-uri '("a" "/b" "c"))))
(test-equal "a + http://localhost + c" "http://localhost/c" (uri-build-path (map make-uri '("a" "http://localhost" "c"))))

(test-end "path")
