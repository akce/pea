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

(test-equal "rfc scheme" "http" (irregex-match-substring rfc-uri 'scheme))
(test-equal "rfc auth" "www.ics.uci.edu" (irregex-match-substring rfc-uri 'authority))
(test-equal "rfc path" "/pub/ietf/uri/" (uri-path rfc-uri))
(test-equal "rfc query" #f (irregex-match-substring rfc-uri 'query))
(test-equal "rfc fragment" "Related" (irregex-match-substring rfc-uri 'fragment))

(define rel-path (make-uri "../dir/filename.mp3"))
(test-equal "rel url" #f (uri-url? rel-path))
(test-equal "rel path" "../dir/filename.mp3" (uri-path rel-path))
(test-equal "rel absolute?" #f (uri-absolute? rel-path))

(define abs-path (make-uri "/home/user/media/filename.mp4"))
(test-equal "abs url" #f (uri-url? abs-path))
(test-equal "abs path" "/home/user/media/filename.mp4" (uri-path abs-path))
(test-assert "abs absolute?" (uri-absolute? abs-path))

(test-end "path")
