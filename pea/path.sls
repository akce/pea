;; PEA path handling.
;; This includes guessing media type based on filename/url.
(library
  (pea path)
  (export
    make-uri uri-url? uri-path
    uri-absolute? uri-media-type)
  (import
    (rnrs)
    (irregex)
    (only (chezscheme) path-absolute? path-extension))

  ;; URI irregex pattern derived from that defined in RFC3986:
  ;; https://tools.ietf.org/html/rfc3986#page-50
  ;; ^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\?([^#]*))?(#(.*))?
  ;;  12            3  4          5       6  7        8 9
  (define uri-pattern
    (irregex
      '(w/nocase 
         (? (submatch-named scheme (+ (~ #\: #\/ #\? #\#))) ":")        ; 2 = scheme
         (? "//" (submatch-named authority (* (~ #\/ #\? #\#))))        ; 4 = authority
         (submatch-named path (* (~ #\? #\#)))                          ; 5 = path
         (? "?" (submatch-named query (* (~ #\#))))                     ; 7 = query
         (? "#" (submatch-named fragment (* nonl)))                     ; 9 = fragment
         )))

  ;; [proc] make-uri: parse the uri string into a uri object.
  ;; A uri object is just an irregex-match object.
  (define make-uri
    (lambda (str)
      (irregex-search uri-pattern str)))

  ;; [proc] uri-url?: Does the uri object refer to a url?
  ;; Assumes that it's a URL if it has a scheme.
  (define uri-url?
    (lambda (uri)
      (irregex-match-substring uri 'scheme)))

  ;; [proc] uri-path: Path contained in uri.
  (define uri-path
    (lambda (uri)
      (irregex-match-substring uri 'path)))

  ;; [proc] uri-absolute?: Does the uri refer to an absolute path or a relative one?
  ;; Relative paths may be joined to build larger paths, whereas absolute must begin a path, or solely be the path.
  (define uri-absolute?
    (lambda (uri)
      (or
        (uri-url? uri)
        (path-absolute? (irregex-match-substring uri 'path)))))

  ;; [proc] uri-media-type: guess general media type based on uri info.
  ;; [return]: AUDIO VIDEO LIST or #f.
  (define uri-media-type
    (let ([audio-hashes (map string-hash '("mp3" "flac" "aac" "wv" "wav" "ogg"))]
          [video-hashes (map string-hash '("mp4" "mkv" "avi" "m4v"))]
          [list-hashes (map string-hash '("" "m3u" "m3u8" "pls"))])
      (lambda (uri)
        (cond
          ;; For now, assume that all URLs are internet radio stations.
          [(uri-url? uri)
           'AUDIO]
          [else
            (let ([ext (string-hash (path-extension (uri-path uri)))])
              (cond
                [(memq ext audio-hashes)
                 'AUDIO]
                [(memq ext video-hashes)
                 'VIDEO]
                [(memq ext list-hashes)
                 'LIST]
                [else
                  #f]))]))))
  )
