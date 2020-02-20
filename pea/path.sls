;; PEA path handling.
;; This includes guessing media type based on filename/url.
(library
  (pea path)
  (export
    make-uri uri? uri->string uri-scheme uri-authority uri-path uri-query uri-fragment
    uri-url? uri-absolute?
    uri-media-type uri-build-path
    )
  (import
    (rnrs)
    (pea util)
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

  (define-record-type uri
    (fields
      ;; [proc] uri->string: return the string representation of uri.
      ;; Note that the string returned is the one used to create the uri.
      ;; ie,
      ;; > (define s "https://localhost.localnet/home/dir")
      ;; > (eq? s (uri->string (make-uri s)))
      ;; #t
      [immutable string uri->string]
      ;; The following fields are as per the uri-pattern.
      scheme
      authority
      path
      query
      fragment)
    (protocol
      (lambda (new)
        ;; [proc] make-uri: parse the uri string into a uri object.
        (lambda (str)
          ;; TODO truncate consecutive '/' chars in paths?
          ;; TODO strip trailing '/' chars in authority and paths?
          (let ([m (irregex-search uri-pattern str)])
            (new
              str
              (irregex-match-substring m 'scheme)
              (irregex-match-substring m 'authority)
              (irregex-match-substring m 'path)
              (irregex-match-substring m 'query)
              (irregex-match-substring m 'fragment)))))))

  ;; [proc] uri-url?: Does the uri object refer to a url?
  ;; Assumes that it's a URL if it has a scheme.
  (define uri-url?
    (lambda (uri)
      (uri-scheme uri)))

  ;; [proc] uri-absolute?: Does the uri refer to an absolute path or a relative one?
  ;; Relative paths may be joined to build larger paths, whereas absolute must begin a path, or solely be the path.
  (define uri-absolute?
    (lambda (uri)
      (or
        (uri-url? uri)
        (path-absolute? (uri-path uri)))))

  ;; [proc] uri-media-type: guess general media type based on uri info.
  ;; [return]: AUDIO VIDEO LIST or #f.
  (define uri-media-type
    (let ([audio-hashes (map string-hash '("mp3" "flac" "aac" "wv" "wav" "ogg"))]
          [video-hashes (map string-hash '("mp4" "mkv" "avi" "m4v"))]
          [list-hashes (map string-hash '("m3u" "m3u8" "pls"))]
          [dir-hash (string-hash "")])
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
                [(eq? ext dir-hash)
                 'DIR]
                [else
                  #f]))]))))

  ;; [proc] uri-build-path: builds a real path based on a list of URIs.
  ;; [return] string representing the URI.
  ;; Used to build a real path when traversing playlists.
  ;; An absolute path resets the return path, relative paths are appended.
  ;;
  ;; TODO return a uri object?
  (define uri-build-path
    (lambda (uris)
      (define (get-parts us parts)
        (cond
          [(null? us)
           (reverse parts)]
          [else
            (let ([u (car us)])
              (get-parts (cdr us)
                         (if (uri-absolute? u)
                             (list u)
                             (cons u parts))))]))
      (apply string-join "/" (map uri->string (get-parts uris '())))))
  )
