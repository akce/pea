;; PEA path handling.
;; This includes guessing media type based on filename/url.
(library
  (pea path)
  (export
    make-uri uri? uri->string uri-scheme uri-authority uri-path uri-query uri-fragment
    uri-url? uri-absolute?
    uri-strip-file uri-media-type uri-join-path
    )
  (import
    (rnrs)
    (pea util)
    (irregex)
    (only (chezscheme) file-directory? path-absolute? path-extension path-parent))

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
        (case-lambda
          ;; [proc] make-uri uri-string: parse the uri string into a uri object.
          [(str)
           ;; TODO truncate consecutive '/' chars in paths?
           ;; TODO strip trailing '/' chars in authority and paths?
           (let ([m (irregex-search uri-pattern str)])
             (new
               str
               (irregex-match-substring m 'scheme)
               (irregex-match-substring m 'authority)
               (irregex-match-substring m 'path)
               (irregex-match-substring m 'query)
               (irregex-match-substring m 'fragment)))]
          ;; [proc] make-uri scheme auth path query fragment: create uri based on component parts.
          [(scheme authority path query fragment)
           (new (string-append
                  ;; recreate uri string. ie, uri-pattern in reverse.
                  (if scheme (string-append scheme ":") "")
                  (if authority (string-append "//" authority) "")
                  (if path path "")
                  (if query (string-append "?" query) "")
                  (if fragment (string-append "#" fragment) ""))
                scheme authority path query fragment)]))))

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

  ;; [proc] uri-strip-file: remove component from uri path.
  ;; [return] new uri if path was changed, original uri object if unchanged.
  (define uri-strip-file
    (lambda (uri)
      (if (uri-path uri)
          (make-uri
            (uri-scheme uri)
            (uri-authority uri)
            (path-parent (uri-path uri))
            (uri-query uri)
            (uri-fragment uri))
          uri)))

  ;; [proc] uri-media-type: guess general media type based on uri info.
  ;; [return]: AUDIO VIDEO M3U PLS DIR or #f.
  ;;
  ;; Every type of file that PEA handles gets its own category here.
  ;; This will be the one place where file types are defined.
  (define uri-media-type
    (let ([audio-hashes (map string-hash '("mp3" "flac" "aac" "m4a" "wv" "wav" "ogg"))]
          [video-hashes (map string-hash '("mp4" "mkv" "avi" "m4v"))]
          [m3u-hashes (map string-hash '("m3u" "m3u8"))]
          [pls-hash (string-hash "pls")]
          [dir-hash (string-hash "")])
      (lambda (uri . parent-uri)
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
                [(memq ext m3u-hashes)
                 'M3U]
                [(eq? ext pls-hash)
                 'PLS]
                [(eq? ext dir-hash)
                 'DIR]
                ;; As a last resort there's a stat to see if it's a directory.
                [(file-directory?
                   (uri-path
                     (if (null? parent-uri)
                         uri
                         (uri-join-path (car parent-uri) uri))))
                 'DIR]
                [else
                  #f]))]))))

  ;; [proc] uri-join-path: builds a real-path-uri based on a list of URIs.
  ;; [return] URI of joined path.
  ;; Used to build a real path when traversing playlists.
  ;; An absolute path resets the return path, relative paths are appended.
  (define uri-join-path
    (lambda uris
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
      (define (uri->path u)
        (let ([s (uri->string u)])
          (if (or (not s) (string=? "" s)) "." s)))
      (make-uri
        (apply string-join "/"
               (map uri->path (get-parts uris '()))))))
  )
