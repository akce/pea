;; Playlist routines.
;;
;; TODO do playlist format types as custom port type? Access via get-track etc.
;;
;; Both m3u and pls formats have an unofficial spec at:
;;   http://forums.winamp.com/showthread.php?threadid=65772
;;
;; Written by Akce 2020.
;; SPDX-License-Identifier: Unlicense
(library
  (pea playlist)
  (export
    make-track track? track-uri track-title track-type

    make-playlist playlist? playlist-path playlist-tracks
    track-ref

    playlist-map
    ;; general factory func.
    playlist-read
    ;;;; debug exports.
    ;; directory contents.
    dir-read
    ;; .m3u .m3u8 files.
    m3u-read
    ;; .pls files.
    pls-read)
  (import
    (rename (rnrs) (vector-map playlist-map))
    (pea path)
    (pea util)
    (irregex)
    (only (chezscheme) directory-list path-extension path-last path-root))

  ;; Simple track record.
  (define-record-type track
    (fields
      uri
      title
      type
      )
    (protocol
      (lambda (new)
        (lambda (path title)
          (let ([uri (make-uri path)])
            (new uri
                 ;; Set the title to last part of the path if title isn't given (#f).
                 (if title title (path-root (path-last path)))
                 (uri-media-type uri)))))))

  (define-record-type playlist
    (fields
      path		; path contains the track identifying the playlist.
      [mutable tracks]
      )
    (protocol
      (lambda (new)
        (lambda (track)
          (new track '#())))))

  ;; [proc] playlist-read: reads known playlist types and returns contained tracks.
  ;; An empty playlist is returned for unknown playlist file types.
  (define playlist-read
    (lambda (pl)
      (playlist-tracks-set! pl
        (case (track-type (playlist-path pl))
            [(DIR)
             (dir-read (uri->string (track-uri (playlist-path pl))))]
            [(M3U)
             (m3u-read (uri->string (track-uri (playlist-path pl))))]
            [(PLS)
             (pls-read (uri->string (track-uri (playlist-path pl))))]
            [else
              '#()]))))

  ;; [proc] track-ref: shorthand for obtaining the playlist track at index.
  (define track-ref
    (lambda (pl index)
      (vector-ref (playlist-tracks pl) index)))

  ;; [proc] dir-read: loads only known media types from directory.
  (define dir-read
    (lambda (path)
      (list->vector
        (filter
          track-type
          (map
            (lambda (entry)
              (make-track entry #f))
            ;; TODO make the sort comparison function configurable.
            (list-sort string-ci<? (directory-list path)))))))

  ;;;; Filetype: M3U M3U8 */*mpegurl
  ;; For further detail:
  ;;   https://en.wikipedia.org/wiki/M3U
  ;; RFC using a much expanded m3u as the playlist format for media streaming:
  ;;   https://tools.ietf.org/html/rfc8216

  ;; [proc] m3u-read: Returns a playlist of tracks from the m3u file.
  ;; Simple parser that reads paths and optionally #EXTINF track titles.
  (define m3u-read
    (lambda (path)
      (let loop ([lines (slurp path)] [tracks '()] [title #f])
        (cond
          [(null? lines)
           (list->vector (reverse tracks))]
          [(m3u-parse-title (car lines)) => (lambda (x)
                                              (loop (cdr lines) tracks x))]
          [(m3u-empty-line? (car lines))
           ;; skip blank lines.
           (loop (cdr lines) tracks title)]
          [else
            (loop (cdr lines) (cons (make-track (car lines) title) tracks) #f)]))))

  ;; [proc] m3u-parse-title: Extract title from EXTINF metadata line.
  ;; #f if the line is not an EXTINF metadata line.
  ;; Note that a trailing : is treated as part of the title despite the description from the unofficial spec.
  ;; No one seems to use them, and even the examples on that page don't have trailing colons.
  (define m3u-parse-title
    (let ([p (irregex '(w/nocase "#EXTINF:" (* (~ #\,) ) #\, (submatch (* nonl))))])
      (lambda (line)
        (let ([m (irregex-search p line)])
          (if (irregex-match-data? m)
              (irregex-match-substring m 1)
              #f)))))

  ;; [proc] m3u-empty-line?: #t if line is empty.
  ;; Comments and leading whitespace are ignored.
  (define m3u-empty-line?
    (lambda (line)
      (= 0 (string-length (m3u-strip line)))))

  ;; [proc] m3u-strip-comments: strips everything after the comment characters.
  (define m3u-strip
    (let ([p (irregex '(w/nocase (* space) (submatch (* (~ #\#)))))])
      (lambda (line)
        (irregex-match-substring (irregex-search p line) 1))))

  ;;;; Filetype: PLS audio/x-scpls
  ;; Follows m$ ini conventions with [playlist] as the section name and entries:
  ;; - Version=\d+
  ;; - NumberOfEntries=\d+
  ;; - File\d+=.*
  ;; Optional entries:
  ;; - Title\d+=.*
  ;; - Length\d+=-?\d+

  ;; [proc] pls-read: Returns a playlist of tracks from the pls file.
  ;; This is a very forgiving parser: it only looks for File (and optionally) Title lines.
  ;; It ignores case for pls keys.
  ;; It does not look for a playlist section header.
  ;; It ignores the Version and NumberOfEntryFields.
  ;; It treats both ; and # as comment characters.
  (define pls-read
    (lambda (path)
      (let ([lines (pls-strip-lines (slurp path))]
            [files (make-eq-hashtable)]
            [titles (make-eq-hashtable)])
        (for-each
          (lambda (line)
            (cond
              [(pls-parse-file line) => (lambda (x)
                                          (hashtable-set! files (car x) (cdr x)))]
              [(pls-parse-title line) => (lambda (x)
                                           (hashtable-set! titles (car x) (cdr x)))]))
          lines)
        ;; Build and return track listing. Use files for keys as they're mandatory in pls files.
        (playlist-map
          (lambda (key)
            (make-track
              (hashtable-ref files key #f)
              (hashtable-ref titles key #f)))
          (vector-sort < (hashtable-keys files))))))

  ;; [proc] pls-parse-file: Parse a pls File line.
  ;; ie, File23=blah -> '(23 "blah")
  ;; Non matching lines are returned as #f.
  (define pls-parse-file
    (let ([p (irregex '(w/nocase "File" (submatch (+ num)) "=" (submatch (+ any))))])
      (lambda (line)
        (let ([m (irregex-search p line)])
          (if (irregex-match-data? m)
              (cons (string->number (irregex-match-substring m 1)) (irregex-match-substring m 2))
              #f)))))

  ;; [proc] pls-parse-title: Parse a pls Title line.
  ;; ie, Title23=blah -> '(23 "blah")
  ;; Non matching lines are returned as #f.
  (define pls-parse-title
    (let ([p (irregex '(w/nocase "Title" (submatch (+ num)) "=" (submatch (+ any))))])
      (lambda (line)
        (let ([m (irregex-search p line)])
          (if (irregex-match-data? m)
              (cons (string->number (irregex-match-substring m 1)) (irregex-match-substring m 2))
              #f)))))

  ;; [proc] pls-strip-comments: strips everything after the comment characters.
  ;; This implementation treats both ; and # as comment chars.
  (define pls-strip-comments
    (let ([p (irregex '(w/nocase (submatch (* (~ #\; #\#)))))])
      (lambda (line)
        (irregex-match-substring (irregex-search p line) 1))))

  ;; [proc] pls-strip: strips comments and surrounding whitespace from a line.
  (define pls-strip
    (lambda (line)
      (string-trim-both (pls-strip-comments line))))

  ;; [proc] pls-strip-lines: applies pls-strip to all lines in the list.
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
