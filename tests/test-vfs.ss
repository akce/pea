#! /usr/bin/env scheme-script

(import
 (rnrs)
 (srfi :64 testing)
 (pea path)
 (pea playlist)
 (pea vfs))     ; module under test

(test-begin "vfs")

(define pl-root "vfs-pl-root.m3u")
(define pl-list1 "tests/list1.m3u")
(define pl-list2 "tests/list2.m3u")

(guard (x [else #f])
  (delete-file pl-root))
(guard (x [else #f])
  (delete-file pl-list1))
(guard (x [else #f])
  (delete-file pl-list2))

;; The playlists need to test for various combinations or relative and absolute paths.
;; Especially with regards to paths relative to playlist files.
(call-with-output-file
  pl-root
  (lambda (p)
    (display "#EXTINF:-1,list1" p)(newline p)
    (display "tests/list1.m3u" p)(newline p)
    (display "#EXTINF:-1,tests dir" p)(newline p)
    (display "tests" p)(newline p)
    (display "#EXTINF:-1,rel to root" p)(newline p)
    (display "file1.mp3" p)(newline p)
    ))

(call-with-output-file
  pl-list1
  (lambda (p)
    (display "#EXTINF:-1,file inside tests dir" p)(newline p)
    (display "file2.mp3" p)(newline p)
    (display "#EXTINF:-1,list2 inside tests dir" p)(newline p)
    (display "list2.m3u" p)(newline p)
    ))

(call-with-output-file
  pl-list2
  (lambda (p)
    (display "#EXTINF:-1,file3 also in tests dir" p)(newline p)
    (display "file3.mp3" p)(newline p)
    (display "#EXTINF:-1,absolute path video file" p)(newline p)
    (display "/home/file4.mp4" p)(newline p)
    ))

(define v (make-vfs pl-root))

(define (track-path t)
  (uri->string (track-uri t)))

(test-equal "test root vpath" '("/") (vfs-vpath v))
(test-equal "test root path" pl-root (vfs-path v))
(test-equal "test root titles"
            '#("list1" "tests dir" "rel to root")
            (playlist-map track-title (playlist-tracks (vfs-playlist v))))
(test-equal "test root track-uri"
            '#("tests/list1.m3u" "tests" "file1.mp3")
            (playlist-map track-path (playlist-tracks (vfs-playlist v))))

(test-equal "test root track paths" '#("./tests/list1.m3u" "./tests" "./file1.mp3") (vfs-tracks->paths v))

(vfs-enter! v 0)

(test-equal "test list1 vpath" '("/" "list1") (vfs-vpath v))
(test-equal "test list1 path" "./tests/list1.m3u" (vfs-path v))

(test-equal "test list1 track paths" '#("./tests/file2.mp3" "./tests/list2.m3u") (vfs-tracks->paths v))

(vfs-enter! v 1)

(test-equal "test list2 vpath" '("/" "list1" "list2 inside tests dir") (vfs-vpath v))
(test-equal "test list2 path" "./tests/list2.m3u" (vfs-path v))
;; TODO clean up the file3 path. It works and is correct but it's messy.
(test-equal "test list2 track paths" '#("./tests/./file3.mp3" "/home/file4.mp4") (vfs-tracks->paths v))

(test-end "vfs")

