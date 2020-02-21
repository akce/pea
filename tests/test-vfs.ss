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

(test-begin "vfs-state")

(define key1 "/key1")
(define key2 "/key2")
(define item1 '(4 . "name1"))
(define item1-1 '(0 . "name0"))
(define item2 '(1 . "name2"))

(define save-file "vfs-state-test.pss")

(test-assert "type" (vfs-state? (vfs-state-make)))

(define vs (vfs-state-make))

(test-equal "empty vpaths" '() (list-sort string-ci<? (vector->list (vfs-state-vpaths vs))))

(test-eq "empty track index" #f (vfs-state-get-track-index vs "/"))
(test-eq "empty track label" #f (vfs-state-get-track-label vs "/"))

;; Serialise.
(vfs-state-save (vfs-state-make) save-file)
(define vsload (vfs-state-read save-file))
(test-equal "loaded empty vpaths" '() (list-sort string-ci<? (vector->list (vfs-state-vpaths vsload))))

;; Add an item.

(vfs-state-set! vs key1 (car item1) (cdr item1))

;; Check it was added correctly.
(test-equal "one item vpaths" (list key1) (list-sort string-ci<? (vector->list (vfs-state-vpaths vs))))

(test-eqv "one item track index" (car item1) (vfs-state-get-track-index vs key1))
(test-equal "one item track label" (cdr item1) (vfs-state-get-track-label vs key1))

;; Test that non-existant entries still return false.
(test-eq "one item dud track index" #f (vfs-state-get-track-index vs "/"))
(test-eq "one item dud track label" #f (vfs-state-get-track-label vs "/"))

;; Serialise.
;; Using the same save-file tests file overwrite.
(vfs-state-save vs save-file)
(set! vsload (vfs-state-read save-file))
(test-equal "loaded one item vpaths" (list key1) (list-sort string-ci<? (vector->list (vfs-state-vpaths vsload))))
(test-eqv "loaded one item track index" (car item1) (vfs-state-get-track-index vsload key1))
(test-equal "loaded one item track label" (cdr item1) (vfs-state-get-track-label vsload key1))

;; Add another item.
(vfs-state-set! vs key2 (car item2) (cdr item2))

(test-equal "two item vpaths" (list key1 key2) (list-sort string-ci<? (vector->list (vfs-state-vpaths vs))))

;; Test the first item is unchanged.
(test-eqv "two item track index 1" (car item1) (vfs-state-get-track-index vs key1))
(test-equal "two item track label 1" (cdr item1) (vfs-state-get-track-label vs key1))

;; Test the second item is correct.
(test-eqv "two item track index 2" (car item2) (vfs-state-get-track-index vs key2))
(test-equal "two item track label 2" (cdr item2) (vfs-state-get-track-label vs key2))

;; Test item replacement.
(vfs-state-set! vs key1 (car item1-1) (cdr item1-1))

(test-equal "replaced two item vpaths" (list key1 key2) (list-sort string-ci<? (vector->list (vfs-state-vpaths vs))))
;; Test the first item is changed.
(test-eqv "replaced two item track index 1" (car item1-1) (vfs-state-get-track-index vs key1))
(test-equal "replaced two item track label 1" (cdr item1-1) (vfs-state-get-track-label vs key1))
;; Test the second item is unchanged.
(test-eqv "replaced two item track index 2" (car item2) (vfs-state-get-track-index vs key2))
(test-equal "replaced two item track label 2" (cdr item2) (vfs-state-get-track-label vs key2))

;; Serialisation.
(vfs-state-save vs save-file)
(set! vsload (vfs-state-read save-file))
(test-equal "loaded two item vpaths" (list key1 key2) (list-sort string-ci<? (vector->list (vfs-state-vpaths vsload))))
;; Test contents.
(test-eqv "loaded two item track index 1" (car item1-1) (vfs-state-get-track-index vsload key1))
(test-equal "loaded two item track label 1" (cdr item1-1) (vfs-state-get-track-label vsload key1))
(test-eqv "loaded two item track index 2" (car item2) (vfs-state-get-track-index vsload key2))
(test-equal "loaded two item track label 2" (cdr item2) (vfs-state-get-track-label vsload key2))

;; TODO add command line or env flag to keep file?
(delete-file save-file)

(test-end "vfs-state")
