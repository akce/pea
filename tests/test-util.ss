#! /usr/bin/env scheme-script

(import
 (rnrs)
 (srfi :64 testing)
 (irregex)
 (pea util))     ; module under test

(test-begin "string-join")

(test-equal "string-join null" "" (string-join "/"))
(test-equal "string-join one" "a" (string-join "/" "a"))
(test-equal "string-join two" "a/b" (string-join "/" "a" "b"))
(test-equal "string-join three" "a/b/c" (string-join "/" "a" "b" "c"))

(test-end "string-join")
