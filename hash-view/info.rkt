#lang info

;; pkg info

(define collection "hash-view")
(define deps '("base" "rackunit-lib" "hash-view-lib" "scribble-lib"))
(define build-deps '("racket-doc"))
(define implies '("hash-view-lib"))
(define pkg-authors '(ryanc))

;; collection info

(define name "hash-view")
(define scribblings '(("hash-view.scrbl" ())))
