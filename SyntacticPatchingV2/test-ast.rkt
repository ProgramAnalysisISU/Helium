#lang racket

(require "./ast.rkt")
(require "./snip.rkt")

(module+ test
  (void (update-node
         (λ (n)
           (displayln (prefab-struct-key n))
           n)
         (read-scm "./test/stmt.c.scm")))
  (struct test (a b) #:prefab)
  (let ([fields '(a b)]
        [values '((3 4) 5)]
        [obj (test 1 2)])
    (my-struct-copy 'test obj fields values)
    ;; (eval `(copy-helper-2 test ,obj ,fields ,values))
    )
  (my-struct-copy 'test (test 1 2) '(a b) '((3 5) 4))
  )


(module+ test
  (map get-callee (map read-scm (get-scms "./test/"))))
