#lang racket

(require "./ast.rkt")
(require "./patch.rkt")
(require "./visual.rkt")
(require "./gen.rkt")

(module+ test
  (patch-parent
   (mark-continue-lines
    (third (tu-get-funcs
            (read-scm "./test/expr.c.scm")))))
  (visualize-ast
   (mark-lines
    (first (tu-get-funcs (read-scm "./test/stmt.c.scm")))
    '(25 28 41 43)))
  (visualize-ast
   (patch-min
    (patch-lca
     (patch-parent
      (mark-lines
       (first (tu-get-funcs (read-scm "./test/stmt.c.scm")))
       ;; '(25 41 43)
       '(25 14 17 21 22 20)
       )))))
  (visualize-ast
   (syntactic-patch
    (mark-lines
     (first (tu-get-funcs (read-scm "./test/stmt.c.scm")))
     ;; '(25 41 43)
     '(25 14 17 21 22 20)
     ))))

(module+ test
  "Test gen-code-select"
  (displayln
   (gen-code-select
    (syntactic-patch
     (mark-lines
      (first (tu-get-funcs (read-scm "./test/stmt.c.scm")))
      ;; '(25 41 43)
      '(25 13)
      )))))

(module+ test
  (display (gen-code (read-scm "./test/expr.c.scm")))
  (visualize-ast (third (tu-get-funcs (read-scm "./test/expr.c.scm"))))
  (filter
   (lambda (c) (eq? c 'select))
   (map-ast
    get-color
    (mark-continue-lines
     (third (tu-get-funcs (read-scm "./test/expr.c.scm"))))))
  (visualize-ast
   (mark-random-lines
    (third (tu-get-funcs (read-scm "./test/expr.c.scm")))))
  (visualize-ast
   (mark-continue-lines
    (third (tu-get-funcs (read-scm "./test/expr.c.scm")))))


  (patch-parent
   (mark-continue-lines
    (third (tu-get-funcs (read-scm "./test/expr.c.scm")))))

  (visualize-ast
   (mark-continue-lines
    (first (tu-get-funcs (read-scm "./test/stmt.c.scm")))))
  
  (filter
   identity
   (map-ast is-leaf? (third (tu-get-funcs (read-scm "./test/expr.c.scm")))))
  (void (update-node (λ (n)
                       ;; (displayln n)
                       ;; (displayln ".")
                       n)
                     (third (tu-get-funcs (read-scm "./test/expr.c.scm"))))))

(module+ test
  (visualize-ast
   (mark-continue-lines (first (tu-get-funcs (read-scm "./test/stmt.c.scm")))))
  (visualize-ast
   (mark-lines
    (first
     (tu-get-funcs (read-scm "./test/stmt.c.scm")))
    '(25 28 41 43)))
  (visualize-ast (read-scm "./test/stmt.c.scm"))
  (map visualize-ast (map read-scm (get-scms "./test/")))
  (visualize-ast (read-scm "./test/expr.c.scm")))

