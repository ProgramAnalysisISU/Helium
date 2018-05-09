#lang racket

(require "ast.rkt")
(require "snip.rkt")
(require "gen.rkt")
(require "patch.rkt")
(require "visual.rkt")
(require "src-man.rkt")

(define benchmark-root (make-parameter #f))


(module+ test
  (define fooc (file->tu "./test/proj/foo.c.scm"))
  (define barc (file->tu "./test/proj/bar.c.scm"))
  (define mainc (file->tu "./test/proj/main.c.scm"))
  ;; (visualize-ast fooc)
  ;; (visualize-ast barc)
  ;; (visualize-ast mainc)

  (define marked (mark-lines fooc '(26)))
  ;; (visualize-ast marked)
  (define patched (syntactic-patch marked))
  ;; (visualize-ast patched)
  (gen-code-select patched)

  ;; (define used-snips (get-used-snippet-select patched))

  ;; (displayln (gen-snip-header patched))
  ;; (gen-main-c patched)
  
  )

