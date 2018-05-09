#lang racket

(require "./ast.rkt")
(require "./gen.rkt")

(provide (all-defined-out))

(define (visualize-ast node)
  "Generate abstract AST by stop at condition expr and stmt
level. This is just for printing purpose"
  (let* ([mark-lst (lambda (n)
                     (case (get-color n)
                       [(select) '(*)]
                       [(parent) '(**)]
                       [(removed) '(--)]
                       [(lca) '(***)]
                       [else '()]))]
         [leaf-helper (lambda (name n)
                        (append (mark-lst n)
                                (list name
                                      (first (get-loc n))
                                      (gen-code n))))]
         [helper (lambda (name n children)
                   (append (mark-lst n) (list name children)))])
    (match node
      [(decl:tu _ decls) (helper 'tu node (map visualize-ast decls))]
      [(decl:func _ sc ret name params body)
       (helper 'func node (list name (map visualize-ast params)
                                (visualize-ast body)))]
      [(stmt:comp _ stmts) (helper 'stmt:comp node (map visualize-ast stmts))]
      [(stmt:if _ cond then else) (helper 'stmt:if node (map visualize-ast (list cond then else)))]
      [(stmt:switch _ cond body) (helper 'stmt:switch node (map visualize-ast (list cond body)))]
      [(stmt:case _ lhs sub) (helper 'stmt:case node (map visualize-ast (list lhs sub)))]
      [(stmt:default _ sub) (helper 'stmt:default node (visualize-ast sub))]
      [(stmt:for _ init cond inc body) (helper 'stmt:for node (map visualize-ast (list init cond inc body)))]
      [(stmt:do _ body cond) (helper 'stmt:do node (map visualize-ast (list body cond)))]
      [(stmt:while _ cond body) (helper 'stmt:while node (map visualize-ast (list cond body)))]
      [(stmt:label _ label sub) (helper 'stmt:label node (map visualize-ast (list sub)))]

      ;; leaf
      [(decl:var _ sc type name init) (leaf-helper 'decl:var node)]
      [(decl:empty _) (leaf-helper 'decl:empty node)]
      
      [(stmt:expr _ expr) (leaf-helper 'stmt:expr node)]
      [(stmt:decl _ decls) (helper 'stmt:decl node (map visualize-ast decls))]
      [(stmt:break _) (leaf-helper 'stmt:break node)]
      [(stmt:cont _) (leaf-helper 'stmt:cont node)]
      [(stmt:return _ value) (leaf-helper 'stmt:return node)]
      [(stmt:null _) (leaf-helper 'stmt:null node)]
      [(stmt:asm _ str) (leaf-helper 'stmt:asm node)]
      [(stmt:goto _ label) (leaf-helper 'stmt:goto node)]
      [#f #f]
      [_ (if (is-expr? node)
             (leaf-helper 'expr node)
             node)])))
