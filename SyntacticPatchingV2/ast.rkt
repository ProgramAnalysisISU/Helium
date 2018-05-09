#lang racket

(require rackunit)

(provide (all-defined-out))

;; use color as mark
;; 1. mark as select or not
;; (struct astnode (color) #:prefab)

;; (struct test (a b) #:prefab)
;; (struct test2 test (c d) #:prefab)
;; (struct test3 test2 (e f) #:prefab)
;; (test2 1 2 3 4)
;; (test2-c (test3 1 2 3 4 5 6))



(define (to-list item)
  (if (list? item) item (list item)))

(define (collect-list . items)
  (apply append (map to-list items)))


;; unfortunately #:prefab structure cannot use generic methods
;; (define-generics astnode
;;   (gen-children astnode))

(define-syntax (define-astnode stx)
  (syntax-case stx ()
    [(_ name field ...) #'(struct name (color field ...) #:prefab)]))

;; (define-syntax (bar stx)
;;   (syntax-case stx ()
;;     [(_ body) (with-syntax ([c (datum->syntax stx 'c)])
;;                 #'(begin
;;                     (define c 9)))]))
;; (bar 3)


(define-syntax (define-astnodes stx)
  (syntax-case stx (hidden leaf nonleaf decl)
    [(_ (name field ...) ...)
     (with-syntax ([children (datum->syntax stx 'children)]
                   [map-ast (datum->syntax stx 'map-ast)]
                   [map-ast-inner (datum->syntax stx 'map-ast-inner)]
                   [update-node (datum->syntax stx 'update-node)]
                   [update-node-inner (datum->syntax stx 'update-node-inner)]
                   [update-node-reverse (datum->syntax stx 'update-node-reverse)]
                   [update-color (datum->syntax stx 'update-color)]
                   [ast->list (datum->syntax stx 'ast->list)]
                   [ast->post-list (datum->syntax stx 'ast->post-list)]
                   [update-color-once (datum->syntax stx 'update-color-once)]
                   [get-loc (datum->syntax stx 'get-loc)]
                   [color-direct-decl-default
                    (datum->syntax stx 'color-direct-decl-default)]
                   [get-color (datum->syntax stx 'get-color)]
                   [replace-child-by-color (datum->syntax stx 'replace-child-by-color)])
       #'(begin
           (struct name (attr field ...) #:prefab) ...
           (define (children node)
             (match node
               [(name _ field ...) (collect-list field ...)] ...
               [#f '()]
               [_ (raise-syntax-error
                   #f (format "Not a valid AST node: ~a" node))]))
           (define (map-ast proc node #:skip [skip (const #f)])
             (let ([helper (lambda (lst)
                             (cons (proc node)
                                   (apply
                                    append
                                    (map (lambda (item)
                                           (map-ast proc item))
                                         lst))))])
               (match node
                 [(name _ field ...)
                  (if (skip node) '()
                      (cons (proc node) (append (map-ast proc field) ...)))
                  ;; (helper (collect-list field ...))
                  ] ...
                 [(list items (... ...))
                  (apply append
                         (map (λ (n)
                                (map-ast proc n))
                              items))]
                 [#f '()]
                 [_ (cond
                      [(struct? node)
                       (raise-syntax-error
                        #f (format "Not a valid AST node (struct): ~v"
                                   node))]
                      [(list? node)
                       (raise-syntax-error
                        #f (format "Not a valid AST node (list): ~v" node))]
                      ;; string node is not a node
                      [(string? node) '()]
                      ;; should be offsetofnode 'field, in general kind
                      ;; attributes of AST nodes
                      [(symbol? node) '()]
                      [else (raise-syntax-error
                             #f (format "Not a valid AST node: ~v" node))])])))
           (define (map-ast-inner proc node #:skip [skip (const #f)])
             "Use map-ast to recur, will not call proc on current node"
             (match node
               [(name _ field ...)
                (append (map-ast proc field) ...)] ...
               [(list items (... ...))
                (apply append
                       (map (λ (n)
                              (map-ast proc n))
                            items))]
               [#f '()]
               [_ (raise-syntax-error #f "Not valid AST node")]))
           
           (define (ast->list node)
             "pre-order"
             (match node
               [(name _ field ...)
                (cons node (append (ast->list field) ...))] ...
               [(list items (... ...))
                (apply append (map ast->list items))]
               [_ '()]))
           (define (ast->post-list node)
             "post-order"
             (match node
               [(name _ field ...)
                (append (ast->list field) ... (list node))] ...
               [(list items (... ...))
                (apply append (map ast->list items))]
               [_ '()]))
           
           (define (update-color proc node)
             "post-order, proc accepts a node, and output color. "
             (match node
               [(name attr field ...)
                (let ([new (struct-copy name node 
                                        [field (update-color proc field)] ...)])
                  (struct-copy name new
                               [attr (cons (proc new)
                                           (rest attr))]))] ...
               [(list item (... ...))
                (map (lambda (n) (update-color proc n)) item)]
               ;; assume already validated
               [_ node]))
           (define (update-color-once proc node)
             (match node
               [(name attr field ...)
                (struct-copy name node
                             [attr (cons (proc node)
                                         (rest attr))])] ...
               ;; assume already validated
               [_ node]))
           (define (update-node proc node #:skip [skip (const #f)])
             "Proc will receive the already constructed node. It can
then manipulate the node (or just return itself in case not), and that
final one gets returned."
             (match node
               [(name _ field ...)
                (if (skip node) node
                    (proc (struct-copy
                           name node
                           [field (update-node proc field #:skip skip)] ...)))] ...
               [(list item (... ...))
                (map (lambda (n) (update-node proc n #:skip skip)) item)]
               [_ node]
               ))
           (define (update-node-inner proc node #:skip [skip (const #f)])
             "This will not recursively call itself, but call
update-node. The proc is not applied on THIS node"
             (match node
               [(name _ field ...)
                (struct-copy
                 name node
                 [field (update-node proc field #:skip skip)] ...)] ...
               [(list item (... ...))
                (map (lambda (n) (update-node proc n #:skip skip)) item)]
               [_ node]))
           
           (define (update-node-reverse proc node)
             "Still post order, but doing the reverse traversal for DFS"
             (match node
               [(name _ field ...)
                (let* ([fields (list field ...)]
                       [field-ids '(field ...)]
                       [vs (foldr (λ (x acc)
                                    (cons (update-node-reverse proc x) acc))
                                  '() fields)])
                  (proc (my-struct-copy 'name node field-ids vs)))
                ] ...
               [(list item (... ...))
                (foldr (λ (n acc)
                         (cons (update-node-reverse proc n) acc))
                       '() item)]
               [_ node]))
           (define (get-color node)
             (match node
               [(name attr field ...) (first attr)] ...
               ;; if-stmt else may be #f
               [#f 'none]
               [_ (raise-syntax-error
                 #f (~a "get-color requires strictly AST node, given " node))]))
           (define (get-loc node)
             (match node
               [(name attr field ...) (second attr)] ...
               [#f '(0 0 0 0)]
               [_ (raise-syntax-error
                   #f (~a "get-loc requires strictly AST node"))]))
           ))]))

(define-syntax (copy-helper stx)
  (syntax-case stx ()
    [(_ name obj (field-id ...) (value ...))
     #'(struct-copy name obj [field-id 'value] ...)]))

(define (my-struct-copy name obj field-ids vs)
  (eval `(copy-helper ,name ,obj ,field-ids ,vs)))

;; (get-color #s(decl:func (none (4 1 6 2)) () #s(type:qual (none (0 0 0 0)) () #s(type:builtin (none (0 0 0 0)) int)) main () #s(stmt:comp (none (4 12 6 2)) (#s(stmt:return (none (5 3 5 11)) #s(expr:liter-int (none (5 10 5 11)) 0))))))

(define (get-declared-var decl)
  (match decl
    [(decl:var _ sc type name init)
     name]
    [_ #f]))
(define (get-declared-vars decls)
  (filter identity (map get-declared-var decls)))

(define (get-used-vars node)
  "Used vars. FIXME This must be a expr or leaf node."
  (filter identity
          (map-ast (λ (n)
                     (match n
                       [(expr:ref-var _ name) name]
                       [_ #f]))
                   node)))

(define (get-callees ast)
  "Get the callee this AST called"
  (filter
   identity
   (map-ast (lambda (n)
              (match n
                [(expr:call _ callee args)
                 (if (expr:ref-func? callee)
                     (expr:ref-func-name callee)
                     #f)]
                [_ #f]))
            ast)))


(define-astnodes
  ;; decl
  (decl:enum name members)
  (decl:enum-const name init)
  (decl:struct name members)
  (decl:union name members)
  (decl:record-field type name)
  (decl:typedef name type)
  ;; nonleaf
  (decl:tu decls)
  (decl:func sc ret name params body)
  (stmt:comp stmts)
  (stmt:if cond then else)
  (stmt:switch cond body)
  (stmt:case lhs sub)
  (stmt:default sub)
  (stmt:for init cond inc body)
  (stmt:do body cond)
  (stmt:while cond body)
  (stmt:decl decls)
  ;; leaf
  (decl:var sc type name init)
  (stmt:expr expr)
  (decl:empty)
  (decl:asm str)
  ;; (decl:label label)
  (stmt:break)
  (stmt:cont)
  (stmt:return value)
  (stmt:null)
  (stmt:asm str)
  (stmt:goto label)
  (stmt:label label sub)
  ;; hidden
  (type:qual qual sub)
  (type:array type size)
  (type:builtin name)
  (type:func ret params)
  (type:paren sub)
  (type:pointer sub)
  (type:ref sub)
  (type:struct name)
  (type:union name)
  (type:enum name)
  (type:typedef name)
  (type:adjust sub)
  (type:elab sub)
  (type:typeof sub)

  (expr:ref-var name)
  (expr:ref-func name)
  (expr:ref-enum-const name)
  (expr:unary op sub)
  (expr:binary op lhs rhs)
  (expr:call callee args)
  (expr:cast type sub)
  (expr:paren sub)
  (expr:member base op member)
  (expr:ternary cond t f)
  (expr:liter-int value)
  (expr:liter-char value)
  (expr:liter-str value)
  (expr:liter-float value)
  (expr:liter-comp expr)
  (expr:array lhs rhs)
  (expr:init-list inits)
  (expr:vaarg sub t)
  (expr:sizeof sub)
  (expr:alignof sub)
  (expr:stmt subcomp)
  ;; offsetof(type, comps exprs)
  ;; (expr:offsetof type comps exprs)
  (expr:offsetof str)
  ;; (helper:offsetofnode kind value)
  (expr:comp-liter init)
  (expr:predefined value)
  (expr:dummy kind value))

(define (is-leaf? node)
  (match node
    [(decl:var _ sc type name init) #t]
    [(stmt:expr _ expr) #t]
    [(decl:empty _) #t]
    [(decl:asm _ str) #t]
    ;; [(decl:label _ label) #t]
    [(stmt:break _) #t]
    [(stmt:cont _) #t]
    [(stmt:return _ value) #t]
    [(stmt:null _) #t]
    [(stmt:asm _ str) #t]
    [(stmt:goto _ label) #t]
    [_ #f]))
(define (is-nonleaf? node)
  (match node
    [(stmt:label _ label sub) #t]
    [(stmt:decl _ decls) #t]
    [(decl:tu _ decls) #t]
    [(decl:func _ sc ret name params body) #t]
    [(stmt:comp _ stmts) #t]
    [(stmt:if _ cond then else) #t]
    [(stmt:switch _ cond body) #t]
    [(stmt:case _ lhs sub) #t]
    [(stmt:default _ sub) #t]
    [(stmt:for _ init cond inc body) #t]
    [(stmt:do _ body cond) #t]
    [(stmt:while _ cond body) #t]
    [_ #f]))
(define (is-loop? node)
  (match node
    [(stmt:for _ init cond inc body) #t]
    [(stmt:do _ body cond) #t]
    [(stmt:while _ cond body) #t]
    [_ #f]))
(define (is-switch? node)
  (stmt:switch? node))
(define (is-type? node)
  (match node
    [(type:qual _ qual sub) #t]
    [(type:array _ type size) #t]
    [(type:builtin _ name) #t]
    [(type:func _ ret params) #t]
    [(type:paren _ sub) #t]
    [(type:pointer _ sub) #t]
    [(type:ref _ sub) #t]
    [(type:struct _ name) #t]
    [(type:union _ name) #t]
    [(type:enum _ name) #t]
    [(type:typedef _ name) #t]
    [(type:adjust _ sub) #t]
    [(type:elab _ sub) #t]
    [(type:typeof _ sub) #t]
    [_ #f]))
(define (is-expr? node)
  (match node
    [(expr:ref-var _ name) #t]
    [(expr:ref-func _ name) #t]
    [(expr:ref-enum-const _ name) #t]
    [(expr:unary _ op sub) #t]
    [(expr:binary _ op lhs rhs) #t]
    [(expr:call _ callee args) #t]
    [(expr:cast _ type sub) #t]
    [(expr:paren _ sub) #t]
    [(expr:member _ base op member) #t]
    [(expr:ternary _ cond t f) #t]
    [(expr:liter-int _ value) #t]
    [(expr:liter-char _ value) #t]
    [(expr:liter-str _ value) #t]
    [(expr:liter-float _ value) #t]
    [(expr:liter-comp _ expr) #t]
    [(expr:array _ lhs rhs) #t]
    [(expr:init-list _ inits) #t]
    [(expr:vaarg _ sub t) #t]
    [(expr:sizeof _ sub) #t]
    [(expr:alignof _ sub) #t]
    [(expr:stmt _ subcomp) #t]
    ;; offsetof(type, comps exprs)
    ;; [(expr:offsetof _ type comps exprs) #t]
    [(expr:offsetof _ str) #t]
    ;; [(helper:offsetofnode _ kind value) #t]
    [(expr:comp-liter _ init) #t]
    [(expr:dummy _ kind value) #t]
    [_ #f]))


(define (children-nodes node)
  (match node
    [(decl:tu _ decls) decls]
    [(decl:func _ sc ret name params body) (append params (list body))]
    [(stmt:comp _ stmts) stmts]
    [(stmt:if _ cond then else) (list cond then else)]
    [(stmt:switch _ cond body) (list cond body)]
    [(stmt:case _ lhs sub) (list lhs sub)]
    [(stmt:default _ sub) (list sub)]
    [(stmt:for _ init cond inc body) (list init cond inc body)]
    [(stmt:do _ body cond) (list body cond)]
    [(stmt:while _ cond body) (list cond body)]
    [(stmt:decl _ decls) decls]
    [(stmt:label _ label sub) (list sub)]
    [_ (list)]))
