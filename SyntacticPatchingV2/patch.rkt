#lang racket

(require "./ast.rkt")

;; types of colors
;; none, select, parent, lca, removed

(provide (all-defined-out))

(define (patch-parent ast)
  "color the parents."
  (update-color
   (lambda (n)
     ;; (displayln (~a "Processing " (syntax->datum #'n)))
     (if (eq? (get-color n) 'none)
         (let* ([colors (map get-color (children-nodes n))]
                [not-none (filter-not (λ (c) (eq? c 'none)) colors)])
           (if (empty? not-none)
               (get-color n)
               (if (= (length not-none) 1) 'parent 'lca)))
         (get-color n)))
   ast))

(define (patch-lca node)
  "The direct children of LCA. This is only for condition exprs. By
case: if, switch, do, while, for. I can actually do this in code-gen,
but I need to move it here because I also need to resolve variables
used inside these conditions."
  (update-node
   (λ (n)
     (if (eq? (get-color n) 'lca)
         (match n
           [(stmt:if _ c t e)
            (struct-copy stmt:if n
                         [cond (update-color (const 'select) c)])]
           [(stmt:switch _ cond body)
            (struct-copy stmt:switch n
                         [cond (update-color (const 'select) cond)])]
           [(stmt:do _ body cond)
            (struct-copy stmt:do n
                         [cond (update-color (const 'select) cond)])]
           [(stmt:while _ cond body)
            (struct-copy stmt:while n
                         [cond (update-color (const 'select) cond)])]
           ;; I'm going to retain for conditions too
           [(stmt:for _ init cond inc body)
            (struct-copy stmt:for n
                         [init (update-color (const 'select) init)]
                         [cond (update-color (const 'select) cond)]
                         [inc (update-color (const 'select) inc)])]
           #;
           [(decl:func _ sc ret name params body)
            (struct-copy decl:func n
                         [params (update-color (const 'select) params)])]
           [_ n])
         n))
   node))

;; patch-min is fake here. That's because actually all parents would
;; be removed, because we are down to expression and statements. Every
;; thing, as long as it is statement, can be along. The conditional
;; expression must have its up-level, and that should be done in
;; patch-parent
(define (patch-min node)
  "Retain only the parents whose selected children is
expr. I.e. remove parent whose selected child is not expr."
  (update-node
   (λ (n)
     (if (and (eq? (get-color n) 'parent)
              (empty?
               (filter
                (λ (x) (member (get-color x) '(parent lca select)))
                (filter is-expr? (children-nodes n)))))
         (update-color-once (const 'removed) n)
         n))
   node))

(define (patch-special node)
  "continue, break, return, goto, case"
  (let ([helper (λ (nlst)
                  (not
                   (empty?
                    (filter
                     (λ (c) (member c '(select parent lca)))
                     (map get-color nlst)))))])
    (update-color
     (λ (n)
       (cond
         [(is-loop? n) (if (helper
                            (append (get-direct-continue n)
                                    (get-direct-break n)))
                           'lca (get-color n))]
         [(is-switch? n) (if (helper
                              (append (get-direct-break n)
                                      (get-direct-case n)))
                             'lca (get-color n))]
         [(decl:func? n) (if (helper
                              (get-return n))
                             'lca (get-color n))]
         [else (get-color n)]))
     node)))

(define (patch-goto node)
  "Get all goto nodes selected, and get the labels. Go another round
and mark stmt:label."
  (let ([labels (filter
                 identity
                 (map-ast (λ (n)
                            (if (member (get-color n) '(select))
                                (match n
                                  [(stmt:goto _ label) label]
                                  [_ #f])
                                #f))
                          node))])
    (update-color (λ (n)
                    (match n
                      [(stmt:label _ label sub)
                       (if (member label labels)
                           'select
                           (get-color n))]
                      [_ (get-color n)]))
                  node)))

(define (get-direct-break node)
  "Get direct break of current node. Current node can be loop, switch,
whatever."
  ;; 1. remove inner loops
  ;; 2. check break
  (filter
   identity
   (map-ast-inner
    (λ (n)
      (if (stmt:break? n) n #f))
    node
    #:skip (λ (n)
             (or (is-loop? n)
                 (is-switch? n))))))
(define (get-direct-continue node)
  "Get direct continue of current node. Current node can be loop, switch,
whatever."
  ;; 1. remove inner loops
  ;; 2. check break
  (filter
   identity
   (map-ast-inner
    (λ (n)
      (if (stmt:cont? n) n #f))
    node
    #:skip is-loop?)))
(define (get-direct-case node)
  "get the direct case"
  (map-ast-inner
   (λ (n)
     (if (stmt:case? n) n #f))
   node
   #:skip stmt:switch?))
(define (get-return node)
  (filter
   identity
   (map-ast
    (λ (n) (if (stmt:return? n) n #f))
    node)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; def-use (more precisely, declare-use)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; I actually need to get the def instead of declare, but declare
;; itself is able to make it compilible, but def alone won't, thus,
;; lets first get the declare, and then research into the def-use
;; literature.

(define (patch-internal-type-decl node)
  "struct/union/enum defined inside function"
  (update-color
   (λ (n)
     (match n
       [(decl:enum _ name members) 'select]
       [(decl:struct _ name members) 'select]
       [(decl:union _ name members) 'select]
       [_ (get-color n)]))
   node))

(define (decl-var-id node)
  (match node
    [(decl:var attr sc type name init)
     ;; name_l_l_l_l
     (string-join (cons name (map number->string (second attr))) "_")]
    [_ (raise-syntax-error #f "decl-var-id only works on decl:var")]))

(define (st-ana node pl)
  "The return value is (node IDs, unbound vars). The visiting order is
right to left."
  #;
  (displayln (~a "node: " (prefab-struct-key node)
                 " at " (first (get-loc node))
                 " with " pl))
  (let ([res (let* ([empty-pl '(() ())]
                    [pl-combine (λ lst
                                  (foldl (λ (x acc)
                                           (list (append (first x) (first acc))
                                                 (append (second x) (second acc))))
                                         empty-pl
                                         lst))])
               (match node
                 ;; adding
                 [(stmt:decl _ decls)
                  (foldr (λ (x acc)
                           (st-ana x acc))
                         pl decls)]
                 [(decl:var attr sc type name init)
                  (let ([IDs (first pl)]
                        [unbound-vars (second pl)])
                    (let ([unbound-vars (append (get-used-vars init) unbound-vars)])
                      (if (member name unbound-vars)
                          (list (cons (decl-var-id node) IDs)
                                (remove name unbound-vars))
                          (list IDs unbound-vars))))]
                 ;; intro, non-adding
                 [(stmt:for _ init cond incr body)
                  (st-ana init (pl-combine (st-ana body empty-pl)
                                           (st-ana incr empty-pl)
                                           (st-ana cond empty-pl)
                                           pl))]
                 [(decl:func _ sc ret name params body)
                  (let ([newpl (st-ana body empty-pl)])
                    (foldr (λ (x acc)
                             (st-ana x acc))
                           newpl
                           params))]
                 [(stmt:if _ c t e)
                  (st-ana c (pl-combine
                             (st-ana c empty-pl)
                             (st-ana t empty-pl)
                             (st-ana e empty-pl)
                             pl))]
                 ;; non-adding
                 [(stmt:comp _ stmts)
                  (foldr (λ (x acc)
                           (st-ana x acc))
                         empty-pl stmts)]
                 [(stmt:switch _ c body)
                  (pl-combine
                   (st-ana body empty-pl)
                   (st-ana c empty-pl)
                   pl)]
                 [(stmt:do _ body c)
                  (pl-combine
                   (st-ana c empty-pl)
                   (st-ana body empty-pl)
                   pl)]
                 [(stmt:while _ c body)
                  (pl-combine
                   (st-ana body empty-pl)
                   (st-ana c empty-pl)
                   pl)]
                 [_ (cond
                      [(is-nonleaf? node)
                       (apply pl-combine
                              (cons pl
                                    (map (λ (n)
                                           (st-ana n empty-pl))
                                         (children node))))]
                      [(or (is-leaf? node)
                           (is-expr? node))
                       (if (member (get-color node) '(select))
                           (let ([vars (get-used-vars node)])
                             (list (first pl)
                                   (append vars (second pl))))
                           pl)]
                      [else pl])]))])
    #;(displayln (~a "return: " (prefab-struct-key node) " with " res))
    res))
(define (patch-declare-new node)
  (let* ([pl (st-ana node '(() ()))]
         [ids (first pl)])
    (update-color
     (λ (n)
       (match n
         [(decl:var _ sc type name init)
          (if (member (decl-var-id n) ids)
              'select
              (get-color n))]
         [_ (get-color n)]))
     node)))

(define (patch-declare node)
  "This should be done the last part. Then, do a patch-parent,
patch-min again. I assert the LCA should only be a comp stmt, because
the declare must dominate the use. Note that if the decl one of the
function parameter, that parameter is selected, but not other
parameters. The function header will be selected as long as there's
any selected nodes inside this functions. The function signature can
be different, because we are generating a different function name for
it."
  (let-values ([(symtbl-add! symtbl-rm! symtbl-member?)
                (let ([symtbl (mutable-set)])
                  (values
                   (λ (x)
                     ;; (displayln (~a "adding " x))
                     (set-add! symtbl x))
                   (λ (x)
                     ;; (displayln (~a "removing " x))
                     (set-remove! symtbl x))
                   (λ (x)
                     ;; (displayln (~a "checking " x))
                     (set-member? symtbl x))))])
    (update-node-reverse
     (λ (n)
       (match n
         ;; TODO for init can also declare var
         [(decl:var _ sc type name init)
          (if (symtbl-member? name)
              (begin
                (symtbl-rm! name)
                ;; add newly used var
                (when init
                 (map symtbl-add! (get-used-vars init)))
                (update-color-once (const 'select) n))
              n)]
         #;
         [(stmt:decl _ decls)
          (let ([vars (get-declared-vars decls)])
            (if (not
                 (empty?
                  (filter
                   identity
                   (map symtbl-member? vars))))
                (begin
                  (map symtbl-rm! vars)
                  ;; udpate the color
                  (update-color-once (const 'select) n))
                n))]
         [_ (when (member (get-color n) '(select))
              (let ([vars (get-used-vars n)])
                (map symtbl-add! vars)))
            n]))
     node)))

(define (contains-select? node)
  (not
   (empty? (filter identity
                   (map-ast
                    (λ (n) (member (get-color n) '(select)))
                    node)))))

(define (patch-func node)
  "If any of the node inside the function is selected, the function
header is selected."
  (when (not (decl:func? node))
    (raise-syntax-error #f "patch-func only apply on function"))
  (if (contains-select? node)
      (update-color-once (const 'select) node)
      node))

(define (syntactic-patch node)
  (patch-func
   ;; outer, again
   (patch-min
    ;; no lca this time, because FIXME lca must be a compstmt
    (patch-lca
     (patch-parent
      ;; inner
      (patch-declare-new
       (patch-lca
        (patch-goto
         (patch-special
          (patch-min
           (patch-lca
            (patch-parent
             ;; the inner most, get all internal type decls
             (patch-internal-type-decl
              node)))))))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; marking lines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (mark-lines node lines)
  (update-node
   (λ (n)
     (if (or (is-leaf? n)
             (is-expr? n))
         (begin
           (match (get-loc n)
             [(list l1 c1 l2 c2)
              (let ([inter (set-intersect (range l1 (+ l2 1)) lines)])
                (if (set-empty? inter) n
                    ;; I'm actually marking everything below it. This
                    ;; is good, I'm marking the decl:var under
                    ;; stmt:decl
                    (update-color (const 'select) n)))]))
         n))
   node))

(define (mark-all node)
  (update-color
   (const 'select) node))
(define (unmark-all node)
  (update-color
   (const 'none) node))

(define (get-line-range node)
  (let ([loc (get-loc node)])
    (values (first loc)
            (third loc))))

(define (get-cont-nums n1 n2)
  "Get continuous numbers between n1 and n2"
  (when (> n1 n2)
    (raise-syntax-error #f (~a "n1=" n1 " n2=" n2)))
  (let* ([num (add1 (- n2 n1))]
         [span (if (> num 20) 10
                   (floor (/ num 2)))]
         [x1 (+ n1
                (random (- num span)))]
         [x2 (+ x1 span)])
    (range x1 x2)))
(define (get-rand-nums n1 n2)
  "Get random numbers between n1 and n2"
  (when (> n1 n2)
    (raise-syntax-error #f (~a "n1=" n1 " n2=" n2)))
  (let* ([num (add1 (- n2 n1))]
         [span (if (> num 20) 10
                   (floor (/ num 2)))])
    (take (shuffle (range n1 n2)) span)))

(define (get-10-cont n1 n2)
  "get 10 continuous numbers. Precondition: n2-n1>10"
  (let ([start (random n1 (- n2 10))])
    (range start (+ start 10))))
(define (get-10-rand n1 n2)
  (sort (take (shuffle (range n1 n2)) 10) <))

(define (tu-get-funcs tu)
  (filter decl:func? (decl:tu-decls tu)))


(define (num-if node)
  (count identity
         (map-ast (λ (n)
                    (match n
                      [(stmt:if _ cond t e) #t]
                      [(stmt:switch _ cond body) #t]
                      [_ #f]))
                  node)))
(define (num-loop node)
  (count identity
         (map-ast (λ (n)
                    (match n
                      [(stmt:for _ init cond inc body) #t]
                      [(stmt:do _ body cond) #t]
                      [(stmt:while _ cond body) #t]
                      [_ #f]))
                  node)))

(define (closure-helper num-t num-f)
  (let ([lst (shuffle
              (append
               (build-list num-t (const #t))
               (build-list num-f (const #f))))]
        [i -1])
    (λ ()
      (set! i (+ i 1))
      (if (>= i (+ num-t num-f))
          (raise-syntax-error #f "Out of range")
          (list-ref lst i)))))

(define (mark-random-if func)
  "mark random if conditionals inside if"
  (if (not (decl:func? func))
      (raise-syntax-error #f "not func")
      ;; 1. get number of conditional
      ;; 2. if num > 5, use 5, otherwise use num/2
      ;; 3. shuffle boolean vertor, with #t and #f
      ;; 4. apply
      (let* ([num (num-if func)]
             [num-t (if (> num 5) 5 (/ 2 num))]
             [num-f (- num num-t)])
        (let ([next (closure-helper num-t num-f)])
          (update-node
           (lambda (n)
             (match n
               [(stmt:if _ cond t e)
                (struct-copy stmt:if
                             [cond (update-color-once
                                    (λ (n) (if (next) 'select 'none))
                                    cond)])]
               [(stmt:switch _ cond body)
                (struct-copy stmt:switch
                             [cond (update-color-once
                                    (λ (n) (if (next) 'select 'none))
                                    cond)])]
               [_ n]))
           func)))))

(define (mark-random-loop func)
  "mark random loops"
  (if (not (decl:func? func))
      (raise-syntax-error #f "not func")
      (let* ([num (num-loop func)]
             [num-t (if (> num 5) 5 (/ 2 num))]
             [num-f (- num num-t)]
             [next (closure-helper num-t num-f)])
        (update-node
         (lambda (n)
           (match n
             [(stmt:for _ init cond inc body)
              (struct-copy stmt:for
                           [cond (update-color-once
                                  (λ (n) (if (next) 'select 'none))
                                  cond)])]
             [(stmt:do _ body cond)
              (struct-copy stmt:do
                           [cond (update-color-once
                                  (λ (n) (if (next) 'select 'none))
                                  cond)])]
             [(stmt:while _ cond body)
              (struct-copy stmt:while
                           [cond (update-color-once
                                  (λ (n) (if (next) 'select 'none))
                                  cond)])]
             [_ n]))
         func))))
(define (mark-random-mix-ifloop func)
  "TODO mark random mixed (nested?) if and loops")

(define (mark-patch func)
  "TODO mark patch and function")


(define (semantic-check ast)
  "TODO Semantically check if the AST is complete")

