#lang racket

(require racket/hash)
(require "./ast.rkt")
(require "gen.rkt")

(provide (all-defined-out))

(struct snippet (file value))

(define (list->counting-hashmap lst)
  "from a list, to a hash set map the key to its counting"
  (for/fold ([res (hash)])
            ([item lst])
    (if (hash-has-key? res item)
        (hash-set res item (+ (hash-ref res item) 1))
        (hash-set res item 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; new snippet system
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (file->tu file)
  (let ([in (open-input-file file)])
    (let ([res (read in)])
      (close-input-port in)
      res)))

(define (file->snippets file)
  (let ([snip-add (λ (snip id key value)
                    (hash-set
                     snip id
                     (hash-set
                      (hash-ref snip id)
                      key value)))])
    (foldr (λ (decl acc)
             (match decl
               [(decl:func _ sc ret name params body)
                (snip-add acc 'func name (snippet file decl))]
               [(decl:enum _ name members)
                (snip-add acc 'enum name (snippet file decl))]
               [(decl:union _ name members)
                (snip-add acc 'union name (snippet file decl))]
               [(decl:struct _ name members)
                (snip-add acc 'struct name (snippet file decl))]
               [(decl:typedef _ name type)
                (snip-add acc 'typedef name (snippet file decl))]
               [(decl:var _ sc type name init)
                (snip-add acc 'var name (snippet file decl))]
               [_ acc]))
           (hash 'func (hash)
                 'enum (hash)
                 'union (hash)
                 'struct (hash)
                 'typedef (hash)
                 'var (hash))
           (decl:tu-decls (file->tu file)))))

(define (dir->scm-files kernel-dir)
  "Read all .scm files within kernel dir."
  ;; 1. find all files
  (for/list ([p (in-directory kernel-dir)]
             #:when (path-has-extension? p ".scm"))
    p))

(define (dir->snippets dir)
  (hash-union
   (map file->snippets (dir->scm-files dir))
   ;; FIXME duplicate snippets
   ;; #:combine/key
   ;; (λ (k v0 v)
   ;;   (if (list? v0) (append v0 (list v)) (list v0 v)))
   ))

(module+ test
  (file->snippets "./test/call-graph.c.scm")
  (file->snippets "./test/decl.c.scm")

  (call-graph (file->snippets "./test/call-graph.c.scm"))
  (type-graph (file->snippets "./test/decl.c.scm"))
  )

;; these call-graph functions are used to validate how tangled the
;; code is, this is directly related to the feasibility
(define (call-graph snippets)
  "generate call graph"
  (for/hash ([(name decl) (in-hash (hash-ref snippets 'func))])
    (values name (get-callees decl))))

(define (type-graph snippets)
  "Call graph for snippets"
  (let ([helper (λ (kind)
                  (for/hash ([(name snip) (in-hash (hash-ref snippets kind))])
                    (values name (get-type-dep snip))))])
    (hash-union
     (helper 'enum)
     (helper 'union)
     (helper 'struct)
     (helper 'typedef)
     (helper 'var)
     #:combine/key
     (λ (k v0 v)
       (append v0 v)))))

(define (get-type-dep snip)
  "Get the dependent struct snippets"
  (filter
   identity
   (map-ast (lambda (n)
              (match n
                [(decl:enum _ name members) name]
                [(decl:struct _ name members) name]
                [(decl:union _ name members) name]
                [(decl:typedef _ name type) name]
                [(decl:record-field _ type name) name]
                [_ #f]))
            (snippet-value snip))))

(define (snip-graph snippets)
  "Get snippet dependence graph. This includes both func and type."
  ;; FIXME same name
  (for/hash ([(kind h) (in-hash snippets)])
    (for/hash ([(name decl) (in-hash h)])
      (values name (get-snip-dep decl)))))

(define (get-snip-dep snip)
  (filter
   identity
   (map-ast (λ (n)
              (match n
                [(type:struct _ name) name]
                [(type:union _ name) name]
                [(type:enum _ name) name]
                [(type:typedef _ name) name]
                [(expr:call _ callee args) callee]
                [_ #f]))
            (snippet-value snip))))


(define (get-unbound-vars-select node)
  "Unbound vars in selected code. This will be global variable. This
should include both name and type."
  ;; locals :: ((type name) ...)
  (let ([lst (ast->list node)])
    ;; FIXME symbol table need scope for sure
    (foldr (λ (n symtbl)
             (match n
               [(decl:var _ sc type name init)
                ;; (displayln (~a "remove " name))
                (set-remove symtbl name)]
               #;
               [(stmt:decl _ decls)
                (let ([vars (get-declared-vars decls)])
                  (foldl (λ (x acc)
                           (displayln (~a "remove " x))
                           (set-remove acc x))
                         symtbl vars))]
               [_ (if (and (member (get-color n) '(select))
                           (or (is-leaf? n)
                               (is-expr? n)))
                      (let ([vars (get-used-vars n)])
                        (foldl (λ (x acc)
                                 ;; (displayln (~a "add " x))
                                 (set-add acc x))
                               symtbl vars))
                      symtbl)]))
           (set) lst)))

(define (get-used-snippet node)
  (filter
   identity
   (map-ast
    (λ (n)
      (match n
        [(expr:ref-func _ name) (list 'func name)]
        [(type:enum _ name) (list 'enum name)]
        [(expr:ref-enum-const _ name) (list 'enum-member name)]
        [(type:struct _ name) (list 'struct name)]
        [(type:union _ name) (list 'union name)]
        [(type:typedef _ name) (list 'typedef name)]
        [_ #f]))
    node)))

(define (get-used-snippet-select node)
  "Get used snippet for selected nodes"
  (apply append
         (map-ast
          (λ (n)
            (if (and
                 ;; just select a parent or lca cannot be the leaf nodes, and
                 ;; cannot get any real staff
                 (member (get-color n) '(select))
                 (or (is-leaf? n)
                     (is-expr? n)))
                (get-used-snippet n)
                '()))
          node)))


#;
(define (get-snippets-spec specs snips)
  "Get initial snippets by descriptions"
  (for/list ([spec specs])
    (let ([kind (first spec)]
          [name (second spec)])
      (let ([h (hash-ref snips kind)])
        (if (hash-has-key? h name)
            (hash-ref h name)
            #f)))))
(define (get-snippets name snips)
  "Get snippets by name"
  (for/list ([(kind h) (in-hash snips)])
    (if (hash-has-key? h name)
        (hash-ref h name) #f)))

(define (get-dep-closure names sg)
  "Given function names, get closure of functions"
  (letrec ([helper (λ (todo done)
                     ;; 
                     (let* ([c (set-first todo)]
                            [wl (set-rest todo)]
                            [newitems (hash-ref sg c)]
                            [done (set-add done c)])
                       (let ([wl (foldr (λ (x acc)
                                          (set-add acc x))
                                        wl newitems)])
                         (helper wl done))))])
    (helper (list->set names) (set))))



(define (sort-snippets snip-lst)
  "This is a list of snippets. Return a sorted list."
  (apply
   append
   (map (λ (lst)
          ;; 2. within file, by loc
          (sort lst (λ (s1 s2)
                      (< (first (get-loc (snippet-value s1)))
                         (first (get-loc (snippet-value s2)))))))
        ;; sort by file include relation
        (sort (λ (lst1 lst2)
                (let ([f1 (snippet-file (first lst1))]
                      [f2 (snippet-file (first lst2))])
                  "TODO header dependencies"))
         ;; 1. group by file
         (group-by snippet-file snip-lst)))))

(define (gen-snippet sorted-snip-lst)
  (string-join
   (map (λ (s)
          (gen-code (snippet-value s)))
        sorted-snip-lst)
   "\n"))


(module+ test
  (define names '(foo bar MyStruct))
  (string-join
   ;; gen code, join together
   (map (λ (snip)
          (gen-code snip))
        ;; sorted snippets
        (sort-snippets
         (map (λ (name snips)
                (get-snippets name snips))
              (get-dep-closure names))))
   "\n"))



#;
(module+ test
  ;; snippet testing
  (call-graph (extract-snippet "./test/call-graph.c.scm"))
  (type-graph (extract-snippet "./test/type-graph.c.scm"))
  
  
  (define test-snippets (extract-snippet-dir "./test"))
  (validate-snippets test-snippets)


  (get-callee
   (snippet-value
    (hash-ref
     (extract-snippet "./test/expr.c.scm")
     "func:foo")))  

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Linux
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; "/home/hebi/Downloads/linux-4.14.9"
  (define linux-snippets (extract-snippet-dir "/home/hebi/benchmark/linux"))
  (length (hash-keys linux-snippets))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Git
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define git-snippets (extract-snippet-dir "/home/hebi/benchmark/git"))
  ;; should be
  ;; ((static-func 135) (func 5) (enum 1) (record 7) (typedef 0))
  (validate-snippets git-snippets)

  ;; 6615
  (length (hash-keys (call-graph git-snippets)))
  ;; 469
  (length (hash-keys (type-graph git-snippets)))

  (snippet-value (hash-ref git-snippets "func:print_error_files"))
  ;; 8667
  (length (hash-keys git-snippets))
  
  )
