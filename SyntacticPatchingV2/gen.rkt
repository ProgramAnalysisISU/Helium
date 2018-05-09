#lang racket

(require "./ast.rkt")

(provide (all-defined-out))

(define (gen-cfg ast)
  "TODO generate CFG from AST")

(define (is-func-type? t)
  (match t
    [(type:qual _ qual sub) (is-func-type? sub)]
    [(type:func _ ret params) #t]
    [_ #f]))

(define (gen-code-var-decl node var)
  "Generate code with type and variable name"
  (match node
    [(type:qual _ qual sub) (~a (string-join qual " ")
                                " " (gen-code-var-decl sub var))]
    [(type:array _ type size) (~a (gen-code-var-decl type var)
                                  "[" (gen-code size) "]")]
    [(type:builtin _ name) (~a name " " var)]
    ;; consume var
    [(type:func _ ret params) (~a (gen-code-var-decl ret "") var "("
                                  (string-join (map gen-code params) ",") ")")]
    [(type:paren _ sub) (if (is-func-type? sub)
                            (gen-code-var-decl sub (~a "(" var ")"))
                            (~a "(" (gen-code-var-decl sub var) ")"))]
    ;; consume var
    [(type:pointer _ sub) (~a (gen-code-var-decl sub (~a "*" var)))]
    ;; consume var
    [(type:ref _ sub) (~a (gen-code-var-decl sub "") "&" var)]
    [(type:enum _ name) (~a "enum " name " " var)]
    [(type:struct _ name) (~a "struct " name " " var)]
    [(type:union _ name) (~a "union " name " " var)]
    [(type:typedef _ name) (~a name " " var)]
    [(type:adjust _ sub) (gen-code-var-decl sub var)]
    [(type:elab _ sub) (gen-code-var-decl sub var)]
    [(type:typeof _ sub) (~a "typeof(" (gen-code-var-decl sub "") ")" var)]
    [_ #;(raise-syntax-error #f (~a "gen-code-var-decl no match: " node))
       (~a (gen-code node) " " var)]
    ))


(define (gen-code ast)
  "generate code of AST"
  (let ([unary-helper (lambda (op substr)
                        (match op
                          ["postinc" (~a substr "++")]
                          ["postdec" (~a substr "--")]
                          ["preinc" (~a "++" substr)]
                          ["predec" (~a "--" substr)]
                          ["addrof" (~a "&" substr)]
                          ["deref" (~a "*" substr)]
                          ["plus" (~a "+" substr)]
                          ["minus" (~a "-" substr)]
                          ["not" (~a "~" substr)]
                          ["lnot" (~a "!" substr)]
                          ["__extension__" (~a "__extension__ " substr)]))])
    (match ast
      [(expr:ref-var _ name) name]
      [(expr:ref-var _ name) name]
      [(expr:ref-func _ name) name]
      [(expr:ref-enum-const _ name) name]
      ;; FIXME prefix or suffix
      [(expr:unary _ op sub) (~a (unary-helper op (gen-code sub)))]
      [(expr:binary _ op lhs rhs) (~a (gen-code lhs) op (gen-code rhs))]
      [(expr:call _ callee args) (~a (gen-code callee) "("
                                     (string-join (map gen-code args) ",") ")")]
      ;; type
      [(expr:cast _ type sub) (~a "(" (gen-code-var-decl type "") ")"
                                  (gen-code sub))]
      [(expr:paren _ sub) (~a "(" (gen-code sub) ")")]
      [(expr:member _ base op member) (~a (gen-code base) op member)]
      [(expr:ternary _ cond t f) (~a (gen-code cond) "?" (gen-code t) ":" (gen-code f))]
      [(expr:liter-int _ value) value]
      [(expr:liter-char _ value) value]
      [(expr:liter-str _ value) (~a "\"" value "\"")]
      [(expr:liter-float _ value) value]
      [(expr:liter-comp _ expr) (gen-code expr)]
      [(expr:array _ lhs rhs) (~a (gen-code lhs) "[" (gen-code rhs) "]")]
      [(expr:init-list _ inits) (~a "{" (string-join
                                         (map gen-code (filter identity inits))
                                         ",")
                                    "}")]
      [(expr:vaarg _ sub t) (~a "va_arg(" (gen-code sub) "," (gen-code-var-decl t "") ")")]
      [(expr:sizeof _ sub) (~a "sizeof(" (gen-code sub) ")")]
      [(expr:alignof _ sub) (~a "alignof(" (gen-code sub) ")")]
      [(expr:stmt _ subcomp) (~a "(" (gen-code subcomp) ")")]
      ;; offsetof(type, comps exprs)
      ;; [(expr:offsetof _ type comps exprs) (~a "TODO offsetof(" (gen-code type) ","  ")")]
      [(expr:offsetof _ str) (~a str)]
      ;; [(helper:offsetofnode _ kind value) (~a "TODO offsetofnode")]
      [(expr:comp-liter _ init) (~a "{" (gen-code init) "}")]
      [(expr:predefined _ value) value]
      [(expr:dummy _ kind value) value]

      [(decl:tu _ decls) (~a (string-join (map gen-code decls) "\n") ";")]
      ;; attr: storage class, for now, static
      [(decl:func _ sc ret name params body)
       (~a (if (member "static" sc) "static" "")
           " " (gen-code-var-decl ret "") " " name
           "(" (string-join (map gen-code params) ",") ")"
           (gen-code body))]
      ;; (map visualize-ast (map read-scm (get-scms "./test/")))
      ;; (visualize-ast (read-scm "./test/type.c.scm"))
      
      [(decl:enum _ name members)
       (~a "enum" " " name "{" (string-join (map gen-code members) ",") "};") ]
      [(decl:enum-const _ name init)
       (~a name (if init (~a "=" (gen-code init)) ""))]
      [(decl:union _ name members)
       (~a "union " name "{" (string-join (map gen-code members) ";") "};")]
      [(decl:struct _ name members)
       (~a "struct " name "{"
           (string-join (map gen-code members)
                        ";"
                        #:after-last ";")
           "};")]
      [(decl:record-field _ type name) (gen-code-var-decl type name)]
      ;; FIXME func type
      [(decl:typedef _ name type) (~a "typedef " (gen-code type) " " name ";")]
      ;; type
      [(decl:var _ sc type name init)
       (~a
        (if (member "static" sc) "static" "")
        " " (gen-code-var-decl type name)
        ;; str
        (if init (~a "=" (gen-code init)) ""))]
      [(decl:empty _) ";"]
      [(decl:asm _ str) "TODO decl:asm"]
      ;; [(decl:label _ label) label]

      [(stmt:expr _ expr) (~a (gen-code expr) ";")]
      [(stmt:decl _ decls) (~a (string-join (map gen-code decls) ";\n") ";\n")]
      [(stmt:comp _ stmts) (~a "{\n" (string-join (map gen-code stmts) "\n") "\n}")]
      [(stmt:if _ cond then else) (~a "if (" (gen-code cond) ") " (gen-code then)
                                      (if else (~a " else " (gen-code else)) ""))]
      [(stmt:switch _ cond body) (~a "switch (" (gen-code cond) ")" (gen-code body))]
      ;; FIXME sub might be duplicate
      [(stmt:case _ lhs sub) (~a "case " (gen-code lhs) ":"
                                 (gen-code sub))]
      [(stmt:default _ sub) (~a "default:" (gen-code sub))]
      [(stmt:for _ init cond inc body)
       (~a "for (" (gen-code init) (gen-code cond) ";" (gen-code inc) ")"
           (gen-code body))]
      [(stmt:do _ body cond)
       (~a "do " (gen-code body) " while (" (gen-code cond) ");")]
      [(stmt:while _ cond body) (~a "while (" (gen-code cond) ")" (gen-code body))]
      [(stmt:break _) "break;"]
      [(stmt:cont _) "continue;"]
      [(stmt:return _ value) (~a "return " (if value (gen-code value) "") ";")]
      [(stmt:null _) ";"]
      [(stmt:asm _ str) (~a "asm volatile (\"" str "\");")]
      [(stmt:goto _ label) (~a "goto " label ";")]
      [(stmt:label _ label sub) (~a label ":" (gen-code sub))]
      [#f ""]
      [_ (if (is-type? ast)
             (gen-code-var-decl ast "")
             (begin
               (displayln (~a "warn: " ast))
               "ERR"))])))

(define (gen-func-decl func)
  (match func
    [(decl:func _ sc ret name params body)
     (if (member "static" sc) ""
         (~a (gen-code ret) " " name "("
             (string-join (map gen-code params) ",")
             ");"))]))

(define (gen-code-select node)
  "Generate code with consideration of selection."
  (let ([is-selected?
         (λ (n)
           (member (get-color n) '(select parent lca removed)))]
        [is-strict-selected?
         (λ (n)
           (member (get-color n) '(select parent lca)))])
    (cond
      [(member (get-color node) '(select parent lca))
       (match node
         [(decl:tu _ decls)
          (string-join (filter non-empty-string? (map gen-code-select decls)) "\n")]
         [(decl:func attr sc ret name params body)
          (~a (if (member "static" sc) "static" "")
              " " (gen-code ret) " " name
              ;; gen params selectively
              "(" (string-join (filter
                                non-empty-string?
                                (map gen-code-select params)) ",")
              (if (and (member 'variadic attr)
                       (member (get-color (last params)) '(select)))
                  ", ..."
                  "")
              ")"
              ;; Adding artificial braces
              "{\n"
              (gen-code-select body)
              "\n}")]
         [(stmt:comp _ stmts)
          (~a "{\n" (string-join (filter
                                  non-empty-string?
                                  (map gen-code-select stmts)) "\n") "\n}")]
         [(stmt:if _ cond t e)
          ;; I'm adding artificial compound here. The reason is that,
          ;; if only declare is selected, it is not capable of being
          ;; the statement of if alone.
          (~a "if (" (gen-code cond) ") {\n"
              (gen-code-select t) "\n}"
              (if (is-selected? e)
                  (~a "else {\n" (gen-code-select e) "}") ""))]
         [(stmt:switch _ cond body)
          (~a "switch (" (gen-code cond) ") {\n"
              (gen-code-select body) "\n}")]
         [(stmt:case _ lhs sub)
          (~a "case " (gen-code lhs) ":"
              ;; adding a semicolon, so that label
              ;; is not the last in stmt:comp
              " ;"
              (gen-code-select sub))]
         [(stmt:for _ init cond inc body)
          (~a "for (" (if init (gen-code init) ";") (gen-code cond) ";" (gen-code inc) ") {\n"
              (gen-code-select body) "\n}")]
         [(stmt:do _ body cond)
          (~a "do {\n" (gen-code-select body)
              "\n} while (" (gen-code cond) ");")]
         [(stmt:while _ cond body)
          (~a "while (" (gen-code cond) ") {\n"
              (gen-code-select body) "\n}")]
         [(stmt:label _ label sub) (~a label ":" ";" (gen-code-select sub))]
         [#f ""]
         [_ (gen-code node)])]
      [(member (get-color node) '(removed))
       (if (stmt:decl? node)
           ;; stmt:decl is not leaf, but the decl:var need semicolon
           (~a (string-join
                (filter
                 non-empty-string?
                 (map gen-code-select (children-nodes node))) ";\n") ";\n")
           (string-join (map gen-code-select (children-nodes node))))]
      [else ""])))


