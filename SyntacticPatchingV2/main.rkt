
#lang racket

(require "ast.rkt")
(require "snip.rkt")
(require "patch.rkt")
(require "visual.rkt")
(require "gen.rkt")
(require racket/flonum)

(require "bench-data.rkt")

(require racket/file)
(require racket/trace)

(provide run-from-selection
         run-random-selection
         makefile-flag
         verbose-mode)

(define (gen-main-c patched-funcs)
  ;; TODO gen func decl for all these functions?? Seems not necessary,
  ;; because I'm not making them calling each other
  (~a
   "#include \"main.h\"\n"
   (string-join
    (map gen-func patched-funcs)
    "\n")
;;    "
;; int main(int argc, char *argv[]) {
;;   return 0;
;; }"
   ))

(define (gen-func func)
  ;; the function header must be selected actually
  ;; I need to modify it
  (let ([orig-name (decl:func-name func)])
    (gen-code-select
     (struct-copy decl:func func
                  [name (~a "helium_" orig-name)]))))

(define (dir-files-by-ext dir ext)
  ;; ext begins with dot
  (for/list ([p (in-directory dir)]
             #:when (path-has-extension? p ext))
    p))


(define makefile-flag (make-parameter ""))

(define (gen-makefile bench-root)
  (let ([objs (string-join (map path->string
                                (dir-files-by-ext
                                 bench-root
                                 ".o"))
                           " ")])
    (let ([cc "clang"])
      (string-join
       (list (~a "CC:=" cc)
             ;; -std=gnu
             ;; by default, clang uses gnu11
             (~a "CFLAGS:=-w"
                 " " (makefile-flag))
             "LINK_FLAGS:=-z muldefs -lz -pthread -lcurl -lssl -lcrypto -lexpat"
             (~a "OBJS:=" objs)
             ;; "COV_FLAGS=-fprofile-arcs -ftest-coverage"
             "COV_FLAGS="
             "SYSTEM_FLAGS="
             ".PHONY: all clean test"
             "all: main.o"
             
             "main: main.o"
             "	$(CC) -o main main.o $(OBJS) $(CFLAGS) $(LINK_FLAGS)"
             "main.o: main.c"
             "	$(CC) -c -g main.c $(CFLAGS) $(COV_FLAGS) $(SYSTEM_FLAGS)"
             "clean:"
             "	rm -rf *.out *.gcda *.gcno *.o main"
             "test:"
             "	bash test.sh"
             "run:"
             "	timeout 1 ./a.out 2>&1 >/dev/null"
             )
       "\n"))))

(define (tu->selected-funcs tu)
  (filter contains-select?
          (filter decl:func? (decl:tu-decls tu))))


(define show-error (make-parameter #f))
(define verbose-mode (make-parameter #f))

(define (run-make dir)
  (parameterize ([current-directory dir])
    (let-values ([(sp stdout stdin _stderr)
                  (subprocess #f #f 'stdout (find-executable-path "make"))])
      ;; block
      ;; (displayln "waiting for make ..")
      (subprocess-wait sp)
      ;; (displayln "done")
      (let ([exit-code (subprocess-status sp)]
            [output (port->string stdout)])
        (close-output-port stdin)
        (close-input-port stdout)
        (values exit-code output)
        ;; display
        (if (verbose-mode)
            (begin
              (display (~a "Output into " dir))
              (if (zero? exit-code)
                  (displayln " .. Success")
                  (displayln " .. Fail")))
            (if (zero? exit-code)
                (display ".")
                (display "x")))
        (when (show-error)
          (displayln (~a "Error: " exit-code " dir: " dir))
          (displayln (~a "output: "
                         (string-join
                          (filter (λ (line)
                                    (not (and (string-prefix? line "clang")
                                              (> (string-length line) 100))))
                                  (string-split output "\n"))
                          "\n")))
          ;; also the debug.txt
          (displayln (~a "debug file: " (build-path dir "debug.txt")))
          (displayln (file->string (build-path dir "debug.txt"))))
        (flush-output)
        ;; return
        exit-code))))

(define (tu->funcs tu)
  (filter decl:func? (decl:tu-decls tu)))

(define (str->file str path)
  (let ([out (open-output-file path #:exists 'replace)])
    (write-string str out)
    (close-output-port out)))

(define (gen&test mainc mainh makefile debug)
  (let ([out-dir (make-temporary-file "rkttmp~a" 'directory)])
    ;; (displayln out-dir)
    (str->file mainc (build-path out-dir "main.c"))
    (str->file mainh (build-path out-dir "main.h"))
    (str->file makefile (build-path out-dir "Makefile"))
    (str->file debug (build-path out-dir "debug.txt"))
    (run-make out-dir)))

(define (count-select node)
  (length
   (filter
    (λ (c)
      (eq? c 'select))
    (map get-color (filter is-leaf? (map-ast identity node))))))
(define (count-lca node)
  (length
   (filter (λ (c)
             (eq? c 'lca))
           (filter identity (map-ast get-color node)))))
(define (count-leaf node)
  (length
   (filter
    identity
    (map-ast is-leaf? node))))

(define (rand-test-func-thread func mainh makefile num-perfunc)
  (for/list ([i (in-range num-perfunc)])
    (let-values ([(n1 n2) (get-line-range func)])
      ;; cont or rand
      (let* ([lines (get-10-rand n1 n2)]
             [marked (mark-lines func lines)]
             ;; [patched (syntactic-patch marked)]
             )
        (let-values ([(patched-lst cpu real gc) (time-apply syntactic-patch (list marked))])
          (let ([patched (first patched-lst)])
            (let ([debug (let ([s (count-select marked)]
                               [sp (count-select patched)]
                               [p (count-leaf marked)]
                               [lca (count-lca patched)])
                           (let ([txt (~a lines "\n" "S: " s #\newline
                                          "S-prime: " sp #\newline "P: " p #\newline
                                          "LCA: " lca #\newline "Time: " cpu)])
                             ;; (displayln txt)
                             txt))])
              (thread
               (λ ()
                 (gen&test
                  (gen-main-c (list patched))
                  mainh makefile debug))))))))))

(define (join-threads threads)
  ;; (displayln (format "Syncing on ~a threads .." (length threads)))
  (when (not (empty? threads))
    (let ([ready-thread (apply sync threads)])
      (join-threads (remove ready-thread threads)))))

(define (rand-test-file bench-root ss-file)
  "Random test all funcs (2 per func) in this file"
  (let ([tu-file (path-replace-extension ss-file "")]
        [he-file (path-replace-extension ss-file ".he")])
    (let* ([mainh (~a "#include \"" (path->string tu-file) "\"\n")]
           [makefile (gen-makefile bench-root)]
           [tu (file->tu ss-file)])
      (displayln (~a "\nfile: " tu-file))
      (displayln (~a "num func: " (length (tu->funcs tu))))
      
      (join-threads
       (apply
        append
        (for/list ([func (tu->funcs tu)]
                   #:when (and 
                           (> (- (third (get-loc func))
                                 (first (get-loc func))) 10)
                           #;(> (random 10) 7)
                           ))
          (rand-test-func-thread func mainh makefile 1)))))))


(define bench-filter (make-parameter (const #t)))

(define bad-files
  ;; infinite loop, 1624
  '("/home/hebi/benchmark/FFmpeg/libavutil/cpu.c.ss"
    ;; infiinite loop, 158
    "/home/hebi/benchmark/linux/kernel/time/timekeeping.c.ss"
    "/home/hebi/benchmark/linux/kernel/bpf/core.c.ss"
    "/home/hebi/benchmark/linux/arch/x86/kernel/cpu/bugs.c.ss"))

(define (run-random-selection bench-root [start-id 0])
  (let ([ss-files
         (for/list
             ([ss-file (in-directory bench-root)]
              #:when (and (not (member (path->string ss-file) bad-files))
                          (path-has-extension? ss-file ".ss")
                          ((bench-filter) ss-file)))
           ss-file)])
    (for ([i (in-naturals)]
          [ss-file ss-files]
          #:when (>= i start-id))
      (display i)
      (rand-test-file bench-root ss-file))))

(define (test-file-with-sel bench-root ss-file lines)
  (let ([tu-file (path-replace-extension ss-file "")]
        [he-file (path-replace-extension ss-file ".he")])
    (let* ([mainh (~a "#include \"" (path->string tu-file) "\"\n")]
           [makefile (gen-makefile bench-root)]
           [tu (file->tu ss-file)])
      (let* ([marked (mark-lines tu lines)]
             [marked-funcs (tu->selected-funcs marked)])
        (let-values ([(patched-lst cpu real gc)
                      (time-apply (λ ()
                                    (map syntactic-patch marked-funcs)) '())])
          (let ([patched-funcs (first patched-lst)])
            (let ([s (apply + (map count-select marked-funcs))]
                  [sp (apply + (map count-select patched-funcs))]
                  [p (apply + (map count-leaf marked-funcs))]
                  [lca (apply + (map count-lca patched-funcs))]
                  [numFunc (length marked-funcs)])
              (let ([debug (let ([txt (~a lines "\n" "S: " s #\newline
                                          "S-prime: " sp #\newline "P: " p #\newline
                                          "LCA: " lca #\newline "Time: " cpu #\newline "Func: " numFunc)]) 
                             txt)])
                (thread
                 (λ ()
                   (gen&test
                    (gen-main-c patched-funcs)
                    mainh makefile debug)))))))))))

(define (run-from-selection bench-root sel-file)
  (join-threads
   (filter-not
    void?
    (for/list ([sel (file->value sel-file)])
      ;; only use one file
      (let ([file (first (first sel))]
            [lines (rest (first sel))])
        (let ([ss-file (build-path bench-root (~a file ".ss"))])
          (when (not (member (path->string ss-file) bad-files))
            ;; (displayln ss-file)
            (test-file-with-sel bench-root ss-file lines))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EXP 1: random selection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (run-random-selection "/home/hebi/github/helium2/test/proj")
  
  (parameterize ([makefile-flag git-flags]
                 [verbose-mode #t])
    (run-random-selection "/home/hebi/benchmark/git"))

  (parameterize ([bench-filter (λ (p)
                                 (string-prefix?
                                  (path->string p)
                                  "/home/hebi/benchmark/linux/kernel"))]
                 [makefile-flag linux-flags])
    (run-random-selection "/home/hebi/benchmark/linux"))
  (parameterize ([makefile-flag ffmpeg-flags])
    (run-random-selection "/home/hebi/benchmark/FFmpeg"))
  (parameterize ([makefile-flag vim-flags])
    (run-random-selection "/home/hebi/benchmark/vim"))
  (parameterize ([makefile-flag openssl-flags])
    (run-random-selection "/home/hebi/benchmark/openssl")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EXP 2: selection from files (in this case changes)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (parameterize ([makefile-flag vim-flags])
    (run-from-selection "/home/hebi/benchmark/vim" "./cmt-data/vim.ss"))
  (parameterize ([makefile-flag openssl-flags])
    (run-from-selection "/home/hebi/benchmark/openssl" "./cmt-data/openssl.ss"))
  (parameterize ([makefile-flag git-flags])
    (run-from-selection "/home/hebi/benchmark/git" "./cmt-data/git.ss"))
  (parameterize ([bench-filter (λ (p)
                                 (string-prefix?
                                  (path->string p)
                                  "/home/hebi/benchmark/linux/kernel"))]
                 [makefile-flag linux-flags])
    (run-from-selection "/home/hebi/benchmark/linux" "./cmt-data/linux.ss"))
  (parameterize ([makefile-flag ffmpeg-flags])
    (run-from-selection "/home/hebi/benchmark/FFmpeg" "./cmt-data/ffmpeg.ss"))
  )





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; process experiment data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (parse-debug.txt f)
  (map (λ (line)
         (string->number (string-trim (second (string-split line ":")))))
       (drop (string-split (file->string f) "\n") 1)))

(define (list-add lsts)
  (foldl (λ (x acc)
           (map + x acc))
         (make-list (length (first lsts)) 0)
         lsts))
(define (data-from-vartmp)
  ;; (S, S', P, lca, time, br, num)
  (append
   (let* ([lst (filter
                (λ (d)
                  (> (first d) 0))
                (for/list ([dir (directory-list "/var/tmp" #:build? #t)]
                           #:when (string-prefix? (path->string dir) "/var/tmp/rkttmp"))
                  (let ([res (parse-debug.txt (build-path dir "debug.txt"))])
                    (if (file-exists? (build-path dir "main.o"))
                        (append res '(1))
                        (append res '(0))))))]
          [len (length lst)])
     (append
      (map (λ (x)
             (/ (->fl x) len)) (list-add lst))
      ;; max S
      (list (first (reverse (sort
                             #;
                             (filter (λ (x)
                                       (= (list-ref x 6) 1))
                                     lst)
                             lst
                             < #:key first))))))
   (list (length (directory-list "/var/tmp" #:build? #t)))))


(module+ test
  ;; calculate metrics
  (data-from-vartmp)
  )

