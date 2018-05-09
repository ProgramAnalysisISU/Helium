#lang racket

(require list-utils)
;; get the changed lines in the commits

(define (my-all-split lst pred)
  (filter-not empty?
              (let ([res '()]
                    [tmp '()])
                (for ([x lst])
                  (if (pred x)
                      (begin
                        (set! res (append res (list tmp)))
                        (set! tmp (list x)))
                      (set! tmp (append tmp (list x)))))
                (append res (list tmp)))))

(define (diff->metrics diff-output)
  (filter (λ (x) (and (string? (first x))
                      (string-suffix? (first x) ".c")))
          (my-all-split
           (let ([res '()])
             (foldl (λ (line acc)
                      (cond
                        [(string-prefix? line "+++ b/") (set! res (append res (list (substring line 6))))
                                                        acc]
                        [(string-prefix? line "+") (set! res (append res (list acc)))
                                                   (add1 acc)]
                        [(string-prefix? line "@@") (let ([s (first
                                                              (string-split
                                                               (second
                                                                (string-split
                                                                 (first
                                                                  (string-split
                                                                   (substring line 3) "@")) "+")) ","))])
                                                      (string->number (string-trim s)))]
                        [(string-prefix? line " ") (add1 acc)]
                        [else acc]))
                    0 (string-split diff-output "\n"))
             res)
           string?)))


;; (my-all-split '("Changelog" 6 7 8 9 10 11 12 13 14 15 16 "libavcodec/vp3.c" 955 958 959) string?)

(define (get-diff bench num [skip '()])
  "get 100 diff"
  (parameterize ([current-directory bench])
    (filter-not empty?
                (for/list ([i (range num)]
                           #:when (not (member i skip)))
                  ;; (displayln i)
                  (display ".") (flush-output)
                  (let ([b (string-append "HEAD" (make-string i #\^))]
                        [a (string-append "HEAD" (make-string (+ i 1) #\^))])
                    (let-values ([(sp stdout stdin _stderr)
                                  (subprocess #f #f 'stdout (find-executable-path "git") "diff" "--exit-code" "--no-color" a b)])
                      ;; (displayln a)
                      ;; (displayln "waiting ..")
                      (subprocess-wait sp)
                      ;; (displayln "done")
                      (let ([output (port->string stdout)])
                        (close-output-port stdin)
                        (close-input-port stdout)
                        ;; output: (file, (4,5,6,7))
                        (diff->metrics output))))))))

(define (validate-ss bench-root data)
  (filter (λ (d)
            (let ([f (build-path bench-root (~a (first (first d)) ".ss"))])
              ;; (displayln f)
              (file-exists? f)))
          data))

(define (write-cmt-data data f)
  (let ([p (open-output-file f #:exists 'replace)])
    (pretty-write data p)
    (close-output-port p)))

(module+ test
  (write-cmt-data
   (validate-ss
    "/home/hebi/benchmark/FFmpeg"
    (get-diff "/home/hebi/benchmark/patch/FFmpeg" 300))
   "cmt-data/ffmpeg.ss")
  (write-cmt-data
   (validate-ss
    "/home/hebi/benchmark/git"
    (get-diff "/home/hebi/benchmark/patch/git" 300 '(1 143 194 223)))
   "cmt-data/git.ss")
  (write-cmt-data
   (take
    (validate-ss
     "/home/hebi/benchmark/linux"
     (get-diff "/home/hebi/benchmark/patch/linux" 300 '(67 141 154 162 175 189 200 228 229 230 246))) 50)
   "cmt-data/linux.ss")
  (write-cmt-data
   (validate-ss
    "/home/hebi/benchmark/openssl"
    (get-diff "/home/hebi/benchmark/patch/openssl" 300))
   "cmt-data/openssl.ss")
  (write-cmt-data
   (validate-ss
    "/home/hebi/benchmark/vim"
    (get-diff "/home/hebi/benchmark/patch/vim" 300 '(3 24 80 134 183)))
   "cmt-data/vim.ss")
  )


#;
(parameterize ([current-directory "/home/hebi/benchmark/patch/git"])
  (let-values ([(sp stdout stdin _stderr)
                (subprocess #f #f 'stdout (find-executable-path "git") "diff" "--exit-code" "--no-color" "HEAD^^")])
    (subprocess-wait sp)
    (displayln "done")
    #;
    (let ([output (port->string stdout)])
      (close-output-port stdin)
      (close-input-port stdout)
      ;; output: (file, (4,5,6,7))
      (diff->metrics output))))
