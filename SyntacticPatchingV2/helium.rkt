#lang racket

;; This is the cmd interface for Helium

(require "main.rkt")
(require "bench-data.rkt")
(require racket/cmdline)

(define cmd-verbose-mode (make-parameter #f))
;; possible values:
;; git, vim, linux, etc
(define cmd-flags (make-parameter ""))
(define cmd-sel (make-parameter ""))

(define benchmark
  (command-line
   #:program "helium"
   #:once-each
   [("-v" "--verbose") "verbose mode"
                       (cmd-verbose-mode #t)]
   [("-f" "--flags") flag "flags to use"
                     (cmd-flags
                      (case flag
                        [("linux") linux-flags]
                        [("git") git-flags]
                        [("vim") vim-flags]
                        [("ffmpeg") ffmpeg-flags]
                        [("openssl") openssl-flags]
                        [("") ""]
                        [else (error "Invalid flags")]))]
   [("-s" "--selection")
    sel
    "selection file. When missing, random selection mode will be
used."
    (cmd-sel sel)]
   ;; #:once-any
   ;; #:multi
   #:args (benchmark) (simplify-path (expand-user-path benchmark))))

(parameterize ([makefile-flag (cmd-flags)]
               [verbose-mode (cmd-verbose-mode)])
  (if (non-empty-string? (cmd-sel))
      ;; selection mode
      (let ([p (simplify-path (expand-user-path (cmd-sel)))])
        (run-from-selection benchmark p))
      ;; random mode
      (run-random-selection benchmark)))

