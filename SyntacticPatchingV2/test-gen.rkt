#lang racket

(require "gen.rkt")
(require "snip.rkt")

(module+ test
  (display (gen-code (read-scm "./test/expr.c.scm"))))
