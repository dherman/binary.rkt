#lang racket/base

(require (for-syntax racket/base "bytes-vector.ss"))
(require "bytes-vector.ss")
(provide read-bytes-list read-bytes-vector)
(provide compile-bytes compile-bytes-list compile-bytes-vector)

(define-syntax (compile-bytes-list stx)
  (syntax-case stx ()
    [(_ in)
     #'(compile-bytes-list 1024 in)]
    [(_ chunk-size in)
     (and (string? (syntax->datum #'in))
          (integer? (syntax->datum #'chunk-size)))
     (with-syntax ([(chunk ...) (with-input-from-file (syntax->datum #'in)
                                  (lambda ()
                                    (read-bytes-list (syntax->datum #'chunk-size))))])
       #'(list chunk ...))]))

(define-syntax (compile-bytes-vector stx)
  (syntax-case stx ()
    [(_ in)
     #'(compile-bytes-vector 1024 in)]
    [(_ chunk-size in)
     (and (string? (syntax->datum #'in))
          (integer? (syntax->datum #'chunk-size)))
     (with-syntax ([(chunk ...) (with-input-from-file (syntax->datum #'in)
                                  (lambda ()
                                    (read-bytes-list (syntax->datum #'chunk-size))))])
       #'(vector chunk ...))]))

(define-syntax (compile-bytes stx)
  (syntax-case stx ()
    [(_ in)
     #'(compile-bytes 1024 in)]
    [(_ max in)
     (and (string? (syntax->datum #'in))
          (integer? (syntax->datum #'max)))
     (with-syntax ([b (with-input-from-file (syntax->datum #'in)
                        (lambda ()
                          (read-bytes (syntax->datum #'max))))])
       (datum->syntax stx #'b))]))
