#lang racket/base

(provide read-bytes-list read-bytes-vector)

(define default-chunk-size 1024)

(define (read-bytes-list [chunk-size default-chunk-size] [in (current-input-port)])
  (let loop ([result '()])
    (let ([next (read-bytes chunk-size in)])
      (if (eof-object? next)
          (reverse result)
          (loop (cons next result))))))

(define (read-bytes-vector [chunk-size default-chunk-size] [in (current-input-port)])
  (list->vector (read-bytes-list chunk-size in)))
