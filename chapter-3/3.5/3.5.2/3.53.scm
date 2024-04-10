#lang sicp
(#%require "../streams.scm")
(#%require "../defining-streams-implicitly.scm")

(define s (cons-stream 1 (add-streams s s)))

(ss s)