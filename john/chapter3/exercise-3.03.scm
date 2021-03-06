#lang sicp

(#%require "common.scm")
;(#%require (only racket/base error))

;   Exercise 3.3
;   ============
;   
;   Modify the make-account procedure so that it creates password-protected
;   accounts.  That is, make-account should take a symbol as an additional
;   argument, as in
;   
;   (define acc (make-account 100 'secret-password))
;   
;   The resulting account object should process a request only if it is
;   accompanied by the password with which the account was created, and
;   should otherwise return a complaint:
;   
;   ((acc 'secret-password 'withdraw) 40)
;   60
;   
;   ((acc 'some-other-password 'deposit) 50)
;   
;   "Incorrect password"
;   
;   ------------------------------------------------------------------------
;   [Exercise 3.3]:  http://sicp-book.com/book-Z-H-20.html#%_thm_3.3
;   3.1.1 Local State Variables - p225
;   ------------------------------------------------------------------------

(-start- "3.3")

(define (make-account initial-password balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch given-password m)
    (if (eq? initial-password given-password)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request -- MAKE-ACCOUNT"
                           m)))
        (lambda (_) "Incorrect Password")))
  dispatch)

(define acc (make-account 'passw0rd 100))
((acc 'passw0rd 'withdraw) 40)
((acc 'Password 'deposit) 50)
(--end-- "3.3")

