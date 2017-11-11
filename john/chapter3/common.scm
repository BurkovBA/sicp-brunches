#lang sicp

(#%require (only racket/base error))
(#%provide error)
#|
When moving from Racket to Sicp I've just removed stuff that sicp
doesn't like.  If/when that causes a problem I'll have to go figure...
|#


(define line-length 72)

(define nl "\n")

; Crude replacement for ~a function in Racket
(define (boolean->string val) (if val "#t" "#f"))
(define (void)
  (if #f "never!"))

(define (~a o)
  (cond
    ((string? o) o)
    ((boolean? o) (boolean->string o))
    ((number? o) (number->string o))
    ((symbol? o) (symbol->string o))
    ((equal? o (void)) (identity ""))
    (else (error "Don't know how to get string for:" o))))

(define (str . parts)
  (apply string-append  (map ~a parts)))


(define (display-line line)
  (display (str "" line nl)))

(define (display-lines lines)
  (for-each display-line lines))

(define (prnl lines)
  (for-each display-line lines))

(define (prn . lines)
  (prnl lines))

(define (dbl-un text)
  (str text nl (make-string (string-length text) #\=)))

(define (double-underline text)
  (apply prn (dbl-un text)))

(define dsh-line (make-string line-length #\-))

(define (dsh-un text)
  (list text (make-string (string-length text) #\-) ""))

(define (out title)
   (list
    dsh-line
    (str "Output: Exercise " title)
    dsh-line
    ""))
    

(define (-start- ex-number)
  (display-lines (out ex-number)))

(define (--end-- _)
  (display "\n\n"))



;(define (present-one function inputs)
;  (list
;   (str "    With: " inputs)
;   (str "    Got:  " (apply function inputs))))
;
;(define (present function . inputs-list)
;  (define (present-input inputs)
;    (apply present-one (list function inputs)))
;  (prn
;   (str "Calling: " function))
;;racket:   (str "Calling: " (object-name function)))
;  (for-each prnl
;            (map present-input inputs-list))
;  (prn ""))
;
;
;
;(define (present-compare-one function inputs expected)
;  (list
;   (str "    With:     " inputs)
;   (str "    Expected: " expected)
;   (str "    Actual:   " (apply function inputs))
;   ""))
;
;(define (present-compare function . input-expected-pairs)
;  (define (present-compare-pair pair)
;    (apply present-compare-one (cons function pair)))
;  (prn
;   (str "Calling: " function))
;;racket   (str "Calling: " (object-name function)))
;  (for-each prnl
;            (map present-compare-pair input-expected-pairs))
;  (prn ""))


(define (ignore . whatever)
  (display ""))


(#%provide (all-defined))

;#########################################################################
;#########################################################################
