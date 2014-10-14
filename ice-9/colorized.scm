;; Copyright (C) 2013,2014 Free Software Foundation, Inc.

;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 3 of the License, or (at your option) any later version.
;;;; 
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this library; if not, write to the Free Software
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

;;;; Author: Mu Lei known as NalaGinrut <nalaginrut@gmail.com>

(define-module (ice-9 colorized)
  #:use-module (system base language)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module ((srfi srfi-1) #:select (filter-map any proper-list?))
  #:use-module (srfi srfi-9)
  #:use-module (system repl common)
  #:export (activate-colorized
            custom-colorized-set!
            color-it
            colorize-it
            colorize
            color-func
            colorize-string
            colorized-display
            add-color-scheme!))

(define *bug-report* "https://github.com/NalaGinrut/guile-colorized")

(define (colorized-repl-printer repl val)
  (colorize-it val))

(define (activate-colorized)
  (let ((rs (fluid-ref *repl-stack*)))
    (repl-default-prompt-set! generate-colored-prompt) ; colorize the prompt
    (if (null? rs)
        (repl-default-option-set! 'print colorized-repl-printer) ; if no REPL started, set as default printer
        (repl-option-set! (car rs) 'print colorized-repl-printer)))) ; or set as the top-REPL printer

;; color-scheme context, contains some info to be used
(define-record-type color-scheme
  (make-color-scheme obj type color control method)
  color-scheme?
  (obj color-scheme-obj) ; the obj to be colored
  (type color-scheme-type) ; the obj type (for debug/test)
  (color color-scheme-color) ; the color
  (control color-scheme-control) ; ansi control code
  (method color-scheme-method)) ; colorized method for the obj type

(define *color-list*
  `((CLEAR       .   "0")
    (RESET       .   "0")
    (BOLD        .   "1")
    (DARK        .   "2")
    (UNDERLINE   .   "4")
    (UNDERSCORE  .   "4")
    (BLINK       .   "5")
    (REVERSE     .   "6")
    (CONCEALED   .   "8")
    (BLACK       .  "30")
    (RED         .  "31")
    (GREEN       .  "32")
    (YELLOW      .  "33")
    (BLUE        .  "34")
    (MAGENTA     .  "35")
    (CYAN        .  "36")
    (WHITE       .  "37")
    (ON-BLACK    .  "40")
    (ON-RED      .  "41")
    (ON-GREEN    .  "42")
    (ON-YELLOW   .  "43")
    (ON-BLUE     .  "44")
    (ON-MAGENTA  .  "45")
    (ON-CYAN     .  "46")
    (ON-WHITE    .  "47")))

(define (get-color color)
  (assq-ref *color-list* color))

(define (generate-color colors)
  (let ((color-list
         (filter-map get-color colors)))
    (if (null? color-list)
        ""
        (string-append "\x1b[" (string-join color-list ";" 'infix) "m"))))

(define* (colorize-string-helper color str control #:optional (rl-ignore #f))
  (if rl-ignore
      (string-append "\x01" (generate-color color) "\x02" str "\x01" (generate-color control) "\x02")
      (string-append (generate-color color) str (generate-color control))))

;; test-helper functions
;; when eanbled, it won't output colored result, but just normal.
;; it used to test the array/list/vector print result.
(define color-func (make-parameter colorize-string-helper))

(define (color-it cs) 
  (let* ((obj (color-scheme-obj cs))
         (str (object->string obj))
         (color (color-scheme-color cs))
         (control (color-scheme-control cs)))
    (color-it-inner color str control)))

(define (color-it-inner color str control)
  ((color-func) color str control))

(define* (space #:optional (port (current-output-port)))
  (display #\sp port))

(define *pre-sign* 
  `((LIST       .   "(")
    (PAIR       .   "(")
    (VECTOR     .   "#(")
    (ARRAY      .   #f))) 
;; array's sign is complicated, return #f so it will be handled by pre-print

(define* (pre-print cs #:optional (port (current-output-port)))
  (let* ((type (color-scheme-type cs))
         (control (color-scheme-control cs))
         (sign (assq-ref *pre-sign* type))
         (color (color-scheme-color cs)))
    (if sign
        (display (color-it-inner color sign control) port)  ; not array
        ;; array complecated coloring
        (display (color-array-inner cs) port))))

(define (print-dot port)
  (let ((light-cyan '(CYAN BOLD)))
    (display (color-it-inner light-cyan "." '(RESET)) port)))

(define (delimiter? ch)
  (char-set-contains? char-set:punctuation ch))

(define (color-array-inner cs)
  (let* ((colors (color-scheme-color cs))
         (control (color-scheme-control cs))
         (sign-color (car colors))
         (attr-color (cadr colors))
         (str (object->string (color-scheme-obj cs)))
         (attrs (string->list 
                 (call-with-input-string str (lambda (p) (read-delimited "(" p))))))
    (call-with-output-string
     (lambda (port)
       (for-each (lambda (ch)
                   (let ((color (if (delimiter? ch) sign-color attr-color)))
                     (display (color-it-inner color (string ch) control) port)))
                 attrs)
       ;; output left-paren
       (display (color-it-inner sign-color "(" control) port)))))

;; Write a closing parenthesis.
(define* (post-print cs #:optional (port (current-output-port)))
  (let* ((c (color-scheme-color cs))
         (control (color-scheme-control cs))
         (color (if (list? (car c)) (car c) c))) ; array has a color-list
    (display (color-it-inner color ")" control) port)))

(define (color-integer cs)
  (color-it cs))

(define (color-char cs)
  (color-it cs))

(define (color-string cs)
  (color-it cs))

(define (color-list cs)
  (let* ((obj (color-scheme-obj cs)))
    (call-with-output-string
     (lambda (port)
       (pre-print cs port)
       (display (string-join (map ->cstr obj) " ") port)
       (post-print cs port)))))

(define lpair (make-parameter #f))
(define (color-long-pair obj port)
  (let ((d1 (car obj))
        (d2 (cdr obj)))
    (colorize d1 port)
    (space port)
    (colorize d2 port)))

(define (color-pair cs)
  (let* ((obj (color-scheme-obj cs))
         (d1 (car obj))
         (d2 (cdr obj)))
  (call-with-output-string
   (lambda (port)
     (and (not (lpair)) (pre-print cs port))
     (colorize d1 port)
     (space port)
     (cond
      ((long-pair? obj)
       (parameterize ((lpair #t))
         (if (long-pair? d2)
             (color-long-pair d2 port)
             (colorize d2 port))))
      ((and (lpair) (not (long-pair? d2)))
       ;; cdr is not long-pair anymore, but it's in a long-pair, should be a end pair
       (parameterize ((lpair #f))
         (print-dot port) (space port)
         (colorize d2 port)))
      (else
       (print-dot port)(space port)
       (colorize d2 port)))
     (and (not (lpair)) (post-print cs port))))))

(define (color-class cs)
  (color-it cs))

(define (color-procedure cs)
  (color-it cs))

(define (color-vector cs)
  (let ((ll (vector->list (color-scheme-obj cs))))
    (call-with-output-string
     (lambda (port)
       (pre-print cs port)
       (display (string-join (map ->cstr ll) " ") port)
       (post-print cs port)))))

(define (color-keyword cs)
  (color-it cs))

;; TODO: maybe print it as char one by one?
(define (color-char-set cs)
  (color-it cs))

(define (color-symbol cs)
  (color-it cs))

(define (color-stack cs)
  (color-it cs))

(define (color-record-type cs)
  (color-it cs))

(define (color-inexact cs)
  (color-it cs))

(define (color-exact cs)
  (let* ((obj (color-scheme-obj cs))
         (colors (color-scheme-color cs))
         (num-color (car colors))
         (div-color (cadr colors))
         (control (color-scheme-control cs))
         (n (object->string (numerator obj)))
         (d (object->string (denominator obj))))
    (call-with-output-string
     (lambda (port)
       (format port "~a~a~a" 
               (color-it-inner num-color n control)
               (color-it-inner div-color "/" control)
               (color-it-inner num-color d control))))))

(define (color-regexp cs)
  (color-it cs))

(define (color-bitvector cs)
  ;; TODO: is it right?
  (color-it cs))

(define (color-boolean cs)
  (color-it cs))

(define (color-array cs)
  (let ((ll (array->list (color-scheme-obj cs))))
    (call-with-output-string
     (lambda (port)
       (pre-print cs port)
       (display (string-join (map ->cstr ll) " ") port)
       (post-print cs port)))))

(define (color-complex cs)
  (color-it cs))

(define (color-hashtable cs)
  (color-it cs))

(define (color-hook cs)
  (color-it cs))

(define (color-unknown cs)
  (color-it cs))

;;--- custom color scheme ---
(define *custom-colorized-list* (make-fluid '()))

(define (custom-colorized-set! ll)
  (fluid-set! *custom-colorized-list* ll))

(define (current-custom-colorized)
  (fluid-ref *custom-colorized-list*))

(define (add-color-scheme! cs-list)
  (let ((ll (current-custom-colorized)))
    (custom-colorized-set! `(,@cs-list ,@ll))))
;;--- custom color scheme end---

(define (is-inexact? obj)
  (and (number? obj) (inexact? obj)))

(define (is-exact? obj)
  (and (number? obj) (exact? obj)))

;; A class is a struct.
(define (class? obj)
  (struct? obj))

;; a pair whose cdr is a pair too is long pair, say, '(a b . c)
;; NOTE: only defined in this project, not a standard type
(define (long-pair? obj)
  (and (pair? obj)
       (or (pair? (cdr obj)) (null? (cdr obj)))))

(define *colorize-list*
  `((,integer? INTEGER ,color-integer (BLUE BOLD))
    (,char? CHAR ,color-char (YELLOW))
    (,string? STRING ,color-string (RED))
    (,proper-list? LIST ,color-list (BLUE BOLD))
    ;; NOTE: proper-list is a <pair>, and cons is <pair> too, so we put list checker before pair
    (,pair? PAIR ,color-pair (BLACK BOLD))
    (,class? CLASS ,color-class (CYAN BOLD))
    (,procedure? PROCEDURE ,color-procedure (YELLOW BOLD))
    (,vector? VECTOR ,color-vector (MAGENTA BOLD))
    (,keyword? KEYWORD ,color-keyword (MAGENTA))
    (,char-set? CHAR-SET ,color-char-set (WHITE))
    (,symbol? SYMBOL ,color-symbol (GREEN BOLD))
    (,stack? STACK ,color-stack (MAGENTA))
    (,record-type? RECORD-TYPE ,color-record-type (BLACK BOLD))
    ;; We don't check REAL here, since it'll cover FLOAT and FRACTION, but user may customs it as they wish.
    (,is-inexact? FLOAT ,color-inexact (YELLOW))
    (,is-exact? FRACTION ,color-exact ((BLUE BOLD) (YELLOW)))
    (,regexp? REGEXP ,color-regexp (GREEN))
    (,bitvector? BITVECTOR ,color-bitvector (YELLOW BOLD))
    (,array? ARRAY ,color-array ((CYAN BOLD) (YELLOW BOLD)))
    (,boolean? BOOLEAN ,color-boolean (BLUE))
    (,complex? COMPLEX ,color-complex (MAGENTA))
    (,hash-table? HASH-TABLE ,color-hashtable (BLUE))
    (,hook? HOOK ,color-hook (GREEN))))
;; TODO: if there's anything to add

(define (obj->token-color obj)
  (let ((proc (lambda (x) (and ((car x) obj) (cdr x)))))
    (or (any proc (current-custom-colorized)) ; checkout user defined obj type
        (any proc *colorize-list*) ; checkout default obj type
        `(UNKNOWN ,color-unknown (WHITE))))) ; no suitable obj type ,return the unknown solution

;; NOTE: we don't use control now, but I write the mechanism for future usage.
(define (generate-color-scheme obj)
  (let* ((r (obj->token-color obj))
         (type (car r))
         (method (cadr r))
         (color (caddr r)))
    (make-color-scheme obj type color '(RESET) method)))

(define (generate-custom-string-color-scheme str color)
  (make-color-scheme str #f color '(RESET) color-string))

(define* (colorize-string str color #:optional (rl-ignore #f))
  "Example: (colorize-string \"hello\" '(BLUE BOLD))" 
  (and (not (list? color)) (error colorize-string "color should be a list!" color))
  (colorize-string-helper color str '(RESET) rl-ignore))

(define (colorized-display str color)
  "Example: (colorized-display \"hello\" '(BLUE BOLD))"
  (display (colorize-string str color)))

(define* (colorize-it obj #:optional (port (current-output-port)))
  (colorize obj port)
  (newline port))

(define* (colorize obj #:optional (port (current-output-port)))
  (let* ((cs (generate-color-scheme obj))
         (f (color-scheme-method cs)))
    (display (f cs) port)))

(define (->cstr obj)
  (call-with-output-string
   (lambda (port)
     (colorize obj port))))

(define (generate-colored-prompt repl)
  (let ((level (length (cond
                        ((fluid-ref *repl-stack*) => cdr)
                        (else '())))))
    (string-append
     (colorize-string (object->string (language-name (repl-language repl))) '(MAGENTA) #t)
     (colorize-string "@(" '(CYAN) #t)
     (colorize-string (format #f "~{~a~^ ~}" (module-name (current-module))) '(WHITE) #t)
     (colorize-string ")" '(CYAN) #t)
     (colorize-string (if (zero? level) "" (format #f " [~a]" level)) '(RED) #t)
     (colorize-string "> " '(CYAN) #t))))
