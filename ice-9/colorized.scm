;; Copyright (C) 2012 Free Software Foundation, Inc.

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
  #:use-module (rnrs bytevectors) 
  #:use-module ((rnrs) #:select (define-record-type vector-for-each))
  #:use-module (ice-9 rdelim)
  #:use-module ((srfi srfi-1) #:select (filter-map proper-list?))
  #:use-module (system repl common)
  #:export (activate-colorized custom-colorized-set! color-it 
	    string-in-color add-color-scheme! display-in-color
	    enable-color-test disable-color-test))

(define (colorized-repl-printer repl val)
  (colorize-it val))
      
(define (activate-colorized)
  (repl-default-option-set! 'print colorized-repl-printer))

(define-record-type color-scheme
  (fields str data type color control method))
  
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

(define get-color
  (lambda (color)
    (assoc-ref *color-list* color)))

(define generate-color
  (lambda (colors)
    (let ((color-list
	   (filter-map (lambda (c) (assoc-ref *color-list* c)) colors)))
      (if (null? color-list)
	  ""
	  (string-join color-list ";" 'infix)))))

(define colorize-the-string
  (lambda (color str control)
    (string-append "\x1b[" (generate-color color) "m" str "\x1b[" (generate-color control) "m")))

(define color-it-test
  (lambda (color str control)
    str))

;; test-helper functions
;; when eanbled, it won't output colored result, but just normal.
;; it used to test the array/list/vector print result.
(define *color-func* (make-fluid colorize-the-string))
(define (disable-color-test) 
  (fluid-set! *color-func* colorize-the-string))
(define (enable-color-test) 
  (fluid-set! *color-func* color-it-test))

(define color-it 
  (lambda (cs)
    (let* ((str (color-scheme-str cs))
	   (color (color-scheme-color cs))
	   (control (color-scheme-control cs)))
      (color-it-inner color str control))))
  
(define color-it-inner 
  (lambda (color str control)
    ((fluid-ref *color-func*) color str control)))

(define* (space #:optional (port (current-output-port)))
  (display #\sp port))

(define (backspace port)
  (seek port -1 SEEK_CUR))

(define *pre-sign* 
  `((LIST       .   "(") 
    (PAIR       .   "(") 
    (VECTOR     .   "#(")
    (BYTEVECTOR .   "#vu8(")
    (ARRAY      .   #f))) 
;; array's sign is complecated, return #f so it will be handled by pre-print

(define* (pre-print cs #:optional (port (current-output-port)))
  (let* ((type (color-scheme-type cs))
	 (control (color-scheme-control cs))
	 (sign (assoc-ref *pre-sign* type))
	 (color (color-scheme-color cs))) 
    (if sign
	(display (color-it-inner color sign control) port)  ; not array
	;; array complecated coloring
	(display (color-array-inner cs) port))))

(define (print-dot port)
  (let ((light-cyan '(CYAN BOLD)))
    (display (color-it-inner light-cyan "." '(RESET)) port)))

(define delimiter?
  (lambda (ch)
    (char-set-contains? char-set:punctuation ch)))

(define color-array-inner
  (lambda (cs)
    (let* ((colors (color-scheme-color cs))
	   (control (color-scheme-control cs))
	   (sign-color (car colors))
	   (attr-color (cadr colors))
	   (str (color-scheme-str cs))
	   (attrs (string->list 
		   (call-with-input-string str (lambda (p) (read-delimited "(" p))))))
      (call-with-output-string
       (lambda (port)
	 (for-each (lambda (ch)
		     (let ((color (if (delimiter? ch) sign-color attr-color)))
		       (display (color-it-inner color (string ch) control) port)))
		   attrs)
	 ;; output left-paren
	 (display (color-it-inner sign-color "(" control) port))))))

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
  (let* ((data (color-scheme-data cs)))
    (if (proper-list? data)
	(call-with-output-string
	 (lambda (port)
	   (pre-print cs port)
	   (for-each (lambda (x) (colorize x port) (space port)) data)
	   (backspace port)  ; remove the redundant space
	   (post-print cs port)))
	(color-pair cs))))
    
(define (color-pair cs)
  (let* ((data (color-scheme-data cs))
	 (d1 (car data))
	 (d2 (cdr data)))
    (call-with-output-string
     (lambda (port)
       (pre-print cs port)
       (colorize d1 port)
       (space port) (print-dot port) (space port)
       (colorize d2 port)
       (post-print cs port)))))

(define (color-class cs)
  (color-it cs))

(define (color-procedure cs)
  (color-it cs))

(define (color-vector cs)
  (let ((vv (color-scheme-data cs)))
    (call-with-output-string
     (lambda (port)
       (pre-print cs port)
       (vector-for-each (lambda (x) (colorize x port) (space port)) vv)
       (backspace port) ; remove the redundant space
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
  (let* ((data (color-scheme-data cs))
	 (colors (color-scheme-color cs))
	 (num-color (car colors))
	 (div-color (cadr colors))
	 (control (color-scheme-control cs))
	 (n (object->string (numerator data)))
	 (d (object->string (denominator data))))
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

(define (color-bytevector cs)
  (let ((ll (bytevector->u8-list (color-scheme-data cs))))
    (call-with-output-string
     (lambda (port)
       (pre-print cs port)
       (for-each (lambda (x) (colorize x port) (space port)) ll)
       (backspace port) ; remove the redundant space
       (post-print cs port)))))

(define (color-boolean cs)
  (color-it cs))

(define (color-array cs)
  (let ((ll (array->list (color-scheme-data cs))))
    (call-with-output-string
     (lambda (port)
       (pre-print cs port)
       (for-each (lambda (x) (colorize x port) (space port)) ll) ; easy life to use list rather than array.
       (backspace port)  ; remove the redundant space
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

(define (class? obj)
  (struct? obj))

(define *colorize-list*
  `((,integer? INTEGER ,color-integer (BLUE BOLD))
    (,char? CHAR ,color-char (YELLOW))
    (,string? STRING ,color-string (RED))
    (,list? LIST ,color-list (BLUE BOLD))
    (,pair? PAIR ,color-list (BLACK BOLD)) ; NOTE: proper-list is a <pair>, and cons is <pair> too, so call color-list either.
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
    (,bytevector? BYTEVECTOR ,color-bytevector (CYAN))
    (,boolean? BOOLEAN ,color-boolean (BLUE))
    (,array? ARRAY ,color-array ((CYAN BOLD) (YELLOW BOLD)))
    (,complex? COMPLEX ,color-complex (MAGENTA))
    (,hash-table? HASH-TABLE ,color-hashtable (BLUE))
    (,hook? HOOK ,color-hook (GREEN))))
;; TODO: if there's anything to add

(define data->token-color
  (lambda (data)
    (call/cc (lambda (return)
	       (for-each (lambda (x)  ; checkout user defined data type
			   (and ((car x) data) (return (cdr x))))
			 (current-custom-colorized))
	       (for-each (lambda (x)  ; checkout default data type
			   (and ((car x) data) (return (cdr x))))
			 *colorize-list*)
	       (return `(UNKNOWN ,color-unknown (WHITE))))))) ; no suitable data type ,return the unknown solution
	      
;; NOTE: we don't use control now, but I write the mechanism for future usage.
(define generate-color-scheme
  (lambda (data)
    (let* ((str (object->string data))
	   (r (data->token-color data))
	   (type (car r))
	   (method (cadr r))
	   (color (caddr r)))
      (make-color-scheme str data type color '(RESET) method)))) 

(define generate-custom-string-color-scheme
  (lambda (str color)
    (make-color-scheme str #f #f color '(RESET) color-string)))

(define string-in-color
  (lambda (str color)
"Example: (string-in-color \"hello\" '(BLUE BOLD))" 
    (and (not (list? color)) (error string-in-color "color should be a list!" color))
    (let ((cs (generate-custom-string-color-scheme str color)))
      (color-it cs))))

(define display-in-color
  (lambda (str color)
"Example: (display-in-color \"hello\" '(BLUE BOLD))"
    (display (string-in-color str color))))

(define* (colorize-it data #:optional (port (current-output-port)))
  (colorize data port)
  (newline port))

(define* (colorize data #:optional (port (current-output-port)))
  (let* ((cs (generate-color-scheme data))
	 (f (color-scheme-method cs)))
    (display (f cs) port)))



