;;  Copyright (C) 2012
;;      "Mu Lei" known as "NalaGinrut" <NalaGinrut@gmail.com>
;;  Ragnarok is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.

;;  Ragnarok is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.

;;  You should have received a copy of the GNU General Public License
;;  along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define-module (ice-9 colorized)
  #:use-module (oop goops)
  #:use-module (rnrs)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:export (activate-colorized))

(define (activate-colorized)
  (add-hook! before-print-hook colorize-it))

(define-record-type color-scheme
  (fields str data class color control method))
  
(define *color-list*
  '((nothing      .  "0;0")
    (black        .  "0;30")
    (red          .  "0;31")
    (green        .  "0;32")
    (brown        .  "0;33")
    (blue         .  "0;34")
    (cyan         .  "0;36")
    (purple       .  "0;35")
    (light-gray   .  "0;37")
    (dark-gray    .  "1;30")
    (light-red    .  "1;31")
    (light-green  .  "1;32")
    (yellow       .  "1;33")
    (light-blue   .  "1;34")
    (light-cyan   .  "1;36")
    (light-purple .  "1;35")
    (white        .  "1;37")))

(define get-color
  (lambda (color)
    (assoc-ref *color-list* color)))

(define color-it
  (lambda (cs)
    (let* ((str (color-scheme-str cs))
	   (color (color-scheme-color cs))
	   (control (color-scheme-control cs)))
      (color-it-inner color str control))))

(define color-it-inner
  (lambda (color str control)
    (string-append "\x1b[" (get-color color) "m" str "\x1b[" control "m")))

(define *pre-sign* 
  `((,<list>       .   "(") 
    (,<pair>       .   "(") 
    (,<vector>     .   "#(")
    (,<bytevector> .   "#vu8(")
    (,<array>      .   #f))) ;; array's sign is complecated.

(define* (pre-print cs #:optional (port (current-output-port)))
  (let* ((class (color-scheme-class cs))
	 (control (color-scheme-control cs))
	 (sign (assoc-ref *pre-sign* class))
	 (color (color-scheme-color cs))) ;; (car color) is the color, (cdr color) is the control
    (if sign
	(display (color-it-inner color sign control) port)  ;; not array
	(display (color-array-inner cs) port) ;; array complecated coloring
	)))

(define (print-dot port)
  (display (color-it-inner 'light-cyan "." "0") port))

(define is-sign?
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
		     (let ((color (if (is-sign? ch) sign-color attr-color)))
		       (display (color-it-inner color (string ch) control) port)))
		   attrs)
	 (display (color-it-inner sign-color "(" control) port) ;; output right-parent
	 )))))

;; I believe all end-sign is ")"      
(define* (post-print cs #:optional (port (current-output-port)))
  (let* ((c (color-scheme-color cs))
	 (control (color-scheme-control cs))
	 (color (if (list? c) (car c) c))) ;; array has a color-list
    (display (color-it-inner color ")" control) port)))

(define (color-integer cs)
  (color-it cs))

(define (color-char cs)
  (color-it cs))

(define (color-string cs)
  (color-it cs))

(define (color-list cs)
  (let ((data (color-scheme-data cs)))
    (if (proper-list? data)
	(call-with-output-string
	 (lambda (port)
	   (pre-print cs port)
	   (for-each (lambda (x) (colorize x port) (display " " port)) data)
	   (seek port -1 SEEK_CUR)
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
       (display " " port) (print-dot port) (display " " port)
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
       (vector-for-each (lambda (x) (colorize x port) (display " " port)) vv)
       (seek port -1 SEEK_CUR)
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

(define (color-real cs)
  (color-it cs))

(define (color-fraction cs)
  (let* ((data (color-scheme-data cs))
	 (colors (color-scheme-color cs))
	 (num-color (car colors))
	 (div-color (cadr colors))
	 (control (color-scheme-control cs))
	 (n (object->string (numerator data)))
	 (d (object->string (denominator data))))
    (call-with-output-string
     (lambda (port)
       (display (color-it-inner num-color n control) port)
       (display (color-it-inner div-color "/" control) port)
       (display (color-it-inner num-color d control) port)))))

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
       (for-each (lambda (x) (colorize x port) (display " " port)) ll)
       (seek port -1 SEEK_CUR)
       (post-print cs port)))))

(define (color-boolean cs)
  (color-it cs))

(define (color-arbiter cs)
  (color-it cs))

(define (color-array cs)
  (let ((ll (array->list (color-scheme-data cs))))
    (call-with-output-string
     (lambda (port)
       (pre-print cs port)
       (for-each (lambda (x) (colorize x port) (display " " port)) ll) ;; easy life to use list rather than array.
       (seek port -1 SEEK_CUR)
       (post-print cs port)))))

(define (color-complex cs)
  (color-it cs))

(define (color-hashtable cs)
  (color-it cs))

(define (color-hook cs)
  (color-it cs))

(define (color-unknown cs)
  (color-it cs))


(define *colorize-list*
  `((,<integer> ,color-integer light-blue)
    (,<char> ,color-char brown)
    (,<string> ,color-string red)
    (,<list> ,color-list light-blue)
    (,<pair> ,color-list light-gray) ;; NOTE: proper-list is a <pair>, and cons is <pair> too, so call color-list either.
    (,<class> ,color-class light-cyan)
    (,<procedure> ,color-procedure yellow)
    (,<vector> ,color-vector light-purple)
    (,<keyword> ,color-keyword purple)
    (,<character-set> ,color-char-set white)
    (,<symbol> ,color-symbol light-green)
    (,<stack> ,color-stack purple)
    (,<record-type> ,color-record-type dark-gray)
    (,<real> ,color-real yellow)
    (,<fraction> ,color-fraction (light-blue yellow))
    (,<regexp> ,color-regexp green)
    (,<bitvector> ,color-bitvector brown)
    (,<bytevector> ,color-bytevector cyan)
    (,<boolean> ,color-boolean blue)
    (,<arbiter> ,color-arbiter blue)
    (,<array> ,color-array (light-cyan brown))
    (,<complex> ,color-complex purple)
    (,<hashtable> ,color-hashtable blue)
    (,<hook> ,color-hook green)
    (,<unknown> ,color-unknown white)
    ;; TODO: if there's anything to add
    ))

;; NOTE: we don't use control now, but I write the mechanism for future usage.
(define generate-color-scheme
  (lambda (data)
    (let* ((class (class-of data))
	   (str (object->string data))
	   (r (assoc-ref *colorize-list* class))
	   (method (car r))
	   (color (cadr r)))
      (make-color-scheme str data class color "0" method)))) 

(define* (colorize-it data #:optional (port (current-output-port)))
  (colorize data port)
  (newline port))

(define* (colorize data #:optional (port (current-output-port)))
  (let* ((cs (generate-color-scheme data))
	 (f (color-scheme-method cs)))
    (display (f cs) port)))



