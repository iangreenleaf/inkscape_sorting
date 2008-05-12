;; This code is available on the World Wide Web as
;; http://www.cs.grinnell.edu/~youngian/inkscape_sorting.ss


;;;    This program is free software: you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License as published by
;;;    the Free Software Foundation, either version 3 of the License, or
;;;    (at your option) any later version.
;;;
;;;    This program is distributed in the hope that it will be useful,
;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;    GNU General Public License for more details.
;;;
;;;    You should have received a copy of the GNU General Public License
;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sorting implementations ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Helper function
;; Equivalent to calling 'and' with every item in ls as arguments
(define apply-and
  (lambda (ls)
    (cond ((null? ls) #t)
          ((not (car ls)) #f)
          (else (apply-and (cdr ls))))))

;; Merge two lists according to comparator
(define (merge ls1 ls2 comparator)
  (cond ((null? ls1) ls2)
        ((null? ls2) ls1)
        ((comparator (car ls1) (car ls2)) (cons (car ls1) (merge (cdr ls1) ls2 comparator)))
        (else (cons (car ls2) (merge ls1 (cdr ls2) comparator)))))

;; Accomplishes one step of the merge-sort process, returns a list of merged lists
(define (merge-step ls comparator)
  (cond ((null? ls) '())
        ((null? (cdr ls)) ls)
        (else (cons (merge (car ls) (cadr ls) comparator) (merge-step (cddr ls) comparator)))))

;; Accomplishes one step of the quick-sort process, returns a list of lists
;; (where each list is a "less-than", "pivot", or "greater-than" result from
;; one of the partitioning steps.
(define (quick-step ls comparator)
  (if (null? ls)
      '()
      (letrec ((sorter
                (lambda (lt gt pivot remain)
                  (if (null? remain)
                      (list lt (list pivot) gt)
                      (if (comparator (car remain) pivot)
                          (sorter (cons (car remain) lt) gt pivot (cdr remain))
                          (sorter lt (cons (car remain) gt) pivot (cdr remain))))))
               (quick-step-kernel
                (lambda (ls)
                  (cond
                    ((null? ls) '())
                    ((null? (cdr ls)) (list ls))
                    (else
                     (let ((middle-index (floor (/ (length ls) 2))))
                       (sorter '() '() (list-ref ls middle-index) (delete-listref ls middle-index))))))))
        (apply append (map quick-step-kernel ls)))))

;; Bubble sort! Yes!!
(define (bubble-step lst comparator)
  (letrec ((kernel
            (lambda (ls)
              (cond ((null? ls) '())
                    ((equal? 1 (length ls)) ls)
                    ((comparator (car ls) (cadr ls))
                     (cons (car ls) (kernel (cdr ls))))
                    (else
                     (cons (cadr ls) (kernel (cons (car ls) (cddr ls)))))))))
    (list (kernel (car lst)))))

;; Helper function. Accepts a list and comparator and checks to see if the list is
;; sorted, in other words, the comparator holds for every step of the list.
(define (is-sorted? ls comparator)
  (if (or (null? ls) (equal? 1 (length ls)))
      #t
      (if (or (comparator (car ls) (cadr ls))
              (not (comparator (cadr ls) (car ls))))
          (is-sorted? (cdr ls) comparator)
          #f)))

  
;; Not actually used
;; This is to demonstrate how a sort-step procedure (like merge-step or quick-step)
;; may be applied to simply sort a list using the given algorithm.
(define (sort-with step-func base-case ls comparator)
  (if (base-case ls comparator)
      ls
      (sort-with step-func base-case (step-func ls comparator) comparator)))

;; Application of merge-step to sort-with
(define (merge-sort ls comparator)
  (if (null? ls)
      '()
      (sort-with merge-step
                 (lambda (ls c) (equal? 1 (length ls)))
                 (map list ls)
                 comparator)))

;; Application of quick-step to sort-with
(define (quick-sort ls comparator)
  (if (null? ls)
      '()
      (sort-with quick-step
                 (lambda (ls c) (apply-and (map (lambda (l) (or (null? l) (null? (cdr l)))) ls)))
                 (list ls (list) (list))
                 comparator)))

(define (bubble-sort ls comparator)
  (sort-with bubble-step
             (lambda (ls c) (is-sorted? (car ls) c)) 
             (list ls) 
             comparator))

;; Prints a graphical representation of the sorting process by stepping through
;; the sort, printing the list of items each iteration
(define sort-print
  (lambda (step-func base-case ls comparator desktop startx starty w h draw-outlines?)
    (if (null? ls)
        '()
        (if (base-case ls comparator)
            (begin
              (list-to-rect (apply append (cons '() ls)) desktop startx starty w h)
              (if draw-outlines?
                  (list-to-rect-outlines ls desktop startx starty w h)))
            (begin
              (list-to-rect (apply append (cons '() ls)) desktop startx starty w h)
              (if draw-outlines?
                  (list-to-rect-outlines ls desktop startx starty w h))
              (sort-print step-func base-case (step-func ls comparator) comparator desktop startx (+ starty h) w h draw-outlines?))))))


(define merge-sort-print
  (lambda (ls comparator desktop startx starty w h)
    (sort-print merge-step
                (lambda (ls c) (equal? 1 (length ls)))
                (map list ls)
                comparator desktop startx starty w h
                (stroke-visible? desktop))))

(define quick-sort-print
  (lambda (ls comparator desktop startx starty w h)
    (sort-print quick-step
                 (lambda (ls c) (apply-and (map (lambda (l) (or (null? l) (null? (cdr l)))) ls)))
                 (list ls (list) (list))
                 comparator desktop startx starty w h
                 (stroke-visible? desktop))))

(define bubble-sort-print
  (lambda (ls comparator desktop startx starty w h)
    (sort-print bubble-step
                (lambda (ls c) (is-sorted? (car ls) c)) 
                (list ls) 
                comparator desktop startx starty w h
                (stroke-visible? desktop))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Randomization functions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Make Inkscape's randomization comply with standard Scheme behavior
(define inkscape-random random) ;; off by one
(define good-random 
  (lambda (n)
    (- (inkscape-random n) 1))) ;; correct

;; Returns ls without the element at index INDEX
(define (delete-listref ls index)
  (cond ((null? ls) '())
        ((zero? index) (list-tail ls 1))
        (else (cons (car ls)
                    (delete-listref (cdr ls) (- index 1))))))

;; Returns ls with its elements in random order
(define (randomize-list ls)
  (letrec ((kernel
            (lambda (remain done)
              (if (null? remain)
                  done
                  (let ((rnum (good-random (length remain))))
                    (kernel (delete-listref remain rnum) (cons (list-ref remain rnum) done)))))))
    (kernel ls '())))

;(randomize-list (make-gradient '(0 0 47) '(209 209 237) 20))

;; Random number library
;; (c) John Stone
;; Warning: Currently returns strange values in TinyScheme.  Using a modification
;; of InkscapeScheme's randomization instead.
;(define random-maker
;  (let* ((multiplier 48271)
;         (modulus 2147483647)
;         (apply-congruence
;          (lambda (current-seed)
;            (let ((candidate (modulo (* current-seed multiplier)
;                                     modulus)))
;              (if (zero? candidate)
;                  modulus
;                  candidate))))
;         (coerce
;          (lambda (proposed-seed)
;            (if (integer? proposed-seed)
;                (- modulus (modulo proposed-seed modulus))
;                19860617))))  ;; an arbitrarily chosen birthday
;  (lambda (initial-seed)
;    (let ((seed (coerce initial-seed)))
;      (lambda args
;        (cond ((null? args)
;               (set! seed (apply-congruence seed))
;               (/ (- modulus seed) modulus))
;              ((null? (cdr args))
;               (let* ((proposed-top
;                       (ceiling (abs (car args))))
;                      (exact-top
;                       (if (inexact? proposed-top)
;                           (inexact->exact proposed-top)
;                           proposed-top))
;                      (top
;                       (if (zero? exact-top)
;                           1
;                           exact-top)))
;                 (set! seed (apply-congruence seed))
;                 (inexact->exact (floor (* top (/ seed modulus))))))
;              ((eq? (cadr args) 'reset)
;               (set! seed (coerce (car args))))
;              (else
;               (display "random: unrecognized message")
;               (newline))))))))
;
;(define random
;  (random-maker 19781116))  ;; another arbitrarily chosen birthday


;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Inkscape wrappers ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; Determine if the current desktop has a visible outline set
;; WARNING: I'm not confident this will delete the correct thing always
;; Also, it's a hack.
(define stroke-visible?
  (lambda (desktop)
    (let* ((foo (line desktop "" 0 0 1 0))
          (rval (equal? "1" (node-get-css desktop foo "style" "stroke-opacity"))))
      (begin (select-set! desktop (list foo))
             (selection-delete desktop)
             rval))))

;; Accepts a list of rgb triplets and displays them as a row of rectangles
;; of specified width starting at coordinates startx,starty
(define list-to-rect
  (lambda (ls desktop startx starty widthx height)
    (if (null? ls)
        '()
        (begin
          (desktop-write-css desktop (string-append "stroke-opacity:0;fill-opacity:1;fill:" (rgb->hex (caar ls) (cadar ls) (caddar ls))))
          (cons (rectangle desktop "" startx starty widthx height 0 0)
                (list-to-rect (cdr ls) desktop (- (+ startx widthx) 1) starty widthx height))))))

;; Accepts a list of lists and displays outlines to delineate the boundaries
;; of the lists (according to width and height), starting at coordinates startx,starty
(define list-to-rect-outlines
  (lambda (ls desktop startx starty widthx height)
    (if (null? ls)
        '()
        (begin (desktop-write-css desktop "stroke-opacity:1;fill-opacity:0")
               (cons (rectangle desktop "" startx starty (* (- widthx 1) (length (car ls))) height 0 0)
                     (list-to-rect-outlines (cdr ls) desktop (+ startx (* (- widthx 1) (length (car ls)))) starty widthx height))))))


;; Accepts a high and low rgb value and a number NUM, returns a list of rgb values
;; of length NUM evenly spaced between HIGHRGB and LOWRGB, inclusive
(define make-gradient
  (lambda (lowrgb highrgb num)
    (letrec ((kernel
              (lambda (lowrgb highrgb num)
                (if (zero? num)
                    (list highrgb)
                    (let ((rstep (inexact->exact (round 
                                   (/ (- (car highrgb) (car lowrgb)) num))))
                          (gstep (inexact->exact (round 
                                   (/ (- (cadr highrgb) (cadr lowrgb)) num))))
                          (bstep (inexact->exact (round
                                   (/ (- (caddr highrgb) (caddr lowrgb)) num)))))
                      (cons lowrgb 
                            (kernel (list (+ rstep (car lowrgb))
                                          (+ gstep (cadr lowrgb))
                                          (+ bstep (caddr lowrgb)))
                                    highrgb
                                    (- num 1))))))))
      (kernel lowrgb highrgb (- num 1)))))


;; Comparator for rgb triplets
(define (rgb< rgb1 rgb2)
  (if (< (apply + rgb1) (apply + rgb2))
      #t
      #f))

;; Get the Luminance value for a given rgb triplet
(define rgb->hsl-l
  (lambda (rgb1)
    (let* ((max (lambda (v1 v2)
                  (if (> v1 v2) v1 v2)))
           (min (lambda (v1 v2)
                  (if (< v1 v2) v1 v2))))
      (inexact->exact 
       (round
        (/ (+ (max (car rgb1) (max (cadr rgb1) (caddr rgb1)))
              (min (car rgb1) (min (cadr rgb1) (caddr rgb1)))) 2))))))



;;;;;;;;;;;;;;;;;;
;; Sample calls ;;
;;;;;;;;;;;;;;;;;;

;(define pg (get-desktop))

;(selection-delete-all pg)

;(desktop-set-css pg "opacity:1;fill:#0000b6;fill-opacity:1;stroke:#000000;stroke-opacity:1")
;(define foo (randomize-list (make-gradient '(0 0 47) '(209 209 237) 20)))
;(merge-sort-print foo rgb< pg 100 170 30 50)
;(quick-sort-print foo rgb< pg 100 170 5 12)
;(bubble-sort-print foo rgb< pg 100 170 5 12)
