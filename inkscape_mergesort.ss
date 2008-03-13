;; This code is available on the World Wide Web as
;; http://www.cs.grinnell.edu/~youngian/inkscape_mergesort.ss

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mergesort implementation ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; Not actually used
(define (merge-sort ls comparator)
  (if (null? ls)
      '()
      (letrec ((kernel
                (lambda (ls)
                  (if (equal? (length ls) 1)
                      (car ls)
                      (kernel (merge-step ls comparator))))))
        (kernel (map list ls)))))

;; Prints a graphical representation of the sorting process by stepping through
;; the sort, printing the list of items each iteration
(define merge-sort-print
  (lambda (ls comparator desktop startx starty w h)
    (if (null? ls)
        '()
        (letrec ((kernel
                  (lambda (ls startx starty)
                    (if (equal? (length ls) 1)
                        (begin
                          (list-to-rect (car ls) desktop startx starty w h)
                          (list-to-rect-outlines ls desktop startx starty w h))
                        (begin
                          (list-to-rect (apply append ls) desktop startx starty w h)
                          (list-to-rect-outlines ls desktop startx starty w h)
                          (kernel (merge-step ls comparator) startx (+ starty h)))))))
          (kernel (map list ls) startx starty)))))


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



;;;;;;;;;;;;;;;;;;
;; Sample calls ;;
;;;;;;;;;;;;;;;;;;

(define pg (get-desktop))

(selection-delete-all pg)

(desktop-set-css pg "opacity:1;fill:#0000b6;fill-opacity:1;stroke:#000000;stroke-opacity:1")
(merge-sort-print (randomize-list (make-gradient '(0 0 47) '(209 209 237) 20)) rgb< pg 100 170 30 50)