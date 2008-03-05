(define pg (get-desktop))

(selection-delete-all pg)

(desktop-set-css pg "opacity:1;fill:#0000b6;fill-opacity:1;stroke:#000000;stroke-opacity:1")

;; Accepts a list of rgb triplets and displays them as a row of rectangles
;; of specified width starting at coordinates startx,starty
(define list-to-rect
  (lambda (ls desktop startx starty widthx height)
    (if (null? ls)
        '()
        (begin (desktop-write-css desktop (string-append "fill:" (rgb->hex (caar ls) (cadar ls) (caddar ls))))
               ;(display (string-append "fill:" (rgb->hex (caar ls) (cadar ls) (caddar ls))))
               (cons (rectangle desktop "" startx starty widthx height 0 0)
                     (list-to-rect (cdr ls) desktop (+ startx widthx) starty widthx height))))))

;(list-to-rect '((100 0 0) (0 0 0) (0 0 80)) pg 100 100 30 30)

;; Accepts a high and low rgb value and a number NUM, returns a list of rgb values
;; of length NUM evenly spaced between HIGHRGB and LOWRGB, inclusive
(define make-gradient
  (lambda (lowrgb highrgb num)
    (letrec ((kernel
              (lambda (lowrgb highrgb num)
                (if (zero? num)
                    (list highrgb)
                    (let ((rstep (round (/ (- (car highrgb) (car lowrgb)) num)))
                          (gstep (round (/ (- (cadr highrgb) (cadr lowrgb)) num)))
                          (bstep (round (/ (- (caddr highrgb) (caddr lowrgb)) num))))
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


;;


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

(define (merge-sort ls comparator)
  (if (null? ls)
      '()
      (letrec ((kernel
                (lambda (ls)
                  (if (equal? (length ls) 1)
                      (car ls)
                      (kernel (merge-step ls comparator))))))
        (kernel (map list ls)))))

(define (merge-sort-print ls comparator desktop startx starty w h)
  (if (null? ls)
      '()
      (letrec ((kernel
                (lambda (ls)
                  (if (equal? (length ls) 1)
                      (list-to-rect (car ls) desktop startx starty w h)
                      (begin
                        (list-to-rect (apply append ls) desktop startx starty w h)
                      (kernel (merge-step ls comparator)))))))
        (kernel (map list ls)))))


(list-to-rect '((100 0 0) (0 0 0) (0 0 80)) pg 100 0 30 30)
(make-gradient '(0 0 0) '(255 255 255) 5)
(list-to-rect '((0 0 0) (85 85 85) (170 170 170) (255 255 255)) pg 100 100 30 30)
;(list-to-rect (make-gradient '(0 0 0) '(255 255 255) 3) pg 100 100 30 30)
;(map (lambda (ls) (apply + ls)) (make-gradient '(0 0 0) '(255 255 255) 3))

(define ls1 '((0 0 0) (28 28 28) (56 56 56) (84 84 84) (112 112 112) (141 141 141) (169 169 169) (198 198 198) (226 226 226) (255 255 255)))
(define ls1 (reverse '((0 0 0) (28 28 28) (56 56 56) (84 84 84) (112 112 112) (141 141 141) (169 169 169) (198 198 198) (226 226 226) (255 255 255))))
(list-to-rect ls1 pg 100 100 30 30)
(define ls1_ (map list ls1))
(display "\n")
;(merge-step ls1_ rgb<)
;(display "\n")
;
;(define ls2 ls1_)
;(list-to-rect (apply append ls2) pg 100 170 30 30)
;
;(define ls2 (merge-step ls2 rgb<))
;(list-to-rect (apply append ls2) pg 100 200 30 30)
;(display "\n1")
;ls2
;(define ls2 (merge-step ls2 rgb<))
;(list-to-rect (apply append ls2) pg 100 230 30 30)
;(display "\n2")
;ls2
;(define ls2 (merge-step ls2 rgb<))
;(list-to-rect (apply append ls2) pg 100 260 30 30)
;(display "\n3")
;ls2
;(define ls2 (merge-step ls2 rgb<))
;(list-to-rect (car ls2) pg 100 290 30 30)
;;(display "\n4")
;;ls2
;;(define ls2 (merge-step ls2 rgb<))
;;(list-to-rect (apply append ls2) pg 100 290 30 30)
;;(display "\n5")
;;ls2