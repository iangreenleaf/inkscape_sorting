;; The first three lines of this file were inserted by DrScheme.
;; They record information about the language level.
#reader(lib "plai-pretty-big-reader.ss" "plai")((modname inkscape_sort) (read-case-sensitive #t) (teachpacks ()))
;Enter Definitions Here
    ;Remove Semicolons below to test the morph function.
(define pg (get-desktop))
    ;(desktop-set-css morphy "opacity:1;fill:#35f563;fill-opacity:0.3;stroke:#0f3a24;stroke-width:1;stroke-linecap:butt;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:0.5")

(selection-delete-all pg)
(desktop-set-css pg "opacity:1;fill:#0000b6;fill-opacity:1;stroke:#000000;stroke-opacity:1")
;(cons (rectangle pg "" 50 50 30 30 0 0) '())
(desktop-write-css pg "fill:#6666b6")
;(rectangle pg "" 80 50 30 30 0 0)
;(string-append "fill:#" (number->string 30))

(define list-to-rect
  (lambda (ls desktop startx starty widthx widthy)
    (if (null? ls)
        '()
        (begin (desktop-write-css desktop (string-append "fill:" (rgb->hex (caar ls) (cadar ls) (caddar ls))))
               ;(display (string-append "fill:" (rgb->hex (caar ls) (cadar ls) (caddar ls))))
               (cons (rectangle desktop "" startx starty widthx widthy 0 0)
                     (list-to-rect (cdr ls) desktop (+ startx widthx) starty widthx widthy))))))

;(list-to-rect '((100 0 0) (0 0 0) (0 0 80)) pg 100 100 30 30)

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


(list-to-rect '((100 0 0) (0 0 0) (0 0 80)) pg 100 0 30 30)
(make-gradient '(0 0 0) '(255 255 255) 5)
(list-to-rect '((0 0 0) (85 85 85) (170 170 170) (255 255 255)) pg 100 100 30 30)
;(list-to-rect (make-gradient '(0 0 0) '(255 255 255) 3) pg 100 100 30 30)
;(map (lambda (ls) (apply + ls)) (make-gradient '(0 0 0) '(255 255 255) 3))
