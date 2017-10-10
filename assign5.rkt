;;; ========================================
;;;  CMPU-101, Fall 2013
;;;  Assignment 5 starter file
;;; ========================================

(require test-engine/racket-tests)

(display "\n         CS101 Assignment 5, Fall 2013\n")
(display "\n         Michael Goldman\n\n")

(newline)
(newline)

(define tester
  (lambda (expr)
    (printf "~a ==> " expr)
    (eval expr)))


(display "Problem  1(a). DIVIDE-POS-NEG\n\n")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PROBLEM 1(A):
;
; Write a Racket function called "divide-pos-neg". This function will
; consume as input a list of integers called "lst". The function will 
; return a list of two lists. Let the two returned lists be called  
; "pos" and "neg" respectively.  The list neg includes the numbers in 
; lst that are less than 0. The list pos includes the numbers in lst 
; that are greater than 0. Neither list pos nor neg should contain the
; number 0. Each number appearing in the lists neg or pos should appear 
; in the same number of times and in the same order as it occurs in the 
; input lst (except for 0, which should not be output in either list). 
;
; USE THE ACCUMULATOR METHOD IN YOUR SOLUTION FOR PART A WITH DIRECT
; RECURSION.
;
; HINT: Use 2 accumulators, pos and neg, to hold the numbers.
;
;      > (divide-pos-neg '())                         
;      (()())
;
;      > (divide-pos-neg '(0 0 0 0 0))
;      (()())
;
;      > (divide-pos-neg '(1 -2 3 4 5 -6 -7 4 -3 0 8 -9))    
;      ((1 3 4 5 4 8) (-2 -6 -7 -3 -9))
;
;      > (divide-pos-neg '(1 -2 0 -4 5 6 -1 8 6 10))  
;      ((1 5 6 8 6 10) (-2 -4 -1))

;; Contract: (divide-pos-neg '(1st)) => A single nested list of numbers
;; Input: 1st => a list of integers 
;; Purpose: To filter (with recursion) between positive and negative numbers
;           in the input 1st

;; Pre-function tests:
(check-expect (divide-pos-neg '(0 0 0 0)) '(()()))
(check-expect (divide-pos-neg '(1 -2 3 4 5 -6 -7 2)) '((1 3 4 5 2) (-2 -6 -7)))
(check-expect (divide-pos-neg '(-1 -2 -3 1 2 3 -4 4)) 
              '((1 2 3 4) (-1 -2 -3 -4)))
(check-expect (divide-pos-neg '()) '(()()))


;; Function definition:
(define divide-pos-neg
  (lambda (1st)
    (local 
      [(define divide-x
         (lambda (1st pos neg)
           (cond
             ;Base case: List is empty, so take the reverse of both pos and neg
             [(empty? 1st) 
              (list (reverse pos) (reverse neg))]
             ;If there is a 0 in the list, it is ignored
             [(equal? 0 (first 1st)) 
              (divide-x (rest 1st) pos neg)]
             ;If the first part of 1st is positive, combine it with list pos
             [(positive? (first 1st))
              (divide-x (rest 1st) (cons (first 1st) pos) neg)]
             ;Else: combine it with the list neg
             [else (divide-x (rest 1st) pos (cons (first 1st) neg))]
             )
           )
         )
       ]      
      (divide-x 1st () ())
      )
    )
  )
 
 ;; Post-function tests:
(tester '(divide-pos-neg '(0 0 0 0)))
(tester '(divide-pos-neg '(1 -2 3 4 -5 -6 7)))
(tester '(divide-pos-neg '(-3 -4 -5 -20 23 43)))
(tester '(divide-pos-neg '()))
 
(newline)

(display "Problem  1(b). DIVIDE-NEG-POS\n\n")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PROBLEM 1(B):
;
; Write a Racket function called "divide-neg-pos". This function will
; consume the same input as the function in problem 1(a).  However,
; it will swap the order of the pos and neg lists in the output. Any
; 0's that are on the input list will not be included in the output 
; lists.
;
; USE ONLY HIGHER-ORDER FUNCTIONS IN YOUR SOLUTION TO PART B. DO NOT USE 
; RECURSION.
;
;      > (divide-neg-pos '())                         
;      (()())
;
;      > (divide-neg-pos '(0 0 0 0 0))
;      (()())
;
;      > (divide-neg-pos '(1 -2 3 4 2 -6 -1 8 -9))    
;      ((-2 -6 -1 -9) (1 3 4 2 8) )
;
;      > (divide-neg-pos '(1 -2 -4 5 6 -7 1 0 9 10))  
;      ((-2 -4 -7) (1 5 6 1 9 10))

;; Contract: (divide-neg-pos '(1st)) => A single nested list of numbers
;; Input: 1st => a list of integers 
;; Purpose:To filter (with higher order function) between positive and negative
;          numbers in the input 1st 

;; Pre-function tests:
(check-expect (divide-neg-pos '(0 0 0 0)) '(()()))
(check-expect (divide-neg-pos '(1 -2 3 4 5 -6 -7 2)) '((-2 -6 -7) (1 3 4 5 2)))
(check-expect (divide-neg-pos '(-1 -2 -3 1 2 3 -4 4)) 
              '((-1 -2 -3 -4) (1 2 3 4)))
(check-expect (divide-neg-pos '()) '(()()))

;; Function definition:
(define divide-neg-pos
  (lambda (1st)
    (local
      [(define filtration
         (lambda (1st pos neg)
           ;Combine both the negative numbers filtered and the positive ones
           ;filtered in a single list
           (cons (filter negative? 1st) (cons (filter positive? 1st) pos))
           )
         )
       ]
      (filtration 1st () ())
      )
    )
  )

;; Post-function tests:
(tester '(divide-neg-pos '(0 0 0 0)))
(tester '(divide-neg-pos '(1 -2 3 4 -5 -6 7)))
(tester '(divide-neg-pos '(-3 -4 -5 -20 23 43)))
(tester '(divide-neg-pos '()))

(newline)

(display "Problem  2(a). ROTATE-RIGHT\n\n")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PROBLEM 2(A):
;;
;; Write rotate-right, a function that consumes a string str and a
;; number num, and produces a new string by rotating each character
;; in str to the right by num characters, wrapping the characters
;; at the end of the string around to the front.

;; ALGORITHM FOR ROTATE-RIGHT:
;; (define rotate-right
;;    (lambda (str num)
;;       (if (num = 0) 
;;           str
;;           (rotate-right 
;;              (move the last char in str to the front of the string)
;;              (num - 1)))))

;; Examples:
;            > (rotate-right "cat" 0) 
;            "cat" 
;
;            > (rotate-right "cat" 1) 
;            "tca"
;
;            > (rotate-right "cat" 2) 
;            "atc"
;
;            > (rotate-right "cat" 3) 
;            "cat"
;
;            > (rotate-right "megalith" 3) 
;            "ithmegal"
;
;            > (rotate-right "megalith" 5) 
;            "alithmeg"

;; Contract: (rotate-right str num) => A string
;; Input: str => A string
;;        num => A non-negative integer
;; Purpose: To alter the orginal input str, by rotating each character to the
;;          right, wrapping the characters at the end towards the beginning

;; Pre-function tests:
(check-expect (rotate-right "cat" 0) "cat")
(check-expect (rotate-right "cat" 1) "tca")
(check-expect (rotate-right "cat" 2) "atc")
(check-expect (rotate-right "stuff" 4) "tuffs")

;; Function definition:
(define rotate-right
  (lambda (str num)
    (if (equal? num 0) 
        str 
        ;If there is no rotation, return original str
        (rotate-right (string-append 
                       ;Merges the soon to be sub-strings
                       (substring str (- (string-length str) 1)
                                  (string-length str)) 
                       ;Makes the last part of str a substring
                            (substring str 0 (- (string-length str) 1)))
                      ;Takes from the first part to the second to last part
                      ;of str
                      (sub1 num))) ;Counter for num
        )
    )

                        

;; Post-function tests:
(tester '(rotate-right "arizona" 4))
(tester '(rotate-right "tomorrow" 5))
(tester '(rotate-right "tacos" 6))


(newline)

(display "Problem  2(b). ROTATE-LEFT\n\n")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PROBLEM 2(B):
;;
;; Write rotate-left, a function that consumes a string str and a
;; number num, and produces a new string by rotating each character
;; in str to the left by num characters, wrapping the characters at
;; the front of the string around to the rear.

;; ALGORITHM FOR ROTATE-LEFT:
;; (define rotate-left
;;    (lambda (str num)
;;       (if (num = 0) 
;;           str
;;           (rotate-left 
;;              (move the first char in str to the end of the string)
;;              (num - 1)))))

; Examples:
;            > (rotate-left "cat" 0) 
;            "cat" 
;
;            > (rotate-left "cat" 1) 
;            "atc"
;
;            > (rotate-left "cat" 2) 
;            "tca"
;
;            > (rotate-left "cat" 3) 
;            "cat"
;
;            > (rotate-left "megalith" 3) 
;            "alithmeg"

;; Contract: (rotate-left str num) => A string
;; Input: str => A string
;;        num => A non-negative integer
;; Purpose: To alter the orginal input str, by rotating each character to the
;;          left, wrapping the characters at the end towards the end  

;; Pre-function tests:
(check-expect (rotate-left "cat" 0) "cat")
(check-expect (rotate-left "cat" 1) "atc")
(check-expect (rotate-left "cat" 2) "tca")
(check-expect (rotate-left "megalith" 3) "alithmeg")


;; Function definition
(define rotate-left
  (lambda (str num)
    (if (equal? num 0) 
        str
        ;If there is no rotation, return str
        (rotate-left (string-append 
                      ;Merges the soon to be sub-strings 
                      (substring str 1 (string-length str))
                      ;Makes a substring of 2nd to the last part of str
                      (substring str 0 1)) (sub1 num))
                      ;Makes a substring of the 1st part of str
        )
    )
  )
          

;; Post-function tests:
(tester '(rotate-left "washington" 4))
(tester '(rotate-left "yesterday" 5))
(tester '(rotate-left "blueberry" 6))

(newline)

(test)