;; CMPU 101, Fall 2013
;; Assignment 3
;; Due on Oct 2, by 8 pm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require test-engine/racket-tests)

;; NOTE:  This file does not require the use of the tester function.
;;        All printing to the Interactions Window is done using
;;        display, newline, and printf statements.

(display "\n         CS101 Assignment 3, Fall 2013")
(display "\n         Michael Goldman\n\n")


; Before starting to write functions, you should read completely through
; every problem statement. There are two parts to problem 1 and two 
; parts for problem 2.


(display "Problem 1(a): Write a function to add all the even numbers\n")
(display "              from 0 to N (add-evens)\n\n")
; 
; Problem 1(a):
; 
;   Write a recursive function called add-evens that consumes 
;   an EVEN integer n (including 0) and that returns the sum of  
;   all the even numbers between 0 and n, including 0 and n. 
;  
;   Several calls to this function as they would appear in the 
;   interactions window are shown below:
; 
;   > (add-evens 8)
;   20
;   > (add-evens 10)
;   30
;   > (add-evens 0)
;   0 
;  
;   The difference between this part and part b is that this
;   problem assumes you call the function with an even number.
;   In part b, the function may be called with either an odd
;   or even number.
;          

; Contract: (add-evens num) => Even number
; Input:   num => An even number
; Purpose: To take the sum of all even numbers from 0 to input num


; Pre-Function tests:
(check-expect (add-evens 10) 30)
(check-expect (add-evens 0) 0)
(check-expect (add-evens 6) 12)

; Function definition:
(define add-evens
  (lambda (num)
    (if (= num 0) 
        0
        (+ num (add-evens (sub1(sub1 num))))
        )
    )
  )


; Post-function printfs:
(printf "(add-evens 12) => ~a~%" (add-evens 12))
(printf "(add-evens 8) => ~a~%" (add-evens 8))
(printf "(add-evens 4) => ~a~%" (add-evens 4))
(newline) 
(newline)

(display "Problem 1(b): Write a function to add all the even numbers\n") 
(display "              from 0 to N (ADD-EVENS-V2)\n\n")
; 
; Problem 1(b):
; 
;   Write a recursive function called add-evens-v2 that consumes 
;   a positive natural number n and returns the sum of all the 
;   even numbers between 0 and n, including 0 and n (if n is even).  
;   If n is odd, your function should call itself with the 
;   argument n - 1.
;   
;   The difference between this function and the one in part (a)
;   is that this function must be able to take either an even
;   or an odd number as the initial value of n.  The return
;   value should be the same.
;  
;   Several calls to this function as they would appear in the 
;   interactions window are shown below:
; 
;   > (add-evens-v2 9)
;   20
;   > (add-evens-v2 11)
;   30
;   > (add-evens-v2 2)
;   2 
;          
;          

; Contract: (add-evens-v2 num) => positive even number
; Input: num => positive number
; Purpose: adds up all the even numbers from 0 to input num


; Pre-Function tests:
(check-expect (add-evens-v2 7) 12)
(check-expect (add-evens-v2 4) 6)
(check-expect (add-evens-v2 11) 30)
(check-expect (add-evens-v2 0) 0)

; Function definition:
(define add-evens-v2
  (lambda (num)
    (cond
      [(= num 0) 0]
      [(> (remainder num 2) 0) (add-evens-v2(sub1 num))] 
      ;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      ;Turns odd number into a one digit lower even number     
      [else (add-evens num)]) ; Calls function from problem 1(a)
    )
  )

; Post-function printfs:
(printf "(add-evens-v2 12) => ~a~%" (add-evens-v2 12))
(printf "(add-evens-v2 13) => ~a~%" (add-evens-v2 13))
(printf "(add-evens-v2 7) => ~a~%" (add-evens-v2 7))
(printf "(add-evens-v2 0) => ~a~%" (add-evens-v2 0))
(newline)
(newline)

(display "Problem 2(a): Write a function to create a string of n stars\n")
; 
; Problem 2(a):
; 
;   Write a RECURSIVE function like the one given in lecture 7, that returns a 
;   string of n asterisks, this time INCLUDING ONE SPACE between each 
;   asterisk, for any positive natural number n. 
;   
;   Examples of running this function at the command line are given
;   below:
;   
;   > (n-stars 9)
;   "* * * * * * * * * "
;   > (n-stars 3)
;   "* * * "
;   > (n-stars 1)
;   "* "
;          
;          

; Contract: (n-stars star) => A string of *s
; Input: star => a positive number
; Purpose: To print out stars up to the number 'star'


; Pre-Function tests (one given for you):
(check-expect (n-stars 10) "* * * * * * * * * * ") 
(check-expect (n-stars 4) "* * * * ")
(check-expect (n-stars 0) "")


; Function definition:
(define n-stars
  (lambda (star)
    (if (= star 0)
        ""
        (string-append "* " (n-stars (sub1 star)))) 
        ;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        ;Combines the *s into a single string and recalls the function
    )
  )



; Post-function printfs (one given for you):
(printf "(n-stars 8) => ~%~a~%" (n-stars 8))
(printf "(n-stars 3) => ~%~a~%" (n-stars 2))
(printf "(n-stars 1) => ~%~a~%" (n-stars 1))

(newline)
(newline)

(display "Problem 2(b): Write a function to PRINT an n X n square of stars\n")
; 
; Problem 2(b):
; 
;   Write a recursive function called n-star-square that consumes 2 
;   positive natural numbers n and m and that returns void. The function 
;   should have the side-effect of printing n rows with m stars in each
;   row, each followed by one space.  This function should use the function 
;   you wrote in 2(a) as a helper to print each of the lines of stars.
;   
;   The arguments to this function should always be the same for both
;   parameters because one is being used as the recursive parameter and
;   the other is being used as an argument to n-stars to print the row 
;   of stars.
;   
;   NOTE: Remember to call printf before making the recursive call. 
;   
;   Examples of running this function at the command line are given
;   below:
;   
;   > (n-star-square 9 9)
;   * * * * * * * * * 
;   * * * * * * * * * 
;   * * * * * * * * * 
;   * * * * * * * * * 
;   * * * * * * * * * 
;   * * * * * * * * * 
;   * * * * * * * * * 
;   * * * * * * * * * 
;   * * * * * * * * * 
; 
;   > (n-star-square 3 3)
;   * * *
;   * * *
;   * * *
;   
;   > (n-star-square 1 1)
;   * 
;   
;   NOTE: IT IS BETTER TO USE A COND STATEMENT THAN AN IF FOR THIS 
;   RECURSIVE FUNCTION.  IN ORDER TO INCLUDE MULTIPLE STATEMENTS IN
;   EITHER THE TRUE OR FALSE PART OF AN IF, YOU NEED TO ENCLOSE THE
;   STATEMENTS IN A (begin    ) SPECIAL FORM.
;          
;          

; Contract: (n-star-square width height) => void
; Input:   width, height => two positve numbers
; Purpose: To create a square (or rectangle of "* "s to the input height and
;          width of input


; Pre-Function tests not possible due to side-effect printing.

; Function definition:
(define n-star-square 
  (lambda (width height)
    (cond
      [(= height 0) (printf "")] ;Width is the constant as height decreases
      [else (printf "~a~%"(n-stars width)) ;Uses previous function for *s
            (n-star-square width (sub1 height))] 
      )
    )
  )

; Post-function printfs (one given for you):
;(The call to n-star-square can't be used as an argument to the printf because
; it has a void return.)
(printf "(n-star-square 8 8) => ~%") 
(n-star-square 8 8)
(printf "(n-star-square 4 4) => ~%")
(n-star-square 4 4)



(newline)
(newline)

(test)


