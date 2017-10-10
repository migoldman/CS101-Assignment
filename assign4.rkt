;; CMPU 101, Fall 2013
;; Assignment 4
;; Due on Oct 30, by 8 pm

(define tester
  (λ (expr)
    (printf "~a ==> " expr)
    (eval expr)))

(require test-engine/racket-tests)

(display "\n         CS101 Assignment 4, Fall 2013")
(display "\n         Michael Goldman\n\n")



(display "\n\nProblem 1: ZIP\n\n")



;;   Contract: (zip list-a list-b) => a list of strings
;;   Input: list-a list-b => lists of strings of equal length
;;   Purpose: To produce a single list of strings based alternating from
;;             list-a and list-b
   
;; Pre-function tests:
(check-expect (zip '(1 2 3 4) '(a b c d)) '(1 a 2 b 3 c 4 d))
(check-expect (zip '("oh" "see" "are") '("I" "you" "empty")) 
              '("oh" "I" "see" "you" "are" "empty"))
(check-expect (zip '() '()) '())
(check-expect (zip '(1 3 4) '(1 2 1 2)) "Error: List Lengths Unequal")


;; Function definition:
(define zip
  (lambda (list-a list-b)
    (cond
      [(< (length list-a) (length list-b)) "Error: List Lengths Unequal"]
      [(> (length list-a) (length list-b)) "Error: List Lengths Unequal"]
      [(null? list-a) empty]
      [else (cons (first list-a) 
                  (cons (first list-b)
                        (zip (rest list-a) (rest list-b))))]
      )
    )
  )


;; Post-function tests:
(tester '(zip '(a c b 6) '(1 3 2 f)))
(tester '(zip '() '()))
(tester '(zip '(s) '(b sd)))
(tester '(zip '(s s) '(b)))


(newline)(newline)


(display "\n\nProblem 2: GEN-RAN-LIST\n\n")

;;  Contract: (gen-ran-list n m) => List of numbers
;;  Input: n m => positive natural numbers
;;  Purpose: To generate a random list of n length between 1 and m

;; Pre-function tests to check length of output list:
(check-expect (gen-ran-list 3 1) '(1 1 1))
(check-expect (gen-ran-list 6 1) '(1 1 1 1 1 1))
(check-expect (gen-ran-list 0 52) '())


;; Function definition:
(define gen-ran-list
  (lambda (n m)
    (cond
      [(equal? n 0) ()]
      [else (cons (add1 (random m)) (gen-ran-list (sub1 n) m))]
      )
    )
  )


;; Post-function tests:
(tester '(gen-ran-list 3 10))
(tester '(gen-ran-list 9 99))
(tester '(gen-ran-list 0 99999))

(newline)(newline)

(display "\n\nProblem 3: GEN-RAN-EVENS-LIST\n\n")

;; Contract: (gen-ran-evens-list n m) => List of even numbers
;; Input: n m => positive natural numbers
;; Purpose: To generate a list of random even integers from 2 - m 
;;          (or m-1 if odd)

;; Pre-function tests to check length of output list:
(check-expect (gen-ran-evens-list 4 3) '(2 2 2 2))
(check-expect (gen-ran-evens-list 2 3) '(2 2))
(check-expect (gen-ran-evens-list 0 2352) ())

;; Function definition:
(define gen-ran-evens-list
  (lambda (n m)
    (let ([r (random m)])
      (cond
        [(equal? n 0) ()]   
        [(equal? 0 r) (gen-ran-evens-list n m)]
        [(equal? (remainder r 2) 0) 
            (cons r (gen-ran-evens-list (sub1 n) m))]
        [else (gen-ran-evens-list n m)]
        )
      )
    )
  )
;; Post-function tests:
(tester '(gen-ran-evens-list 9 87))
(tester '(gen-ran-evens-list 6 16))
(tester '(gen-ran-evens-list 0 124))


(newline)(newline)

(display "\n\nProblem 4: NESTED?\n\n")

;; Contract: (nested? listy) => Boolean
;; Input: listy = A list (potentially with lists nested within)    
;; Purpose: To determine if a list has a nested list within it

;; Pre-function tests:
(check-expect (nested? '(1 (2 3) 4)) #t)
(check-expect (nested? '(3 4 3)) #f)
(check-expect (nested? '()) #f)


;; Function definition:
(define nested?
  (lambda (listy) 
    (and (not (empty? listy))
         (or (list? (first listy)) 
             (nested? (rest listy))))
    )
  )
    
;; Post-function tests:
(tester '(nested? '(1 (2 2) (2 3))))
(tester '(nested? '(4 2 2)))
(tester '(nested? empty))

(newline)(newline)

(test)