;;; =======================================================
;;;  CMPU-101, Fall 2013
;;;  Assignment 9 Part 1 - Implementing the snake game
;;; =======================================================

(display "\n     CS101 Assignment 9 PART I, Fall 2013")
(display "\n       Michael Goldman\n\n")

(require 2htdp/image)
(require 2htdp/universe)
(require test-engine/racket-tests)

; In part I of assignment 9, you will change the state of the 
; world from a single segment to a list of segments. You may
; choose to modify this file or modify your own solution to 
; lab 12.  This will require:
; 
; 1. Creating a list of segs to represent the initial state
;    of the world.
;    
; 2. draw-seg will change to draw-seg-list, a function that
;    places a list of images, one at a time, on the empty-
;    scene.
;    
; 3. steer-seg! or steer-seg will change to steer-seg-list, 
;    a function that changes the direction of the first 
;    segment in the list that is the state of the world.
;    
; 4. off-scene? will change so that it consumes a list of 
;    segments, but only the first seg on the list needs to
;    be checked for being off-scene (because the snake never
;    backs up, right?).
;             
; 5. draw-final will not change except for the call to draw-  
;    seg-list instead of draw-seg.
;    
; 6. the major change in the simulation will be the move-
;    seg (or move-seg!) function.  When you think about moving 
;    the seg-list, remember that you do not need to move every 
;    seg on the list (so no list-recursion is needed). The 
;    illusion of moving can be created by consing a new seg 
;    onto the front of the list of segments and removing the last 
;    segment.  This way, all the segments in between the first 
;    and last appear to move too.
;
; 7. Add constants for (* -1 DIAMETER) and for the text image
;    in the draw-final function as shown in lecture. Add any
;    other constants you deem necessary as you are writing this
;    program.
;
; 8. Remove any code you do not use and provide tests only for
;    the functions you implement (unless they produce only images).
;    You do not need any post-function tests.

;;-------------------> START CONSTANT DEFINITIONS <-------------------;;

;; The following three constants are used to draw segments 
;; for the snake and food respectively
(define RADIUS 10)
(define DOT (circle RADIUS "solid" "red"))
(define FOOD (square RADIUS "solid" "green"))

;; These constants are used when a segment is moving
(define DIAMETER (* RADIUS 2))
(define -DIAMETER (- DIAMETER))

;; side length of empty scene square and background scene
(define SIDE 400)
(define MT 
  (overlay (square SIDE "solid" "black")
           (empty-scene SIDE SIDE)))

;; The number of segment positions that exist in columns and rows
;; of the background scene.
(define NUM (/ SIDE DIAMETER))

;; Clock rate: will change scene every 1/2 second
(define RATE .5)

;End game text
(define LOSE-SELF (text "Snake hit itself! You lose!" 24 "yellow"))
(define LOSE-WALL (text "Snake hit wall! You lose!" 24 "yellow"))

;Tells game whether you lost due to wall or hitting self
(define wall-flag #f)

;;-------------------> END CONSTANT DEFINITIONS <--------------------;;

;;-----------------> START WORLD STATE DEFINITION <-----------------;;


(defstruct <seg> () x y dx dy) ;; Note:  I'm calling the world state seg
;; short for segment

(defstruct <sw> () segs food)
;Create a world state for both segs and food




;;------------------> END WORLD STATE DEFINITION <-------------------;;

;;----------------> START INITIAL WORLD DEFINITION <-----------------;;

;; DEFINE A STRUCT TO REPRESENT THE INITIAL STATE OF THE WORLD:

(define INIT-SEG (list (make-seg 40 60 0 DIAMETER)
                       (make-seg 40 40 0 DIAMETER)
                       (make-seg 40 20 0 DIAMETER)))
;Creates the initial snake segment positions

;; Contract: (generate-food ws) -> coordinates for food
;; Input: ws -> world state
;; Purpose: To randomly generate coordinates for food placement
;; (can't prefunction test since it has randomly generated numbers)

(define generate-food 
  (lambda (ws)
    (define new-seg ;makes new food
      (make-seg (* DIAMETER (add1 (random (sub1 NUM)))) ;Creates the x position
                (* DIAMETER (add1 (random (sub1 NUM)))) ;Creates the y position
                0   
                0)) ;The food doesn't move so both dx and dy are 0
    (if (boolean=? (member new-seg (sw-segs ws)) #f) 
        ;This tests to see if the new food spawned on the snake   
        new-seg
        ;If the member is equal to #f, then it is not part of the list, so spawn
        (generate-food ws))
    ;Or recall the function
    )
  )



(define INIT-FOOD (make-seg 40 280 0 0)) ;The intial spawn for food

(define INIT-SW (make-sw INIT-SEG INIT-FOOD)) ;makes the intial world state

;;---------------->  END INITIAL WORLD DEFINITION  <-----------------;;


;;----------------> START MAIN FUNCTION DEFINITION <-----------------;;


;; Contract: (main) -> world state
;; Input:    none
;; Purpose:  Invoke the big-bang animation engine to start a simulation
;;           of the snake game.

;; Pre-function tests not easily done for images. When you have completed
;; the event-handler function, test main by typing (main) in the interac-
;; tions window.

(define main
  (lambda ()
    (big-bang INIT-SW
              ;; draw-seg will re-draw the scene when the seg moves
              (on-draw draw-seg-list)
              ;; move-seg! will destructively mutate the seg's x,y 
              ;; coordinates every RATE second
              (on-tick move-seg RATE)
              ;; steer-seg! will destructive change the seg's dx and
              ;; dy fields
              (on-key steer-seg-list!)
              ;; end-game? will detect when the segment is off the 
              ;; scene, when the x or y coordinate is 0 or the 
              ;; x or y coordinate is SIDE or if the snake hit itself,
              ;; when the x and y coordinates are equal to another part of
              ;; the snake
              (stop-when end-game? draw-final)
              )))

;;-----------------> END MAIN FUNCTION DEFINITION <------------------;;

;;-------------> START EVENT-HANDLER FUNCTION DEFINITIONS <----------;;


;;;;;;;;;;;;;;;;;; FUNCTION CALLED IN ON-DRAW CLAUSE ;;;;;;;;;;;;;;;;;;;;;;;;;
;;

;; Contract: (same-seg? food-seg slist) -> boolean
;; Input: food-seg slist -> Position and movements of segments and food
;; Purpose: to find the coordinates of the snake head, and see if it matches
;; the positino of the food segments

(check-expect (same-seg? (make-seg 40 40 0 0) (make-seg 40 40 0 0)) #t)
(check-expect (same-seg? (make-seg 40 40 0 0) (make-seg 20 40 0 0)) #f)

(define same-seg?
  (lambda (food-seg slist)
    (define next-seg
      (make-seg (seg-x slist) ;Creates position for current segment
                (seg-y slist)
                (seg-dx slist)
                (seg-dy slist))
      )
    (if (and
         (= (seg-x food-seg)
            (seg-x next-seg))
         (= (seg-y food-seg)
            (seg-y next-seg))) 
        ;If both the food x y coordinates are equal to the snake's
        ;coordinates, then return true, or else false
        #t
        #f)
    )
  )

;; Contract: (draw-seg-list sw) -> image (scene)
;; Input:    sw = world state
;; Purpose:  draws the food and segs at their respective x and y coordinates
;;           on the game screen
;; (No test since it produces images)

(define draw-seg-list
  (lambda (sw)
    (local
      [(define draw-rec ;Creates a local funciton for recursion later
         (lambda (slist)
           (cond
             [(empty? slist) 
              (place-image
               FOOD
               (seg-x (sw-food sw))
               (seg-y (sw-food sw)) MT)]
             ;this creates the food once the slist is empty (a way to spawn it
             ;since slist will always eventually reach the end
             [else
              (place-image
               DOT
               (seg-x (first slist))
               (seg-y (first slist))
               (draw-rec (rest slist)))]
             ;Creates the image image of the snake head and recalls the function
             )
           )
         )]
      (draw-rec (sw-segs sw))
      )
    )
  )



;;;;;;;;;;;;;;;;;; FUNCTION CALLED IN ON-TICK CLAUSE ;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; NOTE: You can make this either a mutating or non-mutating function.
;;       Use standard naming convention for mutating version (!). 
;;       You should delete the starter code for the function you
;;       do not use.

;; Pre-function tests (write at least 2 check-expects):


;; ALTERNATE, NON-MUTATING VERSION:

;; Pre-function tests (write at least 2 check-expects):             

;;Contract: (eating? sw) -> boolean
;;Input: sw -> world state
;;Purpose: To check to see if the snake is eating and to spawn a new head
;; if it is.

(check-expect (eating? (make-sw (list (make-seg 40 40 0 0) 
                                      (make-seg 40 20 0 0))
                                (make-seg 40 40 0 0))) #t)
(check-expect (eating? (make-sw (list (make-seg 20 40 0 0) 
                                      (make-seg 20 20 0 0))
                                (make-seg 40 40 0 0))) #f)

(define eating?
  (lambda (sw)
    (local
      [(define first-seg (first (sw-segs sw))) 
       ;creates a definition for (first (sw-segs sw)) in order to save time
       (define new-seg 
         (make-seg (+ (seg-x first-seg)
                      (seg-dx first-seg))
                   (+ (seg-y first-seg)
                      (seg-dy first-seg))
                   (seg-dx first-seg)
                   (seg-dy first-seg)))]
      ;Creates a helper function that for same-seg?, but 
      ;only takes in one variable. Also makes new head for snake
      (if (same-seg? (sw-food sw) new-seg)
          #t
          #f)
      )
    )
  )

;; Contract: (move-seg ws) -> world state
;; Input:    ws = world state
;; Purpose:  move ws by its current dx and dy values

(check-expect (move-seg (make-sw (list (make-seg 40 40 0 20) 
                                       (make-seg 40 20 0 20))
                                 (make-seg 40 100 0 0)))
              (make-sw (list (make-seg 40 60 0 20)
                             (make-seg 40 40 0 20))
                       (make-seg 40 100 0 0)))
(check-expect (move-seg (make-sw (list (make-seg 80 80 20 0) 
                                       (make-seg 60 80 20 0))
                                 (make-seg 40 100 0 0)))
              (make-sw (list (make-seg 100 80 20 0)
                             (make-seg 80 80 20 0))
                       (make-seg 40 100 0 0)))
(define move-seg
  (lambda (sw)
    ;; return a new ws
    (cond
      [(eating? sw) ;if the snake is eating
       (make-sw
        (cons (make-seg (seg-x (sw-food sw))
                        (seg-y (sw-food sw))
                        (seg-dx (first (sw-segs sw)))
                        (seg-dy (first (sw-segs sw))))
              (sw-segs sw)) ;attach a new head to the snake    
        (generate-food sw))] ;and generate a new piece of food
      [else
       (make-sw (cons (make-seg
                       ;; make a new x field by adding the dx to x
                       (+ (seg-x  (first (sw-segs sw))) 
                          (seg-dx (first (sw-segs sw))))
                       ;; make a new y field by adding the dy to y
                       (+ (seg-y (first (sw-segs sw))) 
                          (seg-dy (first (sw-segs sw))))
                       ;; use the same dx and dy fields in the new sw
                       (seg-dx (first (sw-segs sw)))
                       (seg-dy (first (sw-segs sw))))
                      (remove (last (sw-segs sw)) (sw-segs sw)))
                ;Else move the head over by one by making a new head and 
                ;deleting the tail
                (sw-food sw))] ;keep the food position
      )
    )
  )


;;;;;;;;;;;;;;;;;; FUNCTION CALLED IN ON-KEY CLAUSE ;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; NOTE: You can make this either a mutating or non-mutating function.
;;       Use standard naming convention for mutating version (!). 
;;       You should delete the starter code for the function you
;;       do not use.

;; Change the current dx and dy values as follows:
;;    If the key pressed is "up", change the world state so the
;;    segment moves in the negative y direction.
;;    If the key pressed is "down", change the world state so the
;;    segment moves in the positive y direction.
;;    If the key pressed is "left", change the world state so the
;;    segment moves in the negative x direction.
;;    If the key pressed is "right", change the world state so the
;;    segment moves in the positive x direction.
;;
;;    THE SEGMENT SHOULD MOVE BY DIAMETER PIXELS 

;; Contract: (steer-seg! ws kee) -> world state
;; Input:    ws = world state, kee = string
;; Purpose:  change the current dx and dy values according to arrow key
;;           pressed.
;;
;; Pre-function tests (write at least 4 check-expects):

(check-expect (steer-seg-list! (make-sw (list (make-seg 40 40 20 0) 
                                              (make-seg 40 20 20 0))
                                        (make-seg 40 100 0 0))
                               "up")
              (make-sw (list (make-seg 40 40 0 -20) 
                             (make-seg 40 20 20 0))
                       (make-seg 40 100 0 0)))
(check-expect (steer-seg-list! (make-sw (list (make-seg 40 40 20 0) 
                                              (make-seg 40 20 20 0))
                                        (make-seg 40 100 0 0))
                               "down")
              (make-sw (list (make-seg 40 40 0 20) 
                             (make-seg 40 20 20 0))
                       (make-seg 40 100 0 0)))
(check-expect (steer-seg-list! (make-sw (list (make-seg 40 40 0 20) 
                                              (make-seg 40 20 20 0))
                                        (make-seg 40 100 0 0))
                               "left")
              (make-sw (list (make-seg 40 40 -20 0) 
                             (make-seg 40 20 20 0))
                       (make-seg 40 100 0 0)))
(check-expect (steer-seg-list! (make-sw (list (make-seg 40 40 20 0) 
                                              (make-seg 40 20 20 0))
                                        (make-seg 40 100 0 0))
                               "right")
              (make-sw (list (make-seg 40 40 20 0) 
                             (make-seg 40 20 20 0))
                       (make-seg 40 100 0 0)))

(define steer-seg-list!
  (lambda (ws kee)
    (cond
      ;; if up arrow pressed, change dy to -DIAMETER and dx to 0
      [(string=? kee "up") 
       (if (or (= (seg-dy (first (sw-segs ws))) DIAMETER) 
               (= (seg-dy (first (sw-segs ws))) -DIAMETER))
           ;these if statements are is a little measure to prevent people 
           ;from accidentally hitting the wrong key, making the snake go into 
           ;itself and  automatically losing. It will not prevent if keys are 
           ;spammed. This makes the keys up and down stop working when going 
           ;in a vertical direction
           ws
           (begin (set-seg-dx! (first (sw-segs ws)) 0)
                  (set-seg-dy! (first (sw-segs ws)) (* -1 DIAMETER)) 
                  ws))]
      ;; if down arrow pressed, change dy to DIAMETER and dx to 0
      [(string=? kee "down") 
       (if (or (= (seg-dy (first (sw-segs ws))) DIAMETER)
               (= (seg-dy (first (sw-segs ws))) -DIAMETER))
           ws       
           (begin (set-seg-dx! (first (sw-segs ws)) 0)
                  (set-seg-dy! (first (sw-segs ws)) DIAMETER)
                  ws))]
      ;; if left arrow pressed, change dx to -DIAMETER and dy to 0
      [(string=? kee "left") 
       (if (or (= (seg-dx (first (sw-segs ws))) DIAMETER) 
               (= (seg-dx (first (sw-segs ws))) -DIAMETER))
           ;the same preventative measure for the horizontal direction
           ;This makes the keys left and right stop working when going
           ;in a horizontal direction
           ws
           (begin (set-seg-dx! (first (sw-segs ws)) -DIAMETER)
                  (set-seg-dy! (first (sw-segs ws)) 0)
                  ws))]
      ;; if right arrow pressed, change dx to DIAMETER and dy to 0
      [(string=? kee "right") 
       (if (or (= (seg-dx (first (sw-segs ws))) DIAMETER)
               (= (seg-dx (first (sw-segs ws))) -DIAMETER))
           ws
           (begin (set-seg-dx! (first (sw-segs ws)) DIAMETER)
                  (set-seg-dy! (first (sw-segs ws)) 0)
                  ws))]
      )
    )
  )


;;;;;;;;;;;;;;;;;; FUNCTIONS CALLED IN STOP-WHEN CLAUSE ;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Contract: (off-scene? ws) -> boolean
;; Input:    ws = world state
;; Purpose:  stop the simulation if segment is off the scene

;; Write at least 2 check-expect statements:

(check-expect (off-scene? (make-sw (list (make-seg 40 40 20 0) 
                                         (make-seg 40 20 20 0))
                                   (make-seg 40 100 0 0))) #f)
(check-expect (off-scene? (make-sw (list (make-seg 0 40 20 0) 
                                         (make-seg 20 40 20 0))
                                   (make-seg 40 100 0 0))) #t)
(define off-scene?
  (lambda (ws)
    ;; return true if the x coordinate of the seg is 0 or SIDE or
    ;; if the y coordinate of the seg is 0 or SIDE.
    (if (or (= (seg-x (first (sw-segs ws))) 0) 
            (=  (seg-y(first (sw-segs ws))) 0) 
            (= (seg-x (first (sw-segs ws))) SIDE) 
            (= (seg-y (first (sw-segs ws))) SIDE))
        (begin 
          (set! wall-flag #t)
          wall-flag) ;If it is true, set wall-flag to be true and return it
        ;This allows the game to know the player hit the wall to die
        (begin
          (set! wall-flag #f)
          wall-flag)))) ;else keep wall-flag false
;;Contract: (end-game? ws) -> boolean
;;Input: ws -> world state
;;Purpose: To see if either off-scene? or hitself? is true

(check-expect (end-game? (make-sw (list (make-seg 40 40 20 0) 
                                        (make-seg 40 20 20 0))
                                  (make-seg 40 100 0 0))) #f)
(check-expect (end-game? (make-sw (list (make-seg 40 40 20 0) 
                                        (make-seg 40 40 20 0))
                                  (make-seg 40 100 0 0))) #t)
(check-expect (end-game? (make-sw (list (make-seg 40 0 -20 0) 
                                        (make-seg 40 20 -20 0))
                                  (make-seg 40 100 0 0))) #t)

(define end-game?
  (lambda (ws)
    (or (off-scene? ws) (hit-self? ws))
    ;Checks to see if either off-scene? or hit-self? return true
    )
  )
(check-expect (hit-self? (make-sw (list (make-seg 40 40 20 0) 
                                        (make-seg 40 40 20 0))
                                  (make-seg 40 100 0 0))) #t)
(check-expect (hit-self? (make-sw (list (make-seg 40 40 20 0) 
                                        (make-seg 40 20 20 0))
                                  (make-seg 40 100 0 0))) #f)
(define hit-self? 
  (lambda (ws)
    (ormap ;maps the list with an or function to see...
     (lambda (stuff)              
       (and 
        (equal? (seg-x (first (sw-segs ws))) (seg-x stuff))
        (equal? (seg-y (first (sw-segs ws))) (seg-y stuff))
        )
       ;... if both the xy coordinates of the head match any part of the body
       )
     (rest (sw-segs ws))
     ;call to check the rest of the body
     )
    )
  )   



;; Contract: (draw-final ws) -> image (scene)
;; Input:    ws = world state
;; Purpose:  Create the last scene when the snake hits the wall

;; Pre-function tests not easily done for images. Uncomment the call
;; to the stop-when clause in the main function above to test this 
;; function.

(define draw-final
  (lambda (ws)
    ;; center the text image on the scene
    (if (boolean=? wall-flag #t)
        ;If wall-flag is true, the player hit the wall to lsoe
        (place-image
         LOSE-WALL
         (/ SIDE 2)
         (/ SIDE 2)
         ;; overlay text image on call to draw-seg 
         (draw-seg-list ws))
        ;If wall-flag is still false, the player made the snake hit itself
        (place-image
         LOSE-SELF
         (/ SIDE 2)
         (/ SIDE 2)
         ;overlay text image on call to draw-seg
         (draw-seg-list ws))
        )
    )
  )





(newline)
(test)