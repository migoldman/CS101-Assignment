;;; =======================================================
;;;  CMPU-101, Fall 2013
;;;  Assignment 9 Part 2 - Implementing the snake game
;;; =======================================================

; Due by Wed, Dec 11th with no-cost extension to Fri, Dec 13th 
; if you make a written (e-mail) request to your professor.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; In part II of assignment 9, you will change the state of the 
; world to include a list of segments and a single piece of food.
; As I showed you in class on Wednesday, combining a list of
; segments and a piece of food will probably require a new struct
; definition for the state of the world, with fields for the 
; segment list and the food. 
;
; One way to represent this new world state is with a new 
; data type: (defstruct <sw> () segs food) ; sw for snake world.
; This is only an example of what your snake world may be called,
; choose whatever name you want for the world.
;        
; A snake-world (sw) is a (make-sw INIT-SEG-LIST INIT-FOOD-SEG), 
; where INIT-SEG-LIST is a list of snake segments that fit together
; to form a chain of segments on the scene, and INIT-FOOD-SEG is a 
; seg struct with x and y coordinates that are some multiple of 
; DIAMETER and dx and dy fields are set to 0.
;
; If you start with your program from Part 1 of this assignment,
; you should think about the following points.
; 
; 1. Create a list of segs to represent the snake and a seg
;    to represent the food. Create a new image for the food
;    as a constant.
;    
; 2. Create a struct that combines the snake-list with the 
;    food seg and define a starting state for the world
;    by making a named struct of this type.
;
; 3. Change the function that is used by the on-draw clause
;    to draw the snake, the food, and the current count of 
;    the number of segments on the scene (see #6 below).
;    
; 4. Stop the simulation if the snake hits the wall or if
;    the next move it makes will be to a space occupied by
;    another worm segment. This will probably require changes 
;    in the functions used by the stop-when clause.
;
;    In particular, there will be another condition to be checked
;    in addition to what the off-scene? function already does.
;    I recommend that you create a separate function, possibly
;    called hit-self?, that checks if the next move of the snake will 
;    be to a position that is already occupied by another snake segment. 
;    This includes the situations in which the snake is moving one 
;    direction and the user presses the key to reverse directions. 
;    Either occurrence, off-scene? or hit-self? should cause the game 
;    to end in a loss. 
;
;    You may choose to write the same text for both the off-scene?
;    or hit-self? situations or you may print a different text 
;    image for each situation. The second option is of greater 
;    difficulty.
;    
; 5. One major change in the simulation will be the function used
;    by the on-tick function. This function will need to check
;    if the next move made by the snake will put the head of the
;    snake at the same coordinate as the food.  If so, you should 
;    make the worm grow and generate a new food segment at a 
;    random position whose x coordinate is a multiple of DIAMETER
;    between 20 and 380 and whose y coordinate is a random number 
;    in the same range.  
;
; 6. Include a small text image somewhere near the edge of the
;    scene that gives the number of segments that are currently
;    in the snake. This does not require a change in the world
;    state because it can be implemented inside the function used
;    by the on-draw clause (hint: use the number->string function
;    on the length of the list).
;
; Any updates to this assignment will be posted as they are deemed
; necessary.  You can make this snake game have more "bells and
; whistles" if you want.  Be creative, have fun, and start early!!
