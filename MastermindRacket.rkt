#lang racket/gui
(require racket/trace)
; To DO:
; 1. Display pattern.
; 2. Input length of pattern.
; 3. Number of colors.

; Welcome to MasterMind.  This version has a graphical output and a textual input.
; The shell was written by Dr. Browning February 10, 2014.
;  There are two functions that the student needs to write to complete this project:
;  First, write the code to determine the correct score for a guess.
;  Second, write the code to create a random pattern for the game.

; Set up the initial frame for the output display

(define frame (new frame%
                  [label "MasterMind"]
                  [width 460]
                  [height 140]))

; Show the available colors in a white band at the top of the display

(define mycolors '("red" "orange" "yellow" "green" "SkyBlue" "blue" "purple" "brown"))

(define (display-colors dc)
  (send dc set-pen "black" 1 'solid )  
  (for ([i (in-range cLength)])          ; display each of the eight colors
    (begin (send dc set-brush (get-item (+ i 1) mycolors) 'solid)
          (send dc draw-ellipse (+ 12.5 (* 15 i)) 5 10 10))))

(define (show-colors)
  (new canvas% [parent frame]  ;create a canvas in the frame
      [min-height 50]
      [paint-callback
       (lambda (canvas ac)
         (send ac set-background  "white") ;sets background to white
         (send ac clear)          ;causes new background color to appear
         (send ac set-scale 3 3)  ;makes the dots bigger by a factor of 3
         (display-colors ac)
         )]))

; Set up the secret code
;;;;;;;;;;;;;;
; Assignment 2 :  Make this a random secret code
;;;;;;;;;;;;;;

; **New Code**

(display "Welcome to MasterMind\n") ; Moved to first display statement

; Prompt for number of colors
(display "Enter the amount of colors. (2-8)\n")
(define cLength (read))


; Random Color Generator 
(define (randColor)
  (let ([x (random cLength)])
    (case x
      ((0) '("red"))
      ((1) '("orange"))
      ((2) '("yellow"))
      ((3) '("green"))
      ((4) '("SkyBlue"))
      ((5) '("blue"))
      ((6) '("purple"))
      ((7) '("brown"))
      )))
(define (randomcode size)
  (if (equal? size 1)
     (randColor)
     (append (randomcode (- size 1)) (randColor) )
     )
  )

; Prompt for length of code
(display "Enter the length of the secret code.\n")
(define pLength (read))
(define pattern (randomcode pLength))


; Helper function.  Returns the ith item from mylist
; NOT ROBUST. This assumes i is less than or equal to length of mylist
(define (get-item i mylist)
  (if (= i 1)
     (car mylist)
     (get-item (- i 1) (cdr mylist))))

; Display the five colors of the guess and then the black and white pegs for the score
(define (display-myline dc roundguess rectscore) 
  (send dc set-pen "black" 1 'solid )   
  (for ([i (in-range pLength)])                        ; display guess
    (begin (send dc set-brush (get-item (+ i 1) roundguess) 'solid)
          (send dc draw-ellipse (+ 5 (* 15 i)) 5 10 10)))
  (send dc set-pen "tan" 1 'solid ) 
  (for ([i (in-range pLength)])                        ; display score
    (begin(send dc set-brush (get-item (+ i 1) rectscore) 'solid)
          (send dc draw-rectangle (+ 95 (* 10 i)) 5 6 9))))

; This is the function that we call to add a line to the display
(define (addaline guessin responsein)
  (new canvas% [parent frame]  ;create a canvas in the frame
      [min-height 50]
      [paint-callback
       (lambda (canvas dc)
         (send dc set-background  "tan") ;sets background to tan
         (send dc clear)          ;causes new background color to appear
         (send dc set-scale 3 3)  ;makes the dots bigger by a factor of 3
         (display-myline dc guessin responsein)
         )]))
(trace addaline)
(send frame show #t)  ; make the graphics visible

; Score a guess 
;;;;;;;;;;;;;;
; Assignment 1:  Return the correct string for a score.
;;;;;;;;;;;;;;
; Return a list of length 5 starting with a "black" entry for
; each time an element of mycode matches the element of myguess in the same position.
; The returned list then contains a "white" entry for each time an element of myguess
; also occurs in mycode (ignoring all items previously matched).
; The returned list then contains "tan" entries as needed to fill out the five entries.

;returns the number of black pegs in their position 
(define (score mycode myguess)
  (if (not(list? myguess))
     (if (equal? myguess mycode)
        '("black")
        '("tan")
        )
         
     (if (empty? myguess) null 
        (append (score (car mycode) (car myguess))
               (score (cdr mycode) (cdr myguess))))
     ))

;returns a list containing all the colors in the code that havent been correctly guessed
(define (remove-black-pegs mycode myguess)
  (if (not(list? myguess)) ;assert its an element
     (if (equal? myguess mycode) ;is myguess a black peg
        null ; return nothing if its a black peg
        (list mycode)) ; return the code color if its not a black peg
     (if (empty? myguess) null
        (append (remove-black-pegs (car mycode) (car myguess))
               (remove-black-pegs (cdr mycode) (cdr myguess))) )
     )
  )
;returns t if number is a member of alist, f otherwise
(define (member number alist)
  ;if list is empty : #f
  (if (null? alist) #f
     ; if the car is the key number: #t
     (if (equal? (car alist) number) #t
        ; else member the rest of the list
        (member number (cdr alist))
        )
     ))
;returns the number of occurences of a target in a list
(define (occurences target alist)
  (if (not(list? alist)) ;alist is an element
     (if (equal? target alist) 1
        0
        )
     ;else it is a list
     (if (empty? alist) 0
        (+ (occurences target (car alist) ) (occurences target (cdr alist) ))
        )
     ))
;(occurences "black" '("black" "black" "tan" "tan" "black"))
; returns the number of white pegs in myguess 
(define (white-pegs myguess mycode score)
  (if (not (list? score) ) ; if its not a list its an element
     (if (equal? score "tan" ) ;not black -- could be white
        ; if myguess is a member of mycode
        (if (member mycode myguess);white peg 
           1
           0
           )
        ; if its not tan return 0
        0
        )
     ;else its a list
     (if (empty? mycode)
        0
        (+  (white-pegs myguess (car mycode) (car score))
          (white-pegs myguess (cdr mycode) (cdr score)))
        )
     )
  )


;returns the list of the correct score
(define (finalscore mycode myguess score)
  ;take the score to find the number of white pegs
  ;count the number of black pegs in score
  ;(occurences "black" score)
  ; make a list - black pegs + white pegs  + tan pegs if length is not 5
  (if (equal? (length
               (append (makelist '("black") (occurences "black" score)) (makelist '("white") (white-pegs (remove-black-pegs mycode myguess) myguess score))))
             pLength
             ) (append (makelist '("black") (occurences "black" score)) (makelist '("white") (white-pegs (remove-black-pegs mycode myguess)  myguess score))) ; return the list of black and white pegs
               ;else fill the rest of the list with tan pegs
               (append
                (append (makelist '("black") (occurences "black" score)) (makelist '("white") (white-pegs (remove-black-pegs mycode myguess)  myguess score)))
                (makelist '("tan") (- pLength (length (append (makelist '("black") (occurences "black" score)) (makelist '("white") (white-pegs (remove-black-pegs mycode myguess)  myguess score))))))
                )
               )
  )


;returns a list where peg is the element and multiplicity is the length
(define (makelist peg multiplicity)
  (if (not(equal? multiplicity 0)) ; multiplicity is not 0 means we must add another peg to the list
     ;else we must add more pegs to the list
     (append peg (makelist peg (- multiplicity 1)))
     null)
  )

;(score '("yellow" "red" "blue" "purple" "pink") '( "red" "yellow" "blue" "purple" "pink"))
;(findwhites '("yellow" "red" "blue" "purple" "pink")  '( "red" "yellow" "blue" "purple" "pink") '("tan" "tan" "black" "black" "black"))

; Display a guess and its score
(define (guess mylist)
  (addaline mylist (finalscore pattern mylist (score pattern mylist))))

; Sample calls 
(show-colors)

;(addaline '("green" "purple" "SkyBlue" "brown" "red") '("black" "black" "black" "tan" "tan"))
;(addaline '("red" "brown" "blue" "skyblue" "red") '("black" "white" "tan" "tan" "tan"))
;(addaline '("yellow" "red" "yellow" "blue" "yellow") '("white" "tan" "tan" "tan" "tan"))
;(addaline '("orange" "purple" "green" "yellow" "orange") '("black" "white" "white" "tan" "tan"))
;(addaline '("red" "orange" "yellow" "green" "skyblue") '("black" "black" "black" "black" "black"))


;Display the chosen length and colors.
(display "Guess a code of length ")
(display pLength)
(display " using these colors: ")

;There might be an easier way to print the correct number of colors.
;But this way is simple and clean. 
(cond
  [(equal? cLength 2)(display "red orange")]
  [(equal? cLength 3)(display "red orange yellow")]
  [(equal? cLength 4)(display "red orange yellow green" )]
  [(equal? cLength 5)(display "red orange yellow green blue")]
  [(equal? cLength 6)(display "red orange yellow green blue SkyBlue")]
  [(equal? cLength 7)(display "red orange yellow green blue SkyBlue purple" )]
  [(equal? cLength 8)(display "red orange yellow green blue SkyBlue purple brown")]
  [else (display "Error. Must be between (2-8)")]
  )
  (display "\n")

(display "Here is the format of a sample guess:\n")
(display "(guess '(\"yellow\" \"red\" \"yellow\" \"blue\" \"yellow\"))")

;Show the solution (for testing)

(display "\n")
(display pattern)

;(score '("yellow" "red" "blue" "yellow" "red") '("yellow" "red" "blue" "yellow" "red"))
;(score '("yellow" "red" "blue" "yellow" "red") '("yellow" "red" "red" "red" "red"))
;(makelist '("black") 3)
;(finalscore '("yellow" "red" "blue" "purple" "pink")  '( "red" "yellow" "blue" "purple" "pink") '("tan" "tan" "black" "black" "black"))
;(finalscore '("yellow" "red" "blue" "purple" "pink")  '( "red" "orange" "blue" "purple" "pink") '("tan" "tan" "black" "black" "black"))