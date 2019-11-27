#lang racket
(require racket/gui
         racket/random
         racket/date)

; frame definitions
(define frame-time 333/1000)
(define tile-size 32)
(define grid-size 10)
(define tile-grid (* tile-size grid-size))
(define x-offset (+ tile-size 32))
(define y-offset (+ tile-size tile-size 32))
(define screensize-x (+ tile-grid x-offset))
(define screensize-y (+ tile-grid y-offset))
(define mainframe (new frame% [label "amazing snake game"][width screensize-x][height screensize-y]))(send mainframe show #t)

; creates then instances the drawing area
(define canvas (new (class canvas% (super-new)
                      (define/override (on-char key-event)  ; callback on key event
                        (define key (send key-event get-key-code))
                        (when (or (equal? key 'up)
                                  (equal? key 'left)
                                  (equal? key 'down)
                                  (equal? key 'right))
                          (set! direction key))
                        (when (equal? key '#\r)(new-game))))
                    [parent mainframe]))

; game values

(define direction 0)
(define food 0)
(define snake 0)
(define score 0)
(define high-score 0)

; game functions and setup

(define (random-pos)(list (random 1 grid-size)(random 1 grid-size))) ; random position
(define (limited-random-pos)(list (+ 5 (random 2))(+ 5 (random 2)))) ; random position in a limited area near the centre

(define (set-direction) ; snake starts in a random direction 
  (set! direction (random-ref '(up left down right))))

(define (set-snake) ; creates the starting position for the snake head and tail
  (define start-pos (limited-random-pos))
  (set! snake (list start-pos (cond ((equal? direction 'up)(list (first start-pos)(+ (second start-pos) 1)))
                                     ((equal? direction 'left)(list (+ (first start-pos) 1)(second start-pos)))
                                     ((equal? direction 'down)(list (first start-pos)(- (second start-pos) 1)))
                                     ((equal? direction 'right)(list (- (first start-pos) 1)(second start-pos))))))) 

(define (set-food) ; generates a random new position for the food
  (set! food (random-pos))
  (for ([x snake]) ; if food is on the snake, generate a new position
    (when (equal? food x)(set-food)))
  (when (or (equal? (first food)(first (first snake))) ; if food is on same x coordinate as the head of the snake, generate a new position
            (equal? (second food)(second (first snake))))  ; if food is on same y coordinate as the head of the snake, generate a new position
    (set-food)))

(define (lose-state)
  (cond
    ((or (not (equal? (length (set->list (list->set snake)))(length snake))) ; if snake goes into itself, you lose
         (> (first (first snake)) 10) ; if x is over 10, you lose
         (< (first (first snake)) 1) ; if x is under 1, you lose
         (> (second (first snake)) 10) ; if y is over 10, you lose
         (< (second (first snake)) 1)) 1) ; if y is under 10, you lose
    (else 0)))

(define move-snake ; adds new position to head and removes last position on tail
  (λ (x y)(set! snake (append (list (list (+ x (first (first snake)))(+ y (second (first snake))))) snake))))

(define (new-game)
  (set-direction)
  (set-snake)
  (set-food)
  (set! score 0))

(define (read-high-score) ; gets the high score value saved in highscore.txt
  (call-with-input-file "highscore.txt"
    (lambda (in) (set! high-score (read in)))))

(define (write-high-score) ; when you get a new high score, writes to highscore.txt
  (with-output-to-file "highscore.txt" #:exists 'replace
    (lambda () (write high-score))))


; colortypes definitions

(define green (make-object color% 0 127 0))
(define grey (make-object color% 236 236 236))
(define dark-grey (make-object color% 100 100 100))


; brushtypes definitions

(define bg-brush (new brush% [style 'solid][color grey]))
(define snake-brush (new brush% [style 'solid][color green]))
(define food-brush (new brush% [style 'solid][color "red"]))


; fonttypes definitions

(define menu-font1 (make-object font% 14 'default))
(define menu-font2 (make-object font% 20 'default))


; drawing function definitions

(define dcdenton (send canvas get-dc)) ; get the canvas's drawing context

(define change-brush ; changes brush type to given 
  (λ (brushtype)(send dcdenton set-brush brushtype)))

(define do-rect ; draws a tile on the grid
  (λ (x y)(send dcdenton draw-rectangle (* tile-size x)(* tile-size y) tile-size tile-size)))

(define (draw-food) ; draws the food
  (change-brush food-brush)
  (do-rect (first food)(second food)))

(define (draw-snake) ; draws the snake
  (change-brush snake-brush)
  (for ([xy snake])
    (do-rect (first xy)(second xy))))

(define (draw-edge) ; draws black edges of the screen
  (change-brush bg-brush)
  (send dcdenton draw-rectangle 0 0 (* tile-size grid-size 2) tile-size)
  (send dcdenton draw-rectangle 0 0 tile-size (* tile-size grid-size 2))
  (send dcdenton draw-rectangle 0 (* 11 tile-size) (* tile-size grid-size 2) tile-size)
  (send dcdenton draw-rectangle (* 11 tile-size) 0 tile-size (* tile-size grid-size 2)))

(define (draw-scores) ; draws score counter at the bottom of the screen
  (send dcdenton set-text-foreground dark-grey)
  (send dcdenton set-font menu-font1)  
  (send dcdenton draw-text (string-append "Score "(number->string score)) 50 5)
  (send dcdenton draw-text (string-append "High score "(number->string high-score)) 250 5))

(define (lose-screen) ; wah waaaah
  (send dcdenton set-text-foreground dark-grey)
  (send dcdenton set-font menu-font2)
  (send dcdenton draw-text (string-append "You lost with "(~a (cond ((zero? score) "no")
                                                                    (else score)))
                                          (~a (cond ((equal? 1 score) " point")
                                                    (else " points"))))(* 2.5 tile-size) 125)
  (send dcdenton draw-text "Press R to restart" (* 3.5 tile-size) 175)
  (draw-edge))

(define (mainmenu-text)
  (send dcdenton set-text-foreground dark-grey)
  (send dcdenton set-font menu-font2)  
  (send dcdenton draw-text "amazing snake game" (* 3.5 tile-size) 150))

(define test? 0)
(define testframes 0)

;(gameprocess)
(define (startgame)
  (send dcdenton set-pen
        (new pen%
             [style 'transparent]))
  (new-game) ; initial game
  (read-high-score)
;  (send menulogo show #f)
  (send startmenu show #f)
  (gameprocess))

(define (gameprocess)
  
  (sleep/yield (- frame-time (/ (expt (cond ; speeds up until score of 10
                                        ((> 10 score)(+ 1 score))
                                        (else 10)) 3) 10000)))
  
  (send dcdenton clear)

  (when (> score high-score)(set! high-score score)(write-high-score))  
  (when (zero? (lose-state))
    (cond ((equal? direction 'up)(move-snake 0 -1))
          ((equal? direction 'left)(move-snake -1 0))
          ((equal? direction 'down)(move-snake 0 1))
          ((equal? direction 'right)(move-snake 1 0)))
    
    (when (equal? food(first snake))(set! score (add1 score))(set-food)) ; when snake is on food, add 1 to score and generate new food position
    
    (when (equal? (length snake)(+ 3 score))(set! snake (reverse (rest (reverse snake))))) ; removes tail and additionally allows snake to grow
    
    ; drawing elements
    (draw-food) 
    (draw-snake)
    (draw-edge) 
    (draw-scores)

    (when (equal? test? 1)
      (set! testframes (add1 testframes))
      (displayln "debug")
      (displayln testframes)
      (displayln direction)
      (displayln high-score)
      (displayln score)
      (displayln snake)
      (displayln food)
      (displayln "debug end"))
      (when (equal? testframes 6)(cond ((and (equal? direction 'right)
                                             (equal? high-score 25)
                                             (equal? score 25)
                                             (not (equal? food '(9 5))))
                                             (displayln "success!"))
                                       (#t (displayln "failure"))))
                                        
    
    
    (gameprocess))
  
  (send dcdenton clear)
  
  (lose-screen)
  (gameprocess))

;(define logobitmap (make-object bitmap% 500 500	"amazingsnakegamelogo.png"))	
  
;(define menulogo
 ; (new message%
 ;      [label logobitmap]
 ;      	[min-width 0]	 
 ;  	 	[min-height 0]	 
 ;  	 	[stretchable-width 0]	 
 ;  	 	[stretchable-height 0]	
 ;      [auto-resize #f]
  ;     [parent mainframe]))

(define panel (new horizontal-panel%
                   	[vert-margin 0]	 
   	 	[horiz-margin 0]
                [alignment '(center bottom)]	 
   	 	[min-width 0]	 
   	 	[min-height 0]
                [border 0]	 
   	 	[spacing 0]
                [stretchable-width #f]
                [stretchable-height #f]
                   [parent mainframe]))


(define (tests)
  (set! test? 1)
  (displayln "starting tests")
    (send dcdenton set-pen
        (new pen%
             [style 'transparent]))
  (new-game) ; initial game
;  (send menulogo show #f)
  (send startmenu show #f)
  (set! direction 'right)
  (set! high-score 23)
  (set! score 24)
  (set! snake (list '( 5 5)'(4 5)))
  (set! food '(9 5))
  (gameprocess))


(define startmenu
  (new button%
       [label "Start Game"]
       [parent panel]       
       [callback (lambda (b e)(startgame))]))

(define starttset
  (new button%
       [label "Start Tests"]
       [parent panel]
       [callback (lambda (b e)(tests))]))
  

      
   	 		 
