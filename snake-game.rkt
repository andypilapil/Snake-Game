#lang racket/gui

;------------------------------------------------------------------------------------------------------------------------------
; defining size of grid
(define grid '(15 15))

; defining size of cell
(define cell 50)

; defining initial speed of the snake
(define rate 8)

; defining initial score to 0
(define score 0)

; starts playing the game
(define playing #t)

;------------------------------------------------------------------------------------------------------------------------------

; board function: defines the snake game
(define (Game board)
  (let ([dir (send board get-direction)] [snake (send board get-snake)])
    (send board set-snake (move-snake snake dir))
  )
)

; move-snake function: defines the directions of how the snake will move
(define (move-snake snake dir)
  (cond
    [(equal? dir 'up)
      (check-snake snake (list (caar snake) (if (= 0 (cadar snake)) (- (cadr grid) 1) (- (cadar snake) 1))))
    ]
    [(equal? dir 'down)
      (check-snake snake (list (caar snake) (if (= (- (cadr grid) 1) (cadar snake)) 0 (+ (cadar snake) 1))))
    ]
    [(equal? dir 'left)
      (check-snake snake (list (if (= 0 (caar snake)) (- (car grid) 1) (- (caar snake) 1)) (cadar snake)))
    ]
    [(equal? dir 'right)
      (check-snake snake (list (if (= (- (car grid) 1) (caar snake)) 0 (+ (caar snake) 1)) (cadar snake)))
    ]
  )
)

; check-snake function: checks if the snake encounters itself/food, and will grow itself if it eats the food
(define (check-snake snake piece)
  (if (member piece snake)
    (begin (set! playing #f) (list))
    (if (equal? piece (send board get-food))
      (begin (new-food snake) (append (list piece) snake))
      (append (list piece) (reverse (cdr (reverse snake))))
    )
  )
)

; new-food function: adds score, increases speed, and spawns new food for snake 
(define (new-food snake)
  (set! score (+ score 1))
  (set! rate (* rate 1.01))
  (send loop start (inexact->exact (floor (/ 1000 rate))))
  (let ([x (random (car grid))] [y (random (cadr grid))])
    (if (member (list x y) snake)
      (new-food snake)
      (send board set-food (list x y))
    )
  )
)

;------------------------------------------------------------------------------------------------------------------------------
; canvas function
(define board%
  (class canvas%
    (inherit get-width get-height refresh)

    ; defines starting position and direction of snake and food
    (define direction 'right)
    (define snake (list (list 5 5) (list 5 6)))
    (define food (list 7 7))

    (define/public (set-direction value) (set! direction value))
    (define/public (get-direction) direction)

    (define/public (set-snake value) (set! snake value))
    (define/public (get-snake) snake)

    (define/public (set-food value) (set! food value))
    (define/public (get-food) food)

    (define/override (on-char ke)
      (case (send ke get-key-code)
        [(left right up down)
          (if (or
            [and (equal? direction 'right) (equal? (send ke get-key-code) 'left)]
            [and (equal? direction 'left) (equal? (send ke get-key-code) 'right)]
            [and (equal? direction 'up) (equal? (send ke get-key-code) 'down)]
            [and (equal? direction 'down) (equal? (send ke get-key-code) 'up)]
          )
            (void)
            (set-direction (send ke get-key-code))
          )
          (if playing (refresh) (void))
        ]
      )
    )

    (define/public (step callback)
      (callback)
      (refresh)
    )

    (define/private (paint self dc)
      (if playing
          ; setting the colors of the snake, food, and background
          (begin
          (send dc set-brush "black" 'solid)
          (send dc set-pen "white" 4 'solid)

          (for ([piece snake])
            (let ([x (car piece)] [y (cadr piece)])
              (send dc draw-rectangle (* x cell) (* y cell) cell cell)
            )
          )

          (send dc set-brush "green" 'solid)
          (send dc set-pen "white" 8 'solid)
          (send dc draw-rectangle (* (car food) cell) (* (cadr food) cell) cell cell)

          (let ([text (format "Eat as much green apples as you can!                                                                            Score: ~a" (round  (+ 0 score)))])
            (send dc draw-text text 5 5)
          )
        )
        ; displays score at the end of the game
        (begin
          (let ([text (format "Ouch! Game Over, Score: ~a" (round  (+ 1 score)))])
            (send dc draw-text text 7 7)
          )
        )
      )
    )

    (super-new (paint-callback (lambda (c dc) (paint c dc))))
  )
)

; creating gui window for snake
(define window (new frame%
  [label "Snake Game"]
  [width (* cell (car grid))]
  [height (* cell (cadr grid))]
))
(define board (new board% [parent window]))

; setting up timer
(define loop
  (new timer% [interval (/ 1000 rate)] [notify-callback (lambda ()
    (if playing
      (send board step (lambda ()
        (Game board)
      ))
      (send loop stop)
    )
  )])
)

; displays window
(send window show #t)