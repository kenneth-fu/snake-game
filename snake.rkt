;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname snake) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; --- Constants ---
(define WIDTH 20)
(define HEIGHT 20)
(define SNAKE-CELL (rectangle 1 1 "solid" "green"))
(define FOOD-CELL (rectangle 1 1 "solid" "red"))
(define ENLARGE-FACTOR 20)

(define MTS (rectangle (* ENLARGE-FACTOR WIDTH) (* ENLARGE-FACTOR HEIGHT) "outline" "black"))

;; --- Data Definitions ---
(define-struct food (x y))
;; Food is (make-food Number Number)
;; interp. the food location is x, y in screen coordindates
(define F1 (make-food 1 1))
(define F2 (make-food 2 2))
(define F13 (make-food 13 10))

;; Direction is one of:
;; - "up"
;; - "down"
;; - "left"
;; - "right"
;; interp. currnet direction of the snake
;; examples are redundant

(define-struct sp (x y))
;; SnakePixcel is x y coordinate a snake pixel
(define SP1  (make-sp 10 10)) ; Y: 10
(define SP2  (make-sp 11 10))
(define SP3  (make-sp 12 10))
(define SP4  (make-sp 13 10))
(define SP5  (make-sp 14 10))
(define SP6  (make-sp 15 10))
(define SP7  (make-sp 16 10))
(define SP8  (make-sp 17 10))
(define SP9  (make-sp 18 10))
(define SP10 (make-sp 19 10)) 
(define SP11 (make-sp 20 10))
(define SP21 (make-sp 10 11)) ; Y: 11
(define SP22 (make-sp 11 11))
(define SP23 (make-sp 12 11))
(define SP24 (make-sp 13 11))
(define SP25 (make-sp 14 11))
(define SP26 (make-sp 15 11))
(define SP27 (make-sp 16 11))
(define SP28 (make-sp 17 11))
(define SP29 (make-sp 18 11))
(define SP20 (make-sp 19 11))

;; Snake is one of:
;; - empty
;; - list of SnakePixel (sp)
(define-struct snake (sps))
;; Snake is (make-snake (listof sp)))
(define S1 (make-snake empty))
(define S2 (make-snake (list SP3 SP2 SP1)))          ; inital snake with length-3, head position coordinate 12, 10
(define S3 (make-snake (list SP4 SP3 SP2)))          ; second state snake with lenght-3, head position at coordinate 13, 10
(define S4 (make-snake (list SP11 SP10 SP9)))        ; snake hit the boundary
(define S5 (make-snake (list SP20 SP29 SP28 SP27 SP6 SP10 SP9 SP8 SP7 SP6)))           ; snake hitting, SP6 pixel showing up twice


(define-struct game (snake food dir))
;; GameState is a (make-game [snake] [food] [dir])
;; interp.
(define GS1 (make-game S2 F1 "right"))  ;; game start with snake in the middle
(define GS2 (make-game S3 F1 "right"))  ;; just snake moving one pixel right
(define GS3 (make-game S3 F13 "right"))  ;; snake moving one pixel right, hitting the food
(define GS4 (make-game S4 F1 "right")) ;; snake hitting the boundary
(define GS6 (make-game S3 F1 "up"))     ;; player changed direction u
(define GS7 (make-game (make-snake (cons (make-sp 14 10) (snake-sps S3))) F1 "right")) ;; after snake getting the food, extending the length by one

;; Functions

;; GameState -> GameState
;; run the game, starting with initial game state gs
;; <no tests for main functions>

(define (main gs)
  (big-bang gs
    (on-tick next-gs 0.1)
    (to-draw render-gs)
    (on-key  change-direction)))

;; GameState -> GameState
;; advance the game by checking if the snake coordinate collide with food, or self, or boundary, then move the snake by one pixel
(check-expect (next-gs GS1) GS2) ; moving right with 1 pixel without hitting anything
(check-expect (next-gs GS3) GS7) ; snake hitting the food and extending by 1 pixel


; (define (next-gs gs) gs) ;stub
(define (next-gs gs)
  (cond [(empty? (game-snake gs)) (make-game empty empty empty)]
        [else
         (if (and (= (sp-x (first (snake-sps (game-snake gs)))) (food-x (game-food gs)))
                  (= (sp-y (first (snake-sps (game-snake gs)))) (food-y (game-food gs))))
             (make-game
              (make-snake 
               (cons (cond [(string=? "right" (game-dir gs))
                            (make-sp (add1 (sp-x (first (snake-sps (game-snake gs)))))
                                     (sp-y (first (snake-sps (game-snake gs)))))]
                           [(string=? "left" (game-dir gs))
                            (make-sp (sub1 (sp-x (first (snake-sps (game-snake gs)))))
                                     (sp-y (first (snake-sps (game-snake gs)))))]
                           [(string=? "down" (game-dir gs))
                            (make-sp (sp-x (first (snake-sps (game-snake gs))))
                                     (add1 (sp-y (first (snake-sps (game-snake gs))))))]
                           [(string=? "up" (game-dir gs))
                            (make-sp (sp-x (first (snake-sps (game-snake gs))))
                                     (sub1 (sp-y (first (snake-sps (game-snake gs))))))])      
                     (snake-sps (game-snake gs))))
              (gen-food (snake-sps (game-snake gs)))
              (game-dir gs))
             (make-game
              (if (snake-hit-self? (first (snake-sps (move-snake (snake-sps (game-snake gs)) (game-dir gs))))
                                   (rest (snake-sps (move-snake (snake-sps (game-snake gs)) (game-dir gs)))))
                  empty
                  (move-snake (snake-sps (game-snake gs)) (game-dir gs)))
              (game-food gs)
              (game-dir gs)))]))



(check-expect (move-snake (list SP4 SP3 SP2) "down") (make-snake (list SP24 SP4 SP3)))


;; Snake-Pixels Direction -> Snake
(define (move-snake sps dir)
  (make-snake 
   (cons (move-head (first sps) dir)
         (move-rest sps))))


;; SnakePixel Direction -> SnakePixel
(check-expect (move-head SP1 "down") SP21)

(define (move-head sp dir)
  (cond [(string=? "right" dir) (make-sp (modulo (add1 (sp-x sp)) WIDTH) (sp-y sp))]
        [(string=? "left" dir) (make-sp  (modulo (sub1 (sp-x sp)) WIDTH) (sp-y sp))]
        [(string=? "down" dir) (make-sp (sp-x sp) (modulo (add1 (sp-y sp)) WIDTH))]
        [(string=? "up" dir) (make-sp (sp-x sp) (modulo (sub1 (sp-y sp)) WIDTH))]
        [else sp]))


; (define (snake-hit-self s) true) ;stub

(define (snake-hit-self? h sps)
  (cond [(empty? sps) false]
        [else
         (if (and (= (sp-x (first sps)) (sp-x h)) (= (sp-y (first sps)) (sp-y h)))
             true
             (snake-hit-self? h (rest sps)))]))


;; ListofSp -> ListofSp
(check-expect (move-rest (list SP4 SP3 SP2)) (list SP4 SP3))


(define (move-rest sps)
  (cond [(or (empty? sps) (empty? (rest sps))) empty]
        [else
         (cons (first  sps)
               (move-rest (rest sps)))]))


;; snake -> Food
;; output new food coordinates that doesnt not collide with any existing snake

;(define (gen-food s) (make-food 10 10)) ;stub


(define (gen-food sps)
  (food-safe-helper sps (make-food (random 20)
                                   (random 20))))

(define (food-safe-helper sps f)
  (cond [(food-safe? sps f) f]
        [else (food-safe-helper sps (make-food (random 20)
                                               (random 20)))]))


;; Snake -> 
;(define (food-safe? s f) true) ;stub

(define (food-safe? sps f)
  (cond [(empty? sps) true]
        [else
         (if (and (= (sp-x (first sps)) (food-x f))
                  (= (sp-y (first sps)) (food-y f)))
             false
             (food-safe? (rest sps) f))])
  )


;(define (food-hit s f) boolean) ;stub

(define (in-snake? sp f)
  (and (= (sp-x sp) (food-x f))
       (= (sp-y sp) (food-y f)))
  )


;; GameState -> Image
;; produce the GameState by drawing snake, food at x, y coordinate
(check-expect (render-gs GS1)
              (place-image/align
               (rectangle ENLARGE-FACTOR ENLARGE-FACTOR "solid" "blue")
               (* ENLARGE-FACTOR (sp-x (first (snake-sps (game-snake GS1))))) (* ENLARGE-FACTOR (sp-y (first (snake-sps (game-snake GS1)))))
               "left" "top"
               (place-image/align
                (rectangle ENLARGE-FACTOR ENLARGE-FACTOR "solid" "blue")
                (* ENLARGE-FACTOR (sp-x (first (rest (snake-sps (game-snake GS1)))))) (* ENLARGE-FACTOR (sp-y (first (rest (snake-sps (game-snake GS1))))))
                "left" "top"
                (place-image/align
                 (rectangle ENLARGE-FACTOR ENLARGE-FACTOR "solid" "blue")
                 (* ENLARGE-FACTOR (sp-x (first (rest (rest (snake-sps (game-snake GS1))))))) (* ENLARGE-FACTOR (sp-y (first (rest (rest (snake-sps (game-snake GS1)))))))
                 "left" "top"
                 (place-image/align
                  (rectangle ENLARGE-FACTOR ENLARGE-FACTOR "solid" "red")
                  (* ENLARGE-FACTOR (food-x (game-food GS1))) (* ENLARGE-FACTOR (food-y (game-food GS1)))
                  "left" "top"
                  MTS)))))

(check-expect (render-gs GS2)
              (place-image/align
               (rectangle ENLARGE-FACTOR ENLARGE-FACTOR "solid" "blue")
               (* ENLARGE-FACTOR (sp-x (first (snake-sps (game-snake GS2))))) (* ENLARGE-FACTOR (sp-y (first (snake-sps (game-snake GS2)))))
               "left" "top"
               (place-image/align
                (rectangle ENLARGE-FACTOR ENLARGE-FACTOR "solid" "blue")
                (* ENLARGE-FACTOR (sp-x (first (rest (snake-sps (game-snake GS2)))))) (* ENLARGE-FACTOR (sp-y (first (rest (snake-sps (game-snake GS2))))))
                "left" "top"
                (place-image/align
                 (rectangle ENLARGE-FACTOR ENLARGE-FACTOR "solid" "blue")
                 (* ENLARGE-FACTOR (sp-x (first (rest (rest (snake-sps (game-snake GS2))))))) (* ENLARGE-FACTOR (sp-y (first (rest (rest (snake-sps (game-snake GS2)))))))
                 "left" "top"
                 (place-image/align
                  (rectangle ENLARGE-FACTOR ENLARGE-FACTOR "solid" "red")
                  (* ENLARGE-FACTOR (food-x (game-food GS2))) (* ENLARGE-FACTOR (food-y (game-food GS2)))
                  "left" "top"
                  MTS)))))

; (define (render-game gs) MTS) ;stub

(define (render-gs gs)
  (cond [(empty? (game-snake gs)) (place-image (text "GAME OVER" 30 "red") 200 200 MTS)]
        [else 
         (render-snake (game-snake gs)
                       (render-food (game-food gs)))]))


;; Snake backgroud -> Image
;; place list of snake pixels on the given background

; (define (render-snake s bg) MTS) ;stub

(define (render-snake s bg)
  (cond [(or (empty? s) (empty? (snake-sps s))) bg]
        [else
         (place-image/align
          (rectangle ENLARGE-FACTOR ENLARGE-FACTOR "solid" "blue")
          (* ENLARGE-FACTOR (sp-x (first (snake-sps s))))
          (* ENLARGE-FACTOR (sp-y (first (snake-sps s))))
          "left" "top"
          (render-snake (make-snake (rest (snake-sps s))) bg))]))


;; Food backgroud -> Image
;; place food on the given background

; (define (render-food f) MTS) ;stub

(define (render-food f)
  (place-image/align
   (rectangle ENLARGE-FACTOR ENLARGE-FACTOR "solid" "red")
   (* ENLARGE-FACTOR (food-x f)) (* ENLARGE-FACTOR (food-y f))
   "left" "top"
   MTS))


;; GameState KeyEvent -> Direction
;; update the direction when "up" "down" "left" "right" is pressed
;(define (change-direction gs ke) gs) ;stub

(define (change-direction gs ke)
  (cond [(empty? (game-snake gs)) gs]
        [(string=? "right" ke) (make-game
                                (game-snake gs)
                                (game-food gs)
                                (if (< (sp-x (first (snake-sps (game-snake gs)))) (sp-x (first (rest (snake-sps (game-snake gs)))))) ;snake moving reverse
                                    (game-dir gs)
                                    "right"))]
        [(string=? "left" ke) (make-game
                               (game-snake gs)
                               (game-food gs)
                               (if (> (sp-x (first (snake-sps (game-snake gs)))) (sp-x (first (rest (snake-sps (game-snake gs)))))) ;snake moving reverse
                                   (game-dir gs)
                                   "left"))]
        [(string=? "down" ke) (make-game
                               (game-snake gs)
                               (game-food gs)
                               (if (< (sp-y (first (snake-sps (game-snake gs)))) (sp-y (first (rest (snake-sps (game-snake gs)))))) ;snake moving reverse
                                   (game-dir gs)
                                   "down"))]
        [(string=? "up" ke) (make-game
                             (game-snake gs)
                             (game-food gs)
                             (if (> (sp-y (first (snake-sps (game-snake gs)))) (sp-y (first (rest (snake-sps (game-snake gs)))))) ;snake moving reverse
                                 (game-dir gs)
                                 "up"))]
        [(string=? "r" ke) (make-game
                            S2
                            F1
                            "right")]
        [else gs])) 


; Initialize game
(main
 (make-game
  S2
  (gen-food (snake-sps S2))
  "right"
  ))


