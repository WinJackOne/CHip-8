;; Emulate of the screen, keyboard
;; Interference 
;; 	(set-pixel x y blcak-bit/white-bit) --> nothing
;;		set the pixel that location is x and y in the screen
;;		by using black-bit or white-bit
;;	(get-pixel x y) --> white-bit/black-bit
;;		return the state of the pixel that location is x and y
;;	(send canvas refresh-now) 
;;		refresh the screen
;;	(keyboard-read num)
;;		return the state of keyboard #t/#f
;;	(key-pressed?)
;;		precidate key whether pressed or not
;;	(pressed-key)
;;		return the number of pressed key



(require racket/gui)
(require htdp/draw)
(require lang/posn)
(require loop)



(define my-canvas%
	(class canvas%
		(super-new)
		(define/override on-char
			(lambda (event)
				(let ([key (send event get-key-code)])
					(keyboard key))))
		(define/override on-paint
			(lambda ( )
				(let ([dc (send this get-dc)])
					(draw-canvas dc)))) ))


(define frame (new frame%
			[label "Computer"]
			[x 60]
			[y 120]
			[height 347]
			[width 644]
			[style (list 'no-resize-border
						'no-system-menu)] ))



(define canvas (new my-canvas%
			[parent frame]
			[paint-callback
				(lambda (canvas dc)
					(draw-canvas dc))] ))


;; screen interference is procedure "(draw-canvas dc)"
;; write content by "draw-canvas"
;; refresh screen by "(send canvas refresh)"

;; the keyboard interference is procedure "(keyboard key)"

(define pixels-width 10)
(define pixels-height 10)
(define black-bit 0)
(define white-bit 1)
(define screen-bits
	(vector 
		(make-vector 64 black-bit)
		(make-vector 64 black-bit)
		(make-vector 64 black-bit)
		(make-vector 64 black-bit)
		(make-vector 64 black-bit)
		(make-vector 64 black-bit)
		(make-vector 64 black-bit)
		(make-vector 64 black-bit)

		(make-vector 64 black-bit)
		(make-vector 64 black-bit)
		(make-vector 64 black-bit)
		(make-vector 64 black-bit)
		(make-vector 64 black-bit)
		(make-vector 64 black-bit)
		(make-vector 64 black-bit)
		(make-vector 64 black-bit)

		(make-vector 64 black-bit)
		(make-vector 64 black-bit)
		(make-vector 64 black-bit)
		(make-vector 64 black-bit)
		(make-vector 64 black-bit)
		(make-vector 64 black-bit)
		(make-vector 64 black-bit)
		(make-vector 64 black-bit)

		(make-vector 64 black-bit)
		(make-vector 64 black-bit)
		(make-vector 64 black-bit)
		(make-vector 64 black-bit)
		(make-vector 64 black-bit)
		(make-vector 64 black-bit)
		(make-vector 64 black-bit)
		(make-vector 64 black-bit)
)) ;; 32 x 64 bit

(define draw-canvas 
	(lambda (dc)
		(send dc set-brush (make-object color% 0 0 0 1.0) 'solid)
		;(send dc draw-rectangle x y width height)
		(loop go ([line 0])
			(cond
				[(< line 32)
					(begin
						(draw-line line 
							(vector-ref screen-bits line) 
							dc)
						(go (+ line 1)))] ))))

;; (define draw-line
;;	(lambda (line dc) ; 0~31
;;		(loop go ([i 0])
;;			(cond
;;				[(< i 8) (begin	
;;						(draw-bytes line i
;;							(RAM-read (+ start-point (* line 8) i))
;;							dc)
;;						(go (+ i 1)))]))))
;;

(define draw-line
	(lambda (line v dc)
		(draw-list 0
				(* line pixels-height)
				(vector->list v)
				dc)))

(define draw-list
	(lambda (x y lst dc)
		(if (= 1 (length lst))
			(cond
				[(= (car lst) black-bit)
					(send dc draw-rectangle 
						x y pixels-width pixels-height) ])
			(begin
				(cond
					[(= (car lst) black-bit)
						(send dc draw-rectangle 
							x y pixels-width pixels-height) ])
				(draw-list (+ x pixels-width) y (cdr lst) dc)))))


(define set-pixel!
	(lambda (x y bit)
		(vector-set! 
			(vector-ref screen-bits y)
			x bit)))

(define get-pixel
	(lambda (x y)
		(vector-ref x 
			(vector-ref screen-bits y))))



(define keyboard-bits
	(make-vector 16 #f ))

(define keyboard-class
	(lambda ( )
		(define before -1)
		(lambda (key)
			(let ((key-num (key->number key)))
				(cond
					[(and (>= key-num 0) (<= key-num 15))
						(vector-set! keyboard-bits key-num #t)
						(set! before key-num)]
					[(= key-num 16)
						(cond
							[(not (= before -1))
								(vector-set! keyboard-bits before #f)
								(set! before -1)])]
					)))
					
))
(define key->number
	(lambda (key)
		(cond 
			[(equal? key #\1) 
				0]
			[(equal? key #\2)
				1]
			[(equal? key #\3)	
				2]
			[(equal? key #\4)
				3]			
			[(equal? key #\Q)
				4]
			[(equal? key #\W)
				5]
			[(equal? key #\E)
			6]
		[(equal? key #\R)
			7]							
		[(equal? key #\A)
			8]
		[(equal? key #\S)
			9]
		[(equal? key #\D)
			10]
		[(equal? key #\F)
			11]
							
		[(equal? key #\Z)
			12]
		[(equal? key #\X)
			13]
		[(equal? key #\C)
			14]
		[(equal? key #\V)
			15]
		[(equal? key 'release) 
			16]
		[else 17])
))

(define keyboard (keyboard-class))

(define keyboard-read
	(lambda (num)
		(vector-ref keyboard-bits num)))

(define key-pressed?
	(lambda ( )
		(loop go ([n 15])
			(if (< n 0)
				#f
				(if (keyboard-read n)
					#t
					(go (- n 1))))) ))

(define pressed-key
	(lambda ( )
		(loop go ([n 15])
			(if (keyboard-read n)
				n
				(go (- n 1)))) ))