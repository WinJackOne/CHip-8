;; Emulate of CPU
;; Interference
;;	(CPU ins)
;;		ins --> nothing
;;		cpu get the ins which is the struct Ins, and change
;;		the state of memory, or evaluation



(require racket/match)
(require loop)
(require data/queue)


(struct Ins (a b c d))



(define library
	(lambda ()
		(define err (lambda (a b c d) "error in libary"))
		(define pre-dos (make-queue))
		(define change-error 
			(lambda (error)
				(set! err error)
				'ok))
		(define eval
			(lambda (a b c d predos) 		;; a b c d from (Ins a b c d)
				(if (null? predos)
					(err a b c d)
					(let ((pre&do (car predos)))
						(let ((pre (car pre&do))
							(do (cdr pre&do)))
							(if (pre a b c d)
								(do a b c d)
								(eval a b c d (cdr predos))))))))
		(lambda (pr)
			(cond
				[(equal? pr 'change-error) change-error]
				[(equal? pr 'add-new) (lambda (r) (enqueue! pre-dos r))]
				[(equal? pr 'eval) (lambda (a b c d) (eval a b c d (queue->list pre-dos)))]
				[else (error "Unknow predicate--LIBRARY " pr)]))))



(define CPU-library (library))
(define CPU
	(lambda (ins)
		(match ins
			[(Ins a b c d)
				(define eval (CPU-library 'eval))
				(eval a b c d)])))
(define CPU-add
	(lambda (pre do)
		((CPU-library 'add-new) (cons pre do))))
(define CPU-error
	(lambda (a b c d)
		(display "Unknow instruct : ")
		(display (list a b c d))
		(display "---CPU-error")
		(newline) ))


;; 00E0
(define C0?		
	(lambda (a b c d)
;;(display (= a 0)) (display "-") (display (= b 0)) (display "-") (display (= 14 c)) (display "-") (display d)
		(and (= a 0) (= b 0) (= c 14) (= d 0))))
(define C0-do
	(lambda (a b c d)
		(clear-screen)
		(pc-add-two)))

(define clear-screen
	(lambda ( )
		(loop go1 ([x 0])
			(cond
				[(< x 64)
				(begin
					(loop go2 ([y 0])
			  			(cond 
							[(< y 32) 
							(begin
								(set-pixel! x y black-bit)
								(go2 (+ y 1)))] ))
					(go1 (+ x 1)))]))
))


;; 1NNN
(define C1?
	(lambda (a b c d)
		(and (= a 1))))
(define C1-do
	(lambda (a b c d)
		(let ((nums 
			(fix-length (hion (list b c d))
				16)))
			(set-register! 'PC (list->>bytes nums 16))) ))


;; 6XNN
(define C2?
	(lambda (a b c d)
		(and (= a 6))))
(define C2-do
	(lambda (a b c d)
		(let ((vx b)
			(value (fix-length (hion (list c d))
					16)))
			(set-register! vx (list->>bytes value 8)))
		(pc-add-two) ))


;; 7XNN
(define C3?
	(lambda (a b c d)
		(and (= a 7)) ))
(define C3-do
	(lambda (a b c d)
		(let ((v1 (bytes->number 
				(register-value b)
				8))
			(v2 (list->number
				(fix-length (hion (list c d))
					8))))
			(set-register! b (number->bytes (+ v1 v2) 8)))
		(pc-add-two)))


;; ANNN
(define C4?
	(lambda (a b c d)
		(and (= a 10)) ))
(define C4-do
	(lambda (a b c d)
		(let ((nums 
			(fix-length (hion (list b c d))
				16)))
			(set-register! 'I (list->>bytes nums 16)))
		(pc-add-two) ))


;; DXYN
(define C5?
	(lambda (a b c d)
		(and (= a 13)) ))
(define C5-do
	(lambda (a b c d)
		(let ((x (bytes->number (register-value b) 8))
			(y (bytes->number (register-value c) 8)))
			(screen-display x y d))
		(pc-add-two) ))

(define screen-display
	(lambda (x y height)
		(let ((i (bytes->number (register-value 'I) 16)))
			(loop go ([k 0])
				(cond
					[(< k height) 
						(begin
							(screen-display-bytes x (+ y k) 
								(RAM-read (+ i k)))
							(go (+ k 1)))] )))))	
(define screen-display-bytes
	(lambda (x y bytes)
		(let ((bits (bytes->>list bytes 8)))
			(loop go [(bits1 bits) (i 0)]
				(cond
					[(not (null? bits1))
						(begin
							(set-pixel! (+ x i) y (car bits1))
							(go (cdr bits1) (+ i 1)))] )))))


;; 00EE
(define C6?
	(lambda (a b c d)
		(and (= a 0) (= b 0) (= c 14) (= d 14)) ))
(define C6-do
	(lambda (a b c d)
		(let ((parent-pc (Stack-pop)))
			(set-register! 'PC parent-pc)) ))


;; 2NNN
(define C7?
	(lambda (a b c d)
		(and (= a 2)) ))
(define C7-do
	(lambda (a b c d)
		(Stack-push (register-value 'PC))
		(let ((nums
			(fix-length (hion (list b c d))
				16)))
			(set-register! 'PC 
				(list->>bytes nums 16))) ))


;; 3XNN
(define C8?
	(lambda (a b c d)
		(and (= a 3)) ))
(define C8-do
	(lambda (a b c d)
		(let ((nums
			(fix-length (hion (list c d))
				8)))
			(let ((num1 (list->number nums))
				(num2 (bytes->number 
						(register-value b)
						8)))
				(if (= num1 num2)
					(let ((pc (bytes->number 
							(register-value 'PC) 
							16)))
						(set-register! 'PC
							(number->bytes 
								(+ 4 pc) 16)))
					(pc-add-two)))) ))


;; 4XNN
(define C9?
	(lambda (a b c d)
		(and (= a 4)) ))
(define C9-do
	(lambda (a b c d)
		(let ((nums
			(fix-length (hion (list c d))
				8)))
			(let ((num1 (list->number nums))
				(num2 (bytes->number 
						(register-value b)
						8)))
				(if (not (= num1 num2))
					(let ((pc (bytes->number 
							(register-value 'PC)
							16)))
						(set-register! 'PC
							(number->bytes 
								(+ 4 pc) 16)))
					(pc-add-two)))) ))


;; 5XY0
(define C10?
	(lambda (a b c d)
		(and (= a 5) (= d 0)) ))
(define C10-do
	(lambda (a b c d)
		(let ((num1 (bytes->number 
					(register-value b)
					8))
			(num2 (bytes->number 
					(register-value c)
					8)))
			(if (= num1 num2)
				(let ((pc (bytes->number 
						(register-value 'PC)
						16)))
					(set-register! 'PC
						(number->bytes 
							(+ 4 pc) 16)))
				(pc-add-two))) ))


;; 9XY0
(define C11?
	(lambda (a b c d)
		(and (= a 9) (= d 0)) ))
(define C11-do
	(lambda (a b c d)
		(let ((num1 (bytes->number 
					(register-value b)
					8))
			(num2 (bytes->number 
					(register-value c)
					8)))
			(if (not (= num1 num2))
				(let ((pc (bytes->number 
						(register-value 'PC) 
						16)))
					(set-register! 'PC
						(number->bytes 
							(+ 4 pc) 16)))
				(pc-add-two))) ))


;; 8XY0
(define C12?
	(lambda (a b c d)
		(and (= a 8) (= d 0)) ))
(define C12-do
	(lambda (a b c d)
		(set-register! b 
			(register-value c)) 
		(pc-add-two) ))


;; 8XY1
(define C13?
	(lambda (a b c d)
		(and (= a 8) (= d 1)) ))
(define C13-do
	(lambda (a b c d)
		(set-register! b
			(bytes-or 
				(register-value c) 
				(register-value b)
				8)) 
		(pc-add-two) ))


;; 8XY2
(define C14?
	(lambda (a b c d)
		(and (= a 8) (= d 2)) ))
(define C14-do
	(lambda (a b c d)
		(set-register! b
			(bytes-and
				(register-value c) 
				(register-value b)
				8)) 
		(pc-add-two)))


;; 8XY3
(define C15?
	(lambda (a b c d)
		(and (= a 8) (= d 3)) ))
(define C15-do
	(lambda (a b c d)
		(set-register! b
			(bytes-xor
				(register-value c) 
				(register-value b)
				8)) 
		(pc-add-two) ))


;; 8XY4
(define C16?
	(lambda (a b c d)
		(and (= a 8) (= d 4)) ))
(define C16-do
	(lambda (a b c d)
		(let ((numb (bytes->number
				(register-value b)
				8))
			(numc (bytes->number
				(register-value c)
				8)))
			(if (> (+ numc numb) 255)
				(set-register! 15 (number->bytes 1 8))	;; VF set 1
				(set-register! 15 (number->bytes 0 8)) )
			(set-register! b (number->bytes (+ numc numb) 8))) 
		(pc-add-two) ))


;; 8XY5
(define C17?
	(lambda (a b c d)
		(and (= a 8) (= d 5)) ))
(define C17-do
	(lambda (a b c d)
		(let ((numb (bytes->number
				(register-value b)
				8))
			(numc (bytes->number
				(register-value c)
				8)))
			(if (> (- numb numc) 0)
				(begin
					(set-register! 15 (number->bytes 1 8))
					(set-register! b (number->bytes (- numb numc) 8))) 
				(begin	
					(set-register! 15 (number->bytes 0 8))		;; if underflow , VX is set to 0
					(set-register! b 8-zeros)))) 
		(pc-add-two) ))



;; 8XY7
(define C18?
	(lambda (a b c d)
		(and (= a 8) (= d 7)) ))
(define C18-do
	(lambda (a b c d)
		(let ((numb (bytes->number
				(register-value b)
				8))
			(numc (bytes->number
				(register-value c)
				8)))
			(if (> (- numc numb) 0)
				(begin
					(set-register! 15 (number->bytes 1 8))
					(set-register! b (number->bytes (- numb numc) 8))) 
				(begin	
					(set-register! 15 (number->bytes 0 8))		;; if underflow , VX is set to 0
					(set-register! b 8-zeros)))) 
		(pc-add-two) ))


;; 8XY6
(define C19?
	(lambda (a b c d)
		(and (= a 8) (= d 6)) ))
(define C19-do
	(lambda (a b c d)
		(let ((value (register-value c)))
			(let ((l-bit (least-bit value 8))
				(new-value (bytes-right value 8)))
				(set-register! b new-value)
				(set-register! 15 (number->bytes l-bit 8)))) 
			(pc-add-do) ))


;; 8XYE
(define C20?
	(lambda (a b c d)
		(and (= a 8) (= d 14)) ))
(define C20-do
	(lambda (a b c d)
		(let ((value (register-value c)))
			(let ((la-bit (largest-bit value 8))
				(new-value (bytes-left value 8)))
				(set-register! b new-value)
				(set-register! 15 (number->bytes la-bit 8)))) 
			(pc-add-two)))


;; BNNN
(define C21?
	(lambda (a b c d)
		(and (= a 11))))
(define C21-do
	(lambda (a b c d)
		(let ((v0 (bytes->number (register-value 0) 8))
			(basic (list->number 
					(fix-length (hion (list b c d))
						16) )))
			(set-register! 'PC (number->bytes (+ v0 basic) 16))) ))


;; CXNN
(define C22?
	(lambda (a b c d)
		(and (= a 12))))
(define C22-do
	(lambda (a b c d)
		(let ((ran (random 0 255))
			(n (list->number 
				(fix-length (hion (list c d)) 8))))
			(let ((ran-bytes (number->bytes ran 8))
				(n-bytes (number->bytes n 8)))
				(set-register! b (bytes-and ran-bytes n-bytes 8)))) 
		(pc-add-two) ))


;; EX9E
(define C23?
	(lambda (a b c d)
		(and (= a 14) (= c 9) (= d 14))))
(define C23-do
	(lambda (a b c d)
		(if (keyboard-read b)
			(let ((new-pc 
				(+ 4 (bytes->number 
					(register-value 'PC)
					16))))
				(set-register! 'PC 
					(number->bytes 
						new-pc 16)))
			(pc-add-two)) ))


;; EXA1
(define C24?
	(lambda (a b c d)
		(and (= a 14) (= c 10) (= d 1))))
(define C24-do
	(lambda (a b c d)
		(if (not (keyboard-read b))
			(let ((new-pc 
				(+ 4 (bytes->number 
					(register-value 'PC)
					16))))
				(set-register! 'PC 
					(number->bytes 
						new-pc 16)))
			(pc-add-two)) ))


;; FX07
(define C25?
	(lambda (a b c d)
		(and (= a 15) (= c 0) (= d 7))))
(define C25-do
	(lambda (a b c d)
		(set-register! b 
			(register-value 'Delay-timer))
		(pc-add-two) ))


;; FX15
(define C26?
	(lambda (a b c d)
		(and (= a 15) (= c 1) (= d 5))))
(define C26-do
	(lambda (a b c d)
		(set-register! 'Delay-timer
			(register-value b))
		(pc-add-two) ))


;; FX18
(define C27?
	(lambda (a b c d)
		(and (= a 15) (= c 1) (= d 8))))
(define C27-do
	(lambda (a b c d)
		(set-register! 'Sound-timer
			(register-value b))
		(pc-add-two) ))


;; FX1E
(define C28?
	(lambda (a b c d)
		(and (= a 15) (= c 1) (= d 14))))
(define C28-do
	(lambda (a b c d)
		(let ((i (bytes->number (register-value 'I) 16))
			(v (bytes->number (register-value b) 8))
			(max-num 4095))
				(if (> (+ i v) max-num)
					(begin
						(set-register! 'I 
							(number->bytes
								max-num 16))
						(set-register! 15
							(number->bytes 1 8)))
					(begin
						(set-register! 'I 
							(number->bytes
								(+ i v) 16))
						(set-register! 15
							(number->bytes 0 8))) ))
		(pc-add-two) ))


;; FX0A
(define C29?
	(lambda (a b c d)
		(and (= a 15) (= c 0) (= d 10))))
(define C29-do
	(lambda (a b c d)
		(if (key-pressed?)
			(begin
				(set-register! b (number->bytes (pressed-key) 8))
				(pc-add-two) )
			(C29-do a b c d))))


;; FX29
(define C30?
	(lambda (a b c d)
		(and (= a 15) (= c 2) (= d 9))))
(define C30-do
	(lambda (a b c d)
		(let ((num (bytes->number (register-value b) 8)))
			(set-register! 'I
				(number->bytes num 16)))
		(pc-add-two) ))


;; FX33
(define C31?
	(lambda (a b c d)
		(and (= a 15) (= c 3) (= d 3))))
(define C31-do
	(lambda (a b c d)
		(let ((address (bytes->number (register-value 'I) 16))
			(v (bytes->number (register-value b) 8)))
				(RAM-write (number->bytes (first-digit v) 8)
					address) 
				(RAM-write (number->bytes (second-digit v) 8)
					(+ address 1)) 
				(RAM-write (number->bytes (last-digit v) 8)
					(+ address 2))) 
		(pc-add-two) ))
(define first-digit
	(lambda (v)
		(/ (- v (remainder v 100)) 100)))
(define second-digit
	(lambda (v)
		(let ((n (- v (* 100 (first-digit v)))))
			(/ (- n (remainder n 10)) 10))))
(define last-digit
	(lambda (v)
		(- v (* 100 (first-digit v)) 
			(* 10 (second-digit v)))))



;; FX55
(define C32?
	(lambda (a b c d)
		(and (= a 15) (= c 5) (= d 5))))
(define C32-do
	(lambda (a b c d)
		(let ((address (bytes->number (register-value 'I) 16)))
			(loop go [(n 0)]
				(cond
					[(<= n b)
						(begin
							(RAM-write
								(register-value n)
								(+ address n))
							(go (+ n 1)))]))
			(set-register! 'I (number->bytes 
						(+ address 1 b) 16)))
		(pc-add-two) ))


;; FX65
(define C33?
	(lambda (a b c d)
		(and (= a 15) (= c 6) (= d 5))))
(define C33-do
	(lambda (a b c d)
		(let ((address (bytes->number (register-value 'I) 16)))
			(loop go [(n 0)]
				(cond
					[(<= n b)
						(begin
							(set-register! n
								(RAM-read (+ address n)))
							(go (+ n 1)))]))
			(set-register! 'I (number->bytes 
						(+ address 1 b) 16)))
		(pc-add-two) ))



;; help procedure
;; (h '(12 13 14)) --> '(1 1 0 0  1 1 0 1  1 1 1 0)
;; (h '(12 13)) --> '(1 1 0 0  1 1 0 1)
(define hion
	(lambda (lst)
		(apply append
			(map 
				(lambda (ns)
					(fix-length ns 4))
				(map (lambda (n) 
						(map 
							char->num
							(string->list (number->string n 2))
							))
					lst))
)))


(define pc-add-two
	(lambda ( )
		(set-register! 'PC 
			(number->bytes
				(+ 2 (bytes->number 
					(register-value 'PC) 16))
				16))))



(CPU-add C0? C0-do)
(CPU-add C1? C1-do)
(CPU-add C2? C2-do)
(CPU-add C3? C3-do)
(CPU-add C4? C4-do)
(CPU-add C5? C5-do)
(CPU-add C6? C6-do)
(CPU-add C7? C7-do)
(CPU-add C8? C8-do)
(CPU-add C9? C9-do)
(CPU-add C10? C10-do)
(CPU-add C11? C11-do)
(CPU-add C12? C12-do)	
(CPU-add C13? C13-do)	
(CPU-add C14? C14-do)
(CPU-add C15? C15-do)
(CPU-add C16? C16-do)
(CPU-add C17? C17-do)
(CPU-add C18? C18-do)
(CPU-add C19? C19-do)
(CPU-add C20? C20-do)
(CPU-add C21? C21-do)
(CPU-add C22? C22-do)
(CPU-add C23? C23-do)		;; no test
(CPU-add C24? C24-do)		;; no test
(CPU-add C25? C25-do)
(CPU-add C26? C26-do)
(CPU-add C27? C27-do)
(CPU-add C28? C28-do)
(CPU-add C29? C29-do)
(CPU-add C30? C30-do)
(CPU-add C31? C31-do)
(CPU-add C32? C32-do)
(CPU-add C33? C33-do)