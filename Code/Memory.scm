;; Emulate memory about RAM and registers 
;; Interference
;;	(set-register! name bytes)
;;		register-name x value(8bites/16bites) --> nothing
;;		set a register of a value
;;	(register-value name)
;;		register-name --> 16bites/8bites 
;;		return the value of register by name
;;	(RAM-write bytes index)
;;		8bytes x int --> nothing
;;		set RAM[index] to bytes
;;	(RAM-read index)
;;		return RAM[index]



(require racket/gui/base)
(require racket/class)
(require racket/match)
(require racket/system)


;; ins is a struct defined in the instructe part
(define RAM (make-vector 4096 8-zeros)) ;;	4kb


(define PC 16-zeros)
(define I 16-zeros)
(define Stack (make-vector 16 16-zeros)) ;; a return stack
(define SP 16-zeros)
(define V (make-vector 16 8-zeros))

(define Delay-timer 
	(let ((num 60))
	 (new timer%
		[notify-callback
			(lambda ( )
				(let ((delay-num (bytes->number Delay-timer 8)))
					(cond
						[(> delay-num 0) 
							(set! Delay-timer 
								(number->bytes (- delay-num 1) 8) )]
						))) ]
		[interval 16] )	;; approximate 60Hz
	(number->bytes num 8)) )

(define Sound-timer 		;; havn't sound
	(let ((num 60))
	 (new timer%
		[notify-callback
			(lambda ( )
				(let ((sound-num (bytes->number Sound-timer 8)))
					(cond
						[(> sound-num 0) 
							(set! Sound-timer 
								(number->bytes (- sound-num 1) 8) )]
						))) ]
		[interval 16] )	;; approximate 60Hz
	(number->bytes num 8)) )



;; interferce of registers "PC I SP V0~F Delay-timer"
(define set-register!
	(lambda (name bytes)
		(cond
			[(equal? name 'PC) 
				(set! PC bytes) ]
			[(equal? name 'I) 
				(set! I bytes) ] 
			[(equal? name 'SP) 
				(set! SP bytes) ]

			[(equal? name 0) 
				(vector-set! V 0 
					bytes) ]
			[(equal? name 1) 
				(vector-set! V 1
					bytes) ]
			[(equal? name 2) 
				(vector-set! V 2 
					bytes) ]
			[(equal? name 3) 
				(vector-set! V 3 
					bytes) ]
			[(equal? name 4) 
				(vector-set! V 4 
					bytes) ]
			[(equal? name 5) 
				(vector-set! V 5 
					bytes) ]
			[(equal? name 6) 
				(vector-set! V 6 
					bytes) ]
			[(equal? name 7) 
				(vector-set! V 7
					bytes) ]
			[(equal? name 8) 
				(vector-set! V 8 
					bytes) ]
			[(equal? name 9) 
				(vector-set! V 9 
					bytes) ]
			[(equal? name 10) 
				(vector-set! V 10 
					bytes) ]
			[(equal? name 11) 
				(vector-set! V 11
					bytes) ]
			[(equal? name 12) 
				(vector-set! V 12
					bytes) ]
			[(equal? name 13) 
				(vector-set! V 13
					bytes) ]
			[(equal? name 14) 
				(vector-set! V 14
					bytes) ]
			[(equal? name 15) 
				(vector-set! V 15
					bytes) ]
			[(equal? name 'Delay-timer)
				(set! Delay-timer
					bytes) ]
			[(equal? name 'Sound-timer)
				(set! Sound-timer
					bytes) ]
)))


(define register-value
	(lambda (name)
		(cond
			[(equal? name 'PC) 
				PC ]
			[(equal? name 'I) 
				I ]
			[(equal? name 'SP) 
				SP ]

			[(equal? name 0) 
				(vector-ref V 0) ]
			[(equal? name 1)  
				(vector-ref V 1) ]
			[(equal? name 2)  
				(vector-ref V 2) ]
			[(equal? name 3) 
				(vector-ref V 3) ]
			[(equal? name 4) 
				(vector-ref V 4) ]
			[(equal? name 5) 
				(vector-ref V 5) ]
			[(equal? name 6) 
				(vector-ref V 6) ]
			[(equal? name 7) 
				(vector-ref V 7) ]
			[(equal? name 8) 
				(vector-ref V 8) ]
			[(equal? name 9) 
				(vector-ref V 9) ]
			[(equal? name 10) 
				(vector-ref V 10) ]
			[(equal? name 11) 
				(vector-ref V 11) ]
			[(equal? name 12) 
				(vector-ref V 12) ]
			[(equal? name 13) 
				(vector-ref V 13) ]
			[(equal? name 14) 
				(vector-ref V 14) ]
			[(equal? name 15) 
				(vector-ref V 15) ]
			[(equal? name 'Delay-timer)
				Delay-timer ]
)))



(define RAM-write
	(lambda (bytes index)
		(vector-set! RAM index bytes)))


(define RAM-read
	(lambda (index)
		(vector-ref RAM index)))



(define Stack-pop
	(lambda ( )
		(let ((num (bytes->number 
				(register-value 'SP)
				16)))
			(if (= num 0)
				(error "Stack empty--Stack-pop" num)
				(begin
					(set-register! 'SP 
						(number->bytes (- num 1) 16))
					(vector-ref Stack (- num 1)) ))))) 	;; the stack begin 1~16

		

(define Stack-push
	(lambda (value)
		(let ((num (bytes->number 
				(register-value 'SP)
				16)))
			(if (> num 16)
				(error "Stack overflow--Stack-push" num)
				(begin
					(set-register! 'SP 
						(number->bytes (+ num 1) 16))
					(vector-set! Stack  num value) )))))