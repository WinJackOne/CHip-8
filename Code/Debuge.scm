;; debuge : display "instruct and register" such as
;; (debuge a b c d 
;;	(bytes->number (register-value 'PC) 16)
;;	(bytes->number (register-value 'I) 16)
;;	(bytes->number (register-value 0) 8)
;;	(bytes->number (register-value 1) 8))

(define debuge
	(lambda (a b c d  sp pc i v0 v1)
		(display "PC: ")
		(display pc)
		(newline)

		(display "SP: ")
		(display sp)
		(newline)
		
		(display "I: ")
		(display i)
		(newline)
		
		(display "V0: ")
		(display v0)
		(newline)
		
		(display "v1: ")
		(display v1)
		(newline)
		
		(display "Ins: ")
		(display a)
		(display "  ")
		(display b)
		(display "  ")
		(display c)
		(display "  ")
		(display d)
		(display "  ")
		
		(newline)
		(display "-------------------")))


(define stop-list '( ))

(define add-stop
	(lambda (pc) 	;; int
		(set! stop-list
			(cons pc stop-list))))

(define delete-stop	
	(lambda (pc)
		(remove pc stop-list)))

(define clear-stops
	(lambda ( )
		(set! stop-list '( ))))

(define stop?
	(lambda (n)
		(have? n stop-list)))
(define have?
	(lambda (n lst)
		(if (null? lst) #f
			(or
				(equal? n (car lst))
				(have? n (cdr lst)))) ))

(define CPU-restart
	(lambda ( )
		(CPU-loop)))

(define CPU-next
	(lambda ( )
		(CPU (get-ins))
		(send canvas refresh-now) ))