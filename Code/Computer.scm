;; the next time I will add debuge function,
;;	load font
;;	beautify the gui in the computer
;;	add some pl function
;;		such as, add instruct when running
;;			add more register
;;		or change the bottom in the code
;;	I think it's beautiful 


(load "E:\\T\\Bytes-tool.scm")
(load "E:\\T\\Debuge.scm")
(load "E:\\T\\CPU.scm")
(load "E:\\T\\Keyboard&Screen.scm")
(load "E:\\T\\Memory.scm")
(send frame show #t) ;;have some 


(define CHIP-8
	(lambda (filename )
		(CHIP-init filename)
		(CPU-loop)
))

(define CPU-loop
	(lambda ( )
		(loop go ([n 0] ) 
			(let ((pc (bytes->number (register-value 'PC) 16)))
				(if (stop? pc)
					'CPU-break
					(begin
						(CPU (get-ins))
						(send canvas refresh-now)
						(go (+ n 1)))))) ))

(define CHIP-init
	(lambda (filename )
		(load-file filename)
		(set-register! 'PC (number->bytes 512 16))
		(clear-screen)))

;; load file begin 512
(define load-file
	(lambda (filename)
		(let ((input (open-input-file filename)))
			(loop go ([n 0])
				(let ((read (read-bytes 1 input)))
					(cond
						[(not (equal? read eof))
							(begin
								(let ((ins-num (car (bytes->list read))))
									(RAM-write (number->bytes ins-num 8)
										(+ 512 n))) 
								(go (+ n 1)))]))))))
					
(define get-ins
	(lambda ( )
		(let ((pc (bytes->number 
				(register-value 'PC)
				16)))
			(let ((h-list (bytes->>list 
					(RAM-read pc) 8))
				(l-list (bytes->>list
					(RAM-read (+ pc 1)) 8)))
				(let ((a (list->number (sublist h-list 0 3)))
					(b (list->number (sublist h-list 4 7)))
					(c (list->number (sublist l-list 0 3)))
					(d (list->number (sublist l-list 4 7))))
			(Ins a b c d))) )))
