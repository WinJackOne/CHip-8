;; Tools of the struct 8-bytes and 16-bytes
;; Tools
;;	all-procedure



(require racket/match)



(struct 8-bytes (a b c d  e f g h))
(define 8-zeros (8-bytes 0 0 0 0  0 0 0 0))


(struct 16-bytes (a b))	;; combine two 8-bytes
(define 16-zeros (16-bytes 8-zeros 8-zeros))



(define bytes->number
	(lambda (bytes  type)
		(list->number (bytes->>list bytes type))))

(define bytes->>list
	(lambda (bytes type)	
		(cond 
			[(= type 8)
				(match bytes
					[(8-bytes a b c d  e f g h)
						(list a b c d e f g h)])]
			[(= type 16)
				(match bytes
					[(16-bytes a b)
						(append
							(bytes->>list a 8)
							(bytes->>list b 8))])]
			[else (error "Unknow type" type)])))

(define list->number
	(lambda (lst)
		(string->number 
			(list->string 
				(map number->char lst)) 2)))


(define number->char
	(lambda (num)
		(cond
			[(= num 0) #\0]
			[(= num 1) #\1]
			[else (error "Unknow num" num)])))

(define number->bytes
	(lambda (num type)
		(list->>bytes (number->list num) type)))

(define number->list
	(lambda (num)
		(map char->num 
			(string->list 
				(number->string num 2)))))

(define char->num
	(lambda (ch)
		(cond
			[(equal? ch #\0) 0]
			[(equal? ch #\1) 1]
			[else (error "Unknow char" ch)])))

(define list->>bytes
	(lambda (nums type)
		(let ((new-nums (fix-length nums type)))
			(cond
				[(= type 8) (apply 8-bytes new-nums)]
				[(= type 16)
					(apply 16-bytes
						(list (apply 8-bytes (sublist new-nums 0 7))
							(apply 8-bytes (sublist new-nums 8 15))))]
				[(error "Unknow no type -- LIST->BYTES" type)]))))

(define bytes-or	
	(lambda (a b type) 
		(list->>bytes
			(map 
				(lambda (a1 b1)
					(if (> (+ a1 b1) 0) 1 0))
				(bytes->>list
					a type)
				(bytes->>list 
					b type))
			type)))

(define bytes-and	
	(lambda (a b type) 
		(list->>bytes
			(map 
				(lambda (a1 b1)
					(if (= (+ a1 b1) 2) 1 0))
				(bytes->>list
					a type)
				(bytes->>list 
					b type))
			type)))

(define bytes-xor
	(lambda (a b type) 
		(list->>bytes
			(map 
				(lambda (a1 b1)
					(if (= (+ a1 b1) 1) 1 0))
				(bytes->>list
					a type)
				(bytes->>list 
					b type))
			type)))

(define bytes-left
	(lambda (bytes type)
		(let ((nums (bytes->>list bytes type)))
			(let ((left-nums (append (cdr nums) (list 0))))
				(list->>bytes left-nums type))) ))

(define bytes-right
	(lambda (bytes type)
		(let ((nums (bytes->>list bytes type)))
			(let ((right-nums (cons 0 (sublist nums 0  (- type 2)))))
				(list->>bytes right-nums type))) ))
(define largest-bit
	(lambda (bytes type)
		(car (bytes->>list bytes type)) ))


(define least-bit
	(lambda (bytes type)
		(last (bytes->>list bytes type)) ))

(define fix-length
	(lambda (nums lenth)
		(let ((n-length (length nums)))
			(cond
				[(> n-length lenth)
					(fix-length (cdr nums) lenth)]	
				[(= n-length lenth) 
					nums]
				[else 	(fix-length (cons 0 nums) lenth)]))))

(define sublist
	(lambda (lst begin end)
		(if (= begin 0)
			(if (= end 0)
				(list (car lst))
				(cons (car lst) (sublist (cdr lst) begin (- end 1))))
			(sublist (cdr lst) (- begin 1) (- end 1)))))
