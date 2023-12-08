(define get-data
  (lambda (d)
	(call-with-input-file d
      (lambda (p)
        (define f
          (lambda (x)
            (if (eof-object? x) '()
              (cons x (f (get-line p))))))
        (f (get-line p))))))

(define parse
  (lambda (f)
	(map string->list (get-data f))))

(define rm-alpha
  (lambda (l)
	(cond ((null? l) '())
		  ((char-alphabetic? (car l)) (rm-alpha (cdr l)))
		  (else (cons
				  (car l)
				  (rm-alpha (cdr l)))))))

(define fst-lst
  (lambda (l)
	(cons
	  (car l)
	  (last-pair l))))

(define solution1
  (lambda (f)
	(apply + 
			(map string->number 
				 (map list->string 
					  (map fst-lst 
						   (map rm-alpha 
								(parse f))))))))

(define check
  (lambda (a b c ss)
	(if (= a c) ss
	  (cond
	    ((= b c) (check (+ a 1) (+ a 1) c ss))
	    ((string=? "one" (substring ss a b))
	     (check 0 0 (- c 2)
			    (string-append
				  (substring ss 0 a) "1" (substring ss b c))))
		((string=? "two" (substring ss a b))
	     (check 0 0 (- c 2)
			    (string-append
				  (substring ss 0 a) "2" (substring ss b c))))
		((string=? "three" (substring ss a b))
	     (check 0 0 (- c 4)
			    (string-append
				  (substring ss 0 a) "3" (substring ss b c))))
		((string=? "four" (substring ss a b))
	     (check 0 0 (- c 3)
			    (string-append
				  (substring ss 0 a) "4" (substring ss b c))))
		((string=? "five" (substring ss a b))
	     (check 0 0 (- c 3)
			    (string-append
				  (substring ss 0 a) "5" (substring ss b c))))
		((string=? "six" (substring ss a b))
	     (check 0 0 (- c 2)
			    (string-append
				  (substring ss 0 a) "6" (substring ss b c))))
		((string=? "seven" (substring ss a b))
	     (check 0 0 (- c 4)
			    (string-append
				  (substring ss 0 a) "7" (substring ss b c))))
		((string=? "eight" (substring ss a b))
	     (check 0 0 (- c 4)
			    (string-append
				  (substring ss 0 a) "8" (substring ss b c))))
		((string=? "nine" (substring ss a b))
	     (check 0 0 (- c 3)
			    (string-append
				  (substring ss 0 a) "9" (substring ss b c))))
		(else (check a (+ b 1) c ss))))))

;(check 0 0 (string-length s) s))

(define string-subst
  (lambda (s)
	(substring (check 0 0 (+ (string-length s) 1) (string-append s "z"))
			   0
			   (- (string-length (check 0 0 (+ (string-length s) 1) (string-append s "z"))) 1))))

(define solution2
  (lambda (f)
	(apply +
		   (map string->number
				(map list->string
					 (map fst-lst
						  (map rm-alpha
							   (map string->list
									(map string-subst (get-data f))))))))))

