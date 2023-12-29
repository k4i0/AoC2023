(define get-data
  (lambda (d)
	(call-with-input-file d
      (lambda (p)
        (define f
          (lambda (x)
            (if (eof-object? x) '()
              (cons x (f (get-line p))))))
        (f (get-line p))))))

(define parse1
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

(define solution
  (lambda (l)
	(apply + 
			(map string->number 
				 (map list->string 
					  (map fst-lst 
						   (map rm-alpha l)))))))

;(solution (parse1 "text1.txt"))

(define substr-subst
  (lambda (s)
    (define check
      (lambda (a b c s)
        (cond
          ((string=? "one" (substring s a b))
           (check 0 0 (- c 2)
                  (string-append (substring s 0 a) "1" (substring s b c))))
		  ((string=? "two" (substring s a b))
           (check 0 0 (- c 2)
                  (string-append (substring s 0 a) "2" (substring s b c))))
		  ((string=? "three" (substring s a b))
           (check 0 0 (- c 4)
                  (string-append (substring s 0 a) "3" (substring s b c))))
		  ((string=? "four" (substring s a b))
		   (check 0 0 (- c 3)
                  (string-append (substring s 0 a) "4" (substring s b c))))
		  ((string=? "five" (substring s a b))
		   (check 0 0 (- c 3)
                  (string-append (substring s 0 a) "5" (substring s b c))))
		  ((string=? "six" (substring s a b))
           (check 0 0 (- c 2)
                  (string-append (substring s 0 a) "6" (substring s b c))))
		  ((string=? "seven" (substring s a b))
           (check 0 0 (- c 4)
                  (string-append (substring s 0 a) "7" (substring s b c))))
		   ((string=? "eight" (substring s a b))
			(check 0 0 (- c 4)
                  (string-append (substring s 0 a) "8" (substring s b c))))
		   ((string=? "nine" (substring s a b))
           (check 0 0 (- c 3)
                  (string-append (substring s 0 a) "9" (substring s b c))))
          (else (if (>= a c) s
                  (if (>= b c) (check (+ a 1) (+ a 1) c s)
                    (check a (+ b 1) c s)))))))
    (check 0 0 (string-length s) s)))


(define parse2
  (lambda (f)
	(map string->list
		 (map substr-subst (get-data f)))))

;(solution (parse2 "test2.txt"))

