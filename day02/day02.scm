(define get-data
  (lambda (d)
    (call-with-input-file d
      (lambda (p)
        (define f
          (lambda (x)
            (if (eof-object? x) '()
              (cons x (f (get-line p))))))
        (f (get-line p))))))

;remove multiple members curried to use with map
(define multirember
  (lambda (a)
    (lambda (l)
      (cond
        ((null? l) '())
        ((eq? (car l) a)
         ((multirember a) (cdr l)))
        (else (cons (car l)
                    ((multirember a)
                     (cdr l))))))))

(define rm-pontuation
  (lambda (n l)
    (cond
      ((= n 0) (rm-pontuation (+ n 1) (map (multirember #\,) l)))
      ((= n 1) (rm-pontuation (+ n 1) (map (multirember #\:) l)))
      ((= n 2) (rm-pontuation (+ n 1) (map (multirember #\;) l)))
      (else l))))

(define split
  (lambda (a b)
    (lambda (l)
      (if (null? l)
        (reverse (map reverse b))
        (if (equal? a (car l))
        ((split a (cons '() b)) (cdr l))
        ((split a (cons
                    (cons
                      (car l)
                      (car b))
                    (cdr b)))
         (cdr l)))))))

(define change
  (lambda (x)
    (if (string->number x)
      (string->number x)
      x)))

(define part-join
  (lambda (l)
    (define a
      (lambda (l)
        (filter number? l)))
    (define b
      (lambda (l)
        (filter string? l)))
    (list (a l) (b l))))

(define zip
  (lambda (l)
    (define a (car l))
    (define b (cadr l))
    (define join
      (lambda (a b)
        (cond
          ((null? a) '())
          (else (cons
                  (cons (car a)
                        (cons (car b) '()))
                  (join (cdr a) (cdr b)))))))
    (join a b)))

(define parse
  (lambda (f)
    (map zip
         (map part-join
              (map (lambda (l)
                     (map change l))
                   (map (lambda (l)
                          (map list->string l))
                        (map (split #\space '(()))
                             (rm-pontuation 0
                                            (map string->list
                                                 (get-data f))))))))))

(define check
  (lambda (l)
    (cond
      ((null? l) #f)
      ((or (and (< 12 (caar l)) (equal? "red" (cadar l)))
           (and (< 13 (caar l)) (equal? "green" (cadar l)))
           (and (< 14 (caar l)) (equal? "blue" (cadar l)))) #t)
      (else (check (cdr l))))))


				   
(define solution1
  (lambda (l)
    (apply +
           (filter number?
                   (map (lambda (l)
                          (if (check l)
                            '()
                            (caar l)))
                        l)))))

(define check2
  (lambda (r g b l)
    (cond
      ((null? l) (list r g b))
      ((equal? (cadar l) "red")
       (if (> (caar l) r)
         (check2 (caar l) g b (cdr l))
         (check2 r g b (cdr l))))
      ((equal? (cadar l) "green")
       (if (> (caar l) g)
         (check2 r (caar l) b (cdr l))
         (check2 r g b (cdr l))))
      ((equal? (cadar l) "blue")
       (if (> (caar l) b)
         (check2 r g (caar l) (cdr l))
         (check2 r g b (cdr l))))
      (else (check2 r g b (cdr l))))))

(define solution2
  (lambda (l)
    (apply +
           (map (lambda (l)
                  (apply * l))
                (map (lambda (l)
                       (check2 0 0 0 l))
                     l)))))

