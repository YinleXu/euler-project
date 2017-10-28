(define (next-num n)
  (if (even? n)
      (/ n 2)
      (+ (* 3 n) 1)))


(define (find-chain n)
  (define (iter x chain length)
    (if (= x 1)
	(cons length chain)
	(iter (next-num x) (cons x chain) (+ length 1))))
  (iter n '() 0))

(find-chain 13)

