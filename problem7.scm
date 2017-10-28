(define (prime? n)
  (let ((up-limit (sqrt n)))
    (define (test-loop trial)
      (cond ((> trial up-limit) #t)
	    ((= (remainder n trial) 0) #f)
	    (else 
	     (test-loop (+ trial 1)))))
    (test-loop 2)))


(define (find-nth-prime n start)
  (cond ((= n 0) (- start 2))
	((prime? start)
	 (find-nth-prime (- n 1) (+ start 2)))
	(else
	 (find-nth-prime n (+ start 2)))))

(find-nth-prime 10000 3)
