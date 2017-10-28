; the fibonacci series here starts with 1,2,3,5,8,13...
(define (even-fib-sum up-limit)
  (define (summing term1 term2 sum)
    (let ((term3 (+ term1 term2)))
      (if (<= term3 up-limit)
	  (summing term2 term3 (+ sum term3))
	  (begin
	    (display term3)
	    (display sum)
	    (cond ((even? term3)
		   (/ (- sum term3) 2))
		  ((even? term2)
		   (/ sum 2))
		  (else
		   (/ (- sum term2) 2)))))))
  (summing 1 1 2))

(even-fib-sum 4000000)
