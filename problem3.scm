(define (largest-prime-factor num)
  (define (min-factor num start end)
    (cond ((> start end) 1)
	  ((= (remainder num start) 0) start)
	  (else (min-factor num (+ start 1) end))))
	
  (define (contns-factoring crrt-num max-factor)
    (display max-factor)
    (display "   ")
    (display crrt-num)
    (newline)
    (let ((max-psble (sqrt crrt-num)))
      (let ((min-fact (min-factor crrt-num max-factor max-psble)))
	(if (= min-fact 1)
	    crrt-num
	    (contns-factoring (/ crrt-num min-fact) min-fact)))))
  (contns-factoring num 2))

(largest-prime-factor 600851475143)
