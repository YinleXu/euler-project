(define (square-sum start end gap)
  (if (> start end)
      0
      (+ (square start)
	 (square-sum (+ start gap) end gap))))

(define (sum-square start end gap)
  (if (< end start)
      (error "sum-square, error: end > start")
      (let ((element-number (+ (quotient (- end start) gap) 1)))
	(let ((r-end (+ start (* gap (- element-number 1)))))
	  (square (/ (* element-number (+ start r-end)) 2))))))

(- (sum-square 1 100 1)
   (square-sum 1 100 1))
