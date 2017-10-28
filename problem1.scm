; project Euler, problem 1
(define (multiple-3-5 start limit)
  (cond ((>= start limit) 0)
	((or (= (remainder start 3) 0) (= (remainder start 5) 0))
	 (+ start (multiple-3-5 (+ start 1) limit)))
	(else (multiple-3-5 (+ start 1) limit))))
(multiple-3-5 1 1000)
