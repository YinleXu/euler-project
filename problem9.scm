(define (pythagorean? a b c)
  (= (+ (square a) (square b))
     (square c)))

; a set of pythagorean numbers are sides of a right triangle
; so a+b>c
; if a+b = c, max(a^2 + b^2) = c^2, when either a=0 or b-0
; and a^2+b^2 decreases monotonically towards c^2/2 (min value at a=b=c/2)
; so, let a be the adjacent side (longer side), 
; as a->c/2 from (c-1) and b->c/2 from 1, whenever (a^2 + b^2) < c^2
; it means there does not exists such a,b pair that form a set of pythagorean
; numbers as they change further towards c/2

(define (find-sum-1000 hypotenuse)
  (let ((ao-sum (- 1000 hypotenuse))
	(h-square (square hypotenuse)))
    (define (pythagorean-given-h adjacent)
      (let ((a-square (square adjacent))
	    (o-square (square (- ao-sum adjacent))))
	(cond ((< adjacent (/ ao-sum 2)) -1)
	      ((= h-square (+ a-square o-square)) adjacent)
	      ((> h-square (+ a-square o-square)) -1)
	      (else
	       (pythagorean-given-h (- adjacent 1))))))
    
    (if (< hypotenuse 334)
	(error "find-sum-1000: not found, algorithm wrong!")
	(let ((adjacent-r (pythagorean-given-h (- hypotenuse 1))))
	  (if (=  adjacent-r -1)
	      (find-sum-1000 (- hypotenuse 1))
	      (begin
		(display hypotenuse)
		(display "^2 = ")
		(display adjacent-r)
		(display "^2 + ")
		(display (- ao-sum adjacent-r))
		(display "^2")))))))

(find-sum-1000 499)

(* 425 375 200)
