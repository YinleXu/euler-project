; though this procedure takes account of number base (as 2nd input argument)
; it would not work properly except for decimal base (base-10)
; to get different base work, algebraic operations used have to adjust
; according to the base input, i.e. generic operators

(define (palindrome? num base)
  (define (reverse-num n result)
    (if (= n 0)
	result
	(reverse-num (quotient n base)
		     (+ (remainder n base) (* result base)))))

  (= num (reverse-num num 0)))

(define (find-max-palindrome factor1 factor2 result)
  (define (factor2-loop c-factor2 c-result)
    (cond ((< c-factor2 100) c-result)
	  ((palindrome? (* factor1 c-factor2) 10)
	   (if (> (* factor1 c-factor2) c-result)
	       (* factor1 c-factor2)
	       result))
	  (else
	   (factor2-loop (- c-factor2 1) result))))

  (if (< (* factor1 factor2) result)
      result
      (find-max-palindrome (- factor1 1) (- factor1 1)
			   (factor2-loop factor2 result))))

(find-max-palindrome 999 999 0)
