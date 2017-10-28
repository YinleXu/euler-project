(define (prime-sum up-limit)
  (if (= up-limit 3)
      3
      (if (prime? up-limit)
	  (+ up-limit (prime-sum (- up-limit 2)))
	  (prime-sum (- up-limit 2)))))

(+ 2 (prime-sum 99))

(define (last-in-list l)
  (if (null? (cdr l))
      (car l)
      (last-in-list (cdr l))))

(define (prime-generator given-list list-size)
  (let ((current-list given-list)
	(current-size list-size)
	(current-prime (last-in-list given-list)))
    
    (define (no-multiple-of? num check-list)
      (cond ((null? check-list) #t)
	    ((= (remainder num (car check-list)) 0)
	     #f)
	    (else
	     (no-multiple-of? num (cdr check-list)))))
	    
    (define (find-next-prime start)
      (if (no-multiple-of? start current-list)
	  (begin
	    (set! current-list (append current-list (list start)))
	    (set! current-size (+ current-size 1))
	    (set! current-prime start)
	    start)
	  (find-next-prime (+ start 2))))

    (define (print-list)
      (display current-list))

    (define (generate n)
      (if (<= n 0)
	  (display "")
	  (begin
	    (find-next-prime (+ 2 current-prime))
 	    (generate (- n 1)))))

    (lambda (m)
      (cond ((eq? m 'generate) generate)
	    ((eq? m 'print-list) print-list)
	    ((eq? m 'get-list) current-list)
	    ((eq? m 'get-number) current-size)
	    ((eq? m 'get-largest) current-prime)
	    (else
	     "function not found -- PRIME-GENERATE")))))

(define g1 (prime-generator '(2 3) 2))
(g1 'get-list)

; non-tail-recursive
(define (sum-list l)
  (if (null? l)
      0
      (+ (car l)
	 (sum-list (cdr l)))))

; tail-recursive
(define (sum-list-iter l)
  (define (iter l r)
    (if (null? l)
	r
	(iter (cdr l) (+ r (car l)))))

  (iter l 0))


(begin
  ((g1 'generate) 3)
  (sum-list (g1 'get-list)))

(((lambda (f) (f f))
 (lambda (counting)
   (lambda (n)
     (if (= n 0)
	 "finished"
	 ((counting counting) (- n 1))))))
 5)

(((lambda (f) (f f))
 (lambda (find)
   (lambda ()
     (if (>= (car (g1 'get-list)) 2000000)
	 (sum-list (cdr (g1 'get-list)))
	 (begin
	   ((g1 'generate) 1)
	   ((find find))))))))

(define (find-sum)
  (let ((largest-prime (g1 'get-largest)))
    (if (>= largest-prime 2000000)
	(- (sum-list-iter (g1 'get-list)) largest-prime)
	(begin
	  ((g1 'generate) 1)
	  (find-sum)))))

(find-sum)

(car (g1 'get-list))

(define (sum-prime start)
  (if (>= start 2000000)
      0
      (if (prime? start)
	  (+ start (sum-prime (+ start 2)))
	  (sum-prime (+ start 2)))))

(sum-prime 3)

(g1 'get-largest)
(g1 'get-number)
((g1 'generate) 1000)

(- (sum-list-iter (g1 'get-list)) (g1 'get-largest))
