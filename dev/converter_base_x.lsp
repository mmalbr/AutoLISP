(defun int2base	(n b)
  (if (< n b)
    (chr (+ n
	    (if	(< n 10)
	      48
	      55
	    )
	 )
    )
    (strcat (int2base (/ n b) b) (int2base (rem n b) b))
  )
)

(defun base2int	(n b)
  ((lambda (f)
     (f	(mapcar	'(lambda (x)
		   (- x
		      (if (< x 65)
			48
			55
		      )
		   )
		 )
		(reverse (vl-string->list (strcase n)))
	)
     )
   )
    (lambda (c)
      (if c
	(+ (* b (f (cdr c))) (car c))
	0
      )
    )
  )
)

(defun base2base (n b1 b2)
  (int2base (base2int n b1) b2)
)


(int2base 10 2)
(base2int "1010" 2)
(base2base "1010" 2 10)
