(defun add_osmode (n)
  (prompt
    (strcat "\nModo osnap atual: " (rtos (getvar "osmode")))
  )
  (setvar "osmode" (logior (getvar "osmode") n))
  (prompt
    (strcat "\nNovo modo osnap: " (rtos (getvar "osmode")))
  )
  (princ)
)

(defun del_osmode (n)
  (prompt
    (strcat "\nModo osnap atual: " (rtos (getvar "osmode")))
  )
  (setvar "osmode" (boole 2 (getvar "osmode") n))
  (prompt
    (strcat "\nNovo modo osnap: " (rtos (getvar "osmode")))
  )
(princ)
)

(defun is_osmode_set (n)
  (prompt
    (strcat "\nModo osnap atual: " (rtos (getvar "osmode")))
  )
  (if (= n (logand (getvar "osmode") n))
    (prompt (strcat "\nmodo osnap atual engloba o modo osnap: "
		    (rtos n)
	    )
    )
    (prompt
      (strcat "\nModo osnap atual NÃO engloba o modo osnap: "
	      (rtos n)
      )
    )
  )
  (princ) 
)
