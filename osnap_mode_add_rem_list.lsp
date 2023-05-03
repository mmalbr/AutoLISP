;|

Programa adaptado por Marcos Mendes.
Pode ser distribuido livremente desde que seja conservado este cabeçalho

para contato
mmalbr@gmail.com
https://github.com/mmalbr
03/05/2023

modo de usar:

 descrito em cada função
|;



;; adiciona um osmode a seção do autocad o paramentro de entrada deve ser um numero inteiro
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


;; remove um osmode a seção do autocad o paramentro de entrada deve ser um numero inteiro
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



;; verifica se um osmode esta ativo na seção do autocad o paramentro de entrada deve ser um numero inteiro
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
