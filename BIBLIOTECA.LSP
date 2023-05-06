;|

Programa adaptado por Marcos Mendes.
Pode ser distribuido livremente desde que seja conservado este cabeçalho

para contato
mmalbr@gmail.com
https://github.com/mmalbr
03/05/2023

modo de usar:

 verifique o arquivo help.doc
 se quiser me pagar um café:
https://www.buymeacoffee.com/mmalbr
|;

;;FUNÇOES PARA TRATAMENTO DE ARQUIVO EXTERNO     ***********************

;;le o valor de uma variavel dentro de uma seção de um arquivo do tipo ini.
(defun ini_lerv	(inifile section entry)
  (defun ini_iniline (line sep / str1 str2)
    (if	(= 'STR (type line))
      (progn (setq str1	""
		   str2	""
	     )
	     (while (and (/= "" line) (/= sep (substr line 1 1)))
	       (setq str1 (strcat str1 (substr line 1 1))
		     line (substr line 2)
	       )
	     )
	     (if (= sep (substr line 1 1))
	       (setq str2 (substr line 2))
	     )
      )
    )
    (list str1 str2)
  )
  (defun ini_readini (inifile / ofile line section result)
    (if	(findfile inifile)
      (progn
	(setq ofile (open (findfile inifile) "r"))
	(if ofile
	  (progn
	    (while (and	(setq line (read-line ofile))
			(/= "[" (substr line 1 1))
		   )
	    )
	    (while (and line (= "[" (substr line 1 1)))
	      (setq section (list line))
	      (while (and (setq line (read-line ofile))
			  (/= "[" (substr line 1 1))
		     )
		(if (and (/= ";" (substr line 1 1)) (/= "" line))
		  (setq section (cons (ini_iniline line "=") section))
		)
	      )
	      (setq result (cons (reverse section) result))
	    )
	    (close ofile)
	  )
	)
      )
      (alert (STRCAT inifile "\nNao Encontrado!"))
    )
    (reverse result)
  )
  (defun ini_readsection (inifile section / ofile line result)
    (if	(and (= 'STR (type section)) (/= "[" (substr section 1 1)))
      (setq section (strcat "[" section "]"))
    )
    (if	(findfile inifile)
      (cdr (assoc section (ini_readini (findfile inifile))))
      (alert (STRCAT inifile "\nNao Encontrado!"))
    )
  )
  (if (and (= 'STR (type section)) (/= "[" (substr section 1 1)))
    (setq section (strcat "[" section "]"))
  )
  (setq section (ini_readsection inifile section))
  (cadr (assoc entry section))
)

;; altera ou cria uma nova variavel detro de uma seção de uma arquivo do tipo ini.
(defun ini_criarv (inifile section entry val / ofile ini sec)
  (defun ini_iniline (line sep / str1 str2)
    (if	(= 'STR (type line))
      (progn (setq str1	""
		   str2	""
	     )
	     (while (and (/= "" line) (/= sep (substr line 1 1)))
	       (setq str1 (strcat str1 (substr line 1 1))
		     line (substr line 2)
	       )
	     )
	     (if (= sep (substr line 1 1))
	       (setq str2 (substr line 2))
	     )
      )
    )
    (list str1 str2)
  )
  (defun ini_readini (inifile / ofile line section result)
    (if	(findfile inifile)
      (progn
	(setq ofile (open (findfile inifile) "r"))
	(if ofile
	  (progn
	    (while (and	(setq line (read-line ofile))
			(/= "[" (substr line 1 1))
		   )
	    )
	    (while (and line (= "[" (substr line 1 1)))
	      (setq section (list line))
	      (while (and (setq line (read-line ofile))
			  (/= "[" (substr line 1 1))
		     )
		(if (and (/= ";" (substr line 1 1)) (/= "" line))
		  (setq section (cons (ini_iniline line "=") section))
		)
	      )
	      (setq result (cons (reverse section) result))
	    )
	    (close ofile)
	  )
	)
      )
      (alert (STRCAT inifile "\nNao Encontrado!"))
    )
    (reverse result)
  )
  (if (not (findfile inifile))
    (progn (setq ofile (open inifile "w")) (close ofile))
  )
  (if (and (= 'STR (type section)) (/= "[" (substr section 1 1)))
    (setq section (strcat "[" section "]"))
  )
  (if (setq ofile (findfile inifile))
    (progn
      (setq ini (ini_readini inifile))
      (cond
	((setq sec (assoc section ini))
	 (if (assoc entry (cdr sec))
	   (setq sec (cons section
			   (subst (list entry val)
				  (assoc entry (cdr sec))
				  (cdr sec)
			   )
		     )
	   )
	   (setq sec
		  (cons
		    section
		    (reverse (cons (list entry val) (reverse (cdr sec))))
		  )
	   )
	 )
	 (setq ini (subst sec (assoc section ini) ini))
	 (setq ofile (open ofile "w"))
	 (if ofile
	   (progn
	     (mapcar
	       '(lambda	(x)
		  (write-line (car x) ofile)
		  (mapcar
		    '(lambda (x)
		       (write-line (strcat (car x) "=" (cadr x)) ofile)
		     )
		    (cdr x)
		  )
		  (write-line "" ofile)
		)
	       ini
	     )
	     (close ofile)
	   )
	 )
	)
	(T
	 (setq ofile (open ofile "a"))
	 (if ofile
	   (progn (write-line section ofile)
		  (write-line (strcat entry "=" val) ofile)
		  (close ofile)
	   )
	 )
	)
      )
    )
  )
)

;; cria uma lista do tipo assoc com todas as variaveis de uma seção de um arquivod do tipo ini.
(defun ini_lersv (inifile section / ofile line result)
  (defun ini_iniline (line sep / str1 str2)
    (if	(= 'STR (type line))
      (progn (setq str1	""
		   str2	""
	     )
	     (while (and (/= "" line) (/= sep (substr line 1 1)))
	       (setq str1 (strcat str1 (substr line 1 1))
		     line (substr line 2)
	       )
	     )
	     (if (= sep (substr line 1 1))
	       (setq str2 (substr line 2))
	     )
      )
    )
    (list str1 str2)
  )
  (defun ini_readini (inifile / ofile line section result)
    (if	(findfile inifile)
      (progn
	(setq ofile (open (findfile inifile) "r"))
	(if ofile
	  (progn
	    (while (and	(setq line (read-line ofile))
			(/= "[" (substr line 1 1))
		   )
	    )
	    (while (and line (= "[" (substr line 1 1)))
	      (setq section (list line))
	      (while (and (setq line (read-line ofile))
			  (/= "[" (substr line 1 1))
		     )
		(if (and (/= ";" (substr line 1 1)) (/= "" line))
		  (setq section (cons (ini_iniline line "=") section))
		)
	      )
	      (setq result (cons (reverse section) result))
	    )
	    (close ofile)
	  )
	)
      )
      (alert (STRCAT inifile "\nNao Encontrado!"))
    )
    (reverse result)
  )
  (if (and (= 'STR (type section)) (/= "[" (substr section 1 1)))
    (setq section (strcat "[" section "]"))
  )
  (if (findfile inifile)
    (cdr (assoc section (ini_readini (findfile inifile))))
    (alert (STRCAT inifile "\nNao Encontrado!"))
  )
)

;; cria uma lista com todas as seçoes e todas as variaveis de um arquivo do tipo ini.
(defun ini_ler (inifile / ofile line section result)
  (defun ini_iniline (line sep / str1 str2)
    (if	(= 'STR (type line))
      (progn (setq str1	""
		   str2	""
	     )
	     (while (and (/= "" line) (/= sep (substr line 1 1)))
	       (setq str1 (strcat str1 (substr line 1 1))
		     line (substr line 2)
	       )
	     )
	     (if (= sep (substr line 1 1))
	       (setq str2 (substr line 2))
	     )
      )
    )
    (list str1 str2)
  )
  (if (findfile inifile)
    (progn
      (setq ofile (open (findfile inifile) "r"))
      (if ofile
	(progn (while (and (setq line (read-line ofile))
			   (/= "[" (substr line 1 1))
		      )
	       )
	       (while (and line (= "[" (substr line 1 1)))
		 (setq section (list line))
		 (while	(and (setq line (read-line ofile))
			     (/= "[" (substr line 1 1))
			)
		   (if (and (/= ";" (substr line 1 1)) (/= "" line))
		     (setq section (cons (ini_iniline line "=") section))
		   )
		 )
		 (setq result (cons (reverse section) result))
	       )
	       (close ofile)
	)
      )
    )
    (alert (STRCAT inifile "\nNao Encontrado!"))
  )
  (reverse result)
)

;;FUNÇOES PARA TRATAMENTO DE BLOCOS      	  ***********************

;;converte definicao de bloco para texto
(defun def2txt (ent	     /		tipo	   texto
		  layer	     estilo	escala	   ponto
		  altura     rotacao	ablique	   horizontal
		  vertical   alinhamento
		 )
  (setq ent (entget ent))
  (setq tipo (cdr (assoc 0 ent)))
  (if (= tipo "ATTDEF")
    (progn
      (setq texto (cdr (assoc 2 ent))
      layer (cdr (assoc 8 ent))
       estilo (cdr (assoc 7 ent))
      escala (cdr (assoc 41 ent))
      ponto (cdr (assoc 10 ent))
      altura (cdr (assoc 40 ent))
      rotacao (cdr (assoc 50 ent))
      oblique (cdr (assoc 51 ent))
      horizontal (cdr (assoc 72 ent))
      vertical (cdr (assoc 74 ent))
      alinhamento (cdr (assoc 11 ent)))
      (ENTMAKE
	(LIST '(0 . "TEXT")
	      (CONS 8 layer)
	      (CONS 10 ponto)
	      (cons 7 estilo)
	      (cons 41 escala)
	      (cons 1 texto)
	      (if altura
		(cons 40 altura)
	      )
	      (if rotacao
		(cons 50 rotacao)
	      )
	      (if oblique
		(cons 51 oblique)
	      )
	      (if horizontal
		(cons 72 horizontal)
	      )
	      (if vertical
		(cons 73 vertical)
	      )
	      (if alinhamento
		(cons 11 alinhamento)
	      )
	)
      )
      (entdel (cdr (car ent)))
    )
  )
  (princ)
)

;; pequeno aplicativo para remover blocos duplicados
(defun c:repbloco (/ BLOCOS XX)
  (defun seq_error (s)
    (princ (strcat "\n\*\*\*" s "\*\*\*\n"))
   (setvar "modemacro" ".")
    (if	olderr
      (setq *error* olderr)
    )
    (princ)
  )
  (defun combo (title combo_list / arq_tmp combo_id)
    (defun gravar (arq / arq1)
      (setq arq1 (open arq "w"))
      (write-line
	(strcat
	  "combo \: dialog \{label \= \""
	  title
	  "\" \;  \: row \{    \: boxed_column \{      alignment \= centered \;"
	)
	arq1
      )
      (close arq1)
      (setq arq1 (open arq "a"))
      (write-line
	"  \: text \{        key \= \"c_ed\" \;        width \= 15 \;        edit_width \= 20 \;        edit_limit \= 50 \;        fixed_width \= true \;      \}"
	arq1
      )
      (write-line "     spacer_1\;" arq1)
      (write-line
	"  \: list_box \{        key \= \"c_list\" \;        height \= 6 \;        fixed_height \= true \;   edit_width \= 15\;  \}    \}"
	arq1
      )
      (write-line
	"  \: boxed_column \{      alignment \= centered \;"
	arq1
      )
      (write-line
	"     \: button \{        key \= \"close\" \;        label \= \"&Ok\" \;        is_default \= true \;        is_cancel \= true \;      \}"
	arq1
      )
      (write-line
	"     \: button \{        key \= \"close1\" \;        label \= \"&Cancel\" \;        is_default \= false \;        is_cancel \= true \;      \}  spacer_1\; spacer_1\; \}  \}\}"
	arq1
      )
      (close arq1)
    )
    (setq arq_tmp (strcat (getenv "temp") "\\_.dcl"))
    (gravar arq_tmp)
    (setq combo_id (load_dialog arq_tmp))
    (if	(not (new_dialog "combo" combo_id))
      (exit)
    )
    (start_list "c_list")
    (mapcar 'add_list combo_list)
    (end_list)
    (action_tile
      "c_list"
      "(setq combo_ed
	      (set_tile \"c_ed\"
	      (nth (atoi $value) combo_list)))"
    )
    (action_tile "c_ed" "(setq combo_ed $value)")
    (action_tile
      "close"
      "(setq item (get_tile \"c_ed\"))(done_dialog)"
    )
    (action_tile "close1" "(setq item nil)(done_dialog 0)")
    (action_tile "add2list" "(ad_list)")
    (action_tile "rem2list" "(rm_list)")
    (mode_tile "c_ed" 2)
    (set_tile "c_ed" (nth 0 combo_list))
    (start_dialog)
    (unload_dialog combo_id)
    (setq item item)
  )
  (defun TABCAD	(TABLE / TMP RET)
    (setq TMP (tblnext TABLE 'T))
    (while TMP
      (setq RET	(cons (cdr (assoc 2 TMP)) RET)
	    TMP	(tblnext TABLE)
      )
    )
    (reverse RET)
  )
  (defun bb (bloco    /	       quant	grupo	 s_0	  s_8
	     s_2      s_10     s_41	s_42	 s_43	  s_50
	     s_70     s_71     s_44	s_45	 s_67	  s_210
	     grupo_bl item_conservado	q_item	 b1	  c1
	     d1	      quant_saida
	    )
    (setq quant 0)
    (defun dxf (n ed) (cdr (assoc n (entget ed))))
    (setq grupo (ssget "X" (list (cons 0 "INSERT") (cons 2 bloco))))
    (if	grupo
      (progn
	(princ (strcat "\r" "0" " objetos removido\(s\).\t\t    "))
	(while (/= 0 (sslength grupo))
	  (progn
	    (if	(/= "15" (substr (getvar "acadver") 1 2))
	      (progn (setvar "modemacro"
			     (strcat "Falta analisar "
				     (rtos (sslength grupo) 2 0)
				     " objetos\("
				     bloco
				     "\)."
			     )
		     )
	      )
	    )
	    (setq item (ssname grupo 0))
	    (setq grupo (ssdel item grupo))
	    (if	(/= nil (entget item))
	      (progn
		(setq s_0 (dxf 0 item))
		(setq s_8 (dxf 8 item))
		(setq s_2 (dxf 2 item))
		(setq s_10 (dxf 10 item))
		(setq s_41 (dxf 41 item))
		(setq s_42 (dxf 42 item))
		(setq s_43 (dxf 43 item))
		(setq s_50 (dxf 50 item))
		(setq s_70 (dxf 70 item))
		(setq s_71 (dxf 71 item))
		(setq s_44 (dxf 44 item))
		(setq s_45 (dxf 45 item))
		(setq s_67 (dxf 67 item))
		(setq s_210 (dxf 210 item))
		(setq grupo_bl (ssget "X"
				      (list (cons 0 s_0)
					    (cons 8 s_8)
					    (cons 2 s_2)
					    (cons 10 s_10)
					    (cons 41 s_41)
					    (cons 43 s_43)
					    (cons 50 s_50)
					    (cons 70 s_70)
					    (cons 67 s_67)
					    (cons 71 s_71)
					    (cons 44 s_44)
					    (cons 45 s_45)
					    (cons 210 s_210)
				      )
			       )
		)
		(setq item_conservado (ssname grupo_bl 0))
		(setq grupo_bl (ssdel item_conservado grupo_bl))
		(setq q_item (sslength grupo_bl))
		(if (> q_item 0)
		  (progn
		    (setq quant_elementos_removidos (rment grupo_bl))
		    (setq quant_t (+ quant_t quant_elementos_removidos))
		    (setq quant_saida (show (rtos quant_t 2 0)))
		  )
		)
	      )
	    )
	  )
	)
	(if quant_saida
	  (princ (strcat "\r"
			 quant_saida
			 " objetos\("
			 bloco
			 "\) removido\(s\).\t\t          "
		 )
	  )
	  (princ (strcat "\rNão há objetos\("
			 bloco
			 "\) duplicados.\t\t          "
		 )
	  )
	)
      )
      (princ
	(strcat
	  "Nao foi encontrada nenhuma ocorrencia deste objeto\("
	  bloco
	  "\) neste desenho.\nE recomendável executar o comando \"PURGE\""
	)
      )
    )
    (princ)
  )
  (defun rment (grupo / quant item lista)
    (setq quant (sslength grupo))
    (repeat quant
      (progn (setq item (ssname grupo 0))
	     (setq grupo (ssdel item grupo))
	     (setq lista (append lista (list item)))
      )
    )
    (foreach en lista (entdel en))
    (length lista)
  )
  (defun show (quant)
    (cond
      ((= sho1 nil)
       (progn (princ
		(strcat "\r" quant " objetos removido\(s\).\t    ....")
	      )
	      (setq sho1 "b1")
       )
      )
      ((= sho1 "b1")
       (progn (princ
		(strcat "\r" quant " objetos removido\(s\).\t    o...")
	      )
	      (setq sho1 "c1")
       )
      )
      ((= sho1 "c1")
       (progn (princ
		(strcat "\r" quant " objetos removido\(s\).\t    Oo..")
	      )
	      (setq sho1 "d1")
       )
      )
      ((= sho1 "d1")
       (progn (princ
		(strcat "\r" quant " objetos removido\(s\).\t    oOo.")
	      )
	      (setq sho1 "e1")
       )
      )
      ((= sho1 "e1")
       (progn (princ
		(strcat "\r" quant " objetos removido\(s\).\t    .oOo")
	      )
	      (setq sho1 "f1")
       )
      )
      ((= sho1 "f1")
       (progn (princ
		(strcat "\r" quant " objetos removido\(s\).\t    ..oO")
	      )
	      (setq sho1 "g1")
       )
      )
      ((= sho1 "g1")
       (progn (princ
		(strcat "\r" quant " objetos removido\(s\).\t    ...o")
	      )
	      (setq sho1 nil)
       )
      )
    )
    quant
  )
  (setq	olderr	*error*			;captura erro
	*error*	seq_error
  )
  (setq quant_t 0)
  (setq blocos (tabcad "BLOCK"))
  (if blocos
    (progn (setq xx (combo "Selecione um Bloco..." blocos))
	   (if xx
	     (progn (bb xx) (setvar "modemacro" "."))
	     (princ "\rAplicativo cancelado!\t\t\t")
	   )
    )
    (princ "\rEste desenho não possui blocos!\t\t\t")
  )
  (setq *error* olderr)
  (princ)
)

;; exibe uma caixa de dialogo com os dados dxf do elemento selecionado
(defun c:edxf (/ entl lista ct tmp arq_tmp2 dcl_id ee a LISTA_R quantx1 quantx2)
  (defun rmlista (item lista / lista_nova)
    (repeat (length lista)
      (progn (if (not (equal (car lista) item))
	       (progn
		 (setq lista_nova (append lista_nova (list (car lista))))
	       )
	     )
	     (setq lista (cdr lista))
      )
    )
    lista_nova
  )
  (setq ee (entsel "Selecione o objeto:"))
  (if ee
    (progn (setq entl (entget (car ee) (list "*")))
	   (setq ct 0)
	   (repeat (length entl)
	     R
	     (setq lista (append lista (list (nth ct entl))))
	     (setq ct (1+ ct))
	   )
	   (setq lista_r (assoc -3 lista))
	   (setq lista (rmlista lista_r lista))
	   (setq lista_r (cdr lista_r))
      	   (setq ct 0 compr 1)
	   (repeat (length lista_r)
	     (setq lista (append lista (list (nth ct lista_r))))
	     (setq ct (1+ ct))
	   )
	   (setq tmp (strcat (getenv "temp") "\\_.dcl"))
	   (setq arq_tmp2 (open tmp "w"))
	   (write-line "" arq_tmp2)
	   (close arq_tmp2)
	   (setq arq_tmp2 (open tmp "a"))
	   (foreach item lista (print item arq_tmp2))
	   (close arq_tmp2)
	   (setq lista nil)
	   (setq arq_tmp2 (open tmp "r"))
      (setq quantx1 1)
	   (while (/= nil (setq linha (read-line arq_tmp2)))
	     (progn (setq lista (append lista (list linha)))


(setq quantx2 (strlen linha))
	       (if (> quantx2 quantx1) (setq quantx1 quantx2))




	       )
	   )
      (if (> quantx1 120) (setq quantx1 120))
	   (setq lista (cdr (cdr lista)))
	   (close arq_tmp2)
	   (setq tmp (strcat (getenv "temp") "\\_.dcl"))
	   (setq arq_tmp2 (open tmp "w"))
	   (write-line "" arq_tmp2)
	   (close arq_tmp2)
	   (setq arq_tmp2 (open tmp "a"))
	   (write-line "ddtype : dialog \{" arq_tmp2)
	   (write-line "label \= \"\"\;" arq_tmp2)
	   (write-line "key \= \"title\"\;  " arq_tmp2)
	   (write-line ": list_box \{" arq_tmp2)
	   (write-line "key \= \"list_box\"\;" arq_tmp2)
	   (write-line (strcat "width \= " (rtos quantx1 2 0) " \;" ) arq_tmp2)
	   (write-line "height \= 20\;" arq_tmp2)
	   (write-line "\}" arq_tmp2)
	   (write-line "\: ok_button \{" arq_tmp2)
	   (write-line "is_cancel \= true\;" arq_tmp2)
	   (write-line "\}" arq_tmp2)
	   (write-line "\}" arq_tmp2)
	   (close arq_tmp2)
	   (setq dcl_id (load_dialog tmp))
	   (if (not (new_dialog "ddtype" dcl_id))
	     (exit)
	   )
	   (start_list "list_box")
	   (mapcar 'add_list lista)
	   (end_list)
	   (set_tile "title" "Visualizador de dados DXF    v1.0")
	   (action_tile
	     "accept"
	     "(setq a (get_tile \"list_box\"))(done_dialog)"
	   )
	   (start_dialog)
	   (unload_dialog dcl_id)
      (princ (strcat "\n" (nth (atoi a) lista)))
    )
  )
  
  (princ)
)

;; exibe uma caixa de dialogo com os dados dxf do elemento interno (nentsel) selecionado
(defun c:Ndxf (/ entl lista ct tmp arq_tmp2 dcl_id ee a LISTA_R quantx1 quantx2)
  (defun rmlista (item lista / lista_nova)
    (repeat (length lista)
      (progn (if (not (equal (car lista) item))
	       (progn
		 (setq lista_nova (append lista_nova (list (car lista))))
	       )
	     )
	     (setq lista (cdr lista))
      )
    )
    lista_nova
  )
  (setq ee (nentsel "Selecione o objeto:"))
  (if ee
    (progn (setq entl (entget (car ee) (list "*")))
	   (setq ct 0)
	   (repeat (length entl)
	     R
	     (setq lista (append lista (list (nth ct entl))))
	     (setq ct (1+ ct))
	   )
	   (setq lista_r (assoc -3 lista))
	   (setq lista (rmlista lista_r lista))
	   (setq lista_r (cdr lista_r))
	   (setq ct 0)
	   (repeat (length lista_r)
	     (setq lista (append lista (list (nth ct lista_r))))
	     (setq ct (1+ ct))
	   )
	   (setq tmp (strcat (getenv "temp") "\\_.dcl"))
	   (setq arq_tmp2 (open tmp "w"))
	   (write-line "" arq_tmp2)
	   (close arq_tmp2)
	   (setq arq_tmp2 (open tmp "a"))
	   (foreach item lista (print item arq_tmp2))
	   (close arq_tmp2)
	   (setq lista nil)
	   (setq arq_tmp2 (open tmp "r"))
      (setq quantx1 1)
	   (while (/= nil (setq linha (read-line arq_tmp2)))
	     (progn (setq lista (append lista (list linha))))

(setq quantx2 (strlen linha))
	       (if (> quantx2 quantx1) (setq quantx1 quantx2))


	     
	   )
	   (setq lista (cdr (cdr lista)))
	   (close arq_tmp2)
	   (setq tmp (strcat (getenv "temp") "\\_.dcl"))
	   (setq arq_tmp2 (open tmp "w"))
	   (write-line "" arq_tmp2)
	   (close arq_tmp2)
	   (setq arq_tmp2 (open tmp "a"))
	   (write-line "ddtype : dialog \{" arq_tmp2)
	   (write-line "label \= \"\"\;" arq_tmp2)
	   (write-line "key \= \"title\"\;  " arq_tmp2)
	   (write-line ": list_box \{" arq_tmp2)
	   (write-line "key \= \"list_box\"\;" arq_tmp2)
	   (write-line (strcat "width \= " (rtos quantx1 2 0) "\;") arq_tmp2)
	   (write-line "height \= 20\;" arq_tmp2)
	   (write-line "\}" arq_tmp2)
	   (write-line "\: ok_button \{" arq_tmp2)
	   (write-line "is_cancel \= true\;" arq_tmp2)
	   (write-line "\}" arq_tmp2)
	   (write-line "\}" arq_tmp2)
	   (close arq_tmp2)
	   (setq dcl_id (load_dialog tmp))
	   (if (not (new_dialog "ddtype" dcl_id))
	     (exit)
	   )
	   (start_list "list_box")
	   (mapcar 'add_list lista)
	   (end_list)
	   (set_tile "title" "Visualizador de dados DXF    v1.0")
	   (action_tile
	     "accept"
	     "(setq a (get_tile \"list_box\"))(done_dialog)"
	   )
	   (start_dialog)
	   (unload_dialog dcl_id)(princ (strcat "\n" (nth (atoi a) lista)))
    )
  )
  
  (princ)
)

;; Retorna lista com os tags de todos os atributos
(DeFun ATTLIST (enam / resul dtt k dtt1)
  (Setq dtt (EntGet enam))
  (setq k 1)
  (while k
    (setq dtt1 (entget (entnext (cdr (assoc -1 dtt)))))
    (if	(= (cdr (assoc 0 dtt1)) "ATTRIB")
      (if (listp resul)
	(setq resul (append resul (list (cdr (assoc 2 dtt1)))))
	(setq resul (list (cdr (assoc 2 dtt1))))
      )
    )
    (if	(= (cdr (assoc 0 dtt1)) "SEQEND")
      (setq k nil)
    )
    (setq dtt dtt1)
  )
  resul
)

;; Atualiza o LAYER de um atributo
(DeFun ATTLAYER	(ename tag new_layer / ent tg novo velho sai)
  (if (= "INSERT" (cdr (assoc 0 (entget ename))))
    (progn (setq ent ename)
	   (setq ent (entnext ent))
	   (while ent
	     (progn (setq tg (cdr (assoc 2 (entget ent))))
		    (if	(= tg (strcase tag))
		      (progn (setq novo (cons 8 new_layer))
			     (setq velho (assoc 8 (entget ent)))
			     (setq ent (subst novo velho (entget ent)))
			     (entmod ent)
			     (entupd ename)
			     (setq sai 1)
		      )
		    )
		    (if	(/= sai 1)
		      (setq ent (entnext ent))
		      (setq ent nil)
		    )
	     )
	   )
    )
  )
  (princ)
)

;; Modifica o estado visível ou invisível de um atributo em um bloco
(DeFun ATTSIT (ss_blk tg_att flag70 / n_obj e_blk L_atts e_att)
  (DeFun List_Attribs_Data (e_blk / dtt k dtt1 L_datt resul)
    (Setq dtt (EntGet e_blk))
    (if	(= "INSERT" (cdr (assoc 0 dtt)))
      (progn (setq k 1)
	     (while k
	       (setq dtt1 (entget (entnext (cdr (assoc -1 dtt)))))
	       (if (= (cdr (assoc 0 dtt1)) "ATTRIB")
		 (progn	(setq L_datt (list (cdr (assoc 2 dtt1))
					   (cdr (assoc 1 dtt1))
					   (cdr (assoc -1 dtt1))
				     )
			)
			(if (listp resul)
			  (setq resul (append resul (list L_datt)))
			  (setq resul (list L_datt))
			)
		 )
	       )
	       (if (= (cdr (assoc 0 dtt1)) "SEQEND")
		 (setq k nil)
	       )
	       (setq dtt dtt1)
	     )
      )
    )
    resul
  )
  (DeFun ON_OFF_Attrib (e_att flag70 / dt_att old_flag70 novo velho)
    (if	(= (cdr (assoc 0 (entget e_att))) "ATTRIB")
      (progn (setq dt_att (entget e_att))
	     (setq velho (assoc 70 dt_att))
	     (setq old_flag70 (cdr velho))
	     (if (/= flag70 old_flag70)
	       (progn (setq novo (cons 70 flag70))
		      (setq dt_att (subst novo velho dt_att))
		      (entmod dt_att)
		      (entupd e_att)
		      T
	       )
	     )
      )
    )
  )
  (setq ss_blk (ssadd ss_blk))
  (setq n_obj (- (sslength ss_blk) 1))
  (while (>= n_obj 0)
    (setq e_blk (ssname ss_blk n_obj))
    (setq L_atts (List_Attribs_Data e_blk))
    (setq e_att (last (assoc tg_att L_atts)))
    (ON_OFF_Attrib e_att flag70)
    (setq n_obj (- n_obj 1))
  )
  (princ)
)

;;retorna uma lista com sublista contendo nome do tag, valor associado  e nome da entidade
(DeFun listags (e_blk / dtt k dtt1 L_datt resul)
  (Setq dtt (EntGet e_blk))
  (if (= "INSERT" (cdr (assoc 0 dtt)))
    (progn (setq k 1)
	   (while k
	     (setq dtt1 (entget (entnext (cdr (assoc -1 dtt)))))
	     (if (= (cdr (assoc 0 dtt1)) "ATTRIB")
	       (progn (setq L_datt (list (cdr (assoc 2 dtt1))
					 (cdr (assoc 1 dtt1))
					 (cdr (assoc -1 dtt1))
				   )
		      )
		      (if (listp resul)
			(setq resul (append resul (list L_datt)))
			(setq resul (list L_datt))
		      )
	       )
	     )
	     (if (= (cdr (assoc 0 dtt1)) "SEQEND")
	       (setq k nil)
	     )
	     (setq dtt dtt1)
	   )
    )
  )
  resul
)

;;inserir valores em tag's existentes de um determinado bloco
(DeFun blinsat (enam TG valor / e k e1)
  (Setq e (EntGet enam))
  (setq k 1)
  (while k
    (setq e1 (entget (entnext (cdr (assoc -1 e)))))
    (if	(= (cdr (assoc 0 e1)) "ATTRIB")
      (Cond ((= (cdr (assoc 2 e1)) TG)
	     (PROGN (setq e1 (subst (cons 1 valor) (assoc 1 e1) e1))
		    (entmod e1)
		    (entupd enam)
	     )
	    )
      )
    )
    (if	(= (cdr (assoc 0 e1)) "SEQEND")
      (setq k nil)
    )
    (setq e e1)
  )
  (princ)
)

;;extrair valores dos tag's existentes de um determinado bloco
(DeFun blexat (enam TG / e k e1 valor_tag1)
  (Setq e (EntGet enam))
  (setq k 1)
  (while k
    (setq e1 (entget (entnext (cdr (assoc -1 e)))))
    (if	(= (cdr (assoc 0 e1)) "ATTRIB")
      (Cond ((= (cdr (assoc 2 e1)) TG)
	     (PROGN (Setq valor_tag1 (cdr (assoc 1 e1))))
	    )
      )
    )
    (if	(= (cdr (assoc 0 e1)) "SEQEND")
      (setq k nil)
    )
    (setq e e1)
  )
  (setq valor_tag1 valor_tag1)
)

;; pequeno aplicativo que efetua a contagem de bloco, separando-os por layer
(defun c:cont ()
  (defun seq_error (s)
    (princ (strcat "\*\*\*" s "\*\*\*\n"))
    (if	olderr
      (setq *error* olderr)
    )
    (princ)
  )
  (defun detalhes ()
    (setq lista1 nil)
    (setq lista nil)
    (setq item (tblnext "LAYER" T))
    (setq item2 (assoc 2 item))
    (setq item (cdr item2))
    (setq lista (append lista (list item)))
    (while (/= nil (setq item (tblnext "LAYER")))
      (progn (setq item2 (assoc 2 item))
	     (setq item (cdr item2))
	     (if (/= item nil)
	       (setq lista (append lista (list item)))
	     )
      )
    )
    (setq arq1 (open tmp "w"))
    (write-line "" arq1)
    (close arq1)
    (setq arq1 (open tmp "a"))
    (setq tipo1 "layer")
    (setq tipo2 b1)
    (gravar_arquivo)
    (close arq1)
    (setq dcl_id (load_dialog tmp))
    (if	(not (new_dialog "CONT1" dcl_id))
      (alert "Programa Inconsistente!!!")
      (ok3)
    )
    (setq dcl_id (load_dialog tmp))
    (if	(not (new_dialog "CONT" dcl_id))
      (alert "Programa Inconsistente!!!")
      (ok2)
    )
    (if	(= retorno 1)
      (progn (setq retorno nil) (cont1))
    )
  )
  (defun ok ()
    (action_tile "accept" "(done_dialog 1)")
    (action_tile "info" "(mostra3)")
    (action_tile "bloco" "(mostra1)")
    (action_tile "detalhes" "(mostra2)")
    (mode_tile "bloco" 2)
    (setq b1 (car lista))
    (mk_errdia "Aguarde...")
    (setq grupo (ssget "X" (list (cons 2 b1))))
    (if	(/= nil grupo)
      (setq quant (sslength grupo))
      (setq quant 0)
    )
    (mk_errdia "Pronto!")
    (set_tile "valor" (rtos quant 2 0))
    (set_tile "nome2" b1)
    (start_list "bloco")
    (mapcar 'add_list lista)
    (end_list)
    (setq what_next (start_dialog))
    (unload_dialog dcl_id)
  )
  (defun ok2 ()
    (mk_errdia "Aguarde...")
    (if	(= opcao 1)
      (progn
	(setq quantidade (sslength grupo))
	(repeat	quantidade
	  (progn
	    (setq item1 (ssname grupo 0))
	    (setq grupo (ssdel item1 grupo))
	    (setq la (cdr (assoc 8 (entget item1))))
	    (if	(= nil lista1)
	      (setq lista1 (append lista1 (list la)))
	      (progn (setq ddd (member la lista1))
		     (if (= nil ddd)
		       (setq lista1 (append lista1 (list la)))
		     )
	      )
	    )
	    (cond
	      ((= sho1 nil)
	       (progn (mk_errdia "Aguarde...    |") (setq sho1 "b1"))
	      )
	      ((= sho1 "b1")
	       (progn (mk_errdia "Aguarde...    /") (setq sho1 "c1"))
	      )
	      ((= sho1 "c1")
	       (progn (mk_errdia "Aguarde...    --") (setq sho1 "d1"))
	      )
	      ((= sho1 "d1")
	       (progn (mk_errdia "Aguarde...    \\") (setq sho1 nil))
	      )
	    )
	  )
	)
	(setq lista lista1)
      )
    )
    (setq lista1 (acad_strlsort lista))
    (action_tile "accept" "(done_dialog 1)")
    (action_tile "info" "(mostra3)")
    (action_tile "bloco" "(mostra4)")
    (action_tile "voltar" "(mostra5)")
    (mode_tile "bloco" 2)
    (setq b2 (car lista1))
    (mk_errdia "Aguarde...")
    (setq grupo (ssget "X" (list (cons 8 b2) (cons 2 b1))))
    (if	(/= nil grupo)
      (setq quant (sslength grupo))
      (setq quant 0)
    )
    (mk_errdia "Pronto!")
    (set_tile "valor" (rtos quant 2 0))
    (set_tile "nome2" b2)
    (start_list "bloco")
    (mapcar 'add_list lista1)
    (end_list)
    (setq what_next (start_dialog))
    (unload_dialog dcl_id)
  )
  (defun ok3 ()
    (action_tile "accept" "(mostra6)")
    (action_tile "nao" "(mostra7)")
    (setq what_next (start_dialog))
    (unload_dialog dcl_id)
  )
  (defun mostra6 () (setq opcao 1) (done_dialog 0))
  (defun mostra7 () (setq opcao nil) (done_dialog 0))
  (defun mostra5 () (setq retorno 1) (done_dialog 1))
  (defun mostra2 () (done_dialog 0) (setq det1 1))
  (defun mostra3 ()
    (alert
      "Localizador de Blocos\n\nVersao 1.0\nBy Marcos Mendes de Almeida\n\ne-mail  = mmal@uol.com.br\nhttp://sites.uol.com.br/mmal/"
    )
  )
  (defun mostra4 ()
    (set_tile "valor" "-")
    (set_tile "nome2" "-")
    (setq posicao (atoi (get_tile "bloco")))
    (setq b2 (nth posicao lista1))
    (mk_errdia "Aguarde...")
    (setq grupo (ssget "X" (list (cons 8 b2) (cons 2 b1))))
    (if	(/= nil grupo)
      (setq quant (sslength grupo))
      (setq quant 0)
    )
    (mk_errdia "Pronto!")
    (set_tile "valor" (rtos quant 2 0))
    (set_tile "nome2" b2)
  )
  (defun mostra1 ()
    (set_tile "valor" "-")
    (set_tile "nome2" "-")
    (setq posicao (atoi (get_tile "bloco")))
    (setq b1 (nth posicao lista))
    (mk_errdia "Aguarde...")
    (setq grupo (ssget "X" (list (cons 2 b1))))
    (if	(/= nil grupo)
      (setq quant (sslength grupo))
      (setq quant 0)
    )
    (mk_errdia "Pronto!")
    (set_tile "valor" (rtos quant 2 0))
    (set_tile "nome2" b1)
  )
  (defun mk_errdia (msg) (set_tile "error" msg) nil)
  (defun gravar_arquivo	()
    (write-line "CONT1 \: dialog \{" arq1)
    (write-line "label \= \"Atenção\"\;" arq1)
    (write-line "spacer_1\;" arq1)
    (write-line "\: boxed_row \{" arq1)
    (write-line "label \= \"leia...\"\;" arq1)
    (write-line "\: paragraph \{" arq1)
    (write-line "\: text_part \{" arq1)
    (write-line "key \= \"s1\"\;" arq1)
    (write-line
      "value \= \"Listar somente os layer's em que\"\;"
      arq1
    )
    (write-line "width \= 30\;" arq1)
    (write-line "\}" arq1)
    (write-line "\: text_part \{" arq1)
    (write-line "key \= \"s2\"\;" arq1)
    (write-line "value \= \"o bloco for encontrado?\"\;" arq1)
    (write-line "\}" arq1)
    (write-line "\}\}" arq1)
    (write-line "spacer_1\;" arq1)
    (write-line "\:row \{" arq1)
    (write-line "spacer_1\;" arq1)
    (write-line "ok_button\;" arq1)
    (write-line "\: button \{" arq1)
    (write-line "label \= \"TODOS\"\;" arq1)
    (write-line "mnemonic \= \"N\"\;" arq1)
    (write-line "key \= \"nao\"\;" arq1)
    (write-line "fixed_width \= true\;" arq1)
    (write-line "fixed_height \= true\;" arq1)
    (write-line "\}spacer_1\;" arq1)
    (write-line "\}\}" arq1)
    (write-line "CONT \: dialog \{" arq1)
    (write-line
      "        label \= \"Contador de Blocos - v1.0\"\;"
      arq1
    )
    (write-line "        spacer_1\;" arq1)
    (write-line "        \: boxed_row \{" arq1)
    (write-line
      (strcat "        label \= \"Selecione o " tipo1 "...\"\;")
      arq1
    )
    (write-line "        \: list_box \{" arq1)
    (write-line
      (strcat "            label \= \"" tipo2 "\"\;")
      arq1
    )
    (write-line "            mnemonic \= \"B\"\;" arq1)
    (write-line "            key \= \"bloco\"\;" arq1)
    (write-line "            width \= 20\;" arq1)
    (write-line "        \}" arq1)
    (write-line "        \: boxed_row \{" arq1)
    (write-line "        \: paragraph \{" arq1)
    (write-line "                \: text_part \{" arq1)
    (write-line
      "                        key \= \"nome1\"\;"
      arq1
    )
    (write-line
      "                        value \= \"Nome\: \"\;"
      arq1
    )
    (write-line "                    \}" arq1)
    (write-line "        \: text_part \{" arq1)
    (write-line
      "                        key \= \"nome2\"\;"
      arq1
    )
    (write-line "                        value \= \"-\"\;" arq1)
    (write-line "alignment \= right\;" arq1)
    (write-line "                    \}" arq1)
    (write-line "        \: text_part \{" arq1)
    (write-line
      "                        key \= \"divisa1\"\;"
      arq1
    )
    (write-line
      "                        value \= \"______________\"\;"
      arq1
    )
    (write-line "alignment \= centered\;" arq1)
    (write-line "                    \}" arq1)
    (write-line "spacer_1\;" arq1)
    (write-line "        \: text_part \{" arq1)
    (write-line
      "                        key \= \"quantidade\"\;"
      arq1
    )
    (write-line
      "                        value \= \"Quantidade:\"\;"
      arq1
    )
    (write-line "                        width \= 20\;" arq1)
    (write-line "                    \}" arq1)
    (write-line "		\: text_part \{" arq1)
    (write-line
      "                        key \= \"valor\"\;"
      arq1
    )
    (write-line "alignment \= right\;" arq1)
    (write-line "                        value \= \"-\"\;" arq1)
    (write-line "                    \}" arq1)
    (write-line "        \: text_part \{" arq1)
    (write-line
      "                        key \= \"divisa2\"\;"
      arq1
    )
    (write-line
      "                        value \= \"______________\"\;"
      arq1
    )
    (write-line "alignment \= centered\;" arq1)
    (write-line "                    \}" arq1)
    (write-line "spacer_1\;" arq1)
    (if	(= tipo1 "bloco")
      (progn (write-line "\: button \{" arq1)
	     (write-line "label \= \"Detalhes...\"\;" arq1)
	     (write-line "mnemonic \= \"D\"\;" arq1)
	     (write-line "key \= \"detalhes\"\;" arq1)
	     (write-line "\}" arq1)
      )
      (progn (write-line "\: button \{" arq1)
	     (write-line "label \= \"Voltar...\"\;" arq1)
	     (write-line "mnemonic \= \"V\"\;" arq1)
	     (write-line "key \= \"voltar\"\;" arq1)
	     (write-line "\}" arq1)
      )
    )
    (write-line "        \}" arq1)
    (write-line "        \}" arq1)
    (write-line "            \}" arq1)
    (write-line ": row {" arq1)
    (write-line "            spacer_1\;" arq1)
    (write-line "           ok_button\;" arq1)
    (write-line "           info_button\;" arq1)
    (write-line "            spacer_1\;" arq1)
    (write-line "}" arq1)
    (write-line "       	    errtile\;" arq1)
    (write-line "\}" arq1)
  )
  (defun cont1 (/ item item2 lista dcl_id tmp det1 info1 tipo1)
    (setq olderr  *error*		;captura erro
	  *error* seq_error
    )
    (setq tmp (strcat (getenv "temp") "\\_.dcl"))
    (setq arq1 (open tmp "w"))
    (write-line "" arq1)
    (close arq1)
    (setq arq1 (open tmp "a"))
    (setq tipo1 "bloco")
    (setq tipo2 "Blocos:")
    (gravar_arquivo)
    (close arq1)
    (setq item (tblnext "BLOCK" T))
    (setq item2 (assoc 2 item))
    (setq item (cdr item2))
    (if	(/= item nil)
      (setq lista (append lista (list item)))
    )
    (while (/= nil (setq item (tblnext "BLOCK")))
      (progn (setq item2 (assoc 2 item))
	     (setq item (cdr item2))
	     (if (/= item nil)
	       (setq lista (append lista (list item)))
	     )
      )
    )
    (if	(/= nil lista)
      (progn (setq lista (acad_strlsort lista))
	     (setq dcl_id (load_dialog tmp))
	     (if (not (new_dialog "CONT" dcl_id))
	       (alert "Programa Inconsistente!!!")
	       (ok)
	     )
      )
      (alert "Este desenho não possui blocos!")
    )
    (setq arq1 (open tmp "w"))
    (write-line "contato:\nmmal@uol.com.br" arq1)
    (close arq1)
    (if	(= det1 1)
      (detalhes)
    )
    (setq *error* olderr)
    (princ)
  )
  (cont1)
)

;;FUNÇOES PARA CONVERSÃO DE DADOS      	  ***********************

;; converte graus em radianos
(defun gr2rad (a) (* pi (/ a 180.0)))

;; calcula o fatorial de um numero
(defun fact (n)
  (defun fact1 (m)
    (if	(= m 0)
      1
      (* m (fact (1- m)))
    )
  )
  (if (= n 0)
    1
    (* n (fact1 (1- n)))
  )
)

;; converte radianos em graus
(defun rad2gr (a) (/ (* a 180.0) pi))

;;Funcao que calcula a tangente de um arco em radianos
(defun rad2tan (x) (/ (sin x) (cos x)))

;; inteiro para octal
(defun int2octal (i / s a)
  (setq s "")
  (while (> i 0)
    (setq a (rem i 8)
          i (lsh i -3))
    (setq s (strcat (chr (+ 48 a)) s))))

;; octal para inteiro
(defun octal2int (s / i n a)
  (setq i 0 n 0)
  (while (< i (strlen s))
    (setq a (substr s (setq i (1+ i)) 1))
    (setq n (+ (lsh n 3)
	       (- (ascii a) 48)))))


;;inteiro para hexadecimal
(defun int2hex (i / s a)
  (setq s "")
  (while (> i 0)
    (setq a (rem i 16)
          i (lsh i -4))
    (setq s (strcat
      (if (< a 10)
        (chr (+ 48 a))
        (chr (+ 55 a))) s))))

;;hexadecimal para inteiro
(defun hex2int (s / i n a)
  (setq i 0 n 0 s (strcase s))
  (if (= (substr s 1 2) "0X") (setq s (substr s 3)))
  (while (< i (strlen s))
    (setq a (substr s (setq i (1+ i)) 1))
    (if (not (<= "0" a "F")) (chr a))
    (setq n (+ (lsh n 4)
	       (- (ascii a) (if (<= a "9") 48 55))))))

;; inteiro para binario
(defun int2bin (i / lst)
  (while (> i 0)
    (setq lst (cons (rem i 2) lst)
	  i   (lsh i -1)))
  lst
)

;; binario para inteiro
(defun bin2int (numlst / i n)
  (setq n 0)
  (foreach i numlst
    (setq n
      (cond
        ((= i 0) (lsh n 1))
        ((= i 1) (1+ (lsh n 1)))
        (T (chr (chr (+ 48 i))))))))

;;FUNÇOES PARA TRATAMENTO DE COODENADAS	  ***********************

;;Bulge center point calculation
(defun BULGEC (P1 P2 BLG / DD AA PT)
  (defun tan (x) (/ (sin x) (cos x)))
  (defun MIDPT (P1 P2)
    (mapcar '(lambda (A B) (/ (+ A B) 2.0)) P1 P2)
  )
  (setq	PT (midpt P1 P2)
	AA (* (atan (abs BLG)) 4.0)
	AP (if (> AA PI)
	     (- (* 2 PI) AA)
	     AA
	   )
	BB (/ (- PI AP) 2.0)
	DD (* (tan BB) (/ (distance P1 P2) 2.0))
	AP (angle P1 P2)
	AP (if (or (and (minusp BLG) (> AA PI))
		   (and (not (minusp BLG)) (< AA PI))
	       )
	     (+ AP (* PI 0.5))
	     (- AP (* PI 0.5))
	   )
  )
  (polar PT AP DD)
)

;; retorna o ponto medio de uma reta dada por dois pontos
(defun pm (p1 p2)
  (list	(+ (/ (- (nth 0 p2) (nth 0 p1)) 2) (nth 0 p1))
	(+ (/ (- (nth 1 p2) (nth 1 p1)) 2) (nth 1 p1))
  )
)

;; retorna uma lista contendo as coordenadas que encapsulam um texto 
(defun captxt (ent   /	   entl	 box   boxl  boxh  ang	 ang90 evec
	       veclist	   p1	 p2    p3    p4	   lista p1a   p2a
	       p3a   p4a
	      )
  (setq entl (entget ent))
  (setq box (cadr (textbox entl)))
  (setq boxl (car box))
  (setq boxh (cadr box))
  (setq ang (cdr (assoc 50 entl)))
  (setq ang90 (+ ang (/ pi 2)))
  (setq evec (cdr (assoc 210 entl)))
  (setq	p1 (cdr (assoc 10 entl))
	p2 (polar p1 ang boxl)
	p3 (polar p2 ang90 boxh)
	p4 (polar p1 ang90 boxh)
  )
  (setq	p1a (trans p1 evec 1)
	p2a (trans p2 evec 1)
	p3a (trans p3 evec 1)
	p4a (trans p4 evec 1)
  )
  (setq lista (list p1a p2a p3a p4a))
)

;; converte uma lista de coordendas x,y em texto
(defun coord2str (coord precisao)
  (strcat (rtos (nth 0 coord) 2 precisao)
	  ","
	  (rtos (nth 1 coord) 2 precisao)
  )
)

;;FUNÇOES PARA TRATAMENTO DE LISTAS      	  ***********************

;; retorna a diferença entre a duas listas
(defun diflist (lista1 lista2 / conta lista_final)
  (setq conta 0)
  (foreach item	lista1
    (progn (if (not (member item lista2))
	     (setq lista_final (append lista_final (list item)))
	   )
	   (setq conta (1+ conta))
    )
  )
  lista_final
)

;; retorna a posição de um elemeto na lista
(defun lstpos (a lst / b)
  (if (setq b (member a lst))
    (progn (setq b (- (length lst) (length b))))
  )
  b
)

;; Remove itens repetidos dentro da lista 
(DeFun Rrep (L_strings / L_limpo LL)
  (DeFun acumula_em_lista (item nome_list / list_f)
    (setq list_f nome_list)
    (if	(listp list_f)
      (setq list_f (append list_f (list item)))
      (setq list_f (list item))
    )
    list_f
  )
  (setq L_limpo (list (car L_strings)))
  (ForEach LL L_strings
    (if	(member LL L_limpo)
      nil
      (setq L_limpo (acumula_em_lista LL L_limpo))
    )
  )
  L_limpo
)

;;remove um item de uma lista em uma posição conhecida
(defun rmlistapos (i_item l_lista)
  (if (< i_item (length l_lista))
    (progn (repeat i_item
	     (setq l_lista (append (cdr l_lista) (list (car l_lista))))
	   )
	   (setq l_lista (cdr l_lista))
	   (setq l_lista (reverse l_lista))
	   (repeat i_item
	     (setq l_lista (append (cdr l_lista) (list (car l_lista))))
	   )
	   (setq l_lista (reverse l_lista))
    )
  )
  l_lista
)

;;remove TODAS AS OCORRENCIAS DE UMA LISTA
(defun rmlista (item lista / lista_nova)
  (repeat (length lista)
    (progn (if (not (equal (car lista) item))
	     (progn
	       (setq lista_nova (append lista_nova (list (car lista))))
	     )
	   )
	   (setq lista (cdr lista))
    )
  )
  lista_nova
)

;;edita uma lista
(defun listed (title combo_list rep / arq_tmp combo_id)
  (defun ad_list (/ combo_ed)
    (setq combo_ed (get_tile "c_ed"))
    (if	(/= "" combo_ed)
      (if (= rep 0)
	(progn
	  (progn
	    (if	(= (member combo_ed combo_list) nil)
	      (progn (setq combo_list
			    (acad_strlsort
			      (append combo_list (list combo_ed))
			    )
		     )
		     (set_tile "c_ed" "")
		     (mode_tile "c_ed" 2)
		     (start_list "c_list")
		     (mapcar 'add_list combo_list)
		     (end_list)
	      )
	      (progn (mode_tile "c_ed" 2)
		     (alert "\nUma entrada similar ja existe na lista.")
	      )
	    )
	  )
	)
	(progn (setq combo_list
		      (acad_strlsort
			(append combo_list (list combo_ed))
		      )
	       )
	       (set_tile "c_ed" "")
	       (mode_tile "c_ed" 2)
	       (start_list "c_list")
	       (mapcar 'add_list combo_list)
	       (end_list)
	)
      )
      (PROGN (mode_tile "c_ed" 2) (alert "\nEntrada invalida."))
    )
  )
  (defun rm_list (/ combo_ed quant_list lista_index segunda conta)
    (setq combo_ed (get_tile "c_ed"))
    (if	(/= (member combo_ed combo_list) nil)
      (progn (setq quant_list (length combo_list))
	     (setq lista_index (member combo_ed combo_list))
	     (setq quant_list_nova (length lista_index))
	     (setq segunda (cdr lista_index))
	     (setq conta 0)
	     (repeat (- quant_list quant_list_nova)
	       (progn (setq item (nth conta combo_list))
		      (setq conta (1+ conta))
		      (setq segunda (append segunda (list item)))
	       )
	     )
	     (if segunda
	       (setq combo_list (acad_strlsort segunda))
	       (setq combo_list segunda)
	     )
	     (set_tile "c_ed" "")
	     (mode_tile "c_list" 2)
	     (start_list "c_list")
	     (mapcar 'add_list combo_list)
	     (end_list)
      )
      (PROGN (alert "\nEste item nao exite na lista.")
	     (mode_tile "c_ed" 2)
      )
    )
  )
  (defun gravar	(arq / arq1)
    (setq arq1 (open arq "w"))
    (write-line
      (strcat
	"combo \: dialog \{label \= \""
	title
	"\" \;  \: row \{    \: boxed_column \{      alignment \= centered \;"
      )
      arq1
    )
    (close arq1)
    (setq arq1 (open arq "a"))
    (write-line
      "  \: edit_box \{        key \= \"c_ed\" \;        width \= 15 \;        edit_width \= 20 \;        edit_limit \= 50 \;        fixed_width \= true \;      \}"
      arq1
    )
    (write-line
      "  \: list_box \{        key \= \"c_list\" \;        height \= 6 \;        fixed_height \= true \;      \}    \}"
      arq1
    )
    (write-line
      "  \: boxed_column \{      alignment \= centered \;"
      arq1
    )
    (write-line
      "    \: button \{        key \= \"add2list\" \;        label \= \"&Adicionar\" \;      \}"
      arq1
    )
    (write-line
      "    \: button \{        key \= \"rem2list\" \;        label \= \"&Remover\" \;      \}"
      arq1
    )
    (write-line "     spacer_1\;" arq1)
    (write-line
      "     \: button \{        key \= \"close\" \;        label \= \"&Ok\" \;        is_default \= true \;        is_cancel \= true \;      \}    \}  \}\}"
      arq1
    )
    (close arq1)
  )
  (setq arq_tmp (strcat (getenv "temp") "\\_.dcl"))
  (gravar arq_tmp)
  (setq combo_id (load_dialog arq_tmp))
  (if (not (new_dialog "combo" combo_id))
    (exit)
  )
  (start_list "c_list")
  (mapcar 'add_list combo_list)
  (end_list)
  (action_tile
    "c_list"
    "(setq combo_ed
	      (set_tile \"c_ed\"
	      (nth (atoi $value) combo_list)))"
  )
  (action_tile "c_ed" "(setq combo_ed $value)")
  (action_tile "close" "(done_dialog)")
  (action_tile "add2list" "(ad_list)")
  (action_tile "rem2list" "(rm_list)")
  (mode_tile "c_ed" 2)
  (start_dialog)
  (unload_dialog combo_id)
  (setq combo_list combo_list)
)

;;retorna um elemento da lista
(defun combo (title combo_list / arq_tmp combo_id)
  (defun gravar	(arq / arq1)
    (setq arq1 (open arq "w"))
    (write-line
      (strcat
	"combo \: dialog \{label \= \""
	title
	"\" \;  \: row \{    \: boxed_column \{      alignment \= centered \;"
      )
      arq1
    )
    (close arq1)
    (setq arq1 (open arq "a"))
    (write-line
      "  \: text \{        key \= \"c_ed\" \;        width \= 15 \;        edit_width \= 20 \;        edit_limit \= 50 \;        fixed_width \= true \;      \}"
      arq1
    )
    (write-line "     spacer_1\;" arq1)
    (write-line
      "  \: list_box \{        key \= \"c_list\" \;        height \= 6 \;        fixed_height \= true \;   edit_width \= 15\;  \}    \}"
      arq1
    )
    (write-line
      "  \: boxed_column \{      alignment \= centered \;"
      arq1
    )
    (write-line
      "     \: button \{        key \= \"close\" \;        label \= \"&Ok\" \;        is_default \= true \;        is_cancel \= true \;      \}"
      arq1
    )
    (write-line
      "     \: button \{        key \= \"close1\" \;        label \= \"&Cancel\" \;        is_default \= false \;        is_cancel \= true \;      \}  spacer_1\; spacer_1\; \}  \}\}"
      arq1
    )
    (close arq1)
  )
  (setq arq_tmp (strcat (getenv "temp") "\\_.dcl"))
  (gravar arq_tmp)
  (setq combo_id (load_dialog arq_tmp))
  (if (not (new_dialog "combo" combo_id))
    (exit)
  )
  (start_list "c_list")
  (mapcar 'add_list combo_list)
  (end_list)
  (action_tile
    "c_list"
    "(setq combo_ed
	      (set_tile \"c_ed\"
	      (nth (atoi $value) combo_list)))"
  )
  (action_tile "c_ed" "(setq combo_ed $value)")
  (action_tile
    "close"
    "(setq item (get_tile \"c_ed\"))(done_dialog)"
  )
  (action_tile "close1" "(setq item nil)(done_dialog 0)")
  (action_tile "add2list" "(ad_list)")
  (action_tile "rem2list" "(rm_list)")
  (mode_tile "c_ed" 2)
  (set_tile "c_ed" (nth 0 combo_list))
  (start_dialog)
  (unload_dialog combo_id)
  (setq item item)
)

;;FUNÇOES PARA TRATAMENTO POLYLINE E LW...  ***********************

;;retorna as coordenadas dos vertices de uma polyline ou lwpolyline
(DEFUN lv (ent / pontos tipo)
  (if (or (= (CDR (ASSOC 0 (ENTGET ENT))) "POLYLINE")
	  (= (CDR (ASSOC 0 (ENTGET ENT))) "LWPOLYLINE")
      )
    (PROGN (DeFun LV_P (enam / elist vtx plist)
	     (Setq enam	 (EntNext enam)
		   elist (EntGet enam)
		   plist (List (cdr (assoc 10 elist)))
		   enam	 (EntNext enam)
		   elist (EntGet enam)
	     )
	     (While (= (cdr (assoc 0 elist)) "VERTEX")
	       (Setq vtx   (List (cdr (assoc 10 elist)))
		     plist (Append plist vtx)
		     enam  (EntNext enam)
		     elist (EntGet enam)
	       )
	     )
	     plist
	     (setq inflex plist)
	   )
	   (DeFun Lv_L (nome / dt result lista_final)
	     (DeFun armazena_vertice (elemento / vertice)
	       (setq vertice (member (cadr elemento) elemento))
	       (if (listp lista_final)
		 (setq lista_final (append lista_final (list vertice)))
		 (setq lista_final (list vertice))
	       )
	       lista_final
	     )
	     (setq dt (EntGet nome))
	     (ForEach elemento dt
	       (setq primeiro (car elemento))
	       (If (= primeiro 10)
		 (setq result (armazena_vertice elemento))
	       )
	     )
	     result
	   )
	   (setq tipo (cdr (assoc 0 (entget ent))))
	   (if (= tipo "POLYLINE")
	     (setq pontos (lv_p ent))
	   )
	   (if (= tipo "LWPOLYLINE")
	     (setq pontos (lv_L ent))
	   )
	   (setq pontos pontos)
    )
  )
)

;; retorna os vertices de uma lwpolyline SOMENTE
(defun VLWP (obj / dados vert aux)
  (if (= (CDR (ASSOC 0 (ENTGET obj))) "POLYLINE")
    (PROGN (setq dados (entget obj)
		 vert  nil
		 z     (cdr (assoc 38 dados))
	   )
	   (while (setq aux (assoc 10 dados))
	     (setq vert	 (append vert (list (append (cdr aux) (list z))))
		   dados (cdr (member aux dados))
	     )
	   )
	   vert
    )
  )
)

;; retorna o comprimento de uma polyline OU lwpolyline
(defun cpoly (ent / pontos dist)
  (DEFUN lv (ent / pontos tipo)
    (if	(or (= (CDR (ASSOC 0 (ENTGET ENT))) "POLYLINE")
	    (= (CDR (ASSOC 0 (ENTGET ENT))) "LWPOLYLINE")
	)
      (PROGN (DeFun LV_P (enam / elist vtx plist)
	       (Setq enam  (EntNext enam)
		     elist (EntGet enam)
		     plist (List (cdr (assoc 10 elist)))
		     enam  (EntNext enam)
		     elist (EntGet enam)
	       )
	       (While (= (cdr (assoc 0 elist)) "VERTEX")
		 (Setq vtx   (List (cdr (assoc 10 elist)))
		       plist (Append plist vtx)
		       enam  (EntNext enam)
		       elist (EntGet enam)
		 )
	       )
	       plist
	       (setq inflex plist)
	     )
	     (DeFun Lv_L (nome / dt result lista_final)
	       (DeFun armazena_vertice (elemento / vertice)
		 (setq vertice (member (cadr elemento) elemento))
		 (if (listp lista_final)
		   (setq lista_final (append lista_final (list vertice)))
		   (setq lista_final (list vertice))
		 )
		 lista_final
	       )
	       (setq dt (EntGet nome))
	       (ForEach	elemento dt
		 (setq primeiro (car elemento))
		 (If (= primeiro 10)
		   (setq result (armazena_vertice elemento))
		 )
	       )
	       result
	     )
	     (setq tipo (cdr (assoc 0 (entget ent))))
	     (if (= tipo "POLYLINE")
	       (setq pontos (lv_p ent))
	     )
	     (if (= tipo "LWPOLYLINE")
	       (setq pontos (lv_L ent))
	     )
	     (setq pontos pontos)
      )
    )
  )
  (setq pontos (lv ent))
  (setq dist 0)
  (repeat (1- (length pontos))
    (progn (setq dist (+ dist (distance (car pontos) (nth 1 pontos))))
	   (setq pontos (cdr pontos))
    )
  )
  dist
)

;;FUNÇOES DE MISCELANEA           	  ***********************

;;solicita um numero inteiro- interface grafica
(defun ddinreal	(msg titulo default / sim dado4 var1)
  (defun inputbox (prompt1 title default / tmp arq_tmp2 dc_id)
    (setq tmp (strcat (getenv "temp") "\\_.dcl"))
    (setq arq_tmp2 (open tmp "w"))
    (write-line "" arq_tmp2)
    (close arq_tmp2)
    (setq arq_tmp2 (open tmp "a"))
    (write-line "inputbox \: dialog \{" arq_tmp2)
    (write-line "	key \= \"title\"\;" arq_tmp2)
    (write-line "\: text \{" arq_tmp2)
    (write-line "	key \= \"prompt\"\;" arq_tmp2)
    (write-line "	       \}" arq_tmp2)
    (write-line "	\: edit_box \{" arq_tmp2)
    (write-line "	key \= \"eb1\"\;" arq_tmp2)
    (write-line "	allow_accept \= true\;" arq_tmp2)
    (write-line "		   \}" arq_tmp2)
    (write-line
      "\:row \{spacer_1\;spacer_1\;spacer_1\;spacer_1\;"
      arq_tmp2
    )
    (write-line "	\ok_only\;" arq_tmp2)
    (write-line
      "spacer_1\;spacer_1\;spacer_1\;spacer_1\;\}"
      arq_tmp2
    )
    (write-line "  		 \}" arq_tmp2)
    (close arq_tmp2)
    (setq dcl_id (load_dialog tmp))
    (if	(not (new_dialog "inputbox" dcl_id))
      (exit)
    )
    (set_tile "prompt" prompt1)
    (set_tile "title" title)
    (set_tile "eb1" default)
    (mode_tile "eb1" 2)
    (action_tile
      "cancel"
      "(done_dialog)
     (setq result nil)(setq inputvalue nil)"
    )
    (action_tile
      "accept"
      "(setq inputvalue (get_tile \"eb1\"))
     (done_dialog)
     (setq result T)"
    )
    (start_dialog)
    (unload_dialog dcl_id)
    inputvalue
  )
  (defun strnum	(S / C conta s1)
    (setq s1 s)
    (setq C (substr S 1 1))
    (if	(or (= C "+") (= C "-"))
      (setq S (substr S 2))
    )
    (while (> (strlen S) 0)
      (setq C (substr S 1 1)
	    S (substr S 2)
      )
      (if (and (not (<= 48 (ascii C) 57)) (/= C "."))
	(setq S	""
	      C	nil
	)
      )
      (IF (= C ".")
	(progn (if (= conta nil)
		 (setq conta 0)
	       )
	       (setq conta (1+ conta))
	)
      )
    )
    (if	(or (= "." (substr s1 (strlen s1))) (= "." (substr s1 1 1)))
      (setq conta 2)
    )
    (if	(> (strlen s1) 1)
      (progn
	(if (or (= "-." (substr s1 1 2)) (= "+." (substr s1 1 2)))
	  (setq conta 2)
	)
      )
    )
    (if	(> conta 1)
      nil
      (if C
	'T
	nil
      )
    )
  )
  (setq dado4 (inputbox titulo msg (rtos default)))
  (setq sim (strnum dado4))
  (while (= sim nil)
    (PROGN (setq dado4 (inputbox titulo msg (rtos default)))
	   (if (= dado4 "")
	     (setq sim 1)
	     (setq sim (strnum dado4))
	   )
    )
  )
  (atof dado4)
)

;; Pede entrada de um numero real
(Defun inreal (default tela / inp)
  (setq inp (getreal (strcat "\n" tela " <" (rtos default) "> : ")))
  (if (= inp nil)
    (setq inp default)
  )
  (+ inp 0.0)
)

;; Pede entrada de um numero inteiro
(Defun inint (default tela / inp)
  (setq inp (getint (strcat "\n" tela " <" (itoa default) "> : ")))
  (if (= inp nill)
    (setq inp default)
  )
  (+ inp 0)
)

;; Pede entrada de uma string
(Defun instr (default tela / inp)
  (setq inp (getSTRING T (strcat "\n" tela " <" default "> : ")))
  (if (= inp "")
    (setq inp default)
  )
  (strcat inp "")
)

;; aguarda ate que uma tecla seja pressionada
(defun wait (msg / a ms1)
  (prompt msg)
  (while (/= (car a) 2) (setq a (grread)))
  (setq ms1 "")
  (repeat (strlen msg) (progn (setq ms1 (strcat ms1 " "))))
  (princ (strcat "\r" ms1))
  (princ)
)

;; calcula o valor medio de dois numeros
(defun pmedio (p1 p2)
  (/ (- (float (max p1 p2)) (float (min p1 p2))) 2)
)

;;retorna o codigo dxf
(defun dxf (n ed) (cdr (assoc n (entget ed))))

;; retorna a data atual no formato xx/xx/xxxx
(defun hoje (/ TMP)
  (setq TMP (rtos (getvar "CDATE") 2 0))
  (strcat (substr TMP 7 2)
	  "/"
	  (substr TMP 5 2)
	  "/"
	  (substr TMP 1 4)
  )
)

;; retorna a data atual no formato xx de xx de xxxx
(DeFun hojex (/ cur_date s_data s_ano s_mes s_dia data_br)
  (setq cur_date (getvar "cdate"))
  (setq s_data (rtos cur_date 2 6))
  (setq s_dia (substr s_data 7 2))
  (setq s_mes (substr s_data 5 2))
  (setq s_ano (substr s_data 1 4))
  (cond	((= s_mes "01") (setq s_mes "Janeiro"))
	((= s_mes "02") (setq s_mes "Fevereiro"))
	((= s_mes "03") (setq s_mes "Marco"))
	((= s_mes "04") (setq s_mes "Abril"))
	((= s_mes "05") (setq s_mes "Maio"))
	((= s_mes "06") (setq s_mes "Junho"))
	((= s_mes "07") (setq s_mes "Julho"))
	((= s_mes "08") (setq s_mes "Agosto"))
	((= s_mes "09") (setq s_mes "Setembro"))
	((= s_mes "10") (setq s_mes "Outubro"))
	((= s_mes "11") (setq s_mes "Novembro"))
	((= s_mes "12") (setq s_mes "Dezembro"))
  )
  (setq data_br (strcat s_dia " de " s_mes " de " s_ano))
  data_br
)

;;retorna uma lista com todos os elementos de uma determinada tabela do autocad
(defun TABCAD (TABLE / TMP RET)
  (setq TMP (tblnext TABLE 'T))
  (while TMP
    (setq RET (cons (cdr (assoc 2 TMP)) RET)
	  TMP (tblnext TABLE)
    )
  )
  (reverse RET)
)

;; salva  e restaura variaveis
(defun svar (lista_var / var)
  (defun rvar (/ var valor)
    (foreach item lista_salva_de_variaveis
      (progn (setq var (car item))
	     (setq valor (cdr item))
	     (setvar var valor)
      )
    )
    (princ)
  )
  (setq lista_salva_de_variaveis nil)
  (foreach item	lista_var
    (progn (setq var (getvar item))
	   (setq lista_salva_de_variaveis
		  (append lista_salva_de_variaveis
			  (list (cons item var))
		  )
	   )
    )
  )
)

;; barra de progresso
(defun c:test_prog_bar (/ a conta)
  (setq a 1000)
  (setq conta 0)
  (repeat a (progn (prog_bar1 (setq conta (+ 1 conta)) 1000)))
  (setq a 1000)
  (setq conta 0)
  (repeat a (progn (prog_bar2 (setq conta (+ 1 conta)) 1000)))
)

(defun prog_bar1 (perc_atual perc_total	/	   compl
		  perc_index mostra	porc	   q_por
		 )
  (defun seq_error (error_prog_bar)
    (if	(/= error_prog_bar "Function cancelled.\n")
      (grtext -1 "")
    )
    (if	olderr
      (setq *error* olderr)
    )
    (princ)
  )
  (setq	olderr	*error*
	*error*	seq_error
  )
  (setq compl ".")
  (if (or (zerop perc_atual) (zerop perc_total))
    (progn (grtext -1 "."))
    (progn (setq perc_index (/ (* perc_atual 100.0) perc_total))
	   (if (= 'INT (type perc_index))
	     (setq perc_index (atof (itoa perc_index)))
	   )
	   (setq porc (rtos perc_index 2 0))
	   (setq q_por (strlen porc))
	   (if (= q_por 1)
	     (setq porc (strcat "00" porc))
	   )
	   (if (= q_por 2)
	     (setq porc (strcat "0" porc))
	   )
	   (setq mostra (strcat porc " %"))
	   (while (< (strlen mostra) 7)
	     (setq mostra (strcat mostra " "))
	   )
	   (repeat (atoi (rtos perc_index 2 0))
	     (setq mostra (strcat mostra "\200"))
	   )
	   (while (and (< (strlen mostra) 107) (/= 0 (strlen compl)))
	     (setq mostra (strcat mostra compl))
	   )
	   (grtext -1 mostra)
    )
  )
  (if (= perc_atual perc_total)
    (grtext -1 "")
  )
  (setq *error* olderr)
  (princ)
)

(defun prog_bar2 (perc_atual perc_total	/	   compl
		  perc_index mostra	porc	   q_por
		 )
  (defun seq_error (error_prog_bar)
    (if	(/= error_prog_bar "Function cancelled.\n")
      (grtext -2 "")
    )
    (if	olderr
      (setq *error* olderr)
    )
    (princ)
  )
  (setq	olderr	*error*
	*error*	seq_error
  )
  (setq compl "'")
  (if (or (zerop perc_atual) (zerop perc_total))
    (progn (grtext -2 "'"))
    (progn (setq perc_index (/ (* perc_atual 100.0) perc_total))
	   (if (= 'INT (type perc_index))
	     (setq perc_index (atof (itoa perc_index)))
	   )
	   (setq porc (rtos perc_index 2 0))
	   (setq q_por (strlen porc))
	   (if (= q_por 1)
	     (setq porc (strcat "00" porc))
	   )
	   (if (= q_por 2)
	     (setq porc (strcat "0" porc))
	   )
	   (setq mostra (strcat porc " %"))
	   (while (< (strlen mostra) 7)
	     (setq mostra (strcat mostra " "))
	   )
	   (repeat (atoi (rtos perc_index 2 0))
	     (setq mostra (strcat mostra "|"))
	   )
	   (while (and (< (strlen mostra) 107) (/= 0 (strlen compl)))
	     (setq mostra (strcat mostra compl))
	   )
	   (grtext -2 mostra)
    )
  )
  (if (= perc_atual perc_total)
    (grtext -2 "")
  )
  (setq *error* olderr)
  (princ)
)

;;grava os itens de uma lista de strings em um arquivo externo
(defun L2arq (arquivo lista_linha / arq2 erro_item)
  (setq arq2 (open arquivo "w"))
  (setq item1 (nth 0 lista_linha))
  (write-line item1 arq2)
  (setq lista_linha (cdr lista_linha))
  (close arq2)
  (setq arq2 (open arquivo "a"))
  (foreach erro_item lista_linha (write-line erro_item arq2))
  (close arq2)
)

;;solicita um string
(defun dadoin (title prompt1 default)
  (defun gravar	()
    (write-line "inputbox \: dialog \{" arq_tmp2)
    (write-line "	key \= \"title\"\;" arq_tmp2)
    (write-line "\: text \{" arq_tmp2)
    (write-line "	key \= \"prompt\"\;" arq_tmp2)
    (write-line "	       \}" arq_tmp2)
    (write-line "	\: edit_box \{" arq_tmp2)
    (write-line "	key \= \"eb1\"\;" arq_tmp2)
    (write-line "		   \}" arq_tmp2)
    (write-line "	\ok_cancel\;" arq_tmp2)
    (write-line "  		 \}" arq_tmp2)
  )
  (setq tmp (strcat (getenv "temp") "\\_.dcl"))
  (setq arq_tmp2 (open tmp "w"))
  (write-line "" arq_tmp2)
  (close arq_tmp2)
  (setq arq_tmp2 (open tmp "a"))
  (gravar)
  (close arq_tmp2)
  (setq dcl_id (load_dialog tmp))
  (if (not (new_dialog "inputbox" dcl_id))
    (exit)
  )
  (set_tile "prompt" prompt1)
  (set_tile "title" title)
  (set_tile "eb1" default)
  (mode_tile "eb1" 2)
  (action_tile
    "cancel"
    "(done_dialog)
     (setq result nil)(setq inputvalue nil)"
  )
  (action_tile
    "accept"
    "(setq inputvalue (get_tile \"eb1\"))
     (done_dialog)
     (setq result T)"
  )
  (start_dialog)
  (unload_dialog dcl_id)
  inputvalue
)

;;exibe uma caixa de dialogo com sim ou nao
(defun opcao (msg1 / choice_flag)
  (defun gravar	()
    (write-line "choice \: dialog \{" arq_tmp1)
    (write-line "  label \= \"AutoCAD Message\"\;" arq_tmp1)
    (write-line "  spacer\;" arq_tmp1)
    (write-line "  \: text \{" arq_tmp1)
    (write-line "    label \= \"\"\;" arq_tmp1)
    (write-line "    key \= \"choice_msg\"\;" arq_tmp1)
    (write-line "    alignment \= centered\;" arq_tmp1)
    (write-line
      "    width \= 35\;//this can be larger to suit needs..."
      arq_tmp1
    )
    (write-line "  \}" arq_tmp1)
    (write-line "  spacer\;" arq_tmp1)
    (write-line "  \: row \{" arq_tmp1)
    (write-line "    alignment \= centered\;" arq_tmp1)
    (write-line "    fixed_width \= true\;" arq_tmp1)
    (write-line "    \: ok_button \{" arq_tmp1)
    (write-line "      label \= \"Sim\"\;" arq_tmp1)
    (write-line "      key \= \"accept\"\;" arq_tmp1)
    (write-line "      mnemonic \=\"Y\"\; " arq_tmp1)
    (write-line "   \}" arq_tmp1)
    (write-line "   spacer\;" arq_tmp1)
    (write-line "   \: cancel_button \{" arq_tmp1)
    (write-line "     label \= \"Nao\"\;" arq_tmp1)
    (write-line "     key \= \"cancel\"\;" arq_tmp1)
    (write-line "     mnemonic \=\"N\"\; " arq_tmp1)
    (write-line "   \}" arq_tmp1)
    (write-line "  \}" arq_tmp1)
    (write-line " \}" arq_tmp1)
  )
  (setq tmp (strcat (getenv "temp") "\\_.dcl"))
  (setq arq_tmp1 (open tmp "w"))
  (write-line "" arq_tmp1)
  (close arq_tmp1)
  (setq arq_tmp1 (open tmp "a"))
  (gravar)
  (close arq_tmp1)
  (setq dcl_id (load_dialog tmp))
  (new_dialog "choice" dcl_id)
  (set_tile "choice_msg" msg1)
  (action_tile "accept" "(setq choice_flag T)(done_dialog)")
  (start_dialog)
  (eval choice_flag)
)

;; solicita um senha e retona T ou nil
(defun senha (/ compr novo ola senha acesso)
  (setq senha-or (substr (rtos (getvar "cdate") 2 4) 10))
  (defun seq_error (s)
    (if	(= tres 1)
      (progn
	(alert
	  "Voce ultrapassou o limite maximo de tentativas para esta secao, reinicie o AutoCAD."
	)
	(setq tres nil)
      )
      (alert
	"\nUse Somente o Teclado Para a \nAceitacao de Sua Senha Pessoal."
      )
    )
    (if	(/= s "Function cancelled.\n")
      (princ "Erro...")
    )
    (if	olderr
      (setq *error* olderr)
    )
    (princ)
  )
  (defun confirm (title msg / fn bkText txtList dh result)
    (defun strPart (str sym side / a newstr)
      (if (= (type str) 'STR)
	(cond ((= side "l")
	       (setq a	    (substr str 2 1)
		     newstr (substr str 1 1)
		     str    (substr str 2)
	       )
	       (while (/= sym a)
		 (setq newstr (strcat newstr (substr str 1 1))
		       str    (substr str 2)
		       a      (substr str 1 1)
		 )
	       )
	       newstr
	      )
	      ((= side "r")
	       (setq a	 (substr str 1 1)
		     str (substr str 2)
	       )
	       (while (/= sym a)
		 (setq a   (substr str 1 1)
		       str (substr str 2)
		 )
	       )
	      )
	)
      )
    )
    (defun makeDcl (fileName title textList / fh)
      (if (= title "")
	(setq title " Confirmação ")
      )
      (setq fh (open fileName "w"))
      (write-line "confirm : dialog {" fh)
      (write-line (strcat "label = \"" title "\";") fh)
      (write-line ": paragraph {" fh)
      (foreach item textList
	(write-line
	  (strcat ": text_part {label = \""
		  item
		  "\";alignment = centered;}"
	  )
	  fh
	)
      )
      (write-line "}" fh)
      (write-line "spacer_1;" fh)
      (write-line "ok_cancel;" fh)
      (write-line ": paragraph {" fh)
      (write-line
	(strcat	": text_part {label = \""
		""
		"\";alignment = right;}"
	)
	fh
      )
      (write-line "}" fh)
      (write-line "}" fh)
      (close fh)
    )
    (setq fn (strcat (getenv "temp") "\\confirm.dcl"))
    (while (wcmatch msg (strcat "*\n*"))
      (setq bkText  (strPart msg "\n" "l")
	    msg	    (strPart msg "\n" "r")
	    txtList (cons bkText txtList)
      )
    )
    (setq txtList (reverse (cons msg txtList)))
    (makeDcl fn title txtList)
    (setq dh (load_dialog fn))
    (if	(and dh (new_dialog "confirm" dh))
      (progn (set_tile "message" msg)
	     (if (= (start_dialog) 1)
	       (setq result 'T)
	       (setq result nil)
	     )
	     (unload_dialog dh)
	     result
      )
    )
  )
  (setq	olderr	*error*
	*error*	seq_error
  )
  (if (= contagem 5)
    (progn (setq tres 1) (exit))
    (progn
      (setq compr nil)
      (setq novo 0)
      (setq contagem 0)
      (while (or (= 'T novo) (= 0 novo))
	(progn
	  (if (= contagem 3)
	    (progn (setq tres 1) (exit))
	    (progn
	      (prompt "\nInforme Sua Senha:\n")
	      (setq senha "")
	      (while (/= 13 ola)
		(progn
		  (setq ola (nth 1 (grread)))
		  (if (= 8 ola)
		    (progn (setq compr (strlen senha))
			   (princ "\r")
			   (repeat compr (princ " "))
			   (princ "\r")
			   (repeat (1- compr) (princ "*"))
			   (if (/= 0 compr)
			     (setq senha (substr senha 1 (1- compr)))
			   )
		    )
		    (progn (setq senha (strcat senha (chr ola)))
			   (prompt "*")
		    )
		  )
		)
	      )
	      (if (= senha (strcat senha-or "\r"))
		(progn (setq novo 1)
		       (prompt "\nSenha Correta!\n")
		       (setq acesso 'T)
		)
		(progn (setq senha nil)
		       (setq ola nil)
		       (setq contagem (1+ contagem))
		       (prompt "\nSenha Incorreta!\n")
		       (setq novo
			      (confirm
				"SENHA INCORRETA..."
				"A senha nao confere!.\n Deseja tentar novamente?"
			      )
		       )
		)
	      )
	    )
	  )
	)
      )
      (if (= acesso nil)
	(prompt "\nCancelado!\n")
      )
    )
  )
  (setq *error* olderr)
  (setq acesso acesso)
)

;;retorna a senha digitada
(defun GETPASS (msg / input result)
  (prompt msg)
  (setq result "")
  (while
    (progn (setq input (grread))
	   (cond ((not (eq (car input) 2)))
		 ((member (setq input (cadr input)) '(13 32)) nil)
		 ((and (eq input 8) (not (eq result "")))
		  (setq result (substr result 1 (1- (strlen result))))
		  (princ "\10 \10")
		 )
		 ((< 32 input 127)
		  (setq result (strcat result (chr input)))
		  (princ "*")
		 )
		 (T)
	   )
    )
  )
  result
)

;;crga de arquivo VLX  e ARX
(DeFun VLXFASIN	(nomeVlx /)
  (setq var (getvar "cmdecho"))
  (setvar "cmdecho" 0)
  (if (not vlrts-init)
    (progn (arxload "vlrts") (vlrts-init))
    (vlrts-init)
  )
  (vl-load nomeVlx)
  (setvar "cmdecho" var)
  (princ)
)

;;gerador de numeros ramdomicos
(Defun ran (valor_maximo / s t1)
  (setq valor_maximo (1+ valor_maximo))
  (setq s (* (getvar "cdate") 1000000.0))
  (setq t1 (fix (* valor_maximo (- s (fix s)))))
  (itoa t1)
)

;;; arredonda um numero decimal convertendo-o para inteiro  
(defun round (num)
  (if (or (= (type num) 'REAL) (= (type num) 'INT))
    (fix (+ num 0.5))
    nil
  )
)

;; edita o arquivo acad.pgp
(defun pgp (comando abrevia / w arq)
  (setq var (getvar "cmdecho"))
  (setvar "cmdecho" 0)
  (setq linhapgp (strcat abrevia ",       " "*" comando))
  (setq w (findfile "acad.pgp"))
  (setq arq (open w "a"))
  (write-line linhapgp arq)
  (close arq)
  (command "_.re-init" "16")
  (setvar "cmdecho" var)
  (princ)
)

;;retorna uma lista com todas as funções AutoLisp "c:" definidas na seção corrente
(defun lsp (/ lst n a lista)
  (progn (setq lst (atoms-family 1)
	       lst (acad_strlsort lst)
	 )
	 (setq n 0)
	 (repeat (length lst)
	   (setq a (nth n lst))
	   (if (equal "C:" (strcase (substr a 1 2)))
	     (setq lista (append lista (list (substr a 3))))
	   )
	   (setq n (+ n 1))
	 )
  )
  lista
)

;;Retorna a situação de proteção do layer
(defun stlaylock (la / na e1)
  (setq	na (tblobjname "layer" la)
	e1 (entget na)
  )
  (equal 4 (logand 4 (cdr (assoc 70 e1))))
)

;;Retorna a situação de congelamento do layer
(defun stlayfreeze (la / na e1)
  (setq	na (tblobjname "layer" la)
	e1 (entget na)
  )
  (equal 1 (logand 1 (cdr (assoc 70 e1))))
)

;;Calcula o comprimento de um arco
(defun ARCOM (ent / rad sang eang ang len)
  (if (= (cdr (assoc 0 (entget ent))) "ARC")
    (progn (setq rad (cdr (assoc 40 (entget ent))))
	   (setq sang (cdr (assoc 50 (entget ent))))
	   (setq eang (cdr (assoc 51 (entget ent))))
	   (if (< sang eang)
	     (setq ang (abs (- sang eang)))
	     (setq ang (- (* 2 pi) (abs (- sang eang))))
	   )
	   (setq len (* ang rad))
    )
    nil
  )
)

;;retorna os modos osnap ativados para a seção corrente do autocad
(defun osmodes (/ osnap_modes list_bit bit conta)
  (setq osnap_modes (getvar "osmode"))
  (setq list_bit (append list_bit (list 0)))
  (setq conta 1)
  (repeat 14
    (progn (setq Bit (equal conta (logand conta osnap_modes)))
	   (if bit
	     (setq list_bit (append list_bit (list conta)))
	   )
	   (setq conta (+ conta conta))
    )
  )
  list_bit
)

;; cria um layer se o mesmo ainda não existir
(defun crlayer (layer_t cor / nlayer ll layer existe var1 var2)
  (setq layer (tblnext "LAYER" T))
  (while layer
    (setq nlayer (cdr (assoc 2 layer)))
    (setq ll (append ll (list nlayer)))
    (setq layer (tblnext "LAYER"))
  )
  (setq existe (member layer_t ll))
  (if (not existe)
    (progn (setq var1 (getvar "cmdecho"))
	   (setq var2 (getvar "clayer"))
	   (setvar "cmdecho" 0)
	   (command "layer" "make" layer_t "color" cor layer_t "")
	   (setvar "cmdecho" var1)
	   (setvar "clayer" var2)
	   T
    )
    nil
  )
)

;;adiona um modo osnap ao desenho
(defun adosnap (modo)
  (defun add (/ tem)
    (setq tem (logand modo 16383))
    (IF	(not (member tem (osmodes)))
      (setvar "osmode" (+ tem (getvar "osmode")))
    )
  )
  (defun osmodes (/ osnap_modes list_bit bit conta)
    (setq osnap_modes (getvar "osmode"))
    (setq list_bit (append list_bit (list 0)))
    (setq conta 1)
    (repeat 14
      (progn (setq Bit (equal conta (logand conta osnap_modes)))
	     (if bit
	       (setq list_bit (append list_bit (list conta)))
	     )
	     (setq conta (+ conta conta))
      )
    )
    list_bit
  )
  (if (= (type modo) 'STR)
    (setq modo (strcase (substr modo 1 3) T))
  )
  (cond	((= modo "non") (progn (setq modo 0) (add)))
	((= modo "end") (progn (setq modo 1) (add)))
	((= modo "mid") (progn (setq modo 2) (add)))
	((= modo "cen") (progn (setq modo 4) (add)))
	((= modo "nod") (progn (setq modo 8) (add)))
	((= modo "qua") (progn (setq modo 16) (add)))
	((= modo "int") (progn (setq modo 32) (add)))
	((= modo "ins") (progn (setq modo 64) (add)))
	((= modo "per") (progn (setq modo 128) (add)))
	((= modo "tan") (progn (setq modo 256) (add)))
	((= modo "nea") (progn (setq modo 512) (add)))
	((= modo "qui") (progn (setq modo 1024) (add)))
	((= modo "app") (progn (setq modo 2048) (add)))
	((= modo "ext") (progn (setq modo 4096) (add)))
	((= modo "par") (progn (setq modo 8192) (add)))
  )
  (if (and (/= modo "non")
	   (/= modo "end")
	   (/= modo "mid")
	   (/= modo "cen")
	   (/= modo "nod")
	   (/= modo "qua")
	   (/= modo "int")
	   (/= modo "ins")
	   (/= modo "per")
	   (/= modo "tan")
	   (/= modo "nea")
	   (/= modo "qui")
	   (/= modo "app")
	   (/= modo "ext")
	   (/= modo "par")
      )
    (add)
  )
  (princ)
)

;;remove um modo osnap do desenho
(defun rmosnap (modo)
  (defun add (/ tem)
    (setq tem (logand modo 16383))
    (IF	(member tem (osmodes))
      (setvar "osmode" (- (getvar "osmode") tem))
    )
  )
  (defun osmodes (/ osnap_modes list_bit bit conta)
    (setq osnap_modes (getvar "osmode"))
    (setq list_bit (append list_bit (list 0)))
    (setq conta 1)
    (repeat 14
      (progn (setq Bit (equal conta (logand conta osnap_modes)))
	     (if bit
	       (setq list_bit (append list_bit (list conta)))
	     )
	     (setq conta (+ conta conta))
      )
    )
    list_bit
  )
  (if (= (type modo) 'STR)
    (setq modo (strcase (substr modo 1 3) T))
  )
  (cond	((= modo "non") (progn (setq modo 0) (add)))
	((= modo "end") (progn (setq modo 1) (add)))
	((= modo "mid") (progn (setq modo 2) (add)))
	((= modo "cen") (progn (setq modo 4) (add)))
	((= modo "nod") (progn (setq modo 8) (add)))
	((= modo "qua") (progn (setq modo 16) (add)))
	((= modo "int") (progn (setq modo 32) (add)))
	((= modo "ins") (progn (setq modo 64) (add)))
	((= modo "per") (progn (setq modo 128) (add)))
	((= modo "tan") (progn (setq modo 256) (add)))
	((= modo "nea") (progn (setq modo 512) (add)))
	((= modo "qui") (progn (setq modo 1024) (add)))
	((= modo "app") (progn (setq modo 2048) (add)))
	((= modo "ext") (progn (setq modo 4096) (add)))
	((= modo "par") (progn (setq modo 8192) (add)))
  )
  (if (and (/= modo "non")
	   (/= modo "end")
	   (/= modo "mid")
	   (/= modo "cen")
	   (/= modo "nod")
	   (/= modo "qua")
	   (/= modo "int")
	   (/= modo "ins")
	   (/= modo "per")
	   (/= modo "tan")
	   (/= modo "nea")
	   (/= modo "qui")
	   (/= modo "app")
	   (/= modo "ext")
	   (/= modo "par")
      )
    (add)
  )
  (princ)
)

;; seleciona os ultimos n objetos inseridos no desenho.
(defun lastn (INT2 / SS2 SSL2)
  (setq SS2 (ssadd))
  (repeat INT2
    (if	(entlast)
      (progn (ssadd (entlast) SS2) (entdel (entlast)))
    )
  )
  (setq SSL2 (1- (sslength SS2)))
  (while (>= SSL2 0)
    (entdel (ssname SS2 SSL2))
    (setq SSL2 (1- SSL2))
  )
  SS2
)

;;FUNÇOES PARA CONVERSÃO DE DADOS      	  ***********************

;; Permite associar a um unico selection set dois tipo diferentes de objetos
(defun sel2 (tipo1 tipo2 /)
  (ssget "X"
	 (list (cons -4 "<OR")
	       (cons -4 "<AND")
	       (cons 0 tipo1)
	       (cons -4 "AND>")
	       (cons -4 "<AND")
	       (cons 0 tipo2)
	       (cons -4 "AND>")
	       (cons -4 "OR>")
	 )
  )
)

;;Retorna um grupo de seleção pelo codigo dxf e valor
(defun dxfgrupo	(codigo valor /)
  (ssget "X" (list (cons codigo valor)))
)

;; aculta os elementos contido em um grupo de seleção
(defun ssvis (ss code / na e1 n)
  (if ss
    (progn (setq n 0)
	   (repeat (sslength ss)
	     (setq na (ssname ss n)
		   e1 (entget na)
	     )				;setq
	     (if (not (assoc 60 e1))
	       (setq e1 (append e1 (list (cons 60 code)))) ;setq then
	       (setq e1 (subst (cons 60 code) (assoc 60 e1) e1))
					;setq else
	     )				;if
	     (entmod e1)
	     (entupd na)
	     (setq n (+ n 1))		;setq
	   )				;repeat 
    )					;progn
  )					;if
)

;; junta dois grupo de seleção
(defun jgrupo (g1 g2 / conta gt)
  (setq gt (ssadd))
  (if g1
    (progn (setq conta 0)
	   (repeat (sslength g1)
	     (progn (setq gt (ssadd (ssname g1 conta) gt))
		    (setq conta (1+ conta))
	     )
	   )
    )
  )
  (if g2
    (progn (setq conta 0)
	   (repeat (sslength g2)
	     (progn (setq gt (ssadd (ssname g2 conta) gt))
		    (setq conta (1+ conta))
	     )
	   )
    )
  )
  gt
)

;; subtrai de um grupo de seleção os elementos de outro grupo de seleção 
(defun rgrupo (g2 g1 / gt conta)
  (setq gt (ssadd))
  (setq conta 0)
  (repeat (sslength g2)
    (progn (if (not (ssmemb (ssname g2 conta) g1))
	     (setq gt (ssadd (ssname g2 conta) gt))
	   )
	   (setq conta (1+ conta))
    )
  )
  (setq gt gt)
)

;; Seleciona todos os elementos na coordenada informada
(DeFun Selpt (p_ins pega_a ly_ob tp_ob / pa pb pega_z pa_z pb_z sel_x)
  (setq pa (list (- (car p_ins) pega_a) (- (cadr p_ins) pega_a)))
  (setq pb (list (+ (car p_ins) pega_a) (+ (cadr p_ins) pega_a)))
  (setq pega_z (* pega_a 15.0))
  (setq pa_z (list (- (car p_ins) pega_z) (- (cadr p_ins) pega_z)))
  (setq pb_z (list (+ (car p_ins) pega_z) (+ (cadr p_ins) pega_z)))
  (command "zoom" "w" pa_z pb_z)
  (setq sel_x (ssget "c" pa pb (List (cons 8 ly_ob) (cons 0 tp_ob))))
  (command "zoom" "p")
  sel_x
)

;;FUNÇOES PARA TRATAMENTO DE STRINGS	  ***********************

;; retorna o caractere informado
(defun strichar (str index) (substr str (1+ index) 1))

;;Remove todos os caracteres de espaço que estão antes do primeiro caractere "válido"
(defun strltrim	(textval / len inx conta textval1 carac)
  (setq len (strlen textval))
  (setq textval1 textval)
  (setq conta 0)
  (repeat len
    (progn (setq carac (substr textval1 1 1))
	   (setq textval1 (substr textval1 2))
	   (if (= carac " ")
	     (setq conta (1+ conta))
	   )
    )
  )
  (if (/= conta len)
    (progn (if (= (substr textval 1 1) " ")
	     (progn (setq inx 1)
		    (while (= " " (substr textval 1 1))
		      (setq textval (substr textval 2))
		    )
	     )
	   )
	   textval
    )
    (setq textval "")
  )
)

;;Remove todos os caracteres de espaço que estão depois do último caractere "válido".
(defun strrtrim	(textval / len inx conta textval1 carac)
  (setq len (strlen textval))
  (setq textval1 textval)
  (setq conta 0)
  (repeat len
    (progn (setq carac (substr textval1 1 1))
	   (setq textval1 (substr textval1 2))
	   (if (= carac " ")
	     (setq conta (1+ conta))
	   )
    )
  )
  (if (/= conta len)
    (progn
      (if (= (substr textval len) " ")
	(progn (setq inx 1)
	       (while (= " " (substr textval (strlen textval)))
		 (setq textval (substr textval 1 (- (strlen textval) 1)))
	       )
	)
      )
      textval
    )
    (setq textval "")
  )
)

;;Remove todos os caracteres de espaço que estão antes do último caractere "válido" e estão depois do último caractere "válido".
(defun strtrim (textval / tx2)
  (defun strltrim (textval / len inx conta textval1 carac)
    (setq len (strlen textval))
    (setq textval1 textval)
    (setq conta 0)
    (repeat len
      (progn (setq carac (substr textval1 1 1))
	     (setq textval1 (substr textval1 2))
	     (if (= carac " ")
	       (setq conta (1+ conta))
	     )
      )
    )
    (if	(/= conta len)
      (progn (if (= (substr textval 1 1) " ")
	       (progn (setq inx 1)
		      (while (= " " (substr textval 1 1))
			(setq textval (substr textval 2))
		      )
	       )
	     )
	     textval
      )
      (setq textval "")
    )
  )
  (defun strrtrim (textval / len inx conta textval1 carac)
    (setq len (strlen textval))
    (setq textval1 textval)
    (setq conta 0)
    (repeat len
      (progn (setq carac (substr textval1 1 1))
	     (setq textval1 (substr textval1 2))
	     (if (= carac " ")
	       (setq conta (1+ conta))
	     )
      )
    )
    (if	(/= conta len)
      (progn
	(if (= (substr textval len) " ")
	  (progn
	    (setq inx 1)
	    (while (= " " (substr textval (strlen textval)))
	      (setq textval (substr textval 1 (- (strlen textval) 1)))
	    )
	  )
	)
	textval
      )
      (setq textval "")
    )
  )
  (setq tx2 (strltrim textval))
  (setq textval tx2)
  (strrtrim textval)
)

;;retorna uma lista contendo o indice do(s) caracter(es) informado.
(defun strind (textval strval / len inx index inxl)
  (if (= 1 (strlen strval))
    (progn (setq len (strlen textval))
	   (setq inx 0)
	   (repeat len
	     (progn (setq index (substr textval 1 1))
		    (setq textval (substr textval 2))
		    (setq inx (1+ inx))
		    (if	(= index strval)
		      (setq inxl (append inxl (list inx)))
		    )
	     )
	   )
	   (setq inxl inxl)
    )
    (progn (prompt "\nO segundo parametro esta incorreto!\n")
	   (exit)
    )
  )
)

;;Encontra o numero de ocorrencias de um determinado caractere em uma string.
(defun strcnt (textval strval / cnt len inx chrval)
  (setq len (strlen textval))
  (setq inx 1)
  (setq cnt 0)
  (repeat len
    (setq chrval (substr textval inx 1))
    (if	(= chrval strval)
      (setq cnt (+ cnt 1))
    )
    (setq inx (+ inx 1))
  )
  (setq cnt cnt)
)

;;Converte para minusculo, todos os caracteres de uma string. 
(defun strlc (textval) (strcase textval T))

;;Converte para maiusculo, todos os caracteres de uma string.
(defun struc (textval) (strcase textval))

;;Coverte para maiusculo o primeiro caractere de uma string e o restante para minusculo.
(defun strfc (textval)
  (setq textval (strcase textval T))
  (setq ftxt (substr textval 1 1))
  (setq rtxt (substr textval 2))
  (setq ntxt (strcat (strcase ftxt) rtxt))
)

;;Sustitui todas as ocorrencias de um caractere em uma string por outro caractere ou por uma sequencia de caracteres.
(defun strsub1 (textval strval1 strval2 / len ul str)
  (if (= 1 (strlen strval1))
    (progn (setq str "")
	   (setq len (strlen textval))
	   (setq len1 1)
	   (repeat len
	     (progn (setq ul (substr textval len1 1))
		    (if	(= strval1 ul)
		      (setq ul strval2)
		    )
		    (setq str (strcat str ul))
		    (setq len1 (1+ len1))
	     )
	   )
	   (setq str str)
    )
    (progn (prompt "\nO segundo parametro esta incorreto!\n")
	   (exit)
    )
  )
)

;; Substitui as ocorrencias de um caracter numa string por outro caracter.
(defun strsub2 (em de para / ponteiro)
  (setq ponteiro 1)
  (while (< ponteiro (strlen em))
    (setq sub (substr em ponteiro (strlen de)))
    (if	(= sub de)
      (setq em (strcat (substr em 1 (- ponteiro 1))
		       para
		       (substr em (+ ponteiro (strlen de)))
	       )
      )
    )
    (setq ponteiro (1+ ponteiro))
  )
  em
)

;;Inverte o texto de uma string.
(defun strrev (textval / len ul strinv len1)
  (setq strinv "")
  (setq len (strlen textval))
  (setq len1 len)
  (repeat len
    (progn (setq ul (substr textval len1 1))
	   (setq strinv (strcat strinv ul))
	   (setq len1 (1- len1))
    )
  )
  (setq strinv strinv)
)

;;Retorna uma lista de uma string que contenha algum tipo de delimitador
(DeFun Strlst (niveis delimitador / Numcar PosIni Posic ListLY carac)
  (If niveis
    (progn
      (Setq Numcar (StrLen niveis))
      (Setq PosIni 1)
      (Setq Posic 1)
      (Setq ListLY nil)
      (While (>= NumCar Posic)
	(Setq carac (SubStr niveis Posic 1))
	(If (/= carac delimitador)
	  ()
	  (If (ListP ListLY)
	    (progn (Setq ListLY
			  (append
			    ListLY
			    (List (SubStr niveis PosIni (- Posic PosIni)))
			  )
		   )
		   (Setq PosIni (+ Posic 1))
	    )
	    (progn
	      (Setq ListLY (List (SubStr niveis PosIni (- Posic 1))))
	      (Setq PosIni (+ Posic 1))
	    )
	  )
	)
	(Setq Posic (+ Posic 1))
      )
    )
    ()
  )
  (If (ListP ListLY)
    (progn
      (Setq
	ListLY (append ListLY
		       (List (SubStr niveis PosIni (- Posic PosIni)))
	       )
      )
    )
  )
  ListLY
  (setq ListLY ListLY)
)

;;Efetua uma encriptação=1/desencriptação=0 da string
(defun strcr (textval par / cripta1 letra nova_string conta quant)
  (if (and (/= par 1) (/= par 0))
    (progn (prompt "Parametro incorreto") (exit))
  )
  (setq conta 1)
  (setq nova_string "")
  (setq quant (strlen textval))
  (REPEAT quant
    (SETQ cripta1 (SUBSTR textval conta 1))
    (if	(= par 1)
      (SETQ letra (+ (ASCII cripta1) 13))
    )
    (if	(= par 0)
      (SETQ letra (- (ASCII cripta1) 13))
    )
    (SETQ nova_string (STRCAT nova_string (CHR letra)))
    (setq conta (1+ conta))
  )
  (SETQ nova_string nova_string)
)

;;Completa com um determinado caractere a esquerda de uma string
(defun straddl (textval carac quant / st)
  (setq st "")
  (repeat quant (setq st (strcat carac st)))
  (setq st (strcat st textval))
)

;;Completa com um determinado caractere a direita de uma string
(defun straddr (textval carac quant / st)
  (setq st "")
  (repeat quant (setq st (strcat carac st)))
  (setq st (strcat textval st))
)

;;Completa com zeros a direita de uma string de numero decimal    "12.12" -> "12.12000"
(defun strdecl (textval	  quantidade	      /		posicao
		quant	  antes	    depois    quant_dep	ret_dep
		ss	  fin
	       )
  (defun strpos	(str sub / chave l1 l2 i)
    (setq chave 'T)
    (if	(wcmatch str (strcat "*" sub "*"))
      (progn (setq l1 (+ (strlen str) 1)
		   l2 (strlen sub)
		   i  1
	     )
	     (while (and chave (<= i (- l1 l2)))
	       (if (= sub (substr str i l2))
		 (setq chave nil)
		 (setq i (1+ i))
	       )
	     )
      )
    )
    (if	chave
      nil
      i
    )
  )
  (setq posicao (strpos textval "."))
  (if (= posicao nil)
    (setq textval (strcat textval "."))
  )
  (setq posicao (strpos textval "."))
  (setq antes (substr textval 1 (1- posicao)))
  (setq depois (substr textval (1+ posicao)))
  (setq quant (strlen depois))
  (setq quant_dep (strlen depois))
  (if (> quant_dep quantidade)
    (setq depois (substr depois 1 quantidade))
  )
  (setq ret_dep (- quantidade quant_dep))
  (setq ss "")
  (repeat ret_dep (setq ss (strcat ss "0")))
  (setq fin (strcat antes "." depois ss))
  (if (= 0 quantidade)
    (setq fin antes)
  )
  fin
)

;;Retorna a posicao do PRIMEIRO caractere informado em uma string
(defun strpos (str sub / chave l1 l2 i)
  (setq chave 'T)
  (if (wcmatch str (strcat "*" sub "*"))
    (progn (setq l1 (+ (strlen str) 1)
		 l2 (strlen sub)
		 i  1
	   )
	   (while (and chave (<= i (- l1 l2)))
	     (if (= sub (substr str i l2))
	       (setq chave nil)
	       (setq i (1+ i))
	     )
	   )
    )
  )
  (if chave
    nil
    i
  )
)

;;Retorna uma lista com os caracteres de uma string
(defun strlist (textval / len len1 cara lista)
  (setq len (strlen textval))
  (setq len1 0)
  (repeat len
    (progn (setq len1 (1+ len1))
	   (setq cara (substr textval len1 1))
	   (setq lista (append lista (list cara)))
    )
  )
  lista
)

;;Retorna uma lista contendo apenas uma ocorrencia de cada caractere de uma string
(defun strocl
       (textval / len len1 cara lista quant_lista item lista_fin)
  (setq len (strlen textval))
  (setq len1 0)
  (repeat len
    (progn (setq len1 (1+ len1))
	   (setq cara (substr textval len1 1))
	   (setq lista (append lista (list cara)))
    )
  )
  (setq quant_lista (length lista))
  (repeat quant_lista
    (progn (setq item (car lista))
	   (setq lista (cdr lista))
	   (if (not (member item lista_fin))
	     (setq lista_fin (append lista_fin (list item)))
	   )
    )
  )
  lista_fin
)

;;Retorna uma string contendo apenas uma ocorrencia de cada caractere de uma string
(defun strocs (textval	 /	   len	     len1      cara
	       lista	 quant_lista	     item      lista_fin
	       ll	 conta
	      )
  (setq len (strlen textval))
  (setq len1 0)
  (repeat len
    (progn (setq len1 (1+ len1))
	   (setq cara (substr textval len1 1))
	   (setq lista (append lista (list cara)))
    )
  )
  (setq quant_lista (length lista))
  (repeat quant_lista
    (progn (setq item (car lista))
	   (setq lista (cdr lista))
	   (if (not (member item lista_fin))
	     (setq lista_fin (append lista_fin (list item)))
	   )
    )
  )
  (setq qll (length lista_fin))
  (setq ll "")
  (setq conta -1)
  (repeat qll
    (progn (setq conta (1+ conta))
	   (setq ll (strcat ll (nth conta lista_fin)))
    )
  )
  (if (= (strlen ll) 0)
    (setq ll nil)
  )
  ll
)

;;Retorna uma string contendo apenas os caracteres que se repetem em uma string
(defun strscr (textval	 /	   len	     len1      cara
	       lista	 quant_lista	     item      lista_fin
	       lista_fin1	   ll	     conta     qll
	       conta
	      )
  (setq len (strlen textval))
  (setq len1 0)
  (repeat len
    (progn (setq len1 (1+ len1))
	   (setq cara (substr textval len1 1))
	   (setq lista (append lista (list cara)))
    )
  )
  (setq quant_lista (length lista))
  (repeat quant_lista
    (progn (setq item (car lista))
	   (setq lista (cdr lista))
	   (if (not (member item lista_fin))
	     (setq lista_fin (append lista_fin (list item)))
	     (if (not (member item lista_fin1))
	       (setq lista_fin1 (append lista_fin1 (list item)))
	     )
	   )
    )
  )
  (setq qll (length lista_fin1))
  (setq ll "")
  (setq conta -1)
  (repeat qll
    (progn (setq conta (1+ conta))
	   (setq ll (strcat ll (nth conta lista_fin1)))
    )
  )
  (if (= (strlen ll) 0)
    (setq ll nil)
  )
  ll
)

;;Retorna uma lista contendo apenas os caracteres que se repetem em uma string
(defun strlcr (textval	  /	     len	len1	   cara
	       lista	  quant_lista		item	   lista_fin
	       lista_fin1
	      )
  (setq len (strlen textval))
  (setq len1 0)
  (repeat len
    (progn (setq len1 (1+ len1))
	   (setq cara (substr textval len1 1))
	   (setq lista (append lista (list cara)))
    )
  )
  (setq quant_lista (length lista))
  (repeat quant_lista
    (progn (setq item (car lista))
	   (setq lista (cdr lista))
	   (if (not (member item lista_fin))
	     (setq lista_fin (append lista_fin (list item)))
	     (if (not (member item lista_fin1))
	       (setq lista_fin1 (append lista_fin1 (list item)))
	     )
	   )
    )
  )
  lista_fin1
)

;;Completa com zeros a esquerda de uma string de numero inteiro    "12"  ->  "0.00012"
(defun strdecr (textval quantidade / str ss)
  (if (and (/= "" textval) (/= nil textval))
    (progn (setq textval (rtos (atoi textval) 2 0))
	   (setq ss "")
	   (repeat (- quantidade (strlen textval))
	     (progn (setq ss (strcat ss "0")))
	   )
	   (setq str (strcat "0." ss textval))
    )
  )
  (if (= quantidade 0)
    (setq str textval)
  )
  (if (= textval "")
    (setq str "")
  )
  str
)

;;Remove um caractere de uma string
(defun strrem (textval strval /)
  (if (= 1 (strlen strval))
    (progn (setq quant (strlen textval))
	   (setq str_new "")
	   (setq conta 0)
	   (repeat quant
	     (progn (setq carac (substr textval 1 1))
		    (setq textval (substr textval 2))
		    (if	(= carac strval)
		      (setq carac "")
		    )
		    (setq str_new (strcat str_new carac))
	     )
	   )
	   str_new
    )
    (progn (prompt "\nO segundo parametro esta incorreto!\n")
	   (exit)
    )
  )
)

;;Soma duas strings numericas
(defun str+ (textval1 textval2 dec / textval num1 num2)
  (setq num1 (atof textval1))
  (setq num2 (atof textval2))
  (setq textval (rtos (+ num1 num2) 2 dec))
  textval
)

;;subtrai duas strings numericas
(defun str- (textval1 textval2 dec / textval num1 num2)
  (setq num1 (atof textval1))
  (setq num2 (atof textval2))
  (setq textval (rtos (- num1 num2) 2 dec))
  textval
)

;;multiplica duas strings numericas
(defun str* (textval1 textval2 dec / textval num1 num2)
  (setq num1 (atof textval1))
  (setq num2 (atof textval2))
  (setq textval (rtos (* num1 num2) 2 dec))
  textval
)

;;divide duas strings numericas
(defun str/ (textval1 textval2 dec / textval num1 num2)
  (setq num1 (atof textval1))
  (setq num2 (atof textval2))
  (setq textval (rtos (/ num1 num2) 2 dec))
  textval
)

;;acrescenta zeros a esquerda ->> 22 = 022
(defun strprez (textval q / strz result quant)
  (setq quant (strlen textval))
  (setq strz "")
  (repeat (- q quant) (progn (setq strz (strcat strz "0"))))
  (setq result (strcat strz textval))
  (setq result result)
)


;;acrescenta zeros a direita  ->> 22 = 2200
(defun strposz (textval q / strz result quant)
  (setq quant (strlen textval))
  (setq strz "")
  (repeat (- q quant) (progn (setq strz (strcat strz "0"))))
  (setq result (strcat textval strz))
  (setq result result)
)

;;verifica se a string contem apenas numeros - retorna T se sim e Nil se nao
(defun strcnum (textval / contador letra saida)
  (setq contador 1)
  (repeat (strlen textval)
    (progn (setq letra (substr textval contador 1))
	   (setq contador (1+ contador))
	   (if (not (<= 48 (ascii letra) 57))
	     (setq saida T)
	   )
    )
  )
  (if (= "" textval)
    (setq saida T)
  )
  (if (= saida T)
    nil
    T
  )
)

;;verifica se a string é numerica
(defun strnum (S / C conta s1)
  (setq s1 s)
  (setq C (substr S 1 1))
  (if (or (= C "+") (= C "-"))
    (setq S (substr S 2))
  )
  (while (> (strlen S) 0)
    (setq C (substr S 1 1)
	  S (substr S 2)
    )
    (if	(and (not (<= 48 (ascii C) 57)) (/= C "."))
      (setq S ""
	    C nil
      )
    )
    (IF	(= C ".")
      (progn (if (= conta nil)
	       (setq conta 0)
	     )
	     (setq conta (1+ conta))
      )
    )
  )
  (if (or (= "." (substr s1 (strlen s1))) (= "." (substr s1 1 1)))
    (setq conta 2)
  )
  (if (> (strlen s1) 1)
    (progn
      (if (or (= "-." (substr s1 1 2)) (= "+." (substr s1 1 2)))
	(setq conta 2)
      )
    )
  )
  (if (> conta 1)
    nil
    (if	C
      'T
      nil
    )
  )
)

;;remove todos os espacos de uma string
(defun strsesp (S / C R)
  (setq R "")
  (while (> (strlen S) 0)
    (setq C (substr S 1 1)
	  S (substr S 2)
    )
    (if	(/= C " ")
      (setq R (strcat R C))
    )
  )
  R
)

;;verifica se a string contem espaco - retorna T se sim e Nil se nao
(defun strcesp (textval / contador letra saida)
  (setq contador 1)
  (repeat (strlen textval)
    (progn (setq letra (STRCASE (substr textval contador 1)))
	   (setq contador (1+ contador))
	   (if (= letra " ")
	     (setq saida T)
	   )
    )
  )
  (if (= saida T)
    T
    nil
  )
)

;;verifica se a string contem APENAS letras - retorna T se sim e Nil se nao
(defun strclet (textval / contador letra saida)
  (setq contador 1)
  (repeat (strlen textval)
    (progn (setq letra (strcase (substr textval contador 1)))
	   (setq contador (1+ contador))
	   (if (not (<= 65 (ascii letra) 90))
	     (setq saida T)
	   )
    )
  )
  (if (= "" textval)
    (setq saida T)
  )
  (if (= saida T)
    nil
    T
  )
)

;;verifica se a string contem caracteres de pontuacao - retorna T se sim e Nil se nao
(defun strcpon (textval / contador letra saida)
  (setq contador 1)
  (repeat (strlen textval)
    (progn (setq letra (strcase (substr textval contador 1)))
	   (setq contador (1+ contador))
	   (if (or (= letra "~")
		   (= letra "!")
		   (= letra "@")
		   (= letra "#")
		   (= letra "$")
		   (= letra "%")
		   (= letra "^")
		   (= letra "&")
		   (= letra "*")
		   (= letra "(")
		   (= letra ")")
		   (= letra "_")
		   (= letra "+")
		   (= letra "`")
		   (= letra "-")
		   (= letra "=")
		   (= letra "[")
		   (= letra "]")
		   (= letra "{")
		   (= letra "}")
		   (= letra ";")
		   (= letra "'")
		   (= letra ":")
		   (= letra "\"")
		   (= letra "<")
		   (= letra ",")
		   (= letra ">")
		   (= letra ".")
		   (= letra "?")
		   (= letra "/")
		   (= letra "|")
		   (= letra "\\")
	       )
	     (setq saida T)
	   )
    )
  )
  (if (= saida T)
    T
    nil
  )
)

;;Acrescenta um caractere no inicio e no final de uma string
(defun strentr+	(textval caractere /)
  (if (/= (substr textval 1 1) caractere)
    (setq textval (strcat caractere textval))
  )
  (if (/= (substr textval (strlen textval)) caractere)
    (setq textval (strcat textval caractere))
  )
  textval
)

;;Remove um caractere do inicio e do final de uma string
(defun strentr-	(textval caractere /)
  (if (= (substr textval 1 1) caractere)
    (setq textval (substr textval 2))
  )
  (if (= (substr textval (strlen textval)) caractere)
    (setq textval (substr textval 1 (1- (strlen textval))))
  )
  textval
)

;;FUNÇOES PARA XDATA 		      	  ***********************


;; cria link entre entidades do autocad
(defun mlink (ent_1 ent_2 appname / bit_1 bit_2 dxf_1 dxf_2 id_1 id_2)
  (regapp appname)
  (setq dxf_1 (entget ent_1))
  (setq dxf_2 (entget ent_2))
  (setq id_1 (cdr (assoc 5 dxf_1)))
  (setq id_2 (cdr (assoc 5 dxf_2)))
  (setq bit_1 (append dxf_1 (list (list -3 (list appname (cons 1005 id_2))))))
  (setq bit_2 (append dxf_2 (list (list -3 (list appname (cons 1005 id_1))))))
  (entmod bit_1)
  (entmod bit_2)
(princ)
)


;;Rotina para retornar entidades vinculadas
(defun vlink (ent appname / res2 bit dxf res)
  (setq dxf (entget ent (list appname)))
  (setq bit (assoc -3 dxf))
  (setq bit (cdr (cadadr bit)))
  (if bit
    (setq res (handent bit))
  )
  (if res
    (progn
      (setq dxf (entget res (list appname)))
      (setq bit (assoc -3 dxf))
      (setq bit (cdr (cadadr bit)))
      (if bit
        (setq res2 (handent bit))
      )
    )
  )
  (if (equal res2 ent)
    res
    nil
  )
)





;; retorna um grupo se seleção com todos os elementos que contem o tag (xdata) informado.
(DeFun selxd (cod_xd / sel)
  (Setq
    sel	(ssget
	  "X"
	  (list (read (strcat "(-3 (" (chr 34) cod_xd (chr 34) "))")))
	)
  )
  sel
)

;; retorna um xdata
(defun XDGET (EN APID / EL)
  (setq EL (entget EN (list APID)))
  (cdadr (assoc -3 EL))
)

;;retorna os dados extendios de uma objeto
(DeFun GetData (type1 ename / elist extlist AppList SubList data XdCode
		XdData)
  (Setq data nil)
  (Setq elist (entget ename (List type1)))
  (Setq extlist (assoc -3 elist))
  (If extlist
    (progn (setq extlist (cdr extlist))
	   (setq applist (car extlist))
	   (Setq applist (cdr applist))
	   (while (and applist (null data))
	     (Setq sublist (car applist))
	     (Setq xdcode (car sublist))
	     (Setq xddata (cdr sublist))
	     (Cond ((= 1000 xdcode) (Setq data xddata)))
	     (Setq applist (cdr applist))
	   )
    )
  )
  data
)

;; associa dados extendidos a um objeto do autocad
(DeFun SetData (Type1 ename newdata / extlist oklist newlist name oneapp
		rlist)
  (RegApp type)
  (Setq elist (entget ename (list "*")))
  (Setq extlist (assoc -3 elist))
  (Setq	newlist	(list type
		      (cons 1002 "{")
		      (cons 1000 newdata)
		      (cons 1002 "}")
		)
  )
  (Setq newlist (List newlist))
  (If (null extlist)
    (progn (Setq rlist (cons -3 newlist))
	   (Setq elist (append elist (list rlist)))
    )
    (progn (Setq extlist (cdr extlist))
	   (While extlist
	     (Setq OneApp (car extlist))
	     (Setq ExtList (cdr extlist))
	     (Setq name (car OneApp))
	     (If (/= (strcase name) (strcase type1))
	       (If (null oklist)
		 (Setq oklist (List OneApp))
		 (Setq oklist (Append oklist (List OneApp)))
	       )
	     )
	   )
	   (Setq rlist (append oklist newlist))
	   (Setq rlist (cons -3 rlist))
	   (Setq elist (subst rlist (assoc -3 elist) elist))
    )
  )
  (If (< (xdroom ename) (xdsize newlist))
    (princ "\nLimite de espaco excedido.")
    (EntMod elist)
  )
  (princ)
)

;; retorna o nome de todos os dados extendidos de um objeto
(defun apps (ename	/	   ent1	      dado_ex	 lista_ex
	     lista_geral	   app	      nome	 nome1
	     lista_appid
	    )
  (setq ent1 (entget ename (list "*")))
  (setq dado_ex (assoc -3 ent1))
  (if (/= nil dado_ex)
    (progn
      (setq lista_ex (cdr dado_ex))
      (repeat (length lista_ex)
	(progn (setq app (car lista_ex))
	       (setq lista_ex (cdr lista_ex))
	       (setq nome (car app))
	       (setq app (cdr (cdr app)))
	       (setq app (reverse (cdr (reverse app))))
	       (setq lista_final (cons nome app))
	       (setq lista_geral (append lista_geral (list lista_final)))
	       (setq nome1 (car lista_final))
	       (setq lista_appid (append lista_appid (list nome1)))
	)
      )
    )
  )
  (if (= lista_appid nil)
    nil
    (setq lista_appid lista_appid)
  )
)

;;REMOVE UM XDATA
(defun rxd (en appx / ed xdo xd1 xd app)
  (setq ed (entget en '("*")))
  (setq xdo (assoc -3 ed))
  (setq xd1 (cdr xdo))
  (setq xd (list -3))
  (while (setq app (car xd1))
    (if	(not (equal (car app) appx))
      (setq xd (append xd (list app)))
      (setq xd (append xd (list (list appx))))
    )
    (setq xd1 (cdr xd1))
  )
  (setq ed (subst xd (assoc -3 ed) ed))
  (entmod ed)
  (entupd en)
  (princ)
)
