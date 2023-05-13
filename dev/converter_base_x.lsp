(defun int2base (i base / s a digits)
  (setq digits "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        s "")
  (if (< base 2) (setq base 2))
  (if (> base 36) (setq base 36))
  (while (> i 0)
    (setq a (rem i base)
          i (fix (/ i base))
          s (strcat (substr digits (+ 1 a 0) 1) s)))
  (if (equal s "") (setq s "0"))
  s)

(int2base 556354455 35)






;; agora preciso fazer o reverso
;;  ex: (base2int "FF" 16) ; deve retornar 255
