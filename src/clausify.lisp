;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                              ;
;                                CLAUSIFY-LISP                                 ;
;                                                                              ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;MATRICOLA 735390 GUCCIARDI ALBERTO

;;;; Qui di seguito le definizioni delle componenti che costituiscono le fbf
;;;;
;;;; termine      ::= <costante> | <simbolo> | <funzione>
;;;; costante     ::= <numeri> | <simboli che cominciano con una lettera>
;;;; variabile    ::= <simboli che cominciano con un ?>
;;;; funzione     ::= '(' <simbolo> <termine>+ ')'
;;;; fbf          ::= <predicato>
;;;;              | <negazione> | <congiuinzione>
;;;;              | <disgiunzione> | <implicazione>
;;;;              | <universale> | <esistenziale>
;;;; predicato    ::= <simbolo che comincia con una lettera>
;;;;              | '(' <simbolo> <termine>+ ')'
;;;; negazione    ::= '(' not <fbf> ')'
;;;; congiunzione ::= '(' and <fbf> <fbf> ')'
;;;; disgiunzione ::= '(' or <fbf> <fbf> ')'
;;;; implicazione ::= '(' implies <fbf> <fbf> ')'
;;;; universale   ::= '(' every <variabile> <fbf> ')'
;;;; esistenziale ::= '(' exist <variabile> <fbf> ')'

;;;;cominciamo a trattare costanti


(defun variablep (v)
  (and (symbolp v) (char= #\? (char (symbol-name v) 0))))

(defun is-characterized-num (sym)
  (or (char= #\0 (char (symbol-name sym) 0))
      (char= #\1 (char (symbol-name sym) 0))
      (char= #\2 (char (symbol-name sym) 0))
      (char= #\3 (char (symbol-name sym) 0))
      (char= #\4 (char (symbol-name sym) 0))
      (char= #\5 (char (symbol-name sym) 0))
      (char= #\6 (char (symbol-name sym) 0))
      (char= #\7 (char (symbol-name sym) 0))
      (char= #\8 (char (symbol-name sym) 0))
      (char= #\9 (char (symbol-name sym) 0))))

(defun constp (c)
  (or
    (numberp c)
    (and (symbolp c) (equal (is-characterized-num c) 'nil))))
