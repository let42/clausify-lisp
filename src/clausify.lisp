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

;;;; Regole di sostituzione
;;;; 1. (not (not p)) -> p
;;;; 2. (not (and p q)) -> (or (not p) (not q))
;;;; 3. (not (or p q)) -> (and (not p) (not q))
;;;; 4. (not (every ?x (p ?x))) -> (every ?x (not (p ?x)))
;;;; 5. (not (exist ?x (p ?x))) -> (exist ?x (not (p ?x)))
;;;; 6. (implies p q) -> (or (not p) q)
;;;; 7. (or p (and q w)) -> (and (or p q) (or p w))
;;;; 9. (exist ?x (p ?x) -> (p sk<progressive>)
;;;; 10. (every ?y (exist ?x (p ?x ?y)) -> (every ?y (p sf<pr> ?y) ?y)

;;;; Funzioni di riconoscimento delle variabili costanti e dei simboli


(defun variablep (v)
  "Determina se l'input è una variabile"
  (and (symbolp v) (char= #\? (char (symbol-name v) 0))))

(defun nac(sym)
  "Prende in ingresso un simbolo e verifica se inizia con un numero
nac sta per number above characters"
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

(defun fbf-symp (s)
  "Verifica se s sia un simbolo che inizi con una lettera"
  (and (symbolp s) (not (nac s))))

(defun constp (c)
  "Verifica se c sia una costante come da sintassi espressa in cima
al listato."
  (or
    (numberp c)
    (fbf-symp c)))

(defun notp (fun)
  "Verifica se fun sia una funzione di negazione"
  (and (listp fun) (equal (car fun) 'not)))
	   
(defun andp (fun)
  "Verifica se fun sia una funzione di coniunzione"
  (and (listp fun) (equal (car fun) 'and)))

(defun orp (fun)
  "Verifica se fun sia una funzione di disgiunzione"
  (and (listp fun) (equal (car fun) 'or)))

(defun impliesp (fun)
  "Verifica se fun sia una funzione di implicazione"
  (and (listp fun) (equal (car fun) 'implies)))

(defun universalp (fun)
  "Verifica se fun sia una funzione di quantificazione universale"
  (and (listp fun) (equal (car fun) 'every) (variablep (cadr fun))))

(defun existentialp (fun)
  "Verifica se fun sia una funzione di quantificazione esistenziale"
  (and (listp fun) (equal (car fun) 'exist) (variablep (cadr fun))))

(defun predicatep (pred)
  (or
   (fbf-symp (car pred))))
   
       

(defun funp* (fun)
  (and
   (or (notp fun)
      (andp fun)
      (orp fun)
      (impliesp fun)
      (universalp fun)
      (existentialp fun)
      (fbf-symp (car fun)))
   ()))

;;;; ridefinire singolarmente le funzioni dei connettivi
;;;; funp* ricorsiva
;;;; predicatep ricorsiva
;;;; fbfp ricorsiva
;;;; is-horn verifica se la fbf o la cnf sia trasformabile in
;;;; una clausola di horn.
