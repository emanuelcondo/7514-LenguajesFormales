; TP1 : INTERPRETE TCL
; Autor: Emanuel 

; Devuelve la evaluacion de una expresion
(defun evaluar (exp amb)
	(if (null exp) nil
		(if (atom exp) (if (or (numberp exp) (eq exp T)) exp (buscar exp amb))
			(cond ((eq (car exp) 'quote) (cadr exp))

				  ((eq (car exp) 'null) (null (evaluar (cadr exp) amb)))

				  ((eq (car exp) 'atom) (atom (evaluar (cadr exp) amb)))

				  ((eq (car exp) 'if) (if (evaluar (cadr exp) amb) 
				  						  (evaluar (caddr exp) amb) 
				  						  (evaluar (cadddr exp) amb) 
				  					  )
				  )
				  
				  ;((eq (car exp) 'cond) (cond ((evaluar (cadr exp) amb) (evaluar (caddr exp) amb))
				  ;							  (T    (evaluar (caddd exp) amb))
				  ;						)
				  ;)
				  ((eq (car exp) 'cond) (cond ((evaluar (caadr exp) amb) (evaluar (cadadr exp) amb))
				  							  (T    (evaluar (cons 'cond (cddr exp)) amb))
				  						)
				  )

				  ((eq (car exp) 'lambda) exp)

				  ((eq (car exp) 'and) (if (null (evaluar (cadr exp) amb)) nil
				  						   (evaluar (caddr exp) amb)
				  					   )
				  )

				  ((eq (car exp) 'or) (or (evaluar (cadr exp) amb) 
				  						  (evaluar (caddr exp) amb)
				  					   )
				  )

				  ((eq (car exp) 'not) (not (evaluar (cadr exp) amb)))

				  ((eq (car exp) 'mapcar) (mapcar (lambda (h) (aplicar (cadr exp) (list h) amb)) (evaluar_arg (caddr exp) amb)))

				  ((eq (car exp) 'eq) (eq (evaluar (cadr exp) amb) (evaluar (caddr exp) amb)))

				  (T (aplicar (car exp) (evaluar_arg (cdr exp) amb) amb)
				  ;(T (aplicar (car exp) (mapcar (lambda (x) (evaluar x amb)) (cdr exp)) amb)
				  )
			)
		)
	)
)

; Devuelve el valor asociado a un simbolo
(defun buscar (simbolo amb)
	(cond ((null simbolo) nil)
		  ((eq simbolo (car amb)) (cadr amb))
		  (T (buscar simbolo (cddr amb)))
	)
)

(defun evaluar_arg (exp amb)
	(if (eq (car exp) 'quote) (cadr exp)
		(mapcar (lambda (x) (evaluar x amb)) exp)
	)
)

(defun aplicar (fn lae amb)
	(if (atom fn) (cond ((eq fn 'cons) (cons (car lae) (cadr lae)))
						((eq fn 'list) (list (car lae) (cadr lae)))
						((eq fn 'append) (append (car lae) (cadr lae)))
						((eq fn 'car) (caar lae))
						((eq fn 'caar) (caaar lae))
						((eq fn 'cadr) (cadar lae))
						((eq fn 'cdr) (cdar lae))
						((eq fn 'cddr) (cddar lae))
						((eq fn 'cdar) (cdaar lae))
						((eq fn 'length) (length (car lae)))
						((eq fn '+) (+ (car lae) (cadr lae)))
						((eq fn '-) (- (car lae) (cadr lae)))
						((eq fn '*) (* (car lae) (cadr lae)))
						((eq fn '/) (/ (car lae) (cadr lae)))
						((eq fn '<) (< (car lae) (cadr lae)))
						((eq fn '>) (> (car lae) (cadr lae)))
						(T  (aplicar (buscar fn amb) lae amb))
				  )
				  (evaluar (caddr fn) (amb_extendido (cadr fn) lae amb))
	)
)

(defun amb_extendido (fn lae amb)
	(cond ((null fn) amb)
		  (T  (append (list (car fn) (car lae)) (amb_extendido (cdr fn) (cdr lae) amb)))
	)
)

;*************************************************************************************************************;
;EJEMPLOS

; (evaluar '(cons (suma 3 4) (cdar y)) '((y ((1 5) 4) x ((5 3 2) 6) suma (lambda (a b) (+ a b))))) -----> (7 5)
(defparameter cargarExp1
	'(cons (suma 3 4) (cdar y))
)

(defparameter cargarAmb1
	'(y ((1 5) 4) x ((5 3 2) 6) suma (lambda (a b) (+ a b)))
)

;--------------------------------------------------------------------------------------------------------------;
; (evaluar '(if (> (length x) (length y)) x y) '(y ((1 5) 4 (4 5)) x ((5 3 2) 6))) -----> ((1 5) 4 (4 5))
(defparameter cargarExp2
	'(if (> (length x) (length y)) x y)
)

(defparameter cargarAmb2
	'(y ((1 5) 4 (4 5)) x ((5 3 2) 6))
)

;--------------------------------------------------------------------------------------------------------------;
; (evaluar '(cond ((> (length x) (length y)) x) ((< (length x) (length y)) y)) '(y ((1 5) 4 (4 5)) x ((5 3 2) 6))) -----> ((5 3 2) 6)
(defparameter cargarExp3
    '(cond ((> (length x) (length y)) x) ((atom x) T) (T y))
)

(defparameter cargarAmb3
	'(y ((1 5) 4 (4 5)) x ((5 3 2) 6))
)

;--------------------------------------------------------------------------------------------------------------;
; (evaluar '(cons (car y) (cdr (cdr x))) '(x (z a p) y (1 2 3))) -----> (1 p)
(defparameter cargarExp4
	'(cons (car y) (cdr (cdr x)))
)

(defparameter cargarAmb4
	'(x (z a p) y (1 2 3))
)