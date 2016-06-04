; TP3 : Interprete de C
;Autor: Emanuel

; Funcion principal para correr un programa
(defun run (prog ent &optional (mem nil))
	(if (null prog) nil 
		(if (eq (caar prog) 'int) (run (cdr prog) ent (agregar (cdar prog) mem))
			(if (eq (caar prog) 'main) (ejec (cadar prog) ent mem)
				'falta_main	
			) 
		)
	)
)

; Agrega las variables con sus respectivos valores (si es que tiene asignado alguno) sino se los
; inicializa con cero.
(defun agregar (variables mem)
	(cond ((null (car variables)) nil)
		  ((null (cadr variables)) (cons (car variables) (cons 0 mem)))
		  ((eq (cadr variables) '=) (cons (car variables) (cons (caddr variables) (agregar (cdddr variables) mem))))
		  ( T  (cons (car variables) (cons 0 (agregar (cdr variables) mem))))
	)
)

; Modifica el valor de alguna variable en memoria
(defun modif (variable entrada mem)
	(cond  ((null mem) nil) 
		   ((eq variable (car mem)) (cons variable (cons entrada (cddr mem))))
		   ( T  (cons (car mem) (cons (cadr mem) (modif variable entrada (cddr mem)))))
	)
)

; Ejecuta el programa
(defun ejec (prog ent mem &optional (salida nil))
	(if (null prog) (list ent mem (reverse salida))
		(cond ((eq (caar prog) 'scanf)  (ejec (cdr prog) (cdr ent) (modif (cadar prog) (car ent) mem) salida)) 
			  ((eq (caar prog) 'printf) (ejec (cdr prog) ent mem (cons (evaluar (cdar prog) mem) salida)))
			  ((eq (cadar prog) '=)     (ejec (cdr prog) ent (modif (caar prog) (evaluar (cddar prog) mem) mem) salida))
			  
			  ((pertenece (cadar prog) '(+= -= *= /= %= ++ --)) (ejec (cons (list (caar prog) '= (caar prog) (buscar_operador (cadar prog))
			  																	  (if (pertenece (cadar prog) '(++ --)) 1 (cddr prog))
			  																)
			  																(cdr prog) 
			  														  )
			  														  ent mem salida	
			  													)
			  )

			  ((pertenece (caar prog) '(++ --)) (ejec (cons (reverse (car prog)) (cdr prog)) ent mem salida))

			  ((eq (caar prog) 'if) (if (not (eq (evaluar (cadar prog) mem) nil)) 
			  							(ejec (append (caddar prog) (cdr prog)) ent mem salida)
			  							(if (eq (length (car prog)) 5) 
			  								(ejec (append (car (cddddr (car prog))) (cdr prog)) ent mem salida)
			  								(ejec (cdr prog) ent mem salida)
			  							)
			  						)
			  )

			  ((eq (caar prog) 'while) (if (eq (evaluar (cadar prog) mem) nil)
			  							   (ejec (cdr prog) ent mem salida)
			  							   (ejec (append (caddar prog) prog) ent mem salida)
			  						   )
			  )

		)
	)
)

; Evalua una expresión, por ejemplo suma, resta, incremento, etc
(defun evaluar (exp mem &optional (operadores nil) (operandos nil))
	(if (null exp) (if (null operadores) 
				       ;(car operandos)
				       (if (or (numberp (car operandos)) (eq (car operandos) T)) 
				           (car operandos) 
				       	   (buscar_variable (car operandos) mem)
				       ) 
				       ;(evaluar nil mem (cdr operadores) (cons (operar (car operadores) (cadr operandos) (car operandos)) (cddr operandos)))
				       (evaluar nil mem (cdr operadores) (cons (operar (car operadores) (if (numberp (cadr operandos)) 
				       																		(cadr operandos) 
				       																		(buscar_variable (cadr operandos) mem)
				       																	) 
				       																	(if (numberp (car operandos)) 
				       																		(car operandos) 
				       																		(buscar_variable (car operandos) mem)
				       																	) 
				       										   ) 
				       										   (cddr operandos)
				       									 )
				       )	
		           )

				   (if (es_operador (car exp)) (if (null operadores)
				   								   (evaluar (cdr exp) mem (cons (car exp) operadores) operandos)
				   								   (if (> (peso (car exp)) (peso (car operandos)))
				   								   	   (evaluar (cdr exp) mem (cons (car exp) operadores) operandos)
				   								   	   ;(evaluar exp mem (cdr operadores) (cons (operar (car operadores) (cadr operandos) (car operandos)) (cddr operandos))) 
				   								   	   (evaluar exp mem (cdr operadores) (cons (operar (car operadores) (if (numberp (cadr operandos)) 
				       																										(cadr operandos) 
				       																										(buscar_variable (cadr operandos) mem)
				       																									) 
				       																									(if (numberp (car operandos)) 
				       																										(car operandos) 
				       																										(buscar_variable (car operandos) mem)
				       																									)
				   								   	   										   ) 
				   								   	   										   (cddr operandos)
				   								   	   									 )
				   								   	   ) 
				   								   ) 
				   							   )
				   							   (evaluar (cdr exp) mem operadores (cons (car exp) operandos))
				   )
	)
)

; Devuelve la operacion con el operador entre el operando1 y operando2
(defun operar (operador operando1 operando2)
	(cond ((eq operador '+) (+ operando1 operando2))
		  ((eq operador '-) (- operando1 operando2))
		  ((eq operador '*) (* operando1 operando2))
		  ((eq operador '/) (/ operando1 operando2))
		  ((eq operador '<) (< operando1 operando2))
		  ((eq operador '>) (> operando1 operando2))
		  ((eq operador '==) (eq operando1 operando2))
		  ( T nil)	
	)
)

; Devuelve el valor asociado a la variable buscando en memoria
(defun buscar_variable (variable mem)
	(cond ((null mem) nil)
		  ((eq variable (car mem)) (cadr mem))
		  ( T  (buscar_variable variable (cddr mem)))
	)
)


; Devuelve verdadero si el elemento pertenece al conjunto dado sino falso
(defun pertenece (elemento conjunto)
	(cond ((null conjunto) nil)
		  ((eq (car conjunto) elemento) T)
		  (T (pertenece elemento (cdr conjunto)))
	)
)

; Devuelve verdadero si el operador a evaluar es realmente un operador
(defun es_operador (operador &optional (operadores pesos_operadores))
	(cond ((null operadores) nil)
		  ((eq (caar operadores) operador) T)
		  ( T  (es_operador operador (cdr operadores)))
	)
)

; Devuelve el operador asociado (ejemplo (+=) ----> (+))
(defun buscar_operador (operador &optional (operadores lista_operadores))
	(cond ((null operadores) nil)
		  ((eq (caar operadores) operador) (cadar operadores))
		  ( T  (buscar_operador operador (cdr operadores)))
	)

)

; Devuelve una lista de operadores asociados
(defparameter lista_operadores
	'((+= +) (-= -) (++ +) (-- -) (*= *) (/= /) (%= %))
)

; Devuelve el peso del operador a evaluar
(defun peso (operador &optional (pesos pesos_operadores))
	(cond ((null pesos) nil)
		  ((eq (caar pesos) operador) (cadar pesos))
		  ( T  (peso operador (cdr pesos)))
	)
)

; Devuelve una lista con los pesos asociados a cada operador
(defparameter pesos_operadores
	'((+ 1) (- 1) (== 2) (< 2) (> 2) (* 3) (/ 4))
)

;****************************************************************************************************************;
; Devuelve un programa de ejemplo
(defparameter cargarProg1
	'((int a b = 5) (main ((scanf a) (b = b + a) (printf a + b) (if (a < b) ((printf b)) else ((printf a))))))
)

; Devuelve un programa de ejemplo
(defparameter cargarProg2
	'((int a b = 9) (main ((scanf a) (b = b + a) (printf a + b) (while (a < b) ((a ++) (printf a))) (if (a == 6) ((printf b + 89))))))

)