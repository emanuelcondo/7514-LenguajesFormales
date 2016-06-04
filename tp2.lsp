;TP2 : GPS
;Autor: Emanuel

;Devuelve todos los caminos posibles del nodo "i" al nodo "f"
(defun gps (i f grafo &optional (tray (list (list i))))
	(if (null tray) nil
		(if (eq (caar tray) f) (cons (reverse (car tray)) (gps i f grafo (cdr tray)))
			(gps i f grafo (append (mapcar (lambda(x) (cons x (car tray))) 
											(diferencia (vecinos (caar tray) grafo) (car tray))
									)
								(cdr tray) 
							)
			)
		)
	)
)

;Devuelve el camino minimo del nodo "i" al nodo "f"
(defun gps_minimo (i f grafo &optional (tray (list (list i))))
	(if (null tray) nil
		(if (eq (caar tray) f) (camino_minimo (cons (reverse (car tray)) (gps i f grafo (cdr tray))) )
			(gps_minimo i f grafo (append (mapcar (lambda(x) (cons x (car tray))) 
											      (diferencia (vecinos (caar tray) grafo) (car tray))
									       )
							 	       (cdr tray) 
							       )
			)
		)
	)
)


; Devuelve los elementos del primer conjunto que no se encuentran en el segundo conjunto
(defun diferencia (conjunto1 conjunto2)
	(cond ((null conjunto1) nil)
		  ((pertenece (car conjunto1) conjunto2) (diferencia (cdr conjunto1) conjunto2))
		  (T (cons (car conjunto1) (diferencia (cdr conjunto1) conjunto2))) 
	)
)

; Devuelve verdadero si el elemento pertenece al conjunto dado sino falso
(defun pertenece (elemento conjunto)
	(cond ((null conjunto) nil)
		  ((eq (car conjunto) elemento) T)
		  (T (pertenece elemento (cdr conjunto)))
	)
)

; Devuelve los nodos vecinos(los adyascentes) a un nodo dado un grafo
(defun vecinos (nodo grafo)
	(cond ((null grafo) nil)
		  ((eq nodo (car grafo)) (cadr grafo))
		  (T (vecinos nodo (cddr grafo)))
	)

)

; Devuelve el camino minimo entre varios caminos
(defun camino_minimo (caminos)
	(cond ((null (car caminos)) nil)
		  ((null (cadr caminos)) (car caminos))
		  (  T   (minimo (car caminos) (camino_minimo (cdr caminos))))
	)
)

; Devuelve el minimo entre 2 caminos
(defun minimo (camino1 camino2)
	(cond ((< (length camino1) (length camino2)) camino1)
		  (T camino2)
	)
)

; Muestra por pantalla el recorrido que debe realizarse para llegar a destino
(defun mostrar_camino (camino)
	(if (null camino) nil
		(progn
		 (format t "Ir desde: ~s hasta: ~s~%" (car camino) (cadr camino))
		 (if (cddr camino)
		 	 (mostrar_camino (cdr camino))
		 )
		)	
	)
)

; Devuelve un grafo pequeño por los alrededores de la facultad
(defparameter cargarGrafo1
  (let* ((a 'eeuu_bal) (b 'ind_bal) (c 'chi_bal)
  		(d 'eeuu_pc) (e 'ind_pc) (f 'chi_pc)
  		(g 'eeuu_azop) (h 'ind_pc) (i 'chi_azop))
  		(list a (list d) b (list a) c (list b f) d (list e g) e (list b d f) f (list e i) g (list h) h (list e i)) 
  )
)

; Devuelve un grafo de prueba 2
; a = (EEUU,Balcarce)
; b = (Independencia,Balcarce
; c = (Chile,Balcarce)
; d = (EEUU,PaseoColon)
; e = (Independecia,PaseoColon)
; f = (Chile,PaseoColon)
; g = (EEUU,Azopardo)
; h = (Independencia,PaseoColon)
; i = (Chile,Azopardo)
(defparameter cargarGrafo2 
  '(a (d) b (a) c (b f) d (e g) e (b d f) f (e i) g (h) h (e i))		
)

; Devuelve un grafo de prueba 3
(defparameter cargarGrafo3 
  '(a (b c) b (a d e) c (a d e) d (b c e) e (b c d))
)

