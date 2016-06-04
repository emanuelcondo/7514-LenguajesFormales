# 7514-LenguajesFormales

Trabajos Prácticos en Lenguaje Lisp

* **Trabajo Práctico 1**: Desarrollar un intérprete de Lisp en Lisp para evaluar expresiones.

* **Trabajo Práctico 2**: Desarrollar un algoritmo en Lisp para encontrar camino mínimo entre
                          dos nodos.

* **Trabajo Práctico 3**: Desarrollar un intérprete de C (limitado) en Lisp.

Instalación de Intérprete Lisp
===============================================================================
    [xlisp] (http://www.almy.us/xlisp.html)

Comandos de ejecución
===============================================================================
* **Trabajo Práctico 1**: ejecutar los siguiente comandos (contiene 3 ejemplos)
    (load "tp1")
    (setq exp cargarExp1)
    (setq amb cargarAmb1)
    (evaluar exp amb)

* **Trabajo Práctico 2**: ejecutar los siguiente comandos (contiene 3 ejemplos)
    (load "tp2")
    (setq grafo cargarGrafo1)
    (gps 'a 'd grafo)     // Devuelve todos los caminos de 'a hasta 'd
    (gps_min 'a 'd grafo) // Devuelve el camino mínimo desde 'a hasta 'd

* **Trabajo Práctico 3**: ejecutar los siguiente comandos (contiene 2 ejemplos)
    (load "tp3")
    (setq prog cargarProg1)
    (run prog '(5 1))
