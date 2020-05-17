#lang racket

;Definicion TDA


;Funciones auxiliares para el TDA


;Contar cantidad de elementos de una lista

(define longitud
    (lambda (lista)
        (if (null? lista)
        0
        (+ 1 (longitud (cdr lista)))
            )
        )
    )


;Representacion


;Workspace
;Lista de nombres de archivos
;Ej: '("Archivo1.rkt", "Archivo2.rkt", "README.md")

;Index
;Lista de nombres de archivos
;Ej: '("Archivo1.rkt", "Archivo2.rkt", "README.md")

;Local Repository
;Lista de nombres de archivos
;Ej: '("Archivo1.rkt", "Archivo2.rkt", "README.md")

;Commit
;Lista con id, fecha, persona que hizo el commit y los nombres de los archivos que 
;Ej '('(3456, 443), '(23, 03, 18), "Juan Perez", '("Archivo1.rkt", "README.md"))

;Remote repository
;Lista de listas con commits, agregando el id del commit padre de este
;Ej '('('(3456, 4567), '(23, 03, 18), "Juan Perez", '("Archivo1.rkt", "README.md")), '('(4567, 0), '(20, 03, 18), "Carlos Santana", '("Archivo1.rkt", "README.md")))

;Zonas
;Lista de los otros elementos representados en el orden de Workspace, Index, Local Repository y Remote Repository
;Ej '('("Archivo1.rkt", "Archivo2.rkt", "README.md"), '("Archivo1.rkt", "Archivo2.rkt", "README.md"), '("Archivo1.rkt", "Archivo2.rkt", "README.md"), '('('(3456, 0), '(23, 03, 18), "Juan Perez", '("Archivo1.rkt", "README.md"))))


;Constructores


;Crea un commit

(define construirCommit
	(lambda (id idAnterior dia mes ano nombre listaArchivos)
		(list (list id idAnterior) (list dia mes ano) nombre listaArchivos)
		)
	)



;Crea una lista que represente las zonas cuando uno crea su primer repositorio
;No hay input
;Sale una lista con un repositorio con todas las zonas por defecto, agregando el archivo README.md que git hub coloca por defecto en la creacion del repositorio 

(define crearRepositorio
	(lambda (nombreUsuario dia mes ano)
		(list (list "README.md"), (list ), (list "README.md"), (construirCommit 0 0 dia mes ano nombre (list "README.me")))
		)
	)


;Selectores


;Selecciona al archivo en la posicion n en las zonas locales (local repository, workspace e index)
;Ingresa el TDA que se quiere examinar, la posicion deseada y el valor para el primer elemento de la lista (0 o 1 dependiendo del gusto)
;Sale el contenido de esa posicion
(define posicionArchivo
	(lambda (zonaLocal n espacioActual) 
		(if (= n espacioActual)
			(car zonaLocal)
			(posicionArchivo (cdr zonaLocal) n (+ 1 espacioActual))
			)
		)
	)

;Seleccionar el workspace

(define selecWorkSpace
	(lambda (zonas)
		(car zonas)
		)
	)


;Pertenencia


;Pertenencia Local Repository, Workplace e Index
;Dado que todos comparten los mismos tipos de datos, se hara una funcion para las 3 zonas

(define checkArchivoLocal
	(lambda (zona)
		(if (null? zona)
			#t
			(if (string? (car zona))
				(checkArchivoLocal (cdr zona))
				#f)
			)
		)
	)



