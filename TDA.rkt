#lang racket

;Exportar definiciones

(provide (all-defined-out))

;Definicion TDA


;Funciones auxiliares para el TDA


;Contar cantidad de elementos de una lista
;Entra una lista
;Sale el numero de elementos que esta contiene
(define longitud
    (lambda (lista)
        (if (null? lista)
        0
        (+ 1 (longitud (cdr lista)))
            )
        )
    )


;Buscar un string en una lista dada, retorna True si esta, y False si es que no esta
;Entra el nombre del string que se desea buscar y la lista
;Retorna un True o un False
(define buscarArchivoRep
    (lambda (archivoBuscar lista)
        (if (null? lista)
            #f
            (if (equal? archivoBuscar (car lista))
                #t
                (buscarArchivoRep archivoBuscar (cdr lista))
                )
            )
        )
    )



;Funcion mastara de log. Entrega un Id con los Ids y el mensaje de los 5 ultimos commits
;Entra una lista que corresponda al remote repository, un 0 y un string vacio
;Sale el string con los ids y mensajes de los ultimos 5 commits
(define logMascara
    (lambda (listaRemoteRepository cantidadCommits stringCommits)
        (if (or (= cantidadCommits 5) (null? listaRemoteRepository))
            stringCommits
            (logMascara (cdr listaRemoteRepository) (+ cantidadCommits 1) (string-append stringCommits "Id: " (number->string (selecIdCommit (car listaRemoteRepository))) " "
            "Id Commit Anterior: " (number->string (selecIdAnteriorCommit (car listaRemoteRepository))) " "
            "Mensaje: " (selecMensajeCommit (car listaRemoteRepository)) "\n\n"))
            )
        )
    )
	



;///Representacion\\\


;Workspace
;Lista de nombres de archivos
;Ej: '("Archivo1.rkt" "Archivo2.rkt" "README.md")

;Index
;Lista de nombres de archivos
;Ej: '("Archivo1.rkt" "Archivo2.rkt" "README.md")

;Local Repository
;Lista de nombres de archivos
;Ej: '(((3456 4567) (23 03 18) "Juan Perez" ("Archivo1.rkt" "README.md") "Agregada funcion leer") ((4567 0) (20 03 18) "Carlos Santana" ("Archivo1.rkt" "README.md") "Primer commit")))

;Commit
;Lista con id, fecha, persona que hizo el commit, los nombres de los archivos que contiene y mensaje adjunto al commit
;Ej '((3456 443) (23 03 18) "Juan Perez" ("Archivo1.rkt" "README.md") "Arreglado bug")

;Remote repository
;Lista de listas con commits, agregando el id del commit padre de este
;Ej '(((3456 4567) (23 03 18) "Juan Perez" ("Archivo1.rkt" "README.md") "Agregada funcion leer") ((4567 0) (20 03 18) "Carlos Santana" ("Archivo1.rkt" "README.md" "Primer commit")))

;Registro historico
;Registro de los comandos usados sobre el TDA zonas en orden de izquierda a derecha, siendo derecha el mas nuevo, expresado como una lista de strings
;Ej '("git pull" "git add" "git commit")

;Zonas
;Lista de los otros elementos representados en el orden de Workspace, Index, Local Repository y Remote Repository
;Ej '(("Archivo1.rkt" "Archivo2.rkt" "README.md") ("Archivo1.rkt" "Archivo2.rkt" "README.md") (((3456 0) (23 03 18) "Juan Perez" ("Archivo1.rkt" "README.md") "Agregada funcion leer")) (((3456 0) (23 03 18) "Juan Perez" ("Archivo1.rkt" "README.md") "Agregada funcion leer")) ("git add" "git commit"))


;///Constructores\\\


;Crea un commit
;Se ingresa el id del commit anterior a este, la fecha, el nombre del usuario y una lista con los archivos que van a estar en el
;Sale un TDA commit
(define construirCommit
	(lambda (idAnterior fecha nombre listaArchivos mensaje)
		(list (list (+ idAnterior 1) idAnterior) fecha nombre listaArchivos mensaje)
		)
	)



;Crea una lista que represente las zonas cuando uno crea su primer repositorio, con el mensaje "Primer commit" como mensaje por defecto
;Se ingresa el nombre del usuario y la fecha en la cual se realizo
;Sale una lista con un repositorio con todas las zonas por defecto, agregando el archivo README.md que git hub coloca por defecto en la creacion del repositorio 

(define crearRepositorio
	(lambda (nombreUsuario fecha)
		(list (list "README.md") (list ) (list (construirCommit 0 fecha nombreUsuario (list "README.me") "Primer commit")) (list (construirCommit 0 fecha nombreUsuario (list "README.me") "Primer commit")) (list ))
		)
	)
		
	


;Crea un TDA Zonas con los parametros de cada espacio de la lista que lo compone
;Entran 5 listas, cada una correspondiente a un espacio distinto del TDA zonas (workspace, index, localrepository, remote repository y el hisotrial de comandos)
;Sale una lista correspondiente al TDA Zonas
(define construirZonas
	(lambda (listaWorkSpace listaIndex listaLocalRepository listaRemoteRepository listaHistorial)
		(list listaWorkSpace listaIndex listaLocalRepository listaRemoteRepository listaHistorial)
		)
	)



;///Selectores\\\


;Selecciona al archivo en la posicion n de una lista dada
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
;Se ingresa el TDA zonas
;Sale el primer espacio del TDA
(define selecWorkSpace
	(lambda (zonas)
		(car zonas)
		)
	)

;Seleccionar el index
;Se ingresa el TDA zonas
;Sale el segundo espacio del TDA
(define selecIndex
	(lambda (zonas)
		(car (cdr zonas))
		)
	)

;Seleccionar local repository
;Se ingresa el TDA zonas
;Sale el tercer espacio del TDA
(define selecLocalRepository
	(lambda (zonas)
		(car (cdr (cdr zonas)))
		)
	)

;Seleccionar remote repository
;Se ingresa el TDA zonas
;Sale el cuarto espacio del TDA
(define selecRemoteRepository
	(lambda (zonas)
		(car (cdr (cdr (cdr zonas))))
		)
	)

;Selecciona el historial de comandos
;Se ingresa el TDA zonas
;Sale el quinto espacio del TDA, correspondiente al historial de comandos utilizados
(define selecHistorialComando
	(lambda (zonas)
		(car (cdr (cdr (cdr (cdr zonas)))))
		)
	)



;Seleccionar el id del commit
;Ingresa el TDA commit
;Sale el Id
(define selecIdCommit
	(lambda (commit)
		(car (car commit))
		)
	)

;Seleccionar el id anterior del commit
;Ingresa el TDA commit
;Sale el Id anterior
(define selecIdAnteriorCommit
	(lambda (commit)
		(car (cdr (car commit)))
		)
	)

;Seleccionar la lista con la fecha del commit
;Ingresa el TDA commit
;Sale una lista con la fecha en la que fue emitido
(define selecFechaCommit
	(lambda (commit)
		(car (cdr commit))
		)
	)


;Seleccionar el nombre de la persona que hizo el commit
;Ingresa el TDA commit
;Sale un string con el nombre del autor
(define selecAutorCommit
	(lambda (commit)
		(car (cdr (cdr commit)))
		)
	)

;Seleccionar lista de archivos del commit
;Ingresa el TDA commit
;Sale una lista con los nombres de los archivos en forma de string
(define selecArchivosCommit
	(lambda (commit)
		(car (cdr (cdr (cdr commit))))
		)
	)

;Selecciona el mensaje del autor del commit
;Ingresa un TDA commit
;Sale un string con el mensaje asociado a tal commit
(define selecMensajeCommit
	(lambda (commit)
		(car (cdr (cdr (cdr (cdr commit)))))
		)
	)




;///Pertenencia\\\


;Pertenencia Workplace, Index e Historial, dado que todos comparten los mismos tipos de datos, se hara una funcion para las 3 zonas
;Entra un TDA zona local, el cual puede ser tanto Workplace, Index e Historial
;Sale un booleano sobre si pertenece o no al TDA
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


;'('(3456, 443), '(23, 03, 18), "Juan Perez", '("Archivo1.rkt", "README.md"))

;Pertenencia del TDA commit
;Se ingresa un TDA commit
;Sale un booleano sobre si pertenece o no al TDA commit
(define checkCommit
	(lambda (commit)
		(and (number? (selecIdCommit commit))
			(number? (selecIdAnteriorCommit commit))
			(number? (car (selecFechaCommit commit)))
			(number? (car (cdr (selecFechaCommit commit))))
			(number? (car (cdr (cdr (selecFechaCommit commit)))))
			(string? (selecAutorCommit commit))
			(checkArchivoLocal (selecArchivosCommit commit))
			(string? (selecMensajeCommit commit))
			)
		)
	)


;Revisa si es que todos los elementso de una lista pertenecen al tipo de dato Commit
;Entra una lista
;Sale un booleano
(define checkListaCommit
	(lambda (listaCommits)
		(if (null? listaCommits)
			#t
			(if (checkCommit (car listaCommits))
				(checkListaCommit (cdr listaCommits))
				#f)
			)
		)
	)


;Revisa si es que una lista entregada pertenece al TDA Zonas
;Entra una lista
;Sale un booleano
(define checkZonas
	(lambda (zonas)
		(and (checkArchivoLocal (selecWorkSpace zonas))
			(checkArchivoLocal (selecIndex zonas))
			(checkListaCommit (selecLocalRepository zonas))
			(checkListaCommit (selecRemoteRepository zonas))
			(checkArchivoLocal (selecHistorialComando zonas))
			)
		)
	)


;//Modificadores\\


;Agregar archivos a un TDA de archivos, concatenando el contenido de este con lo que se le quiera agregar (Repository, Workplace e Index)
;Entra una lista con los archivos y otra con los archivos que se le desean agregar
;Sale una lista con los elementos anteriores combinados
(define agregarArchivos
	(lambda (archivos archivosNuevos)
		(append archivos archivosNuevos)
		)
	)


;//Operadores\\




;Funcion recursiva de git pull (cola), , saca los elementos del remote repository y los deja en el remote repository, actualizando los archivos que compartan el mismo nombre
;Se ingresa la lista del TDA workspace y remote repository, ademas de ingresar un 0 para la recursion y la cantidad de elementos en el workspace
;Sale la lista del workspace con los elementos del repositorio remoto
(define pullEnmascarar
    (lambda (listaRemote listaWorkSpace cantidadElemRemote cantidadElemWork)
        (if (= cantidadElemRemote cantidadElemWork)
            listaWorkSpace          ;Lista del ultimo commit
            (if (buscarArchivoRep (car listaRemote) listaWorkSpace)
                (pullEnmascarar (cdr listaRemote) listaWorkSpace cantidadElemRemote (+ cantidadElemWork 1))
                (pullEnmascarar (cdr listaRemote) (append (car (list listaRemote)) listaWorkSpace) cantidadElemRemote (+ cantidadElemWork 1))
                )
            )
        )
    )


;Funcion recursiva de git add (natural)
;Se ingresa la lista correspondiente al workspace y al index en el TDA zonas
;Sale la Lista de Index modificada con sus valores cambiados
(define addEnmascarar
    (lambda (listaWorkSpace listaIndex)
        (if (null? listaWorkSpace)
            listaIndex
            (if (buscarArchivoRep (car listaWorkSpace) listaIndex)
                (addEnmascarar (cdr listaWorkSpace) listaIndex)
                (addEnmascarar (cdr listaWorkSpace) (cons (car listaWorkSpace) listaIndex))
                )
            )
        )
    )


;Dado un dato tipo Commit, busca en una lista si es que este esta o no
;Entra el commit buscado y la lista en la que se desea buscar
;Sale un booleano sobre si existe o no en tal lista
(define buscarCommitIgual
    (lambda (commitBuscar listaBuscar)
        (if (null? listaBuscar)
            #f
            (if (= (selecIdCommit commitBuscar) (selecIdCommit (car listaBuscar)))
                #t
                (buscarCommitIgual commitBuscar (cdr listaBuscar))
                )
            )
        )
    )


;Mueve los Commits que existan del Local Repository al Remote Repository
;Entra la lista del local repository y el remote repository
;Sale la lista del local remote modificada con los elementos del local repository
(define pushEnmascarar
    (lambda (listaLocalRepository listaRemoteRepository)
        (if (null? listaLocalRepository)
            listaRemoteRepository
            (if (buscarCommitIgual (car listaLocalRepository) listaRemoteRepository)
                (pushEnmascarar (cdr listaLocalRepository) listaRemoteRepository)
                (pushEnmascarar (cdr listaLocalRepository) (append (list (car listaLocalRepository)) listaRemoteRepository))
                )
            )
        )
    )



;Concatena en un string separado por saltos de linea el contenido de una lista con archivos (workspace e index)
;Entra una lista con el contenido del archivo local y un string sin elementos ("")
;Sale el string con el contenido de del archivo local
(define mostrarArchivoLocal
    (lambda (listaArchivoLocal stringArchivoLocal)
        (if (null? listaArchivoLocal)
            stringArchivoLocal
            (mostrarArchivoLocal (cdr listaArchivoLocal) (string-append stringArchivoLocal (car listaArchivoLocal) "\n"))
            )
        )
    )


;Transforma un TDA del tipo commit a un string leible
;Ingresa la lista correspondiente a un TDA commit
;Sale un string legible de todas las zonas de un commit
(define commit->string
    (lambda (commit)
        (string-append 
        "Id: " (number->string (selecIdCommit commit)) "\n" 
        "Id Anterior: " (number->string (selecIdAnteriorCommit commit)) "\n" 
        "Fecha: " (number->string (car (selecFechaCommit commit))) "/" (number->string (car (cdr (selecFechaCommit commit)))) "/" (number->string (car (cdr (cdr (selecFechaCommit commit))))) "\n" 
        "Autor: " (selecAutorCommit commit) "\n" 
        "Archivos:\n" (mostrarArchivoLocal (selecArchivosCommit commit) "") "\n" 
        "Mensaje: " (selecMensajeCommit commit) "\n")
        )
    )



;Transforma los archivos de un TDA tipo commit a un string continuo con saltos de lineas
;Entra la lista de los commits y un string vacio
;Sale un string con todos los archivos de los commits como un string con saltos de lineas
(define mostrarCommits
    (lambda (listaCommits stringCommits)
        (if (null? listaCommits)
            stringCommits
            (mostrarCommits (cdr listaCommits) (string-append stringCommits "\n\n" (commit->string (car listaCommits))))
            )
        )
    )