#lang racket

;Simulador de Git
;Por Wladimir Duran
;Version 1.0


;Funciones auxiliares

;Buscar un string en una lista dada, retorna True si esta, y False si es que no esta
;Entra el nombre del string que se desea buscar y la lista
;Retorna un True o un False
(define buscarArchivoRep
    (lambda (archivoBuscar lista)
        (if (null? (cdr lista))
            #f
            (if (equal? archivoBuscar (car lista))
                #t
                (buscarArchivoRep archivoBuscar (cdr lista))
                )
            )
        )
    )
        


;Funciones de Git

;Funcion Git, funcion de orden superior para usar el resto de funciones utilizadas para mover elementos de un espacio a otro
;Entra una funcion y sus respectivos argumentos, asi como el TDA "zonas"
;Entrega la funcion que se introduce como argumento, aplicandolo al TDA zonas ingresado
  
(define git
    (lambda (funcion)
        (lambda (zonas)
            (funcion zonas)
            )
        )
    )


;Funcion recursiva de git pull (cola), , saca los elementos del remote repository y los deja en el remote repository, actualizando los archivos que compartan el mismo nombre
;Se ingresa la lista del TDA workspace y remote repository, ademas de ingresar un 0 para la recursion y la cantidad de elementos en el workspace
;Sale la lista del workspace con los elementos del repositorio remoto
(define pullEnmascarar
    (lambda (listaRemote listaWorkSpace cantidadElemRemote cantidadElemWork)
        (if (= cantidadElemRemote cantidadElemWork)
            listaWorkSpace
            (if (buscarArchivoRep (car listaRemote) listaWorkSpace)
                (pullEnmascarar (cdr listaRemote) listaWorkSpace cantidadElemRemote (+ cantidadElemWork 1))
                (pullEnmascarar (cdr listaRemote) (append (car listaRemote) listaWorkSpace) cantidadElemRemote (+ cantidadElemWork 1))
                )
            )
        )
    )



;Funcion git pull, saca los elementos del remote repository y los deja en el remote repository, actualizando los archivos que compartan el mismo nombre
;Se ingresa un TDA Zonas
;Sale un TDA zonas con los cambios respectivos a pull realizados
(define pull
    (lambda (zonas)
        (if (checkZonas zonas)
            (construirZonas (pullEnmascarar (selecRemoteRepository zonas) (selecWorkspace zonas) (longitud (selecRemoteRepository zonas)) 0) (selecIndex zonas) (selecLocalRepository zonas) (selecRemoteRepository zonas) (append selecHistorialComando "git pull"))
            #f
            )
        )
    )


;Funcion recursiva de git add (natural)
;Se ingresa la lista correspondiente al workspace y al index en el TDA zonas
;Sale la Lista de Index modificada con sus valores cambiados
(define addEnmascarar
    (lambda (listaWorkSpace listaIndex)
        (if (null? (car listaWorkSpace))
            listaIndex
            (if (buscarArchivoRep (car listaWorkSpace) listaIndex)
                (addEnmascarar (cdr listaWorkSpace) listaIndex)
                (addEnmascarar (cdr listaWorkSpace) (cons (car listaWorkSpace) listaIndex))
                )
            )
        )
    )


;Agrega una lista del tipo de dato archivo al TDA Zonas en el espacio Index
;Se ingresa la lista que se desea ingresar y el TDA Zonas
;Sale el TDA zonas modificado
(define add
    (lambda (listaArchivos)
        (lambda (zonas)
            (if (checkZonas zonas)
                (construirZonas (selecWorkspace zonas) (addEnmascarar listaArchivos (selecIndex zonas)) (selecLocalRepository zonas) (selecRemoteRepository zonas) (cons (selecHistorialComando zonas) "git add"))
                #f
                )
            )
        )
    )



;Agrega los elementos del Index a una lista para luego transofrmarlos en commit
;Entra la lista de archivos de la zona Index y Local Repository
;Sale la lista para el Local Repository modificada
(define commitEnmascarar
    (lambda (listaIndex listaLocalRepository)
        (if (null? (car listaIndex))
            listaLocalRepository
            (commitEnmascarar (cdr listaIndex) (cons (car listaIndex) listaLocalRepository))
            )
        )
    )


;Mueve los elementos del Index al local repository en forma de commit
;Entra el mensaje que se quiere colocar en el commit y el TDA zonas
;Sale el TDA Zonas con las modificaciones pertinentes a la funcion
(define commit
    (lambda (mensaje)
        (lambda (zonas)
            (if (checkZonas zonas)
                (construirZonas (selecWorkspace zonas) (list ) (construirCommit (selecIdCommit (car (selecRemoteRepository zonas))) (list 01 06 2020) "Usuario" (commitEnmascarar (selecIndex zonas) (selecLocalRepository zonas)) mensaje) (selecRemoteRepository zonas) (cons (selecHistorialComando zonas) "git commit"))
                #f
                )
            )
        )
    )


;Dado un dato tipo Commit, busca en una lista si es que este esta o no
;Entra el commit buscado y la lista en la que se desea buscar
;Sale un booleano sobre si existe o no en tal lista
(define buscarCommitIgual
    (lambda (commitBuscar listaBuscar)
        (if (null? (car listaBuscar))
            #f
            (if (= (selecIdCommit commit) (selecIdCommit (car listaBuscar)))
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
        (if (null? (car listaLocalRepository))
            listaLocalRepository
            (if (buscarCommitIgual (car listaLocalRepository) listaRemoteRepository)
                (pushEnmascarar (cdr listaLocalRepository) (cdr listaRemoteRepository))
                (pushEnmascarar (cdr listaLocalRepository) (cons (car listaLocalRepository) listaRemoteRepository))
                )
            )
        )
    )


;Mueve los Commits que existan del Local Repository al Remote Repository
;Entra un TDA zonas
;Sale el TDA zonas con sus elementos modificados
(define push
    (lambda (zonas)
        (if (checkZonas zonas)
            (construirZonas (selecWorkspace zonas) (list ) (selecLocalRepository zonas) (pushEnmascarar (selecLocalRepository zonas) (selecRemoteRepository zonas)) (cons (selecHistorialComando zonas) "git push"))
            #f
            )
        )
    )


;Concatena en un string separado por saltos de linea el contenido de una lista con archivos (workspace e index)
;Entra una lista con el contenido del workspace y un string sin elementos ("")
;Sale el string con el contenido de del workspace
(define mostrarArchivoLocal
    (lambda (listaArchivoLocal stringArchivoLocal)
        (if (null? (car listaArchivoLocal))
            stringArchivoLocal
            (mostrarArchivoLocal (cdr listaArchivoLocal) (string-append stringArchivoLocal (car listaArchivoLocal) "\n"))
            )
        )
    )


(define commit->string
    (lambda (listaCommits stringCommit)
        (string-append (number->string (selecIdCommit (car listaCommits))) (number->string (selecIdAnteriorCommit (car listaCommits))) )))

(define mostrarCommits
    (lambda (listaCommits stringCommits)
        (if (null? (car listaCommits))
            stringCommits
            (mostrarCommits (cdr listaCommits) (string-append (selecIdCommit (car listaCommits)) )))))

;'('(3456, 443), '(23, 03, 18), "Juan Perez", '("Archivo1.rkt", "README.md"), "Arreglado bug")

(define zonas->string
    (lambda (zonas)
        (string-append "WorkSpace:\n" (mostrarArchivoLocal (selecWorkspace zonas) "") "\n\n" 
        "Index:\n" (mostrarArchivoLocal (selecIndex zonas) "") "\n\n"
        "Local Repository:\n" ())))