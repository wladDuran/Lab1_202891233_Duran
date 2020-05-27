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


;Funcion recursiva de git pull (cola)
;Se ingresa la lista del TDA workspace y remote repository, ademas de ingresar un 0 para la recursion y la cantidad de elementos en el workspace
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
        (append (pullEnmascarar (selecRemoteRepository zonas) (selecWorkspace zonas) (longitud (selecRemoteRepository zonas)) 0) (selecIndex zonas) (selecLocalRepository zonas) (selecRemoteRepository zonas))
        )
    )

