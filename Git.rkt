#lang racket

;Simulador de Git
;Por Wladimir Duran 20.289.123-3
;Version 1.4

;Importar TDA externo

(require "TDA.rkt")

    


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




;Funcion git pull, saca los elementos del remote repository y los deja en el remote repository, actualizando los archivos que compartan el mismo nombre
;Se ingresa un TDA Zonas
;Sale un TDA zonas con los cambios respectivos a pull realizados
(define pull
    (lambda (zonas)
        (if (checkZonas zonas)
            (construirZonas (pullEnmascarar (selecArchivosCommit (car (selecRemoteRepository zonas))) (selecWorkSpace zonas) (longitud (selecRemoteRepository zonas)) 0) (selecIndex zonas) (selecLocalRepository zonas) (selecRemoteRepository zonas) (append (selecHistorialComando zonas) (list "git pull")))
            #f
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
                (construirZonas (selecWorkSpace zonas) (addEnmascarar listaArchivos (selecIndex zonas)) (selecLocalRepository zonas) (selecRemoteRepository zonas) (append (selecHistorialComando zonas) (list "git add")))
                #f
                )
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
                (construirZonas (selecWorkSpace zonas) (list ) (append (list (construirCommit (selecIdCommit (car (selecRemoteRepository zonas))) (selecFechaCommit (car (selecRemoteRepository zonas))) (selecAutorCommit (car (selecRemoteRepository zonas))) (selecIndex zonas) mensaje)) (selecLocalRepository zonas)) (selecRemoteRepository zonas) (append (selecHistorialComando zonas) (list "git commit")))
                #f
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
            (construirZonas (selecWorkSpace zonas) (list ) (selecLocalRepository zonas) (pushEnmascarar (selecLocalRepository zonas) (selecRemoteRepository zonas)) (append (selecHistorialComando zonas) (list "git push")))
            #f
            )
        )
    )



;Requerimientos extra


;Transforma un TDA Zonas dado a una representacion en string legible
;Entra un TDA Zonas Valido
;Sale un string con una representacion de los elementos del TDA zonas
(define zonas->string
    (lambda (zonas)
        (string-append "WorkSpace:\n" (mostrarArchivoLocal (selecWorkSpace zonas) "") "\n\n\n" 
        "Index:\n" (mostrarArchivoLocal (selecIndex zonas) "") "\n\n\n"
        "Local Repository:\n" (mostrarCommits (selecLocalRepository zonas) "") "\n\n\n" 
        "Remote Repository:\n" (mostrarCommits (selecRemoteRepository zonas) "") "\n\n\n")
        )
    )




;Entrega un string con informacion de la zona de trabajo (Cantidad de archivos en el index, cantidad de commits en el local repository y la rama actual)
;Entra la zona que se desea consultar
;Sale el string con la informacion deseada
(define status
    (lambda (zonas)
        (string-append "Cantidad Archivos Index: " (number->string (longitud (selecIndex zonas))) "\n"
        "Cantidad de commits en el local repository: " (number->string (longitud (selecLocalRepository zonas))) "\n"
        "Rama: Master")
        )
    )




;Muestra el Id, el Id anterior y el mensaje de los ultimos 5 commits del repositorio
;Entra un TDA zonas
;Sale un string con los Ids y mensajes de los 5 ultimos commits
(define log
    (lambda (zonas)
        (logMascara (selecRemoteRepository zonas) 0 "")
        )
    )




;Ejemplos de uso funciones


;Git
;(Git se debe usar previo a la aplicacion de cada funcion, por lo que sus argumentos varian dependiendo la que se desee usar)
;Por esta razon, los ejemplos de uso estaran con cada una de las funciones a continuacion
;((git log) '(("README.md")
; ()
;  (((1 0)
;    (15 6 20)
;    "Pedro Gonzalez"
;    ("README.me")
;    "Primer commit"))
;  (((1 0)
;    (15 6 20)
;    "Pedro Gonzalez"
;    ("README.me")
;    "Primer commit")) 
;    ()))