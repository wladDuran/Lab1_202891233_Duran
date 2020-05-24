#lang racket

;Simulador de Git
;Por Wladimir Duran
;Version 1.0


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



