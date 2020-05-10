#lang racket

;Simulador de Git
;Por Wladimir Duran
;Version 1.0


;Funciones de Git

;Funcion Git

(define git
  (lambda (funcionEscogida)
    (lambda (zonas)
      ;Funcion pull
      (if (equal? funcionEscogida "pull")
         (pull zonas)
         ;Funcion add
         (if (equal? funcionEscogida "add")
             (lambda (listaCambios)
               ((add listaCambios) zonas))
             ;Funcion commit
             (if (equal? funcionEscogida "commit")
                (lambda (mensaje)
                  ((commit mensaje) zonas))
                ;Funcion push
                (if (equal? funcionEscogida "push")
                    (push zonas)
                    #f)
                )
             )
         )
      )
    )
  )
                  