#lang racket

;Definicion TDA

;Representacion

;Zonas

;Workspace
;Lista de nombres de archivos
;Ej: '(Archivo1.rkt, Archivo2.rkt, README.txt)



;Constructores

;Crea una lista con un archivo llamado README
(define crearWorkspace
    (lambda ()
        (list "README.txt")
    )
)