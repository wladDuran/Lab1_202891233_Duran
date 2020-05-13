#lang racket

;Definicion TDA

;Representacion

;Zonas



;Workspace
;Lista de nombres de archivos
;Ej: '("Archivo1.rkt", "Archivo2.rkt", "README.txt")

;Index
;Lista de nombres de archivos
;Ej: '("Archivo1.rkt", "Archivo2.rkt", "README.txt")

;Local Repository
;Lista de nombres de archivos
;Ej: '("Archivo1.rkt", "Archivo2.rkt", "README.txt")

;Commit
;Lista con id, fecha, persona que hizo el commit y los nombres de los archivos que 
;Ej '(3456, '(23, 03, 18), "Juan Perez", '("Archivo1.rkt", "README.txt"))

;Remote repository
;Lista de listas con commits, agregando el id del commit padre de este
;Ej '('('(3456, 4567), '(23, 03, 18), "Juan Perez", '("Archivo1.rkt", "README.txt")), '('(4567, 0), '(20, 03, 18), "Carlos Santana", '("Archivo1.rkt", "README.txt")))



;Constructores

;Crea una lista con un archivo llamado README.txt, que contara como la zona workspace
;No hay input
;Sale una lista con un elemento llamado README.txt
(define crearWorkspace
    (lambda ()
        (list "README.txt")
    )
)



