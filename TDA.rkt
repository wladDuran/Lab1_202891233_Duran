#lang racket

;Definicion TDA

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




