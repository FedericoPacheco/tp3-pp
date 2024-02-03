; TP 3: paradigma funcional
; 		- Marcolini, Facundo
; 		- Pacheco Pilan, Federico Ignacio
; 	~ 2C, 2020 ~


; -------------------------------------------------
;1)
(define tablero-inicial-1 (lambda () '((1 . 3) (5 . 2) (1 . 1) (1 . 4) (3 . 5))))

; ejemplos adicionales
(define tablero-inicial-2 (lambda () '((1 . 5) (2 . 1) (2 . 3) (4 . 2) (4 . 3) (5 . 1) (5 . 2) (5 . 5))))
(define tablero-inicial-3 (lambda () '((1 . 4) (1 . 5) (2 . 2) (2 . 3) (3 . 1) (3 . 2) (5 . 1) (5 . 2) (5 . 4) (5 . 5))))
(define tablero-inicial-4 (lambda () '((3 . 1) (3 . 2) (3 . 3) (3 . 4) (3 . 5) (5 . 1) (5 . 2) (5 . 3) (5 . 4) (5 . 5))))

; -------------------------------------------------
;2)
(define encendida?
  (lambda (tablero i j)
    (if (fueraDeRango (cons i j))                      
        #f
        (miembro tablero (cons i j))
    )
  )
)

(define miembro
  (lambda (tablero luz)
    (if (null? tablero)
        #f
        (if (equal? (car tablero) luz)
            #t
            (miembro (cdr tablero) luz)
        )  
    )
  )
)

(define fueraDeRango
  (lambda (casilla)
    (if
         (or
             (> (i casilla) (filas))
             (< (i casilla) 1)
             (> (j casilla) (columnas))
             (< (j casilla) 1)
         )
         #t
         #f
    )
  )
)

; -------------------------------------------------
;3)
(define cambia-luz
  (lambda (tablero i j)
    (if (fueraDeRango (cons i j))
        tablero
        (if (encendida? tablero i j)
            (apagar tablero (cons i j))
            (encender tablero (cons i j))
        )
    )
  )
)

(define apagar         ;i.e. eliminar de la lista
  (lambda (tablero luz)
    (if (null? tablero)
        '()
        (if (equal? (car tablero) luz)
            (cdr tablero)
            (cons (car tablero) (apagar (cdr tablero) luz))
        )
    )
  )
)

(define encender        ;i.e. agregar a la lista
  (lambda (tablero luz)
    (cons luz tablero)
  )
)

; -------------------------------------------------
;4)
(define pulsa
  (lambda (tablero i j)
    (if (fueraDeRango (cons i j))  ;comprobar que la pulsacion sea valida
         tablero
         (cambia-luz
            (cambia-luz
                (cambia-luz
                    (cambia-luz
                        (cambia-luz
                            tablero
                            i
                            j
                        )
                        (- i 1)
                        j
                    )
                    (+ i 1)
                    j
                )
                i
                (- j 1)
            )
            i
            (+ j 1)
         )
     ) 
  )
)

; -------------------------------------------------
;5)
(define fin-del-juego?
  (lambda (tablero)
    (if (null? tablero)
        #t
        #f
    )
  )
)

; -------------------------------------------------
;6)
(define aplica-pulsaciones
  (lambda (tablero LPulsaciones)
    (if (null? LPulsaciones)
        tablero
        (aplica-pulsaciones
            (pulsa
                tablero
                (i (car LPulsaciones))
                (j (car LPulsaciones))
            )
            (cdr LPulsaciones)
        )
    )
  )
)

; -------------------------------------------------
;7)
(define pulsaciones-ganadoras
  (lambda (tablero)
    (pulsaciones-ganadoras-aux (list tablero '()))
  )
)

(define pulsaciones-ganadoras-aux
  (lambda (tableroYPulsaciones)
    (if (fin-del-juego?
           (car
               (barridoApagaLuces
                   tableroYPulsaciones
                   2
                   (filas)
               )
           )
        )
        (cadr
             (barridoApagaLuces
                 tableroYPulsaciones
                 2
                 (filas)
             )
        )
        (if
           (equal?
              'no-hay-solucion
              (reconocerPatron
                 (barridoApagaLuces
                    tableroYPulsaciones
                    2
                    (filas)
                 )
              )
           )
           'no-hay-solucion
           (cadr
               (barridoApagaLuces
                   (reconocerPatron
                       (barridoApagaLuces
                            tableroYPulsaciones
                            2
                            (filas)
                       )
                   )
                   2
                   (filas)
               )
           )
        )
    )
  )
)

; se mantiene una lista de dos posiciones de la forma '((tablero) (pulsaciones))
(define barridoApagaLuces                   
  (lambda (tableroYPulsaciones i m)
    (if (> i m)
        tableroYPulsaciones
        (barridoApagaLuces
            (barrerFila tableroYPulsaciones i 1 (columnas))
            (+ i 1)
            m
        )
    )
  )
)

(define barrerFila
  (lambda (tableroYPulsaciones i j n)
    (if (> j n)
        tableroYPulsaciones
        (if (encendida? (car tableroYPulsaciones) (- i 1) j)
            (barrerFila
                (list
                    (pulsa (car tableroYPulsaciones) i j)
                    (append (cadr tableroYPulsaciones) (list (cons i j)))
                )
                i
                (+ j 1)
                n
            )
            (barrerFila
                tableroYPulsaciones
                i
                (+ j 1)
                n
            )
        )
    )
  )
)

(define reconocerPatron
  (lambda (tableroYPulsaciones)
    (if (tablerosEquivalentes (car tableroYPulsaciones) '((5 . 1) (5 . 5)))
        (reconocerPatronAux tableroYPulsaciones '((1 . 1) (1 . 2)))

        (if (tablerosEquivalentes (car tableroYPulsaciones) '((5 . 2) (5 . 4)))
            (reconocerPatronAux tableroYPulsaciones '((1 . 1) (1 . 4)))

            (if (tablerosEquivalentes (car tableroYPulsaciones) '((5 . 1) (5 . 2) (5 . 3)))
                (reconocerPatronAux tableroYPulsaciones '((1 . 2)))

                (if (tablerosEquivalentes (car tableroYPulsaciones) '((5 . 3) (5 . 4) (5 . 5)))
                    (reconocerPatronAux tableroYPulsaciones '((1 . 4)))

                    (if (tablerosEquivalentes (car tableroYPulsaciones) '((5 . 1) (5 . 3) (5 . 4)))
                        (reconocerPatronAux tableroYPulsaciones '((1 . 5)))

                        (if (tablerosEquivalentes (car tableroYPulsaciones) '((5 . 2) (5 . 3) (5 . 5)))
                            (reconocerPatronAux tableroYPulsaciones '((1 . 1)))

                            (if (tablerosEquivalentes (car tableroYPulsaciones) '((5 . 1) (5 . 2) (5 . 4) (5 . 5)))
                                (reconocerPatronAux tableroYPulsaciones '((1 . 3)))
                                'no-hay-solucion
                            )
                        )
                    )
                 )
             )
        )                 
    )
  )
)

(define reconocerPatronAux
  (lambda (tableroYPulsaciones LPulsaciones)
    (list
         (aplica-pulsaciones (car tableroYPulsaciones) LPulsaciones)
         (append (cadr tableroYPulsaciones) LPulsaciones)
    )
  )
)

; puede que se tengan las mismas luces encendidas, pero en distinto orden en la lista
(define tablerosEquivalentes     
  (lambda (tablero1 tablero2)
    (if (null? tablero1)
        (if (null? tablero2)
            #t
            #f
        )
        (if (miembro tablero2 (car tablero1))
            (tablerosEquivalentes (cdr tablero1) (quitarElemento (car tablero1) tablero2))
            #f
        )
    )
  )
)

; se da por hecho que el elemento esta en la lista
(define quitarElemento
  (lambda (E L)
    (if (equal? E (car L))
        (cdr L)
        (cons (car L) (quitarElemento E (cdr L)))
    )
  )
)

; -------------------------------------------------
; Miscelaneo

(define filas (lambda() 5))
(define columnas (lambda() 5))
(define i (lambda(luz) (car luz)))
(define j (lambda(luz) (cdr luz)))