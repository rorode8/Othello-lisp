;En general se va a asumir que los espacios vacíos están representados con 0's, las piezas propias con 1's y las rivales con 2's

;ALPHA-BETA PRUNING




    
(defun alphabeta (nodo depth alpha beta maximizing-player-p i)
  (when (or (= depth 0) (gameOver nodo))
    (return-from alphabeta (evalua nodo i)))
  (if maximizing-player-p
      (let ((value -1099511627775))
        (dolist (child (movimientos nodo 1 2))
          (setf value (max value (alphabeta child (1- depth) alpha beta nil (+ i 1))))
          (setf alpha (max alpha value))
          (when (<= beta alpha)
            ;; beta cut-off
            (return)))
        value)
      (let ((value 1099511627775))
        (dolist (child (movimientos nodo 2 1))
          (setf value (min value (alphabeta child (1- depth) alpha beta t (+ i 1))))
          (setf alpha (min alpha value))
          (when (<= beta alpha)
            ;; alpha cut-off
            (return)))
        value)))

;;funcion predefinida para su uso en alphabetaAux
;;alphabetamax (contrario a su nombre) empieza minizando
;; idealmente cambiaremos el '3' por un parametro establecido acorde a la dificultad
(defun alphabetamax (nodo)
	(alphabeta nodo 3 -1099511627775 1099511627775 NIL 0)
)

;;nos regresa el indice del maxico en una lista
(defun maxindex (m lst i j)
	(if (null lst) j
		(if (> (first lst) m) (maxindex (first lst) (rest lst) (+ i 1) i)
			(maxindex m (rest lst) (+ i 1) j))
	)
)

;;corre alphabeta en todos los hijos del primer estado, nos devuelve el estado
;;con mayor puntos
(defun alphabetaAux (nodo)
	(let* 
		((movs (movimientos nodo 1 2))
		 (tuplas (mapcar #'alphabetamax movs))
		 (index (maxindex -1099511627775 tuplas 0 0))	
		)
		 (nth index movs)
		)
	
)







;FUNCIÓN FINAL



;(setq estado '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 2 0 0 0 0 0 0 2 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))		
;(setq estado '(1 0 0 0 0 1 1 1 1 2 0 0 0 1 2 2 1 0 0 0 0 0 0 0 0 2 2 1 2 0 0 0 0 1 1 2 1 0 0 0 0 0 1 1 1 1 2 0 0 0 0 0 0 2 2 2 1 0 0 0 0 0 0 2))		
;(alphabetaAux estado)
;(alphabeta estado 3 -1099511627775 1099511627775 T 0)

; FUNCIÓN DE EVALUACIÓN

(defun evalua (estado numMovimiento)
    (if (or (gameOver estado) (equal (movimientos estado 1 2) (movimientos estado 2 1))) (quienGana estado)
    (+
    (* (+ 312 36 (* 6.24 numMovimiento)) (edgEst estado numMovimiento))
    (* (if (> numMovimiento 25) (+ 75 numMovimiento) (+ 50 (* 2 numMovimiento))) (currMob estado))
    (* 99 (potMob estado)))))

;Estabilidad de orillas
;(setq estado '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63))
;(setq prueba '(1 0 1 1 1 0 0 0 2 2 0 1 2 1 0 0 0 0 0 1 1 1 2 2 2 2 2 1 2 0 1 1 1 1 0 2 2 0 0 0 0 0 0 0 0 0 1 0 0 2 0 0 1 0 2 0 1 0 0 0 0 0 0 0))
;(setq estado '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 2 0 0 0 0 0 0 2 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
(defun edgEst (estado numMovimiento) ;el estado es una lista de 64 elementos (0's 1's y 2's)
    ;Necesitamos descomponer el estado en las listas que nos interesan (4 orillas, 2 diagonales)
    (setq orillaIzq `(,(nth 0 estado) ,(nth 8 estado) ,(nth 16 estado) ,(nth 24 estado) ,(nth 32 estado) ,(nth 40 estado) ,(nth 48 estado) ,(nth 56 estado)) orillaTop (reverse (nthcdr 56 (reverse estado))) orillaDer `(,(nth 7 estado) ,(nth 15 estado) ,(nth 23 estado) ,(nth 31 estado) ,(nth 39 estado) ,(nth 47 estado) ,(nth 55 estado) ,(nth 63 estado)) orillaBot (nthcdr 56 estado) diagonalIzq `(,(nth 0 estado) ,(nth 9 estado) ,(nth 18 estado) ,(nth 27 estado) ,(nth 36 estado) ,(nth 45 estado) ,(nth 54 estado) ,(nth 63 estado)) diagonalDer `(,(nth 7 estado) ,(nth 14 estado) ,(nth 21 estado) ,(nth 28 estado) ,(nth 35 estado) ,(nth 42 estado) ,(nth 45 estado) ,(nth 56 estado)))
    ;Ahora hacemos el calculo de la P para cada esquina
    (setq P1 (calculaP (reverse orillaIzq) orillaBot (reverse diagonalDer) numMovimiento) P2 (calculaP orillaTop orillaIzq diagonalIzq numMovimiento) P3 (calculaP (reverse orillaTop) orillaDer diagonalDer numMovimiento) P4 (calculaP (reverse orillaDer) (reverse orillaBot) (reverse diagonalIzq) numMovimiento))
    ;Ahora tenemos que calcular el valor actual para cada esquina (número de piezas mias menos numero de piezas rivales).
    (setq valorActual1 (+ (evalOrilla (reverse orillaIzq)) (evalOrilla orillaBot)))
    (setq valorActual2 (+ (evalOrilla orillaIzq) (evalOrilla orillaTop)))
    (setq valorActual3 (+ (evalOrilla (reverse orillaTop)) (evalOrilla orillaDer)))
    (setq valorActual4 (+ (evalOrilla (reverse orillaDer)) (evalOrilla (reverse orillaBot))))
    ;Ahora para cada esquina no ocupada, calculamos el valor si el rival jugara en la esquina (incluso si sería un movimiento ilegal)
    (setq orillaIzqN (aplicaCambiosEsquina orillaIzq) orillaIzqNR (aplicaCambiosEsquina (reverse orillaIzq)) orillaDerN (aplicaCambiosEsquina orillaDer) orillaDerNR (aplicaCambiosEsquina (reverse orillaDer)) orillaTopN (aplicaCambiosEsquina orillaTop) orillaTopNR (aplicaCambiosEsquina (reverse orillaTop)) orillaBotN (aplicaCambiosEsquina orillaBot) orillaBotNR (aplicaCambiosEsquina (reverse orillaBot)))
    (setq valorNuevo1 (+ (evalOrilla orillaIzqNR) (evalOrilla orillaBotN)))
    (setq valorNuevo2 (+ (evalOrilla orillaIzqN) (evalOrilla orillaTopN)))
    (setq valorNuevo3 (+ (evalOrilla orillaTopNR) (evalOrilla orillaDerN)))
    (setq valorNuevo4 (+ (evalOrilla orillaDerNR) (evalOrilla orillaBotNR)))
    (round (+ (+ (* (- 1 P1) valorActual1) (* P1 valorNuevo1)) (+ (* (- 1 P2) valorActual2) (* P2 valorNuevo2)) (+ (* (- 1 P3) valorActual3) (* P3 valorNuevo3)) (+ (* (- 1 P4) valorActual4) (* P4 valorNuevo4)))))
    

(defun calculaP (lista1 lista2 lista3 numMovimiento)
    (cond
        ((/= (car lista1) 0) 0)
        ((or (tomaEsquina lista1) (tomaEsquina lista2) (tomaEsquina lista3)) 1)
        (t (- 1 (/ numMovimiento 120)))))

(defun tomaEsquina (lista)
    (cond
        ((eq (cadr lista) 1) (revisaSecuencia (cddr lista)))
        (t NIL)))

(defun revisaSecuencia (lst)
    (cond
        ((null lst) NIL)
        ((= (car lst) 1) (revisaSecuencia (cdr lst)))
        ((= (car lst) 2) T)
        (t NIL)))
        
;(setq suma 0)
;(defun cuentaContiene (elem lst)
;    (setq aux (member elem lst))
;    (cond
;        ((null aux) suma)
;        (t (incf suma) (cuentaContiene elem (cdr aux)))))

;(defun ocurrencias (elem lst)
;   (setq suma 0)
;   (cuentaContiene elem lst))
    
;(defun simulaCambiosEsquina(lst1 lst2)
;   (setq suma 0 cambios1 (auxSimulaCambiosEsquina(cdr lst1)))
;   (setq suma 0 cambios2 (auxSimulaCambiosEsquina(cdr lst2)))
;   (+ cambios1 cambios2))

;(defun auxSimulaCambiosEsquina (lst)
;   (cond
;       ((= (car lst) 1) (incf suma) (auxSimulaCambiosEsquina (cdr lst)))
;       (t suma)))

(defun aplicaCambiosEsquina (lista)
    (cond
        ((= (car lista) 0) (setf (nth 0 lista) 2) (setq lista1 (copy-tree lista) lista2 (copy-tree lista))
        (aplicaCambiosEsquinaAux (cdr lista) 1))
        (t lista)))

(defun aplicaCambiosEsquinaAux (lista i)
    (cond
        ((null lista) lista1)
        ((= (car lista) 1) (setf (nth i lista2) 2) (aplicaCambiosEsquinaAux (cdr lista) (+ i 1)))
        ((= (car lista) 2) lista2)
        (t lista1)))
        
        
(defun casillaEstable (lista n elem) ;n va del 0 al 7. Regresa el valor de la casilla n (que contiene al elemento elem) según su posición y estabilidad
    (setq lista2 (nthcdr (+ n 1) lista) lista1 (nthcdr (- (length lista) n) (reverse lista)))
    (setq aux1 (casillaEstableAux lista1 elem) aux2 (casillaEstableAux lista2 elem))
    ;Inicializamos las listas con los valores para cada ficha según su posición y estabilidad
    (setq valores '(50 1000 200 75 1000 200 -25 1200 200 0 233) indicador (ceiling (abs (- 3.5 n))))
    (cond
        ((or (= aux1 1) (= aux2 1)) (nth (- (* indicador 3) 3 -1) valores))
        ((= aux1 aux2) (nth (- (* indicador 3) 3 -2) valores)) ;ambos son espacios
        (t (nth (- (* indicador 3) 3) valores))))
        

(defun casillaEstableAux (lista elem); 1 es estable, 2 es espacio, 3 es rival
    (cond
        ((null lista) 1)
        ((= (car lista) 0) 2)
        ((= (car lista) elem) (casillaEstableAux (cdr lista) elem))
        (t 3)))

(defun evalOrilla (lista)
    (setq suma 0)
    (evalOrillaAux lista 0))

(defun evalOrillaAux (lista i)
    (cond
        ((= i 8) suma)
        ((= (nth i lista) 1) (incf suma (casillaEstable lista i 1)) (evalOrillaAux lista (+ i 1)))
        ((= (nth i lista) 2) (decf suma (casillaEstable lista i 2)) (evalOrillaAux lista (+ i 1)))
        (t (evalOrillaAux lista (+ i 1)))))

;Current mobility

(defun esMovimientoLegal (lista elem);lista está compuesta por las casillas subsecuentes a la analizada. Elem es el número del rival
    (cond
        ((or (null lista) (= (car lista) 0)) NIL)
        ((= (car lista) elem) (esMovimientoLegal (cdr lista) elem))
        (t T)))

;(defun currMob (estado)
;    (setq suma 0 estadoM `(,(nthcdr 0 (reverse (nthcdr (max (- (length estado) 8) 0) (reverse estado)))) ,(nthcdr 8 (reverse (nthcdr (max (- (length estado) 16) 0) (reverse estado)))) ,(nthcdr 16 (reverse (nthcdr (max (- (length estado) 24) 0) (reverse estado)))) ,(nthcdr 24 (reverse (nthcdr (max (- (length estado) 32) 0) (reverse estado)))) ,(nthcdr 32 (reverse (nthcdr (max (- (length estado) 40) 0) (reverse estado)))) ,(nthcdr 40 (reverse (nthcdr (max (- (length estado) 48) 0) (reverse estado)))) ,(nthcdr 48 (reverse (nthcdr (max (- (length estado) 56) 0) (reverse estado)))) ,(nthcdr 56 (reverse (nthcdr (max (- (length estado) 64) 0) (reverse estado))))))
    ;Armamos listas horizontales alrededor de la casilla i
;    (setq m (floor (/ i 8)) listaH (nth m estadoM) n (mod i 8))
;    (setq lista2H (nthcdr (+ n 1) listaH) lista1H (nthcdr (- (length listaH) n) (reverse listaH)))
    ;Armamos listas verticales
;    (setq listaV `(,(nth n (nth 0 estadoM)) ,(nth n (nth 1 estadoM)) ,(nth n (nth 2 estadoM)) ,(nth n (nth 3 estadoM)) ,(nth n (nth 4 estadoM)) ,(nth n (nth 5 estadoM)) ,(nth n (nth 6 estadoM)) ,(nth n (nth 7 estadoM))) lista2V (nthcdr (+ m 1) listaV) lista1V (nthcdr (- (length listaV) m) (reverse listaV)))
;    (setq lista2V (nthcdr (+ m 1) listaV) lista1V (nthcdr (- (length listaV) m) (reverse listaV)))
    ;Armamos listas diagonal \
;    (setq lstI NIL lstF NIL)
;    (setq lstI NIL lstF NIL listaDI (append (armaDiagonalIzqPrin n m estadoM) (armaDiagonalIzqFin n m estadoM)) lista2DI (nthcdr (+ (min n m) 1) listaDI) lista1DI (nthcdr (- (length listaDI) (min n m)) (reverse listaDI)))
;    (setq lista2DI (nthcdr (+ (min n m) 1) listaDI) lista1DI (nthcdr (- (length listaDI) (min n m)) (reverse listaDI)))
    ;Armamos listas diagonal /
;    (setq lstI NIL lstF NIL listaDD (append (armaDiagonalDerPrin n m estadoM) (armaDiagonalDerFin n m estadoM)) lista2DD (nthcdr (+ (min (- 7 n) m) 1) listaDD) lista1DD (nthcdr (- (length listaDD) (min (- 7 n) m)) (reverse listaDD)))
    
(defun currMob (estado)
    (setq suma1 0 suma2 0 estadoM `(,(nthcdr 0 (reverse (nthcdr (max (- (length estado) 8) 0) (reverse estado)))) ,(nthcdr 8 (reverse (nthcdr (max (- (length estado) 16) 0) (reverse estado)))) ,(nthcdr 16 (reverse (nthcdr (max (- (length estado) 24) 0) (reverse estado)))) ,(nthcdr 24 (reverse (nthcdr (max (- (length estado) 32) 0) (reverse estado)))) ,(nthcdr 32 (reverse (nthcdr (max (- (length estado) 40) 0) (reverse estado)))) ,(nthcdr 40 (reverse (nthcdr (max (- (length estado) 48) 0) (reverse estado)))) ,(nthcdr 48 (reverse (nthcdr (max (- (length estado) 56) 0) (reverse estado)))) ,(nthcdr 56 (reverse (nthcdr (max (- (length estado) 64) 0) (reverse estado))))))
    (currMobAux2 estado estadoM 0))

(defun currMobAux (estadoM i); Regresa 1 si solo yo puedo mover, 2 si solo el puede, 3 si los dos, 0 si ninguno
    (setq m (floor (/ i 8)) listaH (nth m estadoM) n (mod i 8))
    ;Armamos listas horizontales alrededor de la casilla i
    (setq lista2H (nthcdr (+ n 1) listaH) lista1H (nthcdr (- (length listaH) n) (reverse listaH)))
    ;Armamos listas verticales
    (setq listaV `(,(nth n (nth 0 estadoM)) ,(nth n (nth 1 estadoM)) ,(nth n (nth 2 estadoM)) ,(nth n (nth 3 estadoM)) ,(nth n (nth 4 estadoM)) ,(nth n (nth 5 estadoM)) ,(nth n (nth 6 estadoM)) ,(nth n (nth 7 estadoM))) lista2V (nthcdr (+ m 1) listaV) lista1V (nthcdr (- (length listaV) m) (reverse listaV)))
    ;Armamos listas diagonal \
    (setq lstI NIL lstF NIL listaDI (append (armaDiagonalIzqPrin n m estadoM) (armaDiagonalIzqFin n m estadoM)) lista2DI (nthcdr (+ (min n m) 1) listaDI) lista1DI (nthcdr (- (length listaDI) (min n m)) (reverse listaDI)))
    ;Armamos listas diagonal /
    (setq lstI NIL lstF NIL listaDD (append (armaDiagonalDerPrin n m estadoM) (armaDiagonalDerFin n m estadoM)) lista2DD (nthcdr (+ (min (- 7 n) m) 1) listaDD) lista1DD (nthcdr (- (length listaDD) (min (- 7 n) m)) (reverse listaDD)))
    ;Revisamos que sea movimiento legal para alguna de las 8 listas generadas (mi movimiento)
    (setq legal1 (or (esMovimientoLegal lista2H 2) (esMovimientoLegal lista1H 2) (esMovimientoLegal lista2V 2) (esMovimientoLegal lista1V 2) (esMovimientoLegal lista2DI 2) (esMovimientoLegal lista1DI 2) (esMovimientoLegal lista2DD 2) (esMovimientoLegal lista1DD 2)) legal2 (or (esMovimientoLegal lista2H 1) (esMovimientoLegal lista1H 1) (esMovimientoLegal lista2V 1) (esMovimientoLegal lista1V 1) (esMovimientoLegal lista2DI 1) (esMovimientoLegal lista1DI 1) (esMovimientoLegal lista2DD 1) (esMovimientoLegal lista1DD 1)))
    (cond
        ((and legal1 legal2) 3)
        ((not (null legal1)) 1)
        ((not (null legal2)) 2)
        (t 0)))
        
(defun currMobAux2 (estado estadoM i)
    (cond
        ((null estado) (round (* 1000 (/ (- suma1 suma2) (+ suma1 suma2 2)))))
        ((= (car estado) 0) (setq indicador (currMobAux estadoM i))
            (cond
                ((= indicador 1) (incf suma1 2))
                ((= indicador 2) (incf suma2 2))
                ((= indicador 3) (incf suma1) (incf suma2))
                (t NIL))
        (currMobAux2 (cdr estado) estadoM (+ i 1)))
        (t (currMobAux2 (cdr estado) estadoM (+ i 1)))))

(defun armaDiagonalIzqPrin (a b estadoM)
    (cond
        ((or (< a 0) (< b 0)) lstI)
        (t (push (nth a (nth b estadoM)) lstI) (decf a) (decf b) (armaDiagonalIzqPrin a b estadoM))))

(defun armaDiagonalIzqFin (a b estadoM)
    (cond
        ((or (> a 6) (> b 6)) (reverse lstF))
        (t (incf a) (incf b) (push (nth a (nth b estadoM)) lstF) (armaDiagonalIzqFin a b estadoM))))

(defun armaDiagonalDerPrin (a b estadoM)
    (cond
        ((or (> a 7) (< b 0)) lstI)
        (t (push (nth a (nth b estadoM)) lstI) (incf a) (decf b) (armaDiagonalDerPrin a b estadoM))))

(defun armaDiagonalDerFin (a b estadoM)
    (cond
        ((or (< a 1) (> b 6)) (reverse lstF))
        (t (decf a) (incf b) (push (nth a (nth b estadoM)) lstF) (armaDiagonalDerFin a b estadoM))))

;Potential mobility

(defun potMob (estado)
    (setq suma11 0 suma12 0 suma21 0 suma22 0 suma31 0 suma32 0)
    (revisaMatriz estado (copy-tree estado) 0))

(defun revisaMatriz (estado estadoAux i) ; suma1 = algunCuadroVacio, suma2 = algunaPiezaAdyacente, suma3 = numeroCuadrosVacios
    (cond
        ((null estadoAux) (round (* 1000 (/ (- (+ suma11 suma21 suma31) (+ suma12 suma22 suma32)) (+ suma11 suma12 suma21 suma22 suma31 suma32 2)))))
        ((= (car estadoAux) 0)
        (cond
            ((> (sumaAdyacencia i estado 2) 0) (incf suma11) (revisaMatriz estado (cdr estadoAux) (+ i 1))) ;hay alguna pieza adyaciente del rival
            ((> (sumaAdyacencia i estado 1) 0) (incf suma12) (revisaMatriz estado (cdr estadoAux) (+ i 1))) ;hay alguna pieza adyaciente mia
            (t (revisaMatriz estado (cdr estadoAux) (+ i 1)))))
        (t (setq aux (sumaAdyacencia i estado 0)) ;aux tiene el número de cuadros vacíos alrededor de la pieza
        (cond
            ((= aux 0) (revisaMatriz estado (cdr estadoAux) (+ i 1)))
            ((= (car estadoAux) 2) (incf suma21) (incf suma31 aux) (revisaMatriz estado (cdr estadoAux) (+ i 1)))
            (t (incf suma22) (incf suma32 aux) (revisaMatriz estado (cdr estadoAux) (+ i 1)))))))
      
(defun sumaAdyacencia (i estado elem)
    (setq suma 0 m (floor (/ i 8)) n (mod i 8) estadoM `(,(nthcdr 0 (reverse (nthcdr (max (- (length estado) 8) 0) (reverse estado)))) ,(nthcdr 8 (reverse (nthcdr (max (- (length estado) 16) 0) (reverse estado)))) ,(nthcdr 16 (reverse (nthcdr (max (- (length estado) 24) 0) (reverse estado)))) ,(nthcdr 24 (reverse (nthcdr (max (- (length estado) 32) 0) (reverse estado)))) ,(nthcdr 32 (reverse (nthcdr (max (- (length estado) 40) 0) (reverse estado)))) ,(nthcdr 40 (reverse (nthcdr (max (- (length estado) 48) 0) (reverse estado)))) ,(nthcdr 48 (reverse (nthcdr (max (- (length estado) 56) 0) (reverse estado)))) ,(nthcdr 56 (reverse (nthcdr (max (- (length estado) 64) 0) (reverse estado))))))
    (when (and (> n 0) (> m 0) (= (nth (- n 1) (nth (- m 1) estadoM)) elem)) (incf suma))
    (when (and (> n 0) (= (nth (- n 1) (nth m estadoM)) elem)) (incf suma))
    (when (and (> n 0) (< m 7) (= (nth (- n 1) (nth (+ m 1) estadoM)) elem)) (incf suma))
    (when (and (> m 0) (= (nth n (nth (- m 1) estadoM)) elem)) (incf suma))
    (when (and (< n 7) (> m 0) (= (nth (+ n 1) (nth (- m 1) estadoM)) elem)) (incf suma))
    (when (and (< n 7) (= (nth (+ n 1) (nth m estadoM)) elem)) (incf suma))
    (when (and (< n 7) (< m 7) (= (nth (+ n 1) (nth (+ m 1) estadoM)) elem)) (incf suma))
    (when (and (< m 7) (= (nth n (nth (+ m 1) estadoM)) elem)) (incf suma))
    suma)

;GAME OVER

(defun gameOver (estado); Flag se debe de manipular mientras se ejecuta: si detectamos que al expandir un nodo solo hay un nodo posible lo almacenamos en algun lugar (porque puede significar que no hay movimiento posible)
    (cond                    ; si al expandir ese otro nodo solo hay un escenario, entonces comparamos el almacenado con el nuevo, y si son iguales activamos flag (significa que ya no hay movimientos).Regresa NIL si no ha terminado, 0 si es empate, 1 si ganamos, 2 si perdemos
        ((null (member 0 estado)) T);Ya no hay espacios vacíos
        (t NIL)))
        
(defun ganador (estado); Para cuando ya acabo el juego determinar quién ganó
    (cond
        ((null estado) suma)
        ((= (car estado) 1) (incf suma) (ganador (cdr estado)))
        ((= (car estado) 2) (decf suma) (ganador (cdr estado)))
        (t (ganador (cdr estado)))))

(defun quienGana (estado)
    (setq gana (ganador estado))
    (cond
        ((> gana 0) 1099511627775)
        ((< gana 0) -1099511627775)
        (t 0)))

;POSIBLES MOVIMIENTOS

(defun movimientos (estado jugador rival)
    (setq sigNivel NIL estadoM `(,(nthcdr 0 (reverse (nthcdr (max (- (length estado) 8) 0) (reverse estado)))) ,(nthcdr 8 (reverse (nthcdr (max (- (length estado) 16) 0) (reverse estado)))) ,(nthcdr 16 (reverse (nthcdr (max (- (length estado) 24) 0) (reverse estado)))) ,(nthcdr 24 (reverse (nthcdr (max (- (length estado) 32) 0) (reverse estado)))) ,(nthcdr 32 (reverse (nthcdr (max (- (length estado) 40) 0) (reverse estado)))) ,(nthcdr 40 (reverse (nthcdr (max (- (length estado) 48) 0) (reverse estado)))) ,(nthcdr 48 (reverse (nthcdr (max (- (length estado) 56) 0) (reverse estado)))) ,(nthcdr 56 (reverse (nthcdr (max (- (length estado) 64) 0) (reverse estado))))) movs (recorreMatriz estado estadoM 0 jugador rival))
    (if (null movs) estado movs))

(defun recorreMatriz (estado estadoM i jugador rival)
    (cond
        ((null estado) sigNivel)
        ((= (car estado) 0) (setq edoMaux (copy-tree estadoM)) (ejecutaCambiosLegales edoMaux (listaEjes edoMaux i jugador rival) jugador i)
            (when (not (equal edoMaux estadoM)) (push (aplana edoMaux) sigNivel)) (recorreMatriz (cdr estado) estadoM (+ i 1) jugador rival))
        (t (recorreMatriz (cdr estado) estadoM (+ i 1) jugador rival))))
       
(defun aplana (edoMaux)
    (append (nth 0 edoMaux) (nth 1 edoMaux) (nth 2 edoMaux) (nth 3 edoMaux) (nth 4 edoMaux) (nth 5 edoMaux) (nth 6 edoMaux) (nth 7 edoMaux)))
    
(defun calculaCambiosLegales (lista elem1 elem2);lista está compuesta por las casillas subsecuentes a la analizada. Elem1 es mi número, elem2 el del rival. Regresa el número de tus piezas que cambian
    (cond
        ((or (null lista) (= (car lista) 0)) NIL)
        ((= (car lista) elem2) (incf suma) (calculaCambiosLegales (cdr lista) elem1 elem2))
        (t suma)))

(defun listaCambiosLegalesEje (lista jugador rival)
    (setq suma 0 cambios (calculaCambiosLegales lista jugador rival))
    (if (null cambios) 0 suma))
    
(defun listaEjes (edoMaux i jugador rival) ;Regresa la lista de los cambios (números) que se deben de ejecutar en esa celda
    (setq m (floor (/ i 8)) listaH (nth m edoMaux) n (mod i 8))
    ;Armamos listas horizontales alrededor de la casilla i
    (setq lista2H (nthcdr (+ n 1) listaH) lista1H (nthcdr (- (length listaH) n) (reverse listaH)))
    ;Armamos listas verticales
    (setq listaV `(,(nth n (nth 0 edoMaux)) ,(nth n (nth 1 edoMaux)) ,(nth n (nth 2 edoMaux)) ,(nth n (nth 3 edoMaux)) ,(nth n (nth 4 edoMaux)) ,(nth n (nth 5 edoMaux)) ,(nth n (nth 6 edoMaux)) ,(nth n (nth 7 edoMaux))) lista2V (nthcdr (+ m 1) listaV) lista1V (nthcdr (- (length listaV) m) (reverse listaV)))
    ;Armamos listas diagonal \
    (setq lstI NIL lstF NIL listaDI (append (armaDiagonalIzqPrin n m edoMaux) (armaDiagonalIzqFin n m edoMaux)) lista2DI (nthcdr (+ (min n m) 1) listaDI) lista1DI (nthcdr (- (length listaDI) (min n m)) (reverse listaDI)))
    ;Armamos listas diagonal /
    (setq lstI NIL lstF NIL listaDD (append (armaDiagonalDerPrin n m edoMaux) (armaDiagonalDerFin n m edoMaux)) lista2DD (nthcdr (+ (min (- 7 n) m) 1) listaDD) lista1DD (nthcdr (- (length listaDD) (min (- 7 n) m)) (reverse listaDD)))
    ;Ya tenemos las listas ahora checamos para cada dirección cuántas piezas se tienen que agregar si se juega en ese lugar (empieza en pi radianes y va en el sentido de las manecillas)
    (list (listaCambiosLegalesEje lista1H jugador rival) (listaCambiosLegalesEje lista1DI jugador rival) (listaCambiosLegalesEje lista1V jugador rival) (listaCambiosLegalesEje lista1DD jugador rival) (listaCambiosLegalesEje lista2H jugador rival) (listaCambiosLegalesEje lista2DI jugador rival) (listaCambiosLegalesEje lista2V jugador rival) (listaCambiosLegalesEje lista2DD jugador rival)))
    
(defun ejecutaCambiosLegales (edoMaux numCambios jugador i)
    (when (> (apply '+ numCambios) 0) (setf (nth n (nth m edoMaux)) jugador))
    (setq m (floor (/ i 8)) listaH (nth m edoMaux) n (mod i 8))
    (cambiosHorizontales1 edoMaux m n (pop numCambios) jugador)
    (cambiosDiagIzq1 edoMaux m n (pop numCambios) jugador)
    (cambiosVerticales1 edoMaux m n (pop numCambios) jugador)
    (cambiosDiagDer1 edoMaux m n (pop numCambios) jugador)
    (cambiosHorizontales2 edoMaux m n (pop numCambios) jugador)
    (cambiosDiagIzq2 edoMaux m n (pop numCambios) jugador)
    (cambiosVerticales2 edoMaux m n (pop numCambios) jugador)
    (cambiosDiagDer2 edoMaux m n (pop numCambios) jugador)
    edoMaux)
    
(defun cambiosHorizontales1 (edoMaux m n cuantos jugador)
    (cond
        ((or (null cuantos) (= cuantos 0)) edoMaux)
        (t (setf (nth (- n cuantos) (nth m edoMaux)) jugador) (cambiosHorizontales1 edoMaux m n (- cuantos 1) jugador))))
        
(defun cambiosHorizontales2 (edoMaux m n cuantos jugador)
    (cond
        ((or (null cuantos) (= cuantos 0)) edoMaux)
        (t (setf (nth (+ n cuantos) (nth m edoMaux)) jugador) (cambiosHorizontales2 edoMaux m n (- cuantos 1) jugador))))
        
(defun cambiosVerticales1 (edoMaux m n cuantos jugador)
    (cond
        ((or (null cuantos) (= cuantos 0)) edoMaux)
        (t (setf (nth n (nth (- m cuantos) edoMaux)) jugador) (cambiosVerticales1 edoMaux m n (- cuantos 1) jugador))))
        
(defun cambiosVerticales2 (edoMaux m n cuantos jugador)
    (cond
        ((or (null cuantos) (= cuantos 0)) edoMaux)
        (t (setf (nth n (nth (+ m cuantos) edoMaux)) jugador) (cambiosVerticales2 edoMaux m n (- cuantos 1) jugador))))
        
(defun cambiosDiagIzq1 (edoMaux m n cuantos jugador)
    (cond
        ((or (null cuantos) (= cuantos 0)) edoMaux)
        (t (setf (nth (- n cuantos) (nth (- m cuantos) edoMaux)) jugador) (cambiosDiagIzq1 edoMaux m n (- cuantos 1) jugador))))

(defun cambiosDiagIzq2 (edoMaux m n cuantos jugador)
    (cond
        ((or (null cuantos) (= cuantos 0)) edoMaux)
        (t (setf (nth (+ n cuantos) (nth (+ m cuantos) edoMaux)) jugador) (cambiosDiagIzq2 edoMaux m n (- cuantos 1) jugador))))
        
(defun cambiosDiagDer1 (edoMaux m n cuantos jugador)
    (cond
        ((or (null cuantos) (= cuantos 0)) edoMaux)
        (t (setf (nth (+ n cuantos) (nth (- m cuantos) edoMaux)) jugador) (cambiosDiagDer1 edoMaux m n (- cuantos 1) jugador))))
        
(defun cambiosDiagDer2 (edoMaux m n cuantos jugador)
    (cond
        ((or (null cuantos) (= cuantos 0)) edoMaux)
        (t (setf (nth (- n cuantos) (nth (+ m cuantos) edoMaux)) jugador) (cambiosDiagDer2 edoMaux m n (- cuantos 1) jugador))))
        
