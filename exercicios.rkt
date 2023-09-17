;; Nome(s): Gabriel Carvalho Stoffel

;; Atenção: Usar a linguagem "Intermediate Student"

;; ========================================================================== 

(define-struct figura (coord-x coord-y altura cor))
;; ===========
;; TIPO FIGURA:
;; ===========
;; Um elemento do conjunto Figura é
;;     (make-figura x y a c), onde
;;   x: Número, é a coordenada x do centro da figura
;;   y: Número, é a coordenada y do centro da figura
;;   a : Número, é a altura da figura
;;   c : Número, número que representa a cor da figura, de acordo com a função gera-cor 

;; ========================
;; FUNÇÃO GERA-COR:
;; ========================
;; gera-cor : Número -> String
;; Dado um número positivo, devolve uma de 5 cores: "red", "blue", "green", "yellow" ou "cyan".
;; Exemplos:
;;      (gera-cor 3) = "yellow"
;;      (gera-cor 55) = "red"
(define (gera-cor n)
  (cond
    [(= (remainder n 5) 0) "red"]
    [(= (remainder n 5) 1) "blue"]  
    [(= (remainder n 5) 2) "green"]
    [(= (remainder n 5) 3) "yellow"]
    [(= (remainder n 5) 4) "cyan"]))

;; ========================
;; FUNÇÃO DESENHA-TRIANGULO:
;; ========================
;; desenha-triangulo : Número String ->  Imagem
;; Obj.: Dados um tamanho de lado e uma cor, desenha um triângulo.
;; Exemplos:
;;     (desenha-triangulo 20 "red") = .
;;     (desenha-triangulo 50 "darkgreen") = .
(define (desenha-triangulo lado cor)
  (triangle lado "outline" cor))

;; ========================
;; FUNÇÃO SIERPINSKI:
;; ========================
;; sierpinski: Número String -> Imagem
;; Obj: Dados o tamanho do lado e uma cor, desenha um triângulo de Sierpinski
;; desta cor cujo lado do triângulo externo é o lado passado como argumento. 
;; Exemplos:
   ;; (sierpinski-sem-local 4 "blue") = . 
   ;; (sierpinski-sem-local 50 "red") = .
(define (sierpinski lado cor)
  (cond
       ;; se o lado for muito pequeno, desenhar um triângulo com o lado dado
       [(<= lado 5)  (desenha-triangulo lado cor)]
       ;; senão
       ;;      desenha um triângulo de sierpinksi com a metade do tamanho do lado
       ;;      e dá o nome de TRIANGULO para este desenho:
       [else (local
               (
                (define TRIANGULO (sierpinski (/ lado 2) cor))
               )
                ;; e monta a imagem do triângulo de sierpinski colocando um TRIANGULO
                ;; acima de dois outros TRIANGULOs:
               (above TRIANGULO
                      (beside TRIANGULO TRIANGULO)))]))

;; ========================
;; FUNÇÃO TAPETE-SIERPINSKI:
;; ========================
;; tapete-sierpinski: Número String -> Imagem
;; Obj: Dados o tamanho do lado e uma cor, desenha um tapete de Sierpinski
;; desta cor cujo lado do quadrado externo é o lado passado como argumento. 
;; Exemplos:
    ;; (tapete-sierpinski 4 "blue") = . 
   ;;  (tapete-sierpinski 50 "red") = .
(define (tapete-sierpinski lado cor)
    (cond
      ;; se n for zero, desenhar um quadrado da cor com lado 1
      [(<= lado 5) (square lado "solid" cor)]
      ;; senão
      [else
       (local [(define c (tapete-sierpinski  (/ lado 3) cor))
               (define p (square (/ lado 3) "solid" "white"))]
         ;; montar uma imagem com as seguinte linhas, uma acima da outra
         (above
                ;; três carpetes de sierpinski com tamanho n-1, um ao lado do outro
                (beside c c c)
                ;; dois carpetes de sierpinski com tamanho n-1 ao lado de um carpete preto com tamanho n-1
                (beside c p c)
                ;; três carpetesde sierpinski  com tamanho n-1, um ao lado do outro
                (beside c c c)))]))

;; ==========================================================================
;; 1: Função floco-de-neve
;; ==========================================================================
(define (floco-de-neve size color)
  (cond
    
    ;;se o tamanho é menor que 20
    [(< size 20) (overlay (rotate 120 (line 5 size color))
                         (rotate 0 (line 5 size color))
                         (rotate 60 (line 5 size color)))]
                ;;então gera um floco composto pela sobreposição de três retas
                    ;;em ângulos de 0, 60 e 120 graus;
    ;;caso contrário:
    [else
     
     (local
          ;; definições locais do floco de neve;
       [(define f1 (floco-de-neve (/ size 3) color))]
       
      ;;combina (sobrepõe):
       (overlay
        (rotate 120
                ;; o resultado de dois floco de neves em cada ponta de uma reta rotacionada em 120 graus 
        (above  f1 (line 2 size color) f1))
       (rotate 0
               ;; com o resultado de dois floco de neves em cada ponta de uma reta rotacionada em 0 graus
        (above  f1 (line 2 size color) f1))
        
       (rotate 60
               ;; e com o resultado de dois floco de neves em cada ponta de uma reta rotacionada em 60 graus
        (above  f1 (line 2 size color) f1))))
       
       ]
    ))


;;(floco-de-neve 200 "green")
;;(floco-de-neve 90 "red")




;; Argumentação de terminação da função floco-de-neve:

;; ==========================================================================
;; 2: Função desenha-tapete-sierpinski
;; ==========================================================================

;; 
(define figuraTeste (make-figura 200 200 150 6))

(define (desenha-tapete-sierpinski figura)
   
   (tapete-sierpinski (figura-altura figura) (gera-cor (figura-cor figura)))

  
 )

;;(desenha-tapete-sierpinski figuraTeste)

;;==========================================================================
;; 3: Função desenha-floco
;; ==========================================================================

(define (desenha-floco figura)
   (floco-de-neve (figura-altura figura) (gera-cor (figura-cor figura)))
  )

;;(desenha-floco figuraTeste)

;; ==========================================================================
;; 4: Função desenha-figuras
;; ==========================================================================

;; Terminação:

(define (desenha-figuras funcao figura)
  
  ;;se a coord-x ou a coord-y menor que 10, gera uma cena vazia
  
  ;;caso contrário;
      ;;chama a função para a figura e para a figura deslocada
  
  (cond 
    
    [(< (figura-altura figura) 5) (empty-scene 500 500)]
    
    [else
     
     (place-image
      
      (funcao figura)
      (figura-coord-x figura)
      (figura-coord-y figura)
      (desenha-figuras funcao (make-figura 
                                          (/ (figura-coord-x figura) 2)
                                          (/ (figura-coord-y figura) 2)
                                          (/ (figura-altura figura) 2)
                                          (+ (figura-cor figura) 1))
      
      ))]
    
    
    
    
    ))

;;(desenha-figuras desenha-floco figuraTeste)
;; ==========================================================================
;; 5: Função desenha-figuras-gen
;; ==========================================================================

;; (a) Critérios de fim e funções de movimentação 3 funções de criterio e 3 de movimentação:


;;funcoes critérios:
;; criterio: Figura -> Boolean

(define (criterio_um figura)
  ;;criterio de terminação: quando se atinge a figura com a cor verde;
       ;;isto é, para antes de gerar a figura verde, uma vez que quando isso for acontecer, retorna a cena vazia
  (string=? (gera-cor (figura-cor figura)) "green"))
  
  
(define (criterio_dois figura)
  
  ;;para quando a coord-x ou coord-y passam de 500
  (or (> (figura-coord-x figura) 500) (> (figura-coord-y figura) 500)))

(define (criterio_tres figura)
  
  ;;para quando a altura fica muito pequena, isto é, quando ela é menor que 2
    (< (figura-altura figura) 2))

;; altera-figura: Figura -> Figura
;; Dada uma figura, retorna uma nova figura que representa uma alteração da antiga;

;;Exemplos:
;; (altera-figura_um (make-figura 120 10 20 2)) = (make-figura 130 20 40 3)
;; (altera-figura_dois (make-figura 120 10 20 3)) = (make-figura 170 5 10 3)
;; (altera-figura_tres (make-figura 30 1 40 1)) = (make-figura 30 2 44 3)

(define (altera-figura_um figura)
  (make-figura (+ (figura-coord-x figura) 10)
               (+ (figura-coord-y figura) 10)
               (+ (figura-altura figura) 20)
               (+ (figura-cor figura) 1)))

(define (altera-figura_dois figura)
  (make-figura (+ (figura-coord-x figura) 50)
               (/ (figura-coord-y figura) 2)
               (/ (figura-altura figura) 2)
               (figura-cor figura)))

(define (altera-figura_tres figura)
  (make-figura (figura-coord-x figura) (* (figura-coord-y figura) 2)
                                          (+ (figura-altura figura) 4)
                                          (+ (figura-cor figura) 2)))

;;Testes:

(check-expect (altera-figura_um (make-figura 12 1 100 1)) (make-figura 22 11 120 2))
(check-expect (altera-figura_dois (make-figura 12 10 100 3)) (make-figura 62 5 50 3))
(check-expect (altera-figura_tres (make-figura 40 1 50 2)) (make-figura 40 2 54 4))




;; (b) função desenha-figuras-gen:


(define (desenha-figuras-gen funcao figura criterio alteraFigura)
  
  
  (cond
    ;;para quando o criterio passado como parâmetro for atingido
      ;;sendo a funcao criterio uma funcao que recebe uma figura e retorna um bool
       ;;só então gera a cena vazia
    [(criterio figura) (empty-scene 600 600)]
    ;;senão
    [else
     ;;combina
     (place-image
      ;; a imagem gerada pela funcao passada como parâmetro - "funcao"
      (funcao figura)
      ;;em uma posicao aleatoria
      (random 600)
      (random 600)
      ;; com o resultado da combinação das figuras alteradas por meio da funcao (alteraFigura) e geradas pela funcao passada como parâmetro(funcao)
      (desenha-figuras-gen funcao (alteraFigura figura) criterio alteraFigura)
      
      )]))


;;Testes/Exemplos:

;; Chamada1:
(desenha-figuras-gen desenha-tapete-sierpinski 
                     (make-figura 10 1 300 3)
                     criterio_um
                     altera-figura_um)
;;Chamada2:
(desenha-figuras-gen desenha-floco
                     (make-figura 50 1 200 1)
                     criterio_um
                     altera-figura_tres)

(desenha-figuras-gen desenha-tapete-sierpinski 
                     (make-figura 400 450 300 3)
                     criterio_dois
                     altera-figura_um)

(desenha-figuras-gen desenha-tapete-sierpinski
                     (make-figura 200 100 200 3)
                     criterio_tres
                     altera-figura_dois)

;; (c) Argumentação da terminação das chamadas:

;; Chamada1:
;; A função termina devido ao fato de que o critério_um determina que a função pare quando a cor for verde, como a altera-figura_um muda a cor, de uma em uma, a cada chamada, uma hora necessariamente a cor será verde;

;;Chamada2:
;;Do mesmo modo em que na chamada 1, aqui usa-se o mesmo parâmetro, mas dessa vez a cor muda de duas em duas. Como a mudança de cor é cíclica, necessariamente se atingirá o verde;

;;chamada 3:

;;O critério dois determina que a função pare assim que as coordenadas x ou y da figura ultrapassem 500. Como a altera-figura_um aumenta essas coordenadas, uma hora necessariamente alguma delas irá ser maior que 500.

;; chamada 4:

;; o critério_tres determina que a função terminará quando a altura da figura for menor do que dois. Como a altera-figura_dois sempre divide a altura por 2, uma hora necessariamente a altura será menor que 2. 



(define (new-desenha-figuras figura)
  
  ;;se a coord-x ou a coord-y menor que 10, gera uma cena vazia
  
  ;;caso contrário;
      ;;chama a função para a figura e para a figura deslocada
  
  (cond 
    
    [(> (figura-altura figura) 1000) (empty-scene 500 500)]
    
    [else
     
     (place-image

      (desenha-floco figura)
      (figura-coord-x figura)
      (figura-coord-y figura)
      (new-desenha-figuras (make-figura 
                                          (figura-coord-x figura)
                                          (figura-coord-y figura)
                                          (* (figura-altura figura) 2)
                                          (+ (figura-cor figura) 1)))
      
      )]
    
    
    
    
    ))

;;(coord-x coord-y altura cor)

(new-desenha-figuras (make-figura 250 250 2 1))
