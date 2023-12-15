;;; Tabuleiros


(defun tabuleiro-teste ()
"Tabuleiro de teste sem nenhuma jogada realizada"
  '(
    (94 25 54 89 21 8 36 14 41 96) 
    (78 47 56 23 5 49 13 12 26 60) 
    (0 27 17 83 34 93 74 52 45 80) 
    (69 9 77 95 55 39 91 73 57 30) 
    (24 15 22 86 1 11 68 79 76 72) 
    (81 48 32 2 64 16 50 37 29 71) 
    (99 51 6 18 53 28 7 63 10 88) 
    (59 42 46 85 90 75 87 43 20 31) 
    (3 61 58 44 65 82 19 4 35 62) 
    (33 70 84 40 66 38 92 67 98 97)
    )
)

(defun tabuleiro-jogado ()
"Tabuleiro de teste igual ao anterior mas tendo sido colocado o cavalo na posi��o: i=0 e j=0"
  '(
    (T 25 54 89 21 8 36 14 41 96) 
    (78 47 56 23 5 49 13 12 26 60) 
    (0 27 17 83 34 93 74 52 45 80) 
    (69 9 77 95 55 39 91 73 57 30) 
    (24 15 22 86 1 11 68 79 76 72) 
    (81 48 32 2 64 16 50 37 29 71) 
    (99 51 6 18 53 28 7 63 10 88) 
    (59 42 46 85 90 75 87 43 20 31) 
    (3 61 58 44 65 82 19 4 35 62) 
    (33 70 84 40 66 38 92 67 98 97)
    )
)


;;; Exercicios

#|Fun��o que recebe um �ndice e o tabuleiro e retorna uma lista que representa essa linha do tabuleiro.|#
(defun linha (indice tabuleiro)
  "Retorna a linha do tabuleiro correspondente ao �ndice dado"
  (nth indice tabuleiro))

; (linha 0 (tabuleiro-teste)) -> (94 25 54 89 21 8 36 14 41 96)

#|-----------------------------------------------------------------------------------------------------------|#

#|Fun��o que recebe dois �ndices e o tabuleiro e retorna o valor presente nessa c�lula do tabuleiro.|#
(defun celula (linha-indice coluna-indice tabuleiro)
  "Retorna o valor presente na c�lula do tabuleiro correspondente aos �ndices dados"
  (nth coluna-indice (linha linha-indice tabuleiro))) ;; Obt�m o valor da c�lula na coluna indicada da linha indicada

;; (celula 0 1 (tabuleiro-teste)) -> 25

#|-----------------------------------------------------------------------------------------------------------|#

#|Fun��o que recebe um n�mero positivo n e cria uma lista com todos os n�meros
entre 0 (inclusiv�) e o n�mero passado como argumento (exclusiv�). Por default o n � 100.|#
(defun lista-numeros (&optional (n 100))
  "Cria uma lista com todos os n�meros de 0 (inclusivo) at� n (exclusivo). Por padr�o, n � 100."
  (cond ((<= n 0) nil)  ;; Caso base: Se n � 0 ou negativo, retorna uma lista vazia.
        (t (cons (- n 1) (lista-numeros (- n 1))))))  ;; Constr�i a lista do maior para o menor n�mero.

;; Exemplos de uso:
;; (lista-numeros 5) -> (4 3 2 1 0)
;; (lista-numeros)   -> (99 98 97 ... 3 2 1 0)


#|-----------------------------------------------------------------------------------------------------------|#

#|A fun��o remover-se (<predicado> <lista>) permite reconstruir uma lista sem os elementos que verificam 
o predicado passado como argumento.|#
(defun remover-se(pred lista)
  (cond ((null lista) NIL) ;; Lista vazia, retorna NIL
        ;; O primeiro elemento da lista satisfaz o predicado, chama recursivamente para a cauda
        ((funcall pred (car lista)) (remover-se pred (cdr lista)))
        ;; O primeiro elemento n�o satisfaz o predicado, consolida o elemento e chama recursivamente para a cauda 
        (T (cons (car lista) (remover-se pred (cdr lista))))))

;; (remover-se #'(lambda (x) (= x 0)) '(1 2 0 2 0 4)) -> (1 2 2 4)

#|Fun��o recursiva que remove o n�mero da lista igual ao encontrado aleatoriamente|#
(defun baralhar-recursivo (lista)
  "Baralha aleatoriamente os n�meros em uma lista."
  (if (endp lista) ;; Verifica se a lista est� vazia
      nil ;; Retorna NIL
      ;; Passo 2: Escolhe um n�mero aleat�rio da lista
      (let ((numero (nth (random (length lista)) lista)))
        ;; Passo 3: Adiciona o n�mero escolhido e continua recursivamente removendo-o da lista
        (cons numero (baralhar-recursivo (remover-se (lambda (x) (= x numero)) lista))))))

#|Fun��o que recebe uma lista e ir� mudar aleatoriamente os seus n�meros.|#
(defun baralhar (lista)
  "Fun��o principal para baralhar uma lista."
  (baralhar-recursivo lista))

;; (baralhar (lista-numeros))
;; (baralhar (lista-numeros 5))

#|-----------------------------------------------------------------------------------------------------------|#

#|Fun��o que pega numa lista e cria sublistas de n elementos recorrendo � fun��o subseq que tem um comportamento
semelhante ao substring para strings.
Por default a lista ser� o resultado obtido na al�nea 4 (baralhar (lista-numeros)) e o n � 10|#
(defun tabuleiro-aleatorio (&optional (lista (baralhar (lista-numeros))) (n 10))
  "Cria um tabuleiro aleat�rio dividindo a lista em sublistas de tamanho n.
  Por padr�o, a lista � o resultado da fun��o (baralhar (lista-numeros)) e n � 10."
  (cond
    ((null lista) nil)  ; Condi��o de paragem: se a lista estiver vazia, retorna nil
    (t (cons (subseq lista 0 n) (tabuleiro-aleatorio (subseq lista n) n)))
  )
)

;; (tabuleiro-aleatorio)
;; (tabuleiro-aleatorio (baralhar(lista-numeros)) 5)

#|-----------------------------------------------------------------------------------------------------------|#

#|Fun��o que recebe um �ndice, uma lista e um valor (por default o valor � NIL) esubstitui pelo valor pretendido 
nessa posi��o.|#
(defun substituir-posicao (coluna lista &optional (valor nil))
  ;; Verifica se o �ndice � negativo; se sim, retorna a lista original sem modifica��o
  (if (< coluna 0)
      lista
      ;; Divide a lista na posi��o indicada e insere o valor na posi��o desejada
      (let ((antes (subseq lista 0 coluna))
            (depois (subseq lista (1+ coluna))))
        (append antes (list valor) depois))))

;; (substituir-posicao 0 (linha 0 (tabuleiro-teste))) -> (NIL 25 54 89 21 8 36 14 41 96)
;; (substituir-posicao 0 (linha 0 (tabuleiro-teste)) T) -> (T 25 54 89 21 8 36 14 41 96) 

#|-----------------------------------------------------------------------------------------------------------|#

#|Fun��o que recebe dois �ndices, o tabuleiro e um valor (por default o valor � NIL). A
fun��o dever� retornar o tabuleiro com a c�lula substitu�da pelo valor pretendido. Utiliza a fun��o
substituir-posicao definida anteriormente. |#
(defun substituir (linha-indice coluna-indice tabuleiro &optional (valor nil))
  "Substitui o valor na c�lula indicada pelos �ndices no tabuleiro.
  - linha-indice: �ndice da linha na qual realizar a substitui��o.
  - coluna-indice: �ndice da coluna na qual realizar a substitui��o.
  - tabuleiro: Tabuleiro no qual realizar a substitui��o.
  - valor: Valor a ser inserido na c�lula. Por padr�o, � NIL."
  (substituir-posicao linha-indice tabuleiro
                      (substituir-posicao coluna-indice (nth linha-indice tabuleiro) valor)))

;; (substituir 0 0 (tabuleiro-teste) T)

#|-----------------------------------------------------------------------------------------------------------|#

(defun procura-cavalo-na-linha (linha)
  "Procura o cavalo em uma linha espec�fica. Retorna a posi��o de 'T' ou NIL se n�o encontrado."
  (position 'T linha :test #'eql))

(defun posicao-cavalo (tabuleiro &optional (line 0))
 "Retorna a posic��o (x y) de onde se localiza o cavalo"
  (cond    
   ((null tabuleiro) Nil)
   ((equal (find T (car tabuleiro)) T) (list line (position T (car tabuleiro))))
   (t (posicao-cavalo (cdr tabuleiro) (+ line 1)) ))
  )

;; (posicao-cavalo (tabuleiro-teste)) -> NIL
;; (posicao-cavalo (tabuleiro-jogado)) -> (0 0)

#|-----------------------------------------------------------------------------------------------------------|#
#|----------------------------------------------FUN��ES AUXILIARES-------------------------------------------|#
#|-----------------------------------------------------------------------------------------------------------|#

(defun numero-maximo-lista (lista)
  "Retorna o maior n�mero da lista fornecido. Se a lista contiver elementos que n�o s�o n�meros, eles s�o 
  removidos da lista e n�o entram nas compara��es."
  (reduce #'max (remove-if-not #'numberp lista))
)

;; (numero-maximo-lista '(NIL 25 54 89 21 8 36 14 41 96)) -> 96

(defun junta-duas-listas (lista1 lista2)
  "Esta fun��o junta duas listas numa s�. Se a primeira lista tiver 0 elementos, ele retorna a lista2 apenas."
  (cond
      ((null lista1) lista2)
      ((null lista2) lista1) 
      (T (append lista1 lista2))
  )
)

(defun tabuleiro-numa-lista (tabuleiro)
  "Esta fun��o transforma o tabuleiro (uma lista de listas) numa s� lista."
  (cond 
      ((null tabuleiro) NIL)
      (T (junta-duas-listas (car tabuleiro)(tabuleiro-numa-lista (cdr tabuleiro))))
  )
)

#|-----------------------------------------------------------------------------------------------------------|#
#|----------------------------------------------------REGRAS-------------------------------------------------|#
#|-----------------------------------------------------------------------------------------------------------|#

(defun numero-simetrico (numero)
  "Se o n�mero tem dois d�gitos diferentes, retorna o n�mero sim�trico. 
   Caso contr�rio, retorna nil."
  (if (and (>= numero 10) (<= numero 99)) ; Verifica se o n�mero tem dois d�gitos.
    (let ((digito1 (mod numero 10))
          (digito2 (floor numero 10)))
      (if (/= digito1 digito2) ; Verifica se os d�gitos s�o diferentes.
        (+ (* digito1 10) digito2))))) ; Retorna o n�mero sim�trico se os d�gitos forem diferentes.

;; (numero-simetrico 57) -> 75
;; (numero-simetrico 44) -> nil (n�meros iguais)
;; (numero-simetrico 123) -> nil (n�o � um n�mero de dois d�gitos)

#|-----------------------------------------------------------------------------------------------------------|#

(defun numero-duplo (numero)
  "Verifica se o n�mero fornecido � um n�mero duplo (dois d�gitos iguais).
   Retorna T (true) se for um n�mero duplo, caso contr�rio retorna NIL (false)."
  (and (not (null numero))
       (not (eq numero T))
       (>= numero 10) (<= numero 99) ; Verifica se o n�mero tem dois d�gitos.
       (= (mod numero 10) (floor numero 10)))) ; Verifica se os dois d�gitos s�o iguais.

;; (numero-duplo 44) -> T
;; (numero-duplo 57) -> NIL
;; (numero-duplo 123) -> NIL (n�o � um n�mero de dois d�gitos)

#|-----------------------------------------------------------------------------------------------------------|#

(defun duplos-existentes(lista)
  "Retorna uma lista dos n�meros duplos que existem numa lista."
  (cond 
      ((null lista) '())
      ((numero-duplo (car lista))
          (cons (car lista) (duplos-existentes (cdr lista))))
      (T (duplos-existentes (cdr lista)))
  )
)

;;(duplos-existentes (linha 1 (tabuleiro-teste))) -> NIL
;;(duplos-existentes (linha 4 (tabuleiro-teste))) -> (22 11)

#|-----------------------------------------------------------------------------------------------------------|#

(defun duplos-existentes-ordenados (tabuleiro)
  "Retorna uma lista de todos os n�meros duplos existentes no tabuleiro ordenados"
  (sort (duplos-existentes (tabuleiro-numa-lista tabuleiro)) #'>)
)

;; (duplos-existentes-ordenados (tabuleiro-teste)) -> (99 88 77 66 55 44 33 22 11)

#|-----------------------------------------------------------------------------------------------------------|#

(defun maximo-duplo (tabuleiro)
  "Retorna o maior n�mero duplo que existe no tabuleiro"
  (numero-maximo-lista (duplos-existentes-ordenados tabuleiro))
)

#|-----------------------------------------------------------------------------------------------------------|#

(defun movimento-valido (linha coluna tabuleiro)
  "Verifica se a posi��o para onde se pretende movimentar � v�lida. Retorna T em caso de ser v�lido,
   Caso contr�rio retornar� NIL."
  (and (>= linha 0)(<= linha 9)
       (>= coluna 0)(<= coluna 9) ;; Verifica se a coluna e linha est�o entre 0 e 9
       (not (null (celula linha coluna tabuleiro))) ;; Verifica se a celula para onde se movimenta n�o est� a NIL
  )
)

;; (movimento-valido 0 1 (tabuleiro-jogado)) -> T
;; (movimento-valido 0 -1 (tabuleiro-jogado)) -> NIL
;; (movimento-valido 0 1 (tabuleiro-jogado)) -> NIL - ISTO PORQUE ALTERAMOS O VALOR DE 25 PARA NIL PARA TESTARMOS
#|-----------------------------------------------------------------------------------------------------------|#

(defun posicao-valor (valor tabuleiro &optional (line 0))
  "Verifica se a posi��o para onde se pretende movimentar � v�lida. Retorna T em caso de ser v�lido,
   Caso contr�rio retornar� NIL."
  (if (or (null tabuleiro) (> line 9))
      NIL
      (let* 
          (
             (linha-tabuleiro (linha line tabuleiro))
             (coluna-valor (position valor linha-tabuleiro))
          )
          (if coluna-valor
            (list line coluna-valor)
            (posicao-valor valor tabuleiro (1+ line))
          )
      )
  )
)

;; (posicao-valor 94 (tabuleiro-jogado)) -> NIL
;; (posicao-valor 94 (tabuleiro-teste)) -> (0 0)
;; (posicao-valor 92 (tabuleiro-teste)) -> (9 6)

#|-----------------------------------------------------------------------------------------------------------|#
#|--------------------------------------------------OPERADORES-----------------------------------------------|#
#|-----------------------------------------------------------------------------------------------------------|#

(defun inicializar-cavalo (tabuleiro &optional (coluna 0))
  "Coloca o cavalo na posi��o [0, coluna] de uma c�pia do tabuleiro, onde coluna � opcional e deve ser entre 0 e 10."
  (let ((novo-tabuleiro (limpar-cavalos (copy-list tabuleiro)))) ; Cria uma c�pia do tabuleiro e remove cavalos existentes
    (cond
      ((or (< coluna 0) (> coluna 10)) ; Se a coluna � inv�lida
       (setq coluna 0)) ; Ajusta para 0
      (t
       (setf (nth coluna (car novo-tabuleiro)) 'T))) ; Coloca o cavalo na posi��o especificada no novo tabuleiro
    novo-tabuleiro)) ; Retorna o novo tabuleiro

;; (tabuleiro-teste)
;; (inicializar-cavalo (tabuleiro-teste))
;; (tabuleiro-teste)
;; (inicializar-cavalo (tabuleiro-teste) 7)
;; (tabuleiro-teste)

(defun limpar-cavalos (tabuleiro)
  "Remove todos os cavalos ('T') do tabuleiro, substituindo-os por nil."
  (mapcar (lambda (linha) 
            (mapcar (lambda (celula) 
                      (if (eql celula 'T) nil celula))
                    linha))
          tabuleiro))

;; (limpar-cavalos (tabuleiro-teste))


(defun lista-operadores ()
  "Cria uma lista com todos os operadores."
  (list 'operador-1 
        'operador-2 
        'operador-3 
        'operador-4 
        'operador-5 
        'operador-6 
        'operador-7 
        'operador-8)
)


(defun escolhe-operador (tabuleiro numero-operador)
  (if (or (< numero-operador 1) (> numero-operador 8))
      (format t "N�mero do operador inv�lido. Deve ser entre 1 e 8.~%")
      (cond
        ((= numero-operador 1) (operador-geral tabuleiro 2 -1))
        ((= numero-operador 2) (operador-geral tabuleiro 2 +1))
        ((= numero-operador 3) (operador-geral tabuleiro 1 +2))
        ((= numero-operador 4) (operador-geral tabuleiro -1 +2))
        ((= numero-operador 5) (operador-geral tabuleiro -2 +1))
        ((= numero-operador 6) (operador-geral tabuleiro -2 -1))
        ((= numero-operador 7) (operador-geral tabuleiro -1 -2))
        ((= numero-operador 8) (operador-geral tabuleiro 1 -1))
        (t (format t "Operador n�o implementado.~%")))
   )
)


(defun operador-geral (tabuleiro numero-linhas numero-colunas)
  (if (eq (posicao-cavalo tabuleiro) NIL)
      (format t "Cavalo por posicionar.~%")
    (let* 
        (
         (posicao-cavalo-inicio (posicao-cavalo tabuleiro))
         (nova-linha (+ (first posicao-cavalo-inicio) numero-linhas))
         (nova-coluna (+ (second posicao-cavalo-inicio) numero-colunas))
         (posicao-cavalo-final (list nova-linha nova-coluna))
         (movimento-e-valido (movimento-valido nova-linha nova-coluna tabuleiro))
         )
      (if (eq movimento-e-valido NIL)
          (format t "Movimento inv�lido.~%")
        (let* 
            (
             (simetrico (numero-simetrico (celula nova-linha nova-coluna tabuleiro)))
             (posicao-simetrico (posicao-valor simetrico tabuleiro))
             (e-duplo (numero-duplo (celula nova-linha nova-coluna tabuleiro)))
             (maximo-duplo (maximo-duplo tabuleiro))
             (posicao-duplo (posicao-valor maximo-duplo tabuleiro))
             )
          (cond 
           ((eq e-duplo T)
            (substituir (first posicao-cavalo-final)(second posicao-cavalo-final)
                        (substituir (first posicao-cavalo-inicio) (second posicao-cavalo-inicio)
                                    (substituir (first posicao-duplo)(second posicao-duplo) tabuleiro 
                                                NIL)
                                    NIL)
                        T)
            )
           (T
            (substituir (first posicao-cavalo-final)(second posicao-cavalo-final)
                        (substituir (first posicao-cavalo-inicio) (second posicao-cavalo-inicio)
                                    (substituir (first posicao-simetrico)(second posicao-simetrico) tabuleiro 
                                                NIL)
                                    NIL)
                        T)
            )
           )
          )
        )
      )
    )
)


#|-----------------------------------------------------------------------------------------------------------|#
#|---------------------------------------------------- N�S --------------------------------------------------|#
#|-----------------------------------------------------------------------------------------------------------|#




#|-----------------------------------------------------------------------------------------------------------|#
#|---------------------------------------------------- TODO -------------------------------------------------|#
#|-----------------------------------------------------------------------------------------------------------|#

;; -> falta fazer uma fun��o que permita ao utilizador escolher a posi��o inicial do cavalo e a partir
;; da� resolver de acordo com o algoritmo selecionado.

#|-----------------------------------------------------------------------------------------------------------|#

(defun obter-objetivo (problema)
  "Retorna o objetivo de pontos para o problema dado."
  (case problema
    ('A 70)
    ('B 60)
    ('C 270)
    ('D 600)
    ('E 300)
    ('F 2000)
    (otherwise (error "Problema n�o definido ou inv�lido."))))

;; (obter-objetivo 'A) -> 70

#|-----------------------------------------------------------------------------------------------------------|#

;; Fun��o para verificar se pode adicionar pontos
(defun pode-adicionar-pontos? (pontos-atuais pontos-casa limite)
  "Verifica se � poss�vel adicionar os pontos da casa aos pontos atuais, sem ultrapassar o objetivo do problema"
  (let ((novo-total (+ pontos-atuais pontos-casa)))
    (<= novo-total limite)))

;; (pode-adicionar-pontos? 50 20 (obter-objetivo 'A)) -> T
;; (pode-adicionar-pontos? 50 25 (obter-objetivo 'A)) -> NIL

#|-----------------------------------------------------------------------------------------------------------|#

(defun somar-pontos (pontos-atuais pontos-casa)
  "Soma os pontos da casa atual aos pontos atuais e retorna o total."
  (+ pontos-atuais pontos-casa))

;; (somar-pontos 50 20) -> 70

#|-----------------------------------------------------------------------------------------------------------|#

;; -> falta implementar BFS

;; PSEUDO-C�DIGO

#|
N� inicial(s) => ABERTOS. Faz g(s)=0.
Se ABERTOS vazia falha.
Remove o n� de ABERTOS (n) com menor custo (g) e coloca-o em FECHADOS 
Se n for um n� objectivo termina e d� a solu��o.
Expande o n� n. Colocar os sucessores em ABERTOS, colocando os ponteiros para n e calculando o 
g de cada um dos sucessores.
Vai para 2.

|#

;; -> falta implementar DFS

;; PSEUDO-C�DIGO

#|N� inicial => ABERTOS
Se ABERTOS vazia falha.
Remove o primeiro n� de ABERTOS (n) e coloca-o em FECHADOS 
Se a profundidade de n � maior que d vai para 2.
Expande o n� n. Colocar os sucessores no in�cio de ABERTOS, colocando os ponteiros para n.
Se algum dos sucessores � um n� objectivo sai, e d� a solu��o. Caso contr�rio vai para 2.
|#


;; -> falta implementar A*

#|-----------------------------------------------------------------------------------------------------------|#




