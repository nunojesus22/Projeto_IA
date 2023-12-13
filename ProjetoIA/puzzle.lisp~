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
"Tabuleiro de teste igual ao anterior mas tendo sido colocado o cavalo na posição: i=0 e j=0"
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

#|Função que recebe um índice e o tabuleiro e retorna uma lista que representa essa linha do tabuleiro.|#
(defun linha (indice tabuleiro)
  "Retorna a linha do tabuleiro correspondente ao índice dado"
  (nth indice tabuleiro))

; (linha 0 (tabuleiro-teste)) -> (94 25 54 89 21 8 36 14 41 96)

#|-----------------------------------------------------------------------------------------------------------|#

#|Função que recebe dois índices e o tabuleiro e retorna o valor presente nessa célula do tabuleiro.|#
(defun celula (linha-indice coluna-indice tabuleiro)
  "Retorna o valor presente na célula do tabuleiro correspondente aos índices dados"
  (let ((linha (nth linha-indice tabuleiro))) ;; Obtém a linha do tabuleiro correspondente ao índice da linha
    (nth coluna-indice linha))) ;; Obtém o valor da célula na coluna indicada

;; (celula 0 1 (tabuleiro-teste)) -> 25

#|-----------------------------------------------------------------------------------------------------------|#

#|Função que recebe um número positivo n e cria uma lista com todos os números
entre 0 (inclusivé) e o número passado como argumento (exclusivé). Por default o n é 100.|#
(defun lista-numeros (&optional (n 100))
  "Cria uma lista com todos os números de 0 (inclusivo) até n (exclusivo). Por padrão, n é 100."
  (cond ((<= n 0) nil)  ;; Caso base: Se n é 0 ou negativo, retorna uma lista vazia.
        (t (cons (- n 1) (lista-numeros (- n 1))))))  ;; Constrói a lista do maior para o menor número.

;; Exemplos de uso:
;; (lista-numeros 5) -> (4 3 2 1 0)
;; (lista-numeros)   -> (99 98 97 ... 3 2 1 0)


#|-----------------------------------------------------------------------------------------------------------|#

#|A função remover-se (<predicado> <lista>) permite reconstruir uma lista sem os elementos que verificam 
o predicado passado como argumento.|#
(defun remover-se(pred lista)
  (cond ((null lista) NIL) ;; Lista vazia, retorna NIL
        ;; O primeiro elemento da lista satisfaz o predicado, chama recursivamente para a cauda
        ((funcall pred (car lista)) (remover-se pred (cdr lista)))
        ;; O primeiro elemento não satisfaz o predicado, consolida o elemento e chama recursivamente para a cauda 
        (T (cons (car lista) (remover-se pred (cdr lista))))))

;; (remover-se #'(lambda (x) (= x 0)) '(1 2 0 2 0 4)) -> (1 2 2 4)

#|Função recursiva que remove o número da lista igual ao encontrado aleatoriamente|#
(defun baralhar-recursivo (lista)
  "Baralha aleatoriamente os números em uma lista."
  (if (endp lista) ;; Verifica se a lista está vazia
      nil ;; Retorna NIL
      ;; Passo 2: Escolhe um número aleatório da lista
      (let ((numero (nth (random (length lista)) lista)))
        ;; Passo 3: Adiciona o número escolhido e continua recursivamente removendo-o da lista
        (cons numero (baralhar-recursivo (remover-se (lambda (x) (= x numero)) lista))))))

#|Função que recebe uma lista e irá mudar aleatoriamente os seus números.|#
(defun baralhar (lista)
  "Função principal para baralhar uma lista."
  (baralhar-recursivo lista))

;; (baralhar (lista-numeros))
;; (baralhar (lista-numeros 5))

#|-----------------------------------------------------------------------------------------------------------|#

#|Função que pega numa lista e cria sublistas de n elementos recorrendo à função subseq que tem um comportamento
semelhante ao substring para strings.
Por default a lista será o resultado obtido na alínea 4 (baralhar (lista-numeros)) e o n é 10|#
(defun tabuleiro-aleatorio (&optional (lista (baralhar (lista-numeros))) (n 10))
  "Cria um tabuleiro aleatório dividindo a lista em sublistas de tamanho n.
  Por padrão, a lista é o resultado da função (baralhar (lista-numeros)) e n é 10."
  (cond
    ((null lista) nil)  ; Condição de paragem: se a lista estiver vazia, retorna nil
    (t (cons (subseq lista 0 n) (tabuleiro-aleatorio (subseq lista n) n)))
  )
)

;; (tabuleiro-aleatorio)
;; (tabuleiro-aleatorio (baralhar(lista-numeros)) 5)

#|-----------------------------------------------------------------------------------------------------------|#

#|Função que recebe um índice, uma lista e um valor (por default o valor é NIL) esubstitui pelo valor pretendido 
nessa posição.|#
(defun substituir-posicao (indice lista &optional (valor nil))
  ;; Verifica se o índice é negativo; se sim, retorna a lista original sem modificação
  (if (< indice 0)
      lista
      ;; Divide a lista na posição indicada e insere o valor na posição desejada
      (let ((antes (subseq lista 0 indice))
            (depois (subseq lista (1+ indice))))
        (append antes (list valor) depois))))

;; (substituir-posicao 0 (linha 0 (tabuleiro-teste))) -> (NIL 25 54 89 21 8 36 14 41 96)
;; (substituir-posicao 0 (linha 0 (tabuleiro-teste)) T) -> (T 25 54 89 21 8 36 14 41 96) 

#|-----------------------------------------------------------------------------------------------------------|#

#|Função que recebe dois índices, o tabuleiro e um valor (por default o valor é NIL). A
função deverá retornar o tabuleiro com a célula substituída pelo valor pretendido. Utiliza a função
substituir-posicao definida anteriormente. |#
(defun substituir (linha-indice coluna-indice tabuleiro &optional (valor nil))
  "Substitui o valor na célula indicada pelos índices no tabuleiro.
  - linha-indice: Índice da linha na qual realizar a substituição.
  - coluna-indice: Índice da coluna na qual realizar a substituição.
  - tabuleiro: Tabuleiro no qual realizar a substituição.
  - valor: Valor a ser inserido na célula. Por padrão, é NIL."
  (substituir-posicao linha-indice tabuleiro
                      (substituir-posicao coluna-indice (nth linha-indice tabuleiro) valor)))

;; (substituir 0 0 (tabuleiro-teste) T)

#|-----------------------------------------------------------------------------------------------------------|#

(defun procura-cavalo-na-linha (linha)
  "Procura o cavalo em uma linha específica. Retorna a posição de 'T' ou NIL se não encontrado."
  (position 'T linha :test #'eql))

(defun posicao-cavalo (tabuleiro &optional (line 0))
 "Retorna a posicção (x y) de onde se localiza o cavalo"
  (cond    
   ((null tabuleiro) Nil)
   ((equal (find T (car tabuleiro)) T) (list line (position T (car tabuleiro))))
   (t (posicao-cavalo (cdr tabuleiro) (+ line 1)) ))
  )

;; (posicao-cavalo (tabuleiro-teste)) -> NIL
;; (posicao-cavalo (tabuleiro-jogado)) -> (0 0)

#|-----------------------------------------------------------------------------------------------------------|#
#|--------------------------------------------------OPERADORES-----------------------------------------------|#
#|-----------------------------------------------------------------------------------------------------------|#

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


(defun numero-simetrico (numero)
  "Se o número tem dois dígitos diferentes, retorna o número simétrico. 
   Caso contrário, retorna nil."
  (if (and (>= numero 10) (<= numero 99)) ; Verifica se o número tem dois dígitos.
    (let ((digito1 (mod numero 10))
          (digito2 (floor numero 10)))
      (if (/= digito1 digito2) ; Verifica se os dígitos são diferentes.
        (+ (* digito1 10) digito2))))) ; Retorna o número simétrico se os dígitos forem diferentes.

;; (numero-simetrico 57) -> 75
;; (numero-simetrico 44) -> nil (números iguais)
;; (numero-simetrico 123) -> nil (não é um número de dois dígitos)

(defun numero-duplo (numero)
  "Verifica se o número fornecido é um número duplo (dois dígitos iguais).
   Retorna T (true) se for um número duplo, caso contrário retorna NIL (false)."
  (and (>= numero 10) (<= numero 99) ; Verifica se o número tem dois dígitos.
       (= (mod numero 10) (floor numero 10)))) ; Verifica se os dois dígitos são iguais.

;; (numero-duplo 44) -> T
;; (numero-duplo 57) -> NIL
;; (numero-duplo 123) -> NIL (não é um número de dois dígitos)

(defun inicializar-cavalo (tabuleiro)
  "Coloca o cavalo na primeira posição do tabuleiro se ele não estiver presente."
  (unless (posicao-cavalo tabuleiro)
    (setf (car (car tabuleiro)) 'T))
  tabuleiro)


#|operador-1: Função que recebe o tabuleiro e movimenta o cavalo para a posição 2 linhas abaixo e
uma coluna ao lado direito, ou seja, o movimento que de acordo com a Figura 1 leva o cavalo para a
casa de valor 10.|#

(defun operador-1 (tabuleiro)
  "Move o cavalo para 2 linhas abaixo e uma coluna à direita, aplicando a regra de número simétrico ou duplo."
  (let ((posicao (posicao-cavalo (inicializar-cavalo tabuleiro))))
    (let ((nova-linha (+ (first posicao) 2))
          (nova-coluna (- (second posicao) 1)))
      (if (and (>= nova-linha 0) (< nova-linha (length tabuleiro))
               (>= nova-coluna 0) (< nova-coluna (length (first tabuleiro))))
          (let ((numero (celula nova-linha nova-coluna tabuleiro)))
            ;; Aplicar a regra do número simétrico ou duplo
            (when (numero-duplo numero)
              ;; Se for um número duplo, apagar o número.
              (setf (nth nova-coluna (nth nova-linha tabuleiro)) nil))
            (when (numero-simetrico numero)
              ;; Se for um número simétrico, apagar o número simétrico.
              (let ((simetrico (numero-simetrico numero)))
                ;; Remove o simétrico de todas as linhas.
                (setf tabuleiro (mapcar (lambda (linha)
                                          (mapcar (lambda (x) (if (equal x simetrico) nil x)) linha))
                                        tabuleiro))))
            ;; Remover o cavalo da posição atual
            (setf (nth (second posicao) (nth (first posicao) tabuleiro)) nil)
            ;; Mover o cavalo para a nova posição
            (setf (nth nova-coluna (nth nova-linha tabuleiro)) 'T))
          (format t "Movimento inválido.~%"))))
  tabuleiro)

(defun operador-2 (tabuleiro)
  "Move o cavalo para 2 linhas abaixo e uma coluna à direita, aplicando a regra de número simétrico ou duplo."
  (let ((posicao (posicao-cavalo (inicializar-cavalo tabuleiro))))
    (let ((nova-linha (+ (first posicao) 2))
          (nova-coluna (+ (second posicao) 1)))
      (if (and (>= nova-linha 0) (< nova-linha (length tabuleiro))
               (>= nova-coluna 0) (< nova-coluna (length (first tabuleiro))))
          (let ((numero (celula nova-linha nova-coluna tabuleiro)))
            ;; Aplicar a regra do número simétrico ou duplo
            (when (numero-duplo numero)
              ;; Se for um número duplo, apagar o número.
              (setf (nth nova-coluna (nth nova-linha tabuleiro)) nil))
            (when (numero-simetrico numero)
              ;; Se for um número simétrico, apagar o número simétrico.
              (let ((simetrico (numero-simetrico numero)))
                ;; Remove o simétrico de todas as linhas.
                (setf tabuleiro (mapcar (lambda (linha)
                                          (mapcar (lambda (x) (if (equal x simetrico) nil x)) linha))
                                        tabuleiro))))
            ;; Remover o cavalo da posição atual
            (setf (nth (second posicao) (nth (first posicao) tabuleiro)) nil)
            ;; Mover o cavalo para a nova posição
            (setf (nth nova-coluna (nth nova-linha tabuleiro)) 'T))
          (format t "Movimento inválido.~%"))))
  tabuleiro)


;; (operador-2 (tabuleiro-teste)) -- 0 49 não está a ficar a NIL
;; (operador-2 (tabuleiro-jogado)) -- 0 49 não está a ficar a NIL

(defun operador-3 (tabuleiro)
  "Move o cavalo para 2 linhas abaixo e uma coluna à direita, aplicando a regra de número simétrico ou duplo."
  (let ((posicao (posicao-cavalo (inicializar-cavalo tabuleiro))))
    (let ((nova-linha (+ (first posicao) 1))
          (nova-coluna (+ (second posicao) 2)))
      (if (and (>= nova-linha 0) (< nova-linha (length tabuleiro))
               (>= nova-coluna 0) (< nova-coluna (length (first tabuleiro))))
          (let ((numero (celula nova-linha nova-coluna tabuleiro)))
            ;; Aplicar a regra do número simétrico ou duplo
            (when (numero-duplo numero)
              ;; Se for um número duplo, apagar o número.
              (setf (nth nova-coluna (nth nova-linha tabuleiro)) nil))
            (when (numero-simetrico numero)
              ;; Se for um número simétrico, apagar o número simétrico.
              (let ((simetrico (numero-simetrico numero)))
                ;; Remove o simétrico de todas as linhas.
                (setf tabuleiro (mapcar (lambda (linha)
                                          (mapcar (lambda (x) (if (equal x simetrico) nil x)) linha))
                                        tabuleiro))))
            ;; Remover o cavalo da posição atual
            (setf (nth (second posicao) (nth (first posicao) tabuleiro)) nil)
            ;; Mover o cavalo para a nova posição
            (setf (nth nova-coluna (nth nova-linha tabuleiro)) 'T))
          (format t "Movimento inválido.~%"))))
  tabuleiro)

(defun operador-4 (tabuleiro)
  "Move o cavalo para 2 linhas abaixo e uma coluna à direita, aplicando a regra de número simétrico ou duplo."
  (let ((posicao (posicao-cavalo (inicializar-cavalo tabuleiro))))
    (let ((nova-linha (- (first posicao) 1))
          (nova-coluna (+ (second posicao) 2)))
      (if (and (>= nova-linha 0) (< nova-linha (length tabuleiro))
               (>= nova-coluna 0) (< nova-coluna (length (first tabuleiro))))
          (let ((numero (celula nova-linha nova-coluna tabuleiro)))
            ;; Aplicar a regra do número simétrico ou duplo
            (when (numero-duplo numero)
              ;; Se for um número duplo, apagar o número.
              (setf (nth nova-coluna (nth nova-linha tabuleiro)) nil))
            (when (numero-simetrico numero)
              ;; Se for um número simétrico, apagar o número simétrico.
              (let ((simetrico (numero-simetrico numero)))
                ;; Remove o simétrico de todas as linhas.
                (setf tabuleiro (mapcar (lambda (linha)
                                          (mapcar (lambda (x) (if (equal x simetrico) nil x)) linha))
                                        tabuleiro))))
            ;; Remover o cavalo da posição atual
            (setf (nth (second posicao) (nth (first posicao) tabuleiro)) nil)
            ;; Mover o cavalo para a nova posição
            (setf (nth nova-coluna (nth nova-linha tabuleiro)) 'T))
          (format t "Movimento inválido.~%"))))
  tabuleiro)

(defun operador-5 (tabuleiro)
  "Move o cavalo para 2 linhas abaixo e uma coluna à direita, aplicando a regra de número simétrico ou duplo."
  (let ((posicao (posicao-cavalo (inicializar-cavalo tabuleiro))))
    (let ((nova-linha (- (first posicao) 2))
          (nova-coluna (+ (second posicao) 1)))
      (if (and (>= nova-linha 0) (< nova-linha (length tabuleiro))
               (>= nova-coluna 0) (< nova-coluna (length (first tabuleiro))))
          (let ((numero (celula nova-linha nova-coluna tabuleiro)))
            ;; Aplicar a regra do número simétrico ou duplo
            (when (numero-duplo numero)
              ;; Se for um número duplo, apagar o número.
              (setf (nth nova-coluna (nth nova-linha tabuleiro)) nil))
            (when (numero-simetrico numero)
              ;; Se for um número simétrico, apagar o número simétrico.
              (let ((simetrico (numero-simetrico numero)))
                ;; Remove o simétrico de todas as linhas.
                (setf tabuleiro (mapcar (lambda (linha)
                                          (mapcar (lambda (x) (if (equal x simetrico) nil x)) linha))
                                        tabuleiro))))
            ;; Remover o cavalo da posição atual
            (setf (nth (second posicao) (nth (first posicao) tabuleiro)) nil)
            ;; Mover o cavalo para a nova posição
            (setf (nth nova-coluna (nth nova-linha tabuleiro)) 'T))
          (format t "Movimento inválido.~%"))))
  tabuleiro)

(defun operador-6 (tabuleiro)
  "Move o cavalo para 2 linhas abaixo e uma coluna à direita, aplicando a regra de número simétrico ou duplo."
  (let ((posicao (posicao-cavalo (inicializar-cavalo tabuleiro))))
    (let ((nova-linha (- (first posicao) 2))
          (nova-coluna (- (second posicao) 1)))
      (if (and (>= nova-linha 0) (< nova-linha (length tabuleiro))
               (>= nova-coluna 0) (< nova-coluna (length (first tabuleiro))))
          (let ((numero (celula nova-linha nova-coluna tabuleiro)))
            ;; Aplicar a regra do número simétrico ou duplo
            (when (numero-duplo numero)
              ;; Se for um número duplo, apagar o número.
              (setf (nth nova-coluna (nth nova-linha tabuleiro)) nil))
            (when (numero-simetrico numero)
              ;; Se for um número simétrico, apagar o número simétrico.
              (let ((simetrico (numero-simetrico numero)))
                ;; Remove o simétrico de todas as linhas.
                (setf tabuleiro (mapcar (lambda (linha)
                                          (mapcar (lambda (x) (if (equal x simetrico) nil x)) linha))
                                        tabuleiro))))
            ;; Remover o cavalo da posição atual
            (setf (nth (second posicao) (nth (first posicao) tabuleiro)) nil)
            ;; Mover o cavalo para a nova posição
            (setf (nth nova-coluna (nth nova-linha tabuleiro)) 'T))
          (format t "Movimento inválido.~%"))))
  tabuleiro)

(defun operador-7 (tabuleiro)
  "Move o cavalo para 2 linhas abaixo e uma coluna à direita, aplicando a regra de número simétrico ou duplo."
  (let ((posicao (posicao-cavalo (inicializar-cavalo tabuleiro))))
    (let ((nova-linha (- (first posicao) 1))
          (nova-coluna (- (second posicao) 2)))
      (if (and (>= nova-linha 0) (< nova-linha (length tabuleiro))
               (>= nova-coluna 0) (< nova-coluna (length (first tabuleiro))))
          (let ((numero (celula nova-linha nova-coluna tabuleiro)))
            ;; Aplicar a regra do número simétrico ou duplo
            (when (numero-duplo numero)
              ;; Se for um número duplo, apagar o número.
              (setf (nth nova-coluna (nth nova-linha tabuleiro)) nil))
            (when (numero-simetrico numero)
              ;; Se for um número simétrico, apagar o número simétrico.
              (let ((simetrico (numero-simetrico numero)))
                ;; Remove o simétrico de todas as linhas.
                (setf tabuleiro (mapcar (lambda (linha)
                                          (mapcar (lambda (x) (if (equal x simetrico) nil x)) linha))
                                        tabuleiro))))
            ;; Remover o cavalo da posição atual
            (setf (nth (second posicao) (nth (first posicao) tabuleiro)) nil)
            ;; Mover o cavalo para a nova posição
            (setf (nth nova-coluna (nth nova-linha tabuleiro)) 'T))
          (format t "Movimento inválido.~%"))))
  tabuleiro)

(defun operador-8 (tabuleiro)
  "Move o cavalo para 2 linhas abaixo e uma coluna à direita, aplicando a regra de número simétrico ou duplo."
  (let ((posicao (posicao-cavalo (inicializar-cavalo tabuleiro))))
    (let ((nova-linha (+ (first posicao) 1))
          (nova-coluna (- (second posicao) 2)))
      (if (and (>= nova-linha 0) (< nova-linha (length tabuleiro))
               (>= nova-coluna 0) (< nova-coluna (length (first tabuleiro))))
          (let ((numero (celula nova-linha nova-coluna tabuleiro)))
            ;; Aplicar a regra do número simétrico ou duplo
            (when (numero-duplo numero)
              ;; Se for um número duplo, apagar o número.
              (setf (nth nova-coluna (nth nova-linha tabuleiro)) nil))
            (when (numero-simetrico numero)
              ;; Se for um número simétrico, apagar o número simétrico.
              (let ((simetrico (numero-simetrico numero)))
                ;; Remove o simétrico de todas as linhas.
                (setf tabuleiro (mapcar (lambda (linha)
                                          (mapcar (lambda (x) (if (equal x simetrico) nil x)) linha))
                                        tabuleiro))))
            ;; Remover o cavalo da posição atual
            (setf (nth (second posicao) (nth (first posicao) tabuleiro)) nil)
            ;; Mover o cavalo para a nova posição
            (setf (nth nova-coluna (nth nova-linha tabuleiro)) 'T))
          (format t "Movimento inválido.~%"))))
  tabuleiro)








