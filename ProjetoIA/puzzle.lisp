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
  (let ((linha (nth linha-indice tabuleiro))) ;; Obt�m a linha do tabuleiro correspondente ao �ndice da linha
    (nth coluna-indice linha))) ;; Obt�m o valor da c�lula na coluna indicada

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
(defun substituir-posicao (indice lista &optional (valor nil))
  ;; Verifica se o �ndice � negativo; se sim, retorna a lista original sem modifica��o
  (if (< indice 0)
      lista
      ;; Divide a lista na posi��o indicada e insere o valor na posi��o desejada
      (let ((antes (subseq lista 0 indice))
            (depois (subseq lista (1+ indice))))
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

(defun numero-duplo (numero)
  "Verifica se o n�mero fornecido � um n�mero duplo (dois d�gitos iguais).
   Retorna T (true) se for um n�mero duplo, caso contr�rio retorna NIL (false)."
  (and (>= numero 10) (<= numero 99) ; Verifica se o n�mero tem dois d�gitos.
       (= (mod numero 10) (floor numero 10)))) ; Verifica se os dois d�gitos s�o iguais.

;; (numero-duplo 44) -> T
;; (numero-duplo 57) -> NIL
;; (numero-duplo 123) -> NIL (n�o � um n�mero de dois d�gitos)

(defun inicializar-cavalo (tabuleiro)
  "Coloca o cavalo na primeira posi��o do tabuleiro se ele n�o estiver presente."
  (unless (posicao-cavalo tabuleiro)
    (setf (car (car tabuleiro)) 'T))
  tabuleiro)


#|operador-1: Fun��o que recebe o tabuleiro e movimenta o cavalo para a posi��o 2 linhas abaixo e
uma coluna ao lado direito, ou seja, o movimento que de acordo com a Figura 1 leva o cavalo para a
casa de valor 10.|#

(defun operador-1 (tabuleiro)
  "Move o cavalo para 2 linhas abaixo e uma coluna � direita, aplicando a regra de n�mero sim�trico ou duplo."
  (let ((posicao (posicao-cavalo (inicializar-cavalo tabuleiro))))
    (let ((nova-linha (+ (first posicao) 2))
          (nova-coluna (- (second posicao) 1)))
      (if (and (>= nova-linha 0) (< nova-linha (length tabuleiro))
               (>= nova-coluna 0) (< nova-coluna (length (first tabuleiro))))
          (let ((numero (celula nova-linha nova-coluna tabuleiro)))
            ;; Aplicar a regra do n�mero sim�trico ou duplo
            (when (numero-duplo numero)
              ;; Se for um n�mero duplo, apagar o n�mero.
              (setf (nth nova-coluna (nth nova-linha tabuleiro)) nil))
            (when (numero-simetrico numero)
              ;; Se for um n�mero sim�trico, apagar o n�mero sim�trico.
              (let ((simetrico (numero-simetrico numero)))
                ;; Remove o sim�trico de todas as linhas.
                (setf tabuleiro (mapcar (lambda (linha)
                                          (mapcar (lambda (x) (if (equal x simetrico) nil x)) linha))
                                        tabuleiro))))
            ;; Remover o cavalo da posi��o atual
            (setf (nth (second posicao) (nth (first posicao) tabuleiro)) nil)
            ;; Mover o cavalo para a nova posi��o
            (setf (nth nova-coluna (nth nova-linha tabuleiro)) 'T))
          (format t "Movimento inv�lido.~%"))))
  tabuleiro)

(defun operador-2 (tabuleiro)
  "Move o cavalo para 2 linhas abaixo e uma coluna � direita, aplicando a regra de n�mero sim�trico ou duplo."
  (let ((posicao (posicao-cavalo (inicializar-cavalo tabuleiro))))
    (let ((nova-linha (+ (first posicao) 2))
          (nova-coluna (+ (second posicao) 1)))
      (if (and (>= nova-linha 0) (< nova-linha (length tabuleiro))
               (>= nova-coluna 0) (< nova-coluna (length (first tabuleiro))))
          (let ((numero (celula nova-linha nova-coluna tabuleiro)))
            ;; Aplicar a regra do n�mero sim�trico ou duplo
            (when (numero-duplo numero)
              ;; Se for um n�mero duplo, apagar o n�mero.
              (setf (nth nova-coluna (nth nova-linha tabuleiro)) nil))
            (when (numero-simetrico numero)
              ;; Se for um n�mero sim�trico, apagar o n�mero sim�trico.
              (let ((simetrico (numero-simetrico numero)))
                ;; Remove o sim�trico de todas as linhas.
                (setf tabuleiro (mapcar (lambda (linha)
                                          (mapcar (lambda (x) (if (equal x simetrico) nil x)) linha))
                                        tabuleiro))))
            ;; Remover o cavalo da posi��o atual
            (setf (nth (second posicao) (nth (first posicao) tabuleiro)) nil)
            ;; Mover o cavalo para a nova posi��o
            (setf (nth nova-coluna (nth nova-linha tabuleiro)) 'T))
          (format t "Movimento inv�lido.~%"))))
  tabuleiro)


;; (operador-2 (tabuleiro-teste)) -- 0 49 n�o est� a ficar a NIL
;; (operador-2 (tabuleiro-jogado)) -- 0 49 n�o est� a ficar a NIL

(defun operador-3 (tabuleiro)
  "Move o cavalo para 2 linhas abaixo e uma coluna � direita, aplicando a regra de n�mero sim�trico ou duplo."
  (let ((posicao (posicao-cavalo (inicializar-cavalo tabuleiro))))
    (let ((nova-linha (+ (first posicao) 1))
          (nova-coluna (+ (second posicao) 2)))
      (if (and (>= nova-linha 0) (< nova-linha (length tabuleiro))
               (>= nova-coluna 0) (< nova-coluna (length (first tabuleiro))))
          (let ((numero (celula nova-linha nova-coluna tabuleiro)))
            ;; Aplicar a regra do n�mero sim�trico ou duplo
            (when (numero-duplo numero)
              ;; Se for um n�mero duplo, apagar o n�mero.
              (setf (nth nova-coluna (nth nova-linha tabuleiro)) nil))
            (when (numero-simetrico numero)
              ;; Se for um n�mero sim�trico, apagar o n�mero sim�trico.
              (let ((simetrico (numero-simetrico numero)))
                ;; Remove o sim�trico de todas as linhas.
                (setf tabuleiro (mapcar (lambda (linha)
                                          (mapcar (lambda (x) (if (equal x simetrico) nil x)) linha))
                                        tabuleiro))))
            ;; Remover o cavalo da posi��o atual
            (setf (nth (second posicao) (nth (first posicao) tabuleiro)) nil)
            ;; Mover o cavalo para a nova posi��o
            (setf (nth nova-coluna (nth nova-linha tabuleiro)) 'T))
          (format t "Movimento inv�lido.~%"))))
  tabuleiro)

(defun operador-4 (tabuleiro)
  "Move o cavalo para 2 linhas abaixo e uma coluna � direita, aplicando a regra de n�mero sim�trico ou duplo."
  (let ((posicao (posicao-cavalo (inicializar-cavalo tabuleiro))))
    (let ((nova-linha (- (first posicao) 1))
          (nova-coluna (+ (second posicao) 2)))
      (if (and (>= nova-linha 0) (< nova-linha (length tabuleiro))
               (>= nova-coluna 0) (< nova-coluna (length (first tabuleiro))))
          (let ((numero (celula nova-linha nova-coluna tabuleiro)))
            ;; Aplicar a regra do n�mero sim�trico ou duplo
            (when (numero-duplo numero)
              ;; Se for um n�mero duplo, apagar o n�mero.
              (setf (nth nova-coluna (nth nova-linha tabuleiro)) nil))
            (when (numero-simetrico numero)
              ;; Se for um n�mero sim�trico, apagar o n�mero sim�trico.
              (let ((simetrico (numero-simetrico numero)))
                ;; Remove o sim�trico de todas as linhas.
                (setf tabuleiro (mapcar (lambda (linha)
                                          (mapcar (lambda (x) (if (equal x simetrico) nil x)) linha))
                                        tabuleiro))))
            ;; Remover o cavalo da posi��o atual
            (setf (nth (second posicao) (nth (first posicao) tabuleiro)) nil)
            ;; Mover o cavalo para a nova posi��o
            (setf (nth nova-coluna (nth nova-linha tabuleiro)) 'T))
          (format t "Movimento inv�lido.~%"))))
  tabuleiro)

(defun operador-5 (tabuleiro)
  "Move o cavalo para 2 linhas abaixo e uma coluna � direita, aplicando a regra de n�mero sim�trico ou duplo."
  (let ((posicao (posicao-cavalo (inicializar-cavalo tabuleiro))))
    (let ((nova-linha (- (first posicao) 2))
          (nova-coluna (+ (second posicao) 1)))
      (if (and (>= nova-linha 0) (< nova-linha (length tabuleiro))
               (>= nova-coluna 0) (< nova-coluna (length (first tabuleiro))))
          (let ((numero (celula nova-linha nova-coluna tabuleiro)))
            ;; Aplicar a regra do n�mero sim�trico ou duplo
            (when (numero-duplo numero)
              ;; Se for um n�mero duplo, apagar o n�mero.
              (setf (nth nova-coluna (nth nova-linha tabuleiro)) nil))
            (when (numero-simetrico numero)
              ;; Se for um n�mero sim�trico, apagar o n�mero sim�trico.
              (let ((simetrico (numero-simetrico numero)))
                ;; Remove o sim�trico de todas as linhas.
                (setf tabuleiro (mapcar (lambda (linha)
                                          (mapcar (lambda (x) (if (equal x simetrico) nil x)) linha))
                                        tabuleiro))))
            ;; Remover o cavalo da posi��o atual
            (setf (nth (second posicao) (nth (first posicao) tabuleiro)) nil)
            ;; Mover o cavalo para a nova posi��o
            (setf (nth nova-coluna (nth nova-linha tabuleiro)) 'T))
          (format t "Movimento inv�lido.~%"))))
  tabuleiro)

(defun operador-6 (tabuleiro)
  "Move o cavalo para 2 linhas abaixo e uma coluna � direita, aplicando a regra de n�mero sim�trico ou duplo."
  (let ((posicao (posicao-cavalo (inicializar-cavalo tabuleiro))))
    (let ((nova-linha (- (first posicao) 2))
          (nova-coluna (- (second posicao) 1)))
      (if (and (>= nova-linha 0) (< nova-linha (length tabuleiro))
               (>= nova-coluna 0) (< nova-coluna (length (first tabuleiro))))
          (let ((numero (celula nova-linha nova-coluna tabuleiro)))
            ;; Aplicar a regra do n�mero sim�trico ou duplo
            (when (numero-duplo numero)
              ;; Se for um n�mero duplo, apagar o n�mero.
              (setf (nth nova-coluna (nth nova-linha tabuleiro)) nil))
            (when (numero-simetrico numero)
              ;; Se for um n�mero sim�trico, apagar o n�mero sim�trico.
              (let ((simetrico (numero-simetrico numero)))
                ;; Remove o sim�trico de todas as linhas.
                (setf tabuleiro (mapcar (lambda (linha)
                                          (mapcar (lambda (x) (if (equal x simetrico) nil x)) linha))
                                        tabuleiro))))
            ;; Remover o cavalo da posi��o atual
            (setf (nth (second posicao) (nth (first posicao) tabuleiro)) nil)
            ;; Mover o cavalo para a nova posi��o
            (setf (nth nova-coluna (nth nova-linha tabuleiro)) 'T))
          (format t "Movimento inv�lido.~%"))))
  tabuleiro)

(defun operador-7 (tabuleiro)
  "Move o cavalo para 2 linhas abaixo e uma coluna � direita, aplicando a regra de n�mero sim�trico ou duplo."
  (let ((posicao (posicao-cavalo (inicializar-cavalo tabuleiro))))
    (let ((nova-linha (- (first posicao) 1))
          (nova-coluna (- (second posicao) 2)))
      (if (and (>= nova-linha 0) (< nova-linha (length tabuleiro))
               (>= nova-coluna 0) (< nova-coluna (length (first tabuleiro))))
          (let ((numero (celula nova-linha nova-coluna tabuleiro)))
            ;; Aplicar a regra do n�mero sim�trico ou duplo
            (when (numero-duplo numero)
              ;; Se for um n�mero duplo, apagar o n�mero.
              (setf (nth nova-coluna (nth nova-linha tabuleiro)) nil))
            (when (numero-simetrico numero)
              ;; Se for um n�mero sim�trico, apagar o n�mero sim�trico.
              (let ((simetrico (numero-simetrico numero)))
                ;; Remove o sim�trico de todas as linhas.
                (setf tabuleiro (mapcar (lambda (linha)
                                          (mapcar (lambda (x) (if (equal x simetrico) nil x)) linha))
                                        tabuleiro))))
            ;; Remover o cavalo da posi��o atual
            (setf (nth (second posicao) (nth (first posicao) tabuleiro)) nil)
            ;; Mover o cavalo para a nova posi��o
            (setf (nth nova-coluna (nth nova-linha tabuleiro)) 'T))
          (format t "Movimento inv�lido.~%"))))
  tabuleiro)

(defun operador-8 (tabuleiro)
  "Move o cavalo para 2 linhas abaixo e uma coluna � direita, aplicando a regra de n�mero sim�trico ou duplo."
  (let ((posicao (posicao-cavalo (inicializar-cavalo tabuleiro))))
    (let ((nova-linha (+ (first posicao) 1))
          (nova-coluna (- (second posicao) 2)))
      (if (and (>= nova-linha 0) (< nova-linha (length tabuleiro))
               (>= nova-coluna 0) (< nova-coluna (length (first tabuleiro))))
          (let ((numero (celula nova-linha nova-coluna tabuleiro)))
            ;; Aplicar a regra do n�mero sim�trico ou duplo
            (when (numero-duplo numero)
              ;; Se for um n�mero duplo, apagar o n�mero.
              (setf (nth nova-coluna (nth nova-linha tabuleiro)) nil))
            (when (numero-simetrico numero)
              ;; Se for um n�mero sim�trico, apagar o n�mero sim�trico.
              (let ((simetrico (numero-simetrico numero)))
                ;; Remove o sim�trico de todas as linhas.
                (setf tabuleiro (mapcar (lambda (linha)
                                          (mapcar (lambda (x) (if (equal x simetrico) nil x)) linha))
                                        tabuleiro))))
            ;; Remover o cavalo da posi��o atual
            (setf (nth (second posicao) (nth (first posicao) tabuleiro)) nil)
            ;; Mover o cavalo para a nova posi��o
            (setf (nth nova-coluna (nth nova-linha tabuleiro)) 'T))
          (format t "Movimento inv�lido.~%"))))
  tabuleiro)








