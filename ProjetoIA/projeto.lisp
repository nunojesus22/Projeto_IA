(defun diretoria-atual ()
  "Define o caminho para os ficheiros do projeto a partir da raiz"
  (let ((path "C:\\Users\\johnn\\OneDrive\\Ambiente de Trabalho\\Projeto_IA\\ProjetoIA\\"))
    path))
;; "C:\\Users\\nunoj\\Documents\\DEV\\Projeto_IA\\ProjetoIA\\"
;; "C:\\Users\\johnn\\OneDrive\\Ambiente de Trabalho\\Projeto_IA\\ProjetoIA\\"

(defun diretoria-solucao ()
  "Define o caminho para os ficheiros do projeto a partir da raiz"
  (let ((path "C:\\Users\\johnn\\OneDrive\\Ambiente de Trabalho\\Projeto_IA\\ProjetoIA\\solucao.dat"))
    path))

(defun play ()
  "Permite iniciar o programa carregando e compilando"
  (load (concatenate 'string (diretoria-atual) "problemas.dat" ))
  (load (concatenate 'string (diretoria-atual) "puzzle.lisp"   ))
  (menu-principal))

(defun imprimir-tabuleiro (tabuleiro)
  "Imprime o tabuleiro passado como par�metro."
  (dolist (linha tabuleiro)
    (dolist (elemento linha)
      (format t "~a " (if elemento elemento "--"))) ; Imprime o elemento ou "--" se for NIL
    (format t "~%"))) ; Nova linha ap�s imprimir cada linha do tabuleiro


#|-----------------------------------------------------------------------------------------------------------|#
#|------------------------------------------- MENU PRINCIPAL ------------------------------------------------|#
#|-----------------------------------------------------------------------------------------------------------|#

(defun menu-principal ()
  "Menu principal com op��es do programa"
  (loop
    (progn
      ;; Linha em branco
      (format t "~%")
      ;; Cabe�alho do menu
      (format t "~%          ++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
      (format t "~%          +                                                      +")
      (format t "~%          +                    JOGO DO CAVALO                    +")
      (format t "~%          +                                                      +")
      ;; Op��es do menu
      (format t "~%          +                    1-Jogar Jogo                      +")
      (format t "~%          +                    2-Mostrar Problemas               +")
      (format t "~%          +                    3-Regras do Jogo                  +")
      (format t "~%          +                    4-Sair                            +")
      (format t "~%          +                                                      +")
      (format t "~%          ++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
      ;; Linha em branco
      (format t "~%")
      ;; Entrada do utilizador
      (format t "~% Op��o => "))
    (let ((opcao (read)))
      (cond ((not (and (numberp opcao) (< opcao 5) (> opcao 0)))
             (progn
               ;; Mensagem de erro para op��o inv�lida
               (format t "~% Op��o Inv�lida~% Op��o => ")
               (setf opcao (read))))
            (T
             (progn
               ;; Op��o selecionada
               (format t "~% Op��o ~a selecionada." opcao)
               (case opcao
                 (1 (progn (format t "~% Resolver Jogo ainda n�o implementado ~% ")))
                 (2 (progn (format t "~% Abrindo Menu de Problemas... ~% ") (menu-problemas)))
                 (3 (progn (format t "~% Abrindo Menu de Regras... ~% ") (informacoes-regras)))
                 (4 (progn
                      ;; Mensagem de t�rmino do programa
                      (format t "~% Programa Terminado.")
                      (return))))))))))

#|-----------------------------------------------------------------------------------------------------------|#
#|------------------------------------- MENU COM OS DIFERENTES PROBLEMAS ------------------------------------|#
#|-----------------------------------------------------------------------------------------------------------|#

(defun menu-problemas ()
  "Menu para escolher o problema a ser resolvido"
  (loop
    (progn
      ;; Linha em branco
      (format t "~%")
      ;; Exibir cabe�alho do menu
      (format t "~%          ++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
      (format t "~%          +                   ESCOLHER UM PROBLEMA               +")
      (format t "~%          +                                                      +")
      (format t "~%          +                    1-Problema A                      +")
      (format t "~%          +                    2-Problema B                      +")
      (format t "~%          +                    3-Problema C                      +")
      (format t "~%          +                    4-Problema D                      +")
      (format t "~%          +                    5-Problema E                      +")
      (format t "~%          +                    6-Problema F                      +")
      (format t "~%          +                    7-Voltar ao Menu Principal        +")
      (format t "~%          +                                                      +")
      (format t "~%          ++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
      (format t "~%~%~% Op��o => "))
    
    (let ((escolha (read)))
      (cond ((not (and (numberp escolha) (< escolha 8) (> escolha 0)))
             (progn
               ;; Mensagem de erro para op��o inv�lida
               (format t "~% Op��o Inv�lida~% Op��o => ")
               (setf escolha (read))))
            (T
             (progn
               ;; Op��o selecionada
               (format t "~% Op��o ~a selecionada." escolha)
               (case escolha
                 (1 (progn (format t "~% Problema A: ~% ~%") (imprimir-tabuleiro (problem-A))(menu-problema-selecionado (problem-A))))
                 (2 (progn (format t "~% Problema B: ~% ~%") (imprimir-tabuleiro (problem-B))(menu-problema-selecionado (problem-B))))
                 (3 (progn (format t "~% Problema C: ~% ~%") (imprimir-tabuleiro (problem-C))(menu-problema-selecionado (problem-C))))
                 (4 (progn (format t "~% Problema D: ~% ~%") (imprimir-tabuleiro (problem-D))(menu-problema-selecionado (problem-D))))
                 (5 (progn (format t "~% Problema E: ~% ~%") (imprimir-tabuleiro (problem-E))(menu-problema-selecionado (problem-E))))
                 (6 (progn (format t "~% Problema F: ~% ~%") (imprimir-tabuleiro (problem-F))(menu-problema-selecionado (problem-F))))
                 (7 (progn (format t "~% Voltando ao Menu Principal... ~% ") (return)))
                 (T (progn (format t "~% Op��o Inv�lida~% Op��o => ") (setf escolha (read)))))))))))


#|-----------------------------------------------------------------------------------------------------------|#
#|--------------------------------------- MENU DO PROBLEMA SELECIONADO --------------------------------------|#
#|-----------------------------------------------------------------------------------------------------------|#

(defun menu-problema-selecionado (problema-selecionado)
  "Menu do problema a ser resolvido"
  (loop
     (progn
      ;; Linha em branco
      (format t "~%")
      ;; Exibir cabe�alho do menu
      (format t "~%          ++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
      (format t "~%          +                     MODO DE JOGO                     +")
      (format t "~%          +                                                      +")
      (format t "~%          +                    1-Escolher um algoritmo           +")
      (format t "~%          +                    2-Configurar o tabuleiro          +")
      (format t "~%          +                    3-Voltar ao Menu Problemas        +")
      (format t "~%          +                                                      +")
      (format t "~%          ++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
      (format t "~%~%~% Op��o => "))
     (let ((escolha (read)))
      (cond ((not (and (numberp escolha) (< escolha 4) (> escolha 0)))
             (progn
               ;; Mensagem de erro para op��o inv�lida
               (format t "~% Op��o Inv�lida~% Op��o => ")
               (setf escolha (read))))
            (T
             (progn
               ;; Op��o selecionada
               (format t "~% Op��o ~a selecionada." escolha)
               (case escolha
                (1 (progn (format t "~% Abrindo o menu dos algoritmos...: ~% ~%") (menu-algoritmos problema-selecionado))) ; Passa o problema selecionado
                 (2 (progn (format t "~% Abrindo o menu de confiurar o tabuleiro...: ~% ~%") (menu-configurar-tabuleiro)))
                 (3 (progn (format t "~% Voltando ao Menu Problemas... ~% ") (return)))
                 (T (progn (format t "~% Op��o Inv�lida~% Op��o => ") (setf escolha (read)))))))))))

#|-----------------------------------------------------------------------------------------------------------|#
#|------------------------------------------- MENU DOS ALGORITMOS -------------------------------------------|#
#|-----------------------------------------------------------------------------------------------------------|#

(defun menu-algoritmos (problema-selecionado)
  "Menu dos algoritmos para resolver o problema selecionado."
  (loop
     (progn
      ;; Linha em branco
      (format t "~%")
      ;; Exibir cabe�alho do menu
      (format t "~%          ++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
      (format t "~%          +                    ESCOLHA UM ALGORITMO              +")
      (format t "~%          +                                                      +")
      (format t "~%          +                    1-Algoritmo BFS                   +")
      (format t "~%          +                    2-Algoritmo DFS                   +")
      (format t "~%          +                    3-Algoritmo A*                    +")
      (format t "~%          +                    4-Voltar ao Menu Modo Jogo        +")
      (format t "~%          +                                                      +")
      (format t "~%          ++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
      (format t "~%~%~% Op��o => "))

     (let ((escolha (read)))
      (cond ((not (and (numberp escolha) (< escolha 5) (> escolha 0)))
             (progn
               ;; Mensagem de erro para op��o inv�lida
               (format t "~% Op��o Inv�lida~% Op��o => ")
               (setf escolha (read))))
            (T
             (progn
               ;; Op��o selecionada
               (format t "~% Op��o ~a selecionada." escolha)
               (case escolha
                (1 (progn 
     (format t "~% Aplicando o algoritmo BFS...: ~% ~%")
     ;; Apenas uma chamada para executar-e-capturar-bfs
     (let ((bfs-output (executar-e-capturar-output #'bfs (inicializar-cavalo problema-selecionado))))
       (escrever-menu-estatisticas problema-selecionado "BFS" bfs-output))
   ))
         (2 (progn 
     (format t "~% Aplicando o algoritmo DFS...: ~% ~%")
     ;; Apenas uma chamada para executar-e-capturar-dfs
     (let ((dfs-output (executar-e-capturar-output #'dfs (inicializar-cavalo problema-selecionado))))
       (escrever-menu-estatisticas problema-selecionado "DFS" dfs-output))
   ))          
                 (3 (progn (format t "~% Abrindo o menu do algoritmo A* ...: ~% ~%") (menu-aestrela problema-selecionado)))
                 (4 (progn (format t "~% Voltando ao Menu Modo de Jogo...") (return)))
                 (T (progn (format t "~% Op��o Inv�lida~% Op��o => ") (setf escolha (read)))))))))))

#|-----------------------------------------------------------------------------------------------------------|#
#|---------------------------------------------------- MENU A* ----------------------------------------------|#
#|-----------------------------------------------------------------------------------------------------------|#

(defun menu-aestrela (problema-selecionado)
  "Menu do problema a ser resolvido"
  (loop
     (progn
      ;; Linha em branco
      (format t "~%")
      ;; Exibir cabe�alho do menu
      (format t "~%          ++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
      (format t "~%          +                           A*                         +")
      (format t "~%          +                                                      +")
      (format t "~%          +           1- Heur�stica Base : h(x) = o(x)/m(x)      +")
      (format t "~%          +                2-Voltar ao Menu Algoritmos           +")
      (format t "~%          +                                                      +")
      (format t "~%          ++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
      (format t "~%~%~% Op��o => "))
     (let ((escolha (read)))
      (cond ((not (and (numberp escolha) (< escolha 3) (> escolha 0)))
             (progn
               ;; Mensagem de erro para op��o inv�lida
               (format t "~% Op��o Inv�lida~% Op��o => ")
               (setf escolha (read))))
            (T
             (progn
               ;; Op��o selecionada
               (format t "~% Op��o ~a selecionada." escolha)
               (case escolha
                 (1 (progn (format t "~% Aplicando o algoritmo A* com a h base.. ~% ") (definir-heuristica 'base)(let ((a-estrela-output (executar-e-capturar-output #'a-estrela (inicializar-cavalo problema-selecionado))))
       (escrever-menu-estatisticas problema-selecionado "A-ESTRELA" a-estrela-output))
   ))          
                 (2 (progn (format t "~% Voltando ao Menu Algoritmos... ~% ") (return)))
                 (T (progn (format t "~% Op��o Inv�lida~% Op��o => ") (setf escolha (read)))))))))))



#|-----------------------------------------------------------------------------------------------------------|#
#|---------------------------------------- MENU CONFIGURAR TABULEIRO ----------------------------------------|#
#|-----------------------------------------------------------------------------------------------------------|#

(defun menu-configurar-tabuleiro ()
  "Menu do problema a ser resolvido"
  (loop
     (progn
      ;; Linha em branco
      (format t "~%")
      ;; Exibir cabe�alho do menu
      (format t "~%          ++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
      (format t "~%          +                ESCOLHA A POSI��O DO CAVALO           +")
      (format t "~%          +                APENAS NA PRIMEIRA LINHA              +")
      (format t "~%          +                                                      +")
      (format t "~%          +                1-Voltar ao Menu Modo Jogo            +")
      (format t "~%          +                                                      +")
      (format t "~%          ++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
      (format t "~%~%~% Op��o => "))
     (let ((escolha (read)))
      (cond ((not (and (numberp escolha) (< escolha 2) (> escolha 0)))
             (progn
               ;; Mensagem de erro para op��o inv�lida
               (format t "~% Op��o Inv�lida~% Op��o => ")
               (setf escolha (read))))
            (T
             (progn
               ;; Op��o selecionada
               (format t "~% Op��o ~a selecionada." escolha)
               (case escolha
             
                 (1 (progn (format t "~% Voltando ao Menu Modo Jogo...") (return)))
                 (T (progn (format t "~% Op��o Inv�lida~% Op��o => ") (setf escolha (read)))))))))))






#|-----------------------------------------------------------------------------------------------------------|#
#|-------------------------------------------------- REGRAS -------------------------------------------------|#
#|-----------------------------------------------------------------------------------------------------------|#

(defun informacoes-regras ()
  "Menu de regras do jogo"
  (format t "

  ~%          +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ~%          +                                              GAME RULES                                             +
  ~%          +                                                                                                     +
  ~%          +  O tabuleiro tem dimens�es de 10x10, e cada c�lula cont�m um valor entre 00 e 99,                   +
  ~%          +  sem repeti��o.                                                                                     +
  ~%          +  Cada novo jogo gera um novo tabuleiro com valores distribu�dos aleatoriamente.                     +
  ~%          +  O objetivo � acumular mais pontos que o advers�rio, usando um cavalo de xadrez. Cada               +
  ~%          +  jogador tem um cavalo da sua cor (branco ou preto).                                                +
  ~%          +  O jogo come�a com a coloca��o do cavalo branco numa c�lula da 1� linha (A1-J1).                    +
  ~%          +  Esta casa � escolhida pelo jogador com o cavalo branco.                                            +
  ~%          +  Se a casa escolhida tiver um n�mero com dois d�gitos diferentes, por exemplo, 57,                  +
  ~%          +  durante o resto do jogo. Em outras palavras, nenhum cavalo pode terminar outra jogada              +
  ~%          +  nessa casa                                                                                         +
  ~%          +  Se um cavalo for colocado numa casa com um n�mero duplo, por exemplo, 66,                          +
  ~%          +  ent�o qualquer outro n�mero duplo pode ser removido, e o jogador deve escolher                     +
  ~%          +  com base na sua estrat�gia (por padr�o, remover o de maior valor). Depois de um jogador            +
  ~%          +  deixae a casa para se movimentar para outra, a casa onde estava fica tamb�m inacess�vel            +
  ~%          +  para o jogo, ficando o numero da casa apagado.                                                     +
  ~%          +  Ap�s a primeira jogada (colocar o cavalo branco), segue-se a jogada do advers�rio com coloca��o do +
  ~%          +  cavalo preto numa casa da 10� linha (A10-J10) do tabuleiro, casa essa que � escolhida pelo 2�      +
  ~%          +  jogador. O n�mero sim�trico da casa correspondente tamb�m � apagado.                               +
  ~%          +  Depois de ambos os cavalos serem colocados, todas as jogadas seguintes s�o efetuadas               +
  ~%          +  atrav�s de um movimento de cavalo (usando as regras tradicionais do Xadrez para o cavalo).         +
  ~%          +  Um cavalo n�o pode saltar para uma casa vazia (sem n�mero) e tamb�m n�o pode faz�-lo para uma      +
  ~%          +  casa que esteja amea�ada pelo cavalo advers�rio.                                                   +
  ~%          +  A cada jogada de um jogador repete-se a regra do sim�trico ou duplo.                               +
  ~%          +  Um jogador ganha pontos por cada casa visitada pelo seu cavalo (igual ao valor da casa).           +
  ~%          +  Os pontos s�o contabilizados apenas para as casas visitadas, n�o pelos n�meros sim�tricos          +
  ~%          +  ou duplos removidos.                                                                               +
  ~%          +  A transforma��o do jogo num problema, para esta fase do projeto, pressup�e as seguintes diferen�as +
  ~%          +  ao n�vel das regras:                                                                               +
  ~%          +  - Existe apenas um jogador (cavalo branco);                                                        +
  ~%          +  - O jogador come�a por colocar o cavalo numa casa da primeira linha do tabuleiro;                  +
  ~%          +  - O estado final � atingido quando o cavalo chega a uma casa que lhe permite obter uma pontua��o   +
  ~%          +    igual ou superior ao objetivo definido;                                                          +
  ~%          +  - Se n�o for poss�vel atingir o objetivo, o programa dever� informar o utilizador de que o         +
  ~%          +    problema n�o tem solu��o;                                                                        +
  ~%          +  - Os objetivos para os problemas A-F, fornecidos no documento anexo e que t�m de ser resolvidos    +
  ~%          +    no �mbito deste projeto, s�o: A: 70, B: 60, C: 270, D:600, E: 300, F:2000;                       +
  ~%          +  A inicializa��o do processo de resolu��o do problema consiste na aplica��o de um operador          +
  ~%          +  especial, de coloca��o do cavalo numa casa da primeira linha que tenha uma pontua��o num�rica.     +
  ~%          +  Esse operador permite fazer a gera��o dos sucessores do n�vel 1 a partir do n� raiz do grafo       +
  ~%          +  que representa cada um dos problemas acima referidos. A partir da�, s�o aplic�veis os operadores   +
  ~%          +  de movimenta��o do cavalo.                                                                         +
  ~%          +                                                                                                     +
  ~%          +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
  )
)

(defun executar-e-capturar-output (funcao &rest args)
  "Executa uma fun��o e captura seu output, retornando-o como uma string."
  (with-output-to-string (str)
    (let ((*standard-output* str))  ; Redireciona o standard output para a string 'str'
      (apply funcao args))))


(defun executar-e-capturar-bfs (problema)
  "Executa o algoritmo BFS e captura o output."
  (with-output-to-string (str)
    (bfs (inicializar-cavalo problema))))

(defun executar-e-capturar-dfs (problema)
  "Executa o algoritmo BFS e captura o output."
  (with-output-to-string (str)
    (dfs (inicializar-cavalo problema))))

(defun executar-e-capturar-a-estrela (problema)
  "Executa o algoritmo BFS e captura o output."
  (with-output-to-string (str)
    (a-estrela (inicializar-cavalo problema))))


(defun escrever-menu-estatisticas (problema algoritmo output)
  (with-open-file (stream (diretoria-solucao)
                          :direction :output
                          :if-exists :append
                          :if-does-not-exist :create)
    (format stream "~% ~% Problema: ~a~%Algoritmo: ~a~%Output: ~a~%" problema algoritmo output)))







