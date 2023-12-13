
(defun diretoria-atual ()
  "Define o caminho para os ficheiros do projeto a partir da raiz"
  (merge-pathnames "CC:/Users/johnn/OneDrive/Ambiente de Trabalho/ProjetoIA/" *default-pathname-defaults*))

(defun start ()
  "Permite iniciar o programa carregando e compilando"
  (load (compile-file (merge-pathnames "problemas.dat" (diretoria-atual))))
  (load (compile-file (merge-pathnames "puzzle.lisp" (diretoria-atual))))
  ;; (load (compile-file (merge-pathnames "procura.lisp" (diretoria-atual)))) ;
  (main-menu))

#|-----------------------------------------------------------------------------------------------------------|#
#|------------------------------------------- MENU PRINCIPAL ------------------------------------------------|#
#|-----------------------------------------------------------------------------------------------------------|#

(defun main-menu ()
  "Main menu with program options"
  (loop
    (progn
      ;; Empty line
      (format t "~%")
      ;; Menu header
      (format t "~%          ++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
      (format t "~%          +                                                      +")
      (format t "~%          +                     KNIGHT GAME                      +")
      (format t "~%          +                                                      +")
      ;; Menu options
      (format t "~%          +                     1-Play Game                      +")
      (format t "~%          +                     2-Show Problems                  +")
      (format t "~%          +                     3-Game Rules                     +")
      (format t "~%          +                     4-Quit                           +")
      (format t "~%          +                                                      +")
      (format t "~%          ++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
      ;; Empty line
      (format t "~%")
      ;; User input
      (format t "~% Option => "))
    (let ((choice (read)))
      (cond ((not (and (numberp choice) (< choice 5) (> choice 0)))
             (progn
               ;; Error message for invalid choice
               (format t "~% Invalid Choice~% Option => ")
               (setf choice (read))))
            (T
             (progn
               ;; Selected option
               (format t "~% Option ~a selected." choice)
               (case choice
                 (1 (progn (format t "~% Solve a Game not implemented yet")))
                 (2 (progn (format t "~% Opening Problems Menu...") (problems-menu)))
                 (3 (progn (format t "~% Opening Rules Menu...") (rules-info)))
                 (4 (progn
                      ;; Program termination message
                      (format t "~% Program Terminated.")
                      (return))))))))))

#|-----------------------------------------------------------------------------------------------------------|#
#|------------------------------------- MENU COM OS DIFERENTES PROBLEMAS ------------------------------------|#
#|-----------------------------------------------------------------------------------------------------------|#

(defun problems-menu ()
  "Menu to choose the problem for solving"
  (loop
    (progn
      ;; Empty line
      (format t "~%")
      ;; Display the menu header
      (format t "~%          ++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
      (format t "~%          +                   CHOOSE A PROBLEM                   +")
      (format t "~%          +                                                      +")
      ;; Menu options based on the number of problems in your file
      (format t "~%          +                    1-Problem A                       +")
      (format t "~%          +                    2-Problem B                       +")
      (format t "~%          +                    3-Problem C                       +")
      (format t "~%          +                    4-Problem D                       +")
      (format t "~%          +                    5-Problem E                       +")
      (format t "~%          +                    6-Problem F                       +")
      (format t "~%          +                    7-Return to Main Menu             +")
      (format t "~%          +                                                      +")
      (format t "~%          ++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
      (format t "~%~%~%          Option => "))
    
   (cond ((not (let ((escolha (read)))
                 (cond 
                  ((and (numberp escolha) (< escolha 9) (> escolha 0) ) (case escolha
                                                                          (1 (print (pprint (problem-A))))
                                                                          (2 (print (pprint (problem-B))))
                                                                          (3 (print (pprint (problem-C))))
                                                                          (4 (print (pprint (problem-D))))
                                                                          (5 (print (pprint (problem-E))))
                                                                          (6 (print (pprint (problem-F))))
                                                                          (8 (progn nil))))
                  ( T (progn   (format t "~% Invalid Choice~% Option => ")(setf escolha (read))))))) 
          (return)))))


#|-----------------------------------------------------------------------------------------------------------|#
#|-------------------------------------------------- REGRAS -------------------------------------------------|#
#|-----------------------------------------------------------------------------------------------------------|#

(defun rules-info ()
  "2-Game rules menu"
  (format t "

  +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  +                                              GAME RULES                                             +
  +                                                                                                     +
  +  O tabuleiro tem dimens�es de 10x10, e cada c�lula cont�m um valor entre 00 e 99,                   +
  +  sem repeti��o.                                                                                     +
  +  Cada novo jogo gera um novo tabuleiro com valores distribu�dos aleatoriamente.                     +
  +  O objetivo � acumular mais pontos que o advers�rio, usando um cavalo de xadrez. Cada               +
  +  jogador tem um cavalo da sua cor (branco ou preto).                                                +
  +  O jogo come�a com a coloca��o do cavalo branco numa c�lula da 1� linha (A1-J1).                    +
  +  Esta casa � escolhida pelo jogador com o cavalo branco.                                            +
  +  Se a casa escolhida tiver um n�mero com dois d�gitos diferentes, por exemplo, 57,                  +
  +  ent�o o n�mero sim�trico 75 � apagado do tabuleiro, tornando esta casa inacess�vel                 +
  +  durante o resto do jogo. Em outras palavras, nenhum cavalo pode terminar outra jogada              +
  +  nessa casa                                                                                         +
  +  Se um cavalo for colocado numa casa com um n�mero duplo, por exemplo, 66,                          +
  +  ent�o qualquer outro n�mero duplo pode ser removido, e o jogador deve escolher                     +
  +  com base na sua estrat�gia (por padr�o, remover o de maior valor). Depois de um jogador            +
  +  deixae a casa para se movimentar para outra, a casa onde estava fica tamb�m inacess�vel            +
  +  para o jogo, ficando o numero da casa apagado.                                                     +
  +  Ap�s a primeira jogada (colocar o cavalo branco), segue-se a jogada do advers�rio com coloca��o do +
  +  cavalo preto numa casa da 10� linha (A10-J10) do tabuleiro, casa essa que � escolhida pelo 2�      +
  +  jogador. O n�mero sim�trico da casa correspondente tamb�m � apagado.                               +
  +  Depois de ambos os cavalos serem colocados, todas as jogadas seguintes s�o efetuadas               +
  +  atrav�s de um movimento de cavalo (usando as regras tradicionais do Xadrez para o cavalo).         +
  +  Um cavalo n�o pode saltar para uma casa vazia (sem n�mero) e tamb�m n�o pode faz�-lo para uma      +
  +  casa que esteja amea�ada pelo cavalo advers�rio.                                                   +
  +  A cada jogada de um jogador repete-se a regra do sim�trico ou duplo.                               +
  +  Um jogador ganha pontos por cada casa visitada pelo seu cavalo (igual ao valor da casa).           +
  +  Os pontos s�o contabilizados apenas para as casas visitadas, n�o pelos n�meros sim�tricos          +
  +  ou duplos removidos.                                                                               +
  +  A transforma��o do jogo num problema, para esta fase do projeto, pressup�e as seguintes diferen�as +
  +  ao n�vel das regras:                                                                               +
  +  - Existe apenas um jogador (cavalo branco);                                                        +
  +  - O jogador come�a por colocar o cavalo numa casa da primeira linha do tabuleiro;                  +
  +  - O estado final � atingido quando o cavalo chega a uma casa que lhe permite obter uma pontua��o   +
  +    igual ou superior ao objetivo definido;                                                          +
  +  - Se n�o for poss�vel atingir o objetivo, o programa dever� informar o utilizador de que o         +
  +    problema n�o tem solu��o;                                                                        +
  +  - Os objetivos para os problemas A-F, fornecidos no documento anexo e que t�m de ser resolvidos    +
  +    no �mbito deste projeto, s�o: A: 70, B: 60, C: 270, D:600, E: 300, F:2000;                       +
  +  A inicializa��o do processo de resolu��o do problema consiste na aplica��o de um operador          +
  +  especial, de coloca��o do cavalo numa casa da primeira linha que tenha uma pontua��o num�rica.     +
  +  Esse operador permite fazer a gera��o dos sucessores do n�vel 1 a partir do n� raiz do grafo       +
  +  que representa cada um dos problemas acima referidos. A partir da�, s�o aplic�veis os operadores   +
  +  de movimenta��o do cavalo.                                                                         +
  +                                                                                                     +
  +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
  )
)














