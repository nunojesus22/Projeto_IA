
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
  +  O tabuleiro tem dimensões de 10x10, e cada célula contém um valor entre 00 e 99,                   +
  +  sem repetição.                                                                                     +
  +  Cada novo jogo gera um novo tabuleiro com valores distribuídos aleatoriamente.                     +
  +  O objetivo é acumular mais pontos que o adversário, usando um cavalo de xadrez. Cada               +
  +  jogador tem um cavalo da sua cor (branco ou preto).                                                +
  +  O jogo começa com a colocação do cavalo branco numa célula da 1ª linha (A1-J1).                    +
  +  Esta casa é escolhida pelo jogador com o cavalo branco.                                            +
  +  Se a casa escolhida tiver um número com dois dígitos diferentes, por exemplo, 57,                  +
  +  então o número simétrico 75 é apagado do tabuleiro, tornando esta casa inacessível                 +
  +  durante o resto do jogo. Em outras palavras, nenhum cavalo pode terminar outra jogada              +
  +  nessa casa                                                                                         +
  +  Se um cavalo for colocado numa casa com um número duplo, por exemplo, 66,                          +
  +  então qualquer outro número duplo pode ser removido, e o jogador deve escolher                     +
  +  com base na sua estratégia (por padrão, remover o de maior valor). Depois de um jogador            +
  +  deixae a casa para se movimentar para outra, a casa onde estava fica também inacessível            +
  +  para o jogo, ficando o numero da casa apagado.                                                     +
  +  Após a primeira jogada (colocar o cavalo branco), segue-se a jogada do adversário com colocação do +
  +  cavalo preto numa casa da 10ª linha (A10-J10) do tabuleiro, casa essa que é escolhida pelo 2º      +
  +  jogador. O número simétrico da casa correspondente também é apagado.                               +
  +  Depois de ambos os cavalos serem colocados, todas as jogadas seguintes são efetuadas               +
  +  através de um movimento de cavalo (usando as regras tradicionais do Xadrez para o cavalo).         +
  +  Um cavalo não pode saltar para uma casa vazia (sem número) e também não pode fazê-lo para uma      +
  +  casa que esteja ameaçada pelo cavalo adversário.                                                   +
  +  A cada jogada de um jogador repete-se a regra do simétrico ou duplo.                               +
  +  Um jogador ganha pontos por cada casa visitada pelo seu cavalo (igual ao valor da casa).           +
  +  Os pontos são contabilizados apenas para as casas visitadas, não pelos números simétricos          +
  +  ou duplos removidos.                                                                               +
  +  A transformação do jogo num problema, para esta fase do projeto, pressupõe as seguintes diferenças +
  +  ao nível das regras:                                                                               +
  +  - Existe apenas um jogador (cavalo branco);                                                        +
  +  - O jogador começa por colocar o cavalo numa casa da primeira linha do tabuleiro;                  +
  +  - O estado final é atingido quando o cavalo chega a uma casa que lhe permite obter uma pontuação   +
  +    igual ou superior ao objetivo definido;                                                          +
  +  - Se não for possível atingir o objetivo, o programa deverá informar o utilizador de que o         +
  +    problema não tem solução;                                                                        +
  +  - Os objetivos para os problemas A-F, fornecidos no documento anexo e que têm de ser resolvidos    +
  +    no âmbito deste projeto, são: A: 70, B: 60, C: 270, D:600, E: 300, F:2000;                       +
  +  A inicialização do processo de resolução do problema consiste na aplicação de um operador          +
  +  especial, de colocação do cavalo numa casa da primeira linha que tenha uma pontuação numérica.     +
  +  Esse operador permite fazer a geração dos sucessores do nível 1 a partir do nó raiz do grafo       +
  +  que representa cada um dos problemas acima referidos. A partir daí, são aplicáveis os operadores   +
  +  de movimentação do cavalo.                                                                         +
  +                                                                                                     +
  +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
  )
)














