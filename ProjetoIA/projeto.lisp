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
  "Imprime o tabuleiro passado como parâmetro."
  (dolist (linha tabuleiro)
    (dolist (elemento linha)
      (format t "~a " (if elemento elemento "--"))) ; Imprime o elemento ou "--" se for NIL
    (format t "~%"))) ; Nova linha após imprimir cada linha do tabuleiro


#|-----------------------------------------------------------------------------------------------------------|#
#|------------------------------------------- MENU PRINCIPAL ------------------------------------------------|#
#|-----------------------------------------------------------------------------------------------------------|#

(defun menu-principal ()
  "Menu principal com opções do programa"
  (loop
    (progn
      ;; Linha em branco
      (format t "~%")
      ;; Cabeçalho do menu
      (format t "~%          ++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
      (format t "~%          +                                                      +")
      (format t "~%          +                    JOGO DO CAVALO                    +")
      (format t "~%          +                                                      +")
      ;; Opções do menu
      (format t "~%          +                    1-Jogar Jogo                      +")
      (format t "~%          +                    2-Mostrar Problemas               +")
      (format t "~%          +                    3-Regras do Jogo                  +")
      (format t "~%          +                    4-Sair                            +")
      (format t "~%          +                                                      +")
      (format t "~%          ++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
      ;; Linha em branco
      (format t "~%")
      ;; Entrada do utilizador
      (format t "~% Opção => "))
    (let ((opcao (read)))
      (cond ((not (and (numberp opcao) (< opcao 5) (> opcao 0)))
             (progn
               ;; Mensagem de erro para opção inválida
               (format t "~% Opção Inválida~% Opção => ")
               (setf opcao (read))))
            (T
             (progn
               ;; Opção selecionada
               (format t "~% Opção ~a selecionada." opcao)
               (case opcao
                 (1 (progn (format t "~% Resolver Jogo ainda não implementado ~% ")))
                 (2 (progn (format t "~% Abrindo Menu de Problemas... ~% ") (menu-problemas)))
                 (3 (progn (format t "~% Abrindo Menu de Regras... ~% ") (informacoes-regras)))
                 (4 (progn
                      ;; Mensagem de término do programa
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
      ;; Exibir cabeçalho do menu
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
      (format t "~%~%~% Opção => "))
    
    (let ((escolha (read)))
      (cond ((not (and (numberp escolha) (< escolha 8) (> escolha 0)))
             (progn
               ;; Mensagem de erro para opção inválida
               (format t "~% Opção Inválida~% Opção => ")
               (setf escolha (read))))
            (T
             (progn
               ;; Opção selecionada
               (format t "~% Opção ~a selecionada." escolha)
               (case escolha
                 (1 (progn (format t "~% Problema A: ~% ~%") (imprimir-tabuleiro (problem-A))(menu-problema-selecionado (problem-A))))
                 (2 (progn (format t "~% Problema B: ~% ~%") (imprimir-tabuleiro (problem-B))(menu-problema-selecionado (problem-B))))
                 (3 (progn (format t "~% Problema C: ~% ~%") (imprimir-tabuleiro (problem-C))(menu-problema-selecionado (problem-C))))
                 (4 (progn (format t "~% Problema D: ~% ~%") (imprimir-tabuleiro (problem-D))(menu-problema-selecionado (problem-D))))
                 (5 (progn (format t "~% Problema E: ~% ~%") (imprimir-tabuleiro (problem-E))(menu-problema-selecionado (problem-E))))
                 (6 (progn (format t "~% Problema F: ~% ~%") (imprimir-tabuleiro (problem-F))(menu-problema-selecionado (problem-F))))
                 (7 (progn (format t "~% Voltando ao Menu Principal... ~% ") (return)))
                 (T (progn (format t "~% Opção Inválida~% Opção => ") (setf escolha (read)))))))))))


#|-----------------------------------------------------------------------------------------------------------|#
#|--------------------------------------- MENU DO PROBLEMA SELECIONADO --------------------------------------|#
#|-----------------------------------------------------------------------------------------------------------|#

(defun menu-problema-selecionado (problema-selecionado)
  "Menu do problema a ser resolvido"
  (loop
     (progn
      ;; Linha em branco
      (format t "~%")
      ;; Exibir cabeçalho do menu
      (format t "~%          ++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
      (format t "~%          +                     MODO DE JOGO                     +")
      (format t "~%          +                                                      +")
      (format t "~%          +                    1-Escolher um algoritmo           +")
      (format t "~%          +                    2-Configurar o tabuleiro          +")
      (format t "~%          +                    3-Voltar ao Menu Problemas        +")
      (format t "~%          +                                                      +")
      (format t "~%          ++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
      (format t "~%~%~% Opção => "))
     (let ((escolha (read)))
      (cond ((not (and (numberp escolha) (< escolha 4) (> escolha 0)))
             (progn
               ;; Mensagem de erro para opção inválida
               (format t "~% Opção Inválida~% Opção => ")
               (setf escolha (read))))
            (T
             (progn
               ;; Opção selecionada
               (format t "~% Opção ~a selecionada." escolha)
               (case escolha
                (1 (progn (format t "~% Abrindo o menu dos algoritmos...: ~% ~%") (menu-algoritmos problema-selecionado))) ; Passa o problema selecionado
                 (2 (progn (format t "~% Abrindo o menu de confiurar o tabuleiro...: ~% ~%") (menu-configurar-tabuleiro)))
                 (3 (progn (format t "~% Voltando ao Menu Problemas... ~% ") (return)))
                 (T (progn (format t "~% Opção Inválida~% Opção => ") (setf escolha (read)))))))))))

#|-----------------------------------------------------------------------------------------------------------|#
#|------------------------------------------- MENU DOS ALGORITMOS -------------------------------------------|#
#|-----------------------------------------------------------------------------------------------------------|#

(defun menu-algoritmos (problema-selecionado)
  "Menu dos algoritmos para resolver o problema selecionado."
  (loop
     (progn
      ;; Linha em branco
      (format t "~%")
      ;; Exibir cabeçalho do menu
      (format t "~%          ++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
      (format t "~%          +                    ESCOLHA UM ALGORITMO              +")
      (format t "~%          +                                                      +")
      (format t "~%          +                    1-Algoritmo BFS                   +")
      (format t "~%          +                    2-Algoritmo DFS                   +")
      (format t "~%          +                    3-Algoritmo A*                    +")
      (format t "~%          +                    4-Voltar ao Menu Modo Jogo        +")
      (format t "~%          +                                                      +")
      (format t "~%          ++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
      (format t "~%~%~% Opção => "))

     (let ((escolha (read)))
      (cond ((not (and (numberp escolha) (< escolha 5) (> escolha 0)))
             (progn
               ;; Mensagem de erro para opção inválida
               (format t "~% Opção Inválida~% Opção => ")
               (setf escolha (read))))
            (T
             (progn
               ;; Opção selecionada
               (format t "~% Opção ~a selecionada." escolha)
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
                 (T (progn (format t "~% Opção Inválida~% Opção => ") (setf escolha (read)))))))))))

#|-----------------------------------------------------------------------------------------------------------|#
#|---------------------------------------------------- MENU A* ----------------------------------------------|#
#|-----------------------------------------------------------------------------------------------------------|#

(defun menu-aestrela (problema-selecionado)
  "Menu do problema a ser resolvido"
  (loop
     (progn
      ;; Linha em branco
      (format t "~%")
      ;; Exibir cabeçalho do menu
      (format t "~%          ++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
      (format t "~%          +                           A*                         +")
      (format t "~%          +                                                      +")
      (format t "~%          +           1- Heurística Base : h(x) = o(x)/m(x)      +")
      (format t "~%          +                2-Voltar ao Menu Algoritmos           +")
      (format t "~%          +                                                      +")
      (format t "~%          ++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
      (format t "~%~%~% Opção => "))
     (let ((escolha (read)))
      (cond ((not (and (numberp escolha) (< escolha 3) (> escolha 0)))
             (progn
               ;; Mensagem de erro para opção inválida
               (format t "~% Opção Inválida~% Opção => ")
               (setf escolha (read))))
            (T
             (progn
               ;; Opção selecionada
               (format t "~% Opção ~a selecionada." escolha)
               (case escolha
                 (1 (progn (format t "~% Aplicando o algoritmo A* com a h base.. ~% ") (definir-heuristica 'base)(let ((a-estrela-output (executar-e-capturar-output #'a-estrela (inicializar-cavalo problema-selecionado))))
       (escrever-menu-estatisticas problema-selecionado "A-ESTRELA" a-estrela-output))
   ))          
                 (2 (progn (format t "~% Voltando ao Menu Algoritmos... ~% ") (return)))
                 (T (progn (format t "~% Opção Inválida~% Opção => ") (setf escolha (read)))))))))))



#|-----------------------------------------------------------------------------------------------------------|#
#|---------------------------------------- MENU CONFIGURAR TABULEIRO ----------------------------------------|#
#|-----------------------------------------------------------------------------------------------------------|#

(defun menu-configurar-tabuleiro ()
  "Menu do problema a ser resolvido"
  (loop
     (progn
      ;; Linha em branco
      (format t "~%")
      ;; Exibir cabeçalho do menu
      (format t "~%          ++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
      (format t "~%          +                ESCOLHA A POSIÇÃO DO CAVALO           +")
      (format t "~%          +                APENAS NA PRIMEIRA LINHA              +")
      (format t "~%          +                                                      +")
      (format t "~%          +                1-Voltar ao Menu Modo Jogo            +")
      (format t "~%          +                                                      +")
      (format t "~%          ++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
      (format t "~%~%~% Opção => "))
     (let ((escolha (read)))
      (cond ((not (and (numberp escolha) (< escolha 2) (> escolha 0)))
             (progn
               ;; Mensagem de erro para opção inválida
               (format t "~% Opção Inválida~% Opção => ")
               (setf escolha (read))))
            (T
             (progn
               ;; Opção selecionada
               (format t "~% Opção ~a selecionada." escolha)
               (case escolha
             
                 (1 (progn (format t "~% Voltando ao Menu Modo Jogo...") (return)))
                 (T (progn (format t "~% Opção Inválida~% Opção => ") (setf escolha (read)))))))))))






#|-----------------------------------------------------------------------------------------------------------|#
#|-------------------------------------------------- REGRAS -------------------------------------------------|#
#|-----------------------------------------------------------------------------------------------------------|#

(defun informacoes-regras ()
  "Menu de regras do jogo"
  (format t "

  ~%          +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ~%          +                                              GAME RULES                                             +
  ~%          +                                                                                                     +
  ~%          +  O tabuleiro tem dimensões de 10x10, e cada célula contém um valor entre 00 e 99,                   +
  ~%          +  sem repetição.                                                                                     +
  ~%          +  Cada novo jogo gera um novo tabuleiro com valores distribuídos aleatoriamente.                     +
  ~%          +  O objetivo é acumular mais pontos que o adversário, usando um cavalo de xadrez. Cada               +
  ~%          +  jogador tem um cavalo da sua cor (branco ou preto).                                                +
  ~%          +  O jogo começa com a colocação do cavalo branco numa célula da 1ª linha (A1-J1).                    +
  ~%          +  Esta casa é escolhida pelo jogador com o cavalo branco.                                            +
  ~%          +  Se a casa escolhida tiver um número com dois dígitos diferentes, por exemplo, 57,                  +
  ~%          +  durante o resto do jogo. Em outras palavras, nenhum cavalo pode terminar outra jogada              +
  ~%          +  nessa casa                                                                                         +
  ~%          +  Se um cavalo for colocado numa casa com um número duplo, por exemplo, 66,                          +
  ~%          +  então qualquer outro número duplo pode ser removido, e o jogador deve escolher                     +
  ~%          +  com base na sua estratégia (por padrão, remover o de maior valor). Depois de um jogador            +
  ~%          +  deixae a casa para se movimentar para outra, a casa onde estava fica também inacessível            +
  ~%          +  para o jogo, ficando o numero da casa apagado.                                                     +
  ~%          +  Após a primeira jogada (colocar o cavalo branco), segue-se a jogada do adversário com colocação do +
  ~%          +  cavalo preto numa casa da 10ª linha (A10-J10) do tabuleiro, casa essa que é escolhida pelo 2º      +
  ~%          +  jogador. O número simétrico da casa correspondente também é apagado.                               +
  ~%          +  Depois de ambos os cavalos serem colocados, todas as jogadas seguintes são efetuadas               +
  ~%          +  através de um movimento de cavalo (usando as regras tradicionais do Xadrez para o cavalo).         +
  ~%          +  Um cavalo não pode saltar para uma casa vazia (sem número) e também não pode fazê-lo para uma      +
  ~%          +  casa que esteja ameaçada pelo cavalo adversário.                                                   +
  ~%          +  A cada jogada de um jogador repete-se a regra do simétrico ou duplo.                               +
  ~%          +  Um jogador ganha pontos por cada casa visitada pelo seu cavalo (igual ao valor da casa).           +
  ~%          +  Os pontos são contabilizados apenas para as casas visitadas, não pelos números simétricos          +
  ~%          +  ou duplos removidos.                                                                               +
  ~%          +  A transformação do jogo num problema, para esta fase do projeto, pressupõe as seguintes diferenças +
  ~%          +  ao nível das regras:                                                                               +
  ~%          +  - Existe apenas um jogador (cavalo branco);                                                        +
  ~%          +  - O jogador começa por colocar o cavalo numa casa da primeira linha do tabuleiro;                  +
  ~%          +  - O estado final é atingido quando o cavalo chega a uma casa que lhe permite obter uma pontuação   +
  ~%          +    igual ou superior ao objetivo definido;                                                          +
  ~%          +  - Se não for possível atingir o objetivo, o programa deverá informar o utilizador de que o         +
  ~%          +    problema não tem solução;                                                                        +
  ~%          +  - Os objetivos para os problemas A-F, fornecidos no documento anexo e que têm de ser resolvidos    +
  ~%          +    no âmbito deste projeto, são: A: 70, B: 60, C: 270, D:600, E: 300, F:2000;                       +
  ~%          +  A inicialização do processo de resolução do problema consiste na aplicação de um operador          +
  ~%          +  especial, de colocação do cavalo numa casa da primeira linha que tenha uma pontuação numérica.     +
  ~%          +  Esse operador permite fazer a geração dos sucessores do nível 1 a partir do nó raiz do grafo       +
  ~%          +  que representa cada um dos problemas acima referidos. A partir daí, são aplicáveis os operadores   +
  ~%          +  de movimentação do cavalo.                                                                         +
  ~%          +                                                                                                     +
  ~%          +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
  )
)

(defun executar-e-capturar-output (funcao &rest args)
  "Executa uma função e captura seu output, retornando-o como uma string."
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







