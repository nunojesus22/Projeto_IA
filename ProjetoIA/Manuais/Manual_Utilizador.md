# Manual de Utilizador da 1ª Fase do Projeto - Época Normal

<p align="center">
  <img src="Imagens/logo_ests.png" alt="alt text">
</p>

## Inteligência Artificial - Escola Superior de Tecnologia de Setúbal 2023/2024

Prof. Joaquim Filipe

Eng. Filipe Mariano

# Jogo do Cavalo

Realizado por:

João Silva - 202100233

Nuno Jesus - 202100238

18 de dezembro de 2023

# Índice

1. Introdução
2. Instalação
3. Acrónimos e Convenções usadas
4. Utilização
5. Input/Output

# 1. Introdução

Este documento foi elaborado utilizando a linguagem de marcação Markdown e serve como um relatório detalhado do Manual do Utilizador para a 1ª Fase do Projeto "Jogo do Cavalo".

No contexto da unidade curricular de Inteligência Artificial, fomos encarregados de desenvolver o mencionado "Jogo do Cavalo". Este documento, redigido com minúcia, apresenta todos os passos e procedimentos necessários para que o utilizador efetue a instalação e interaja de forma eficaz com a aplicação do jogo.

# 2. Instalação

<p align="center">
  <img src="Imagens/lispworks.png" alt="alt text">
</p>

A aplicação requer a instalação do IDE LispWorks.

LispWorks é uma plataforma integrada que atua como ferramenta de desenvolvimento para a linguagem Common Lisp. Pode adquirir a edição PersonalEdition e efetuar o download [aqui](https://www.lispworks.com/products/lispworks.html).

# 3. Acrónimos e Convenções usadas

* CAR e CDR: Estes termos vêm de instruções de hardware de máquinas IBM antigas. CAR refere-se ao "Contents of the Address part of Register number", basicamente a primeira parte de uma célula de memória Lisp. CDR ("Contents of the Decrement part of Register number") refere-se à segunda parte.

```Lisp
(car '(a b c d)) ; Retorna 'a', o primeiro elemento da lista
(cdr '(a b c d)) ; Retorna '(b c d)', a lista sem o primeiro elemento
```

* CONS: Vem de "construct" e é usado para criar pares ou listas. Um "cons cell" é um par constituído por dois elementos: o CAR e o CDR.

```Lisp
(cons 'x '(y z)) ; Cria a lista '(x y z)'
```

* ATOM: Originalmente significando algo indivisível, em Lisp é usado para referir-se a qualquer coisa que não seja um par (ou seja, não um cons cell), como números, strings, símbolos.

```Lisp
(atom 'x) ; Retorna T, porque 'x' é um átomo
(atom '(a b)) ; Retorna NIL, porque '(a b)' não é um átomo, é uma lista
```

* LAMBDA: Usado para definir funções anónimas. O termo vem do cálculo lambda, uma formalização da computação.

```Lisp
(mapcar (lambda (x) (* x x)) '(1 2 3 4)) ; Retorna '(1 4 9 16)'
```

* NIL: Representa tanto a lista vazia quanto o valor booleano falso.

```Lisp
(if nil 'verdadeiro 'falso) ; Retorna 'falso'
```

* T: Representa o valor booleano verdadeiro.

```Lisp
(if t 'verdadeiro 'falso) ; Retorna 'verdadeiro'
```

* DEFUN: Utilizado para definir funções. É uma abreviação de "DEFine FUNction".

```Lisp
(defun quadrado (x)
  (* x x))
(quadrado 4) ; Retorna 16
```

* LET: Usado para ligar variáveis locais.

```Lisp
(let ((x 5)
      (y 3))
  (+ x y)) ; Retorna 8
```

* COND: Usado para construções condicionais, equivalente a um conjunto de instruções if-else.

```Lisp
(cond ((> 3 2) 'maior)
      ((< 3 2) 'menor)
      (t 'igual)) ; Retorna 'maior'
```

# 4. Utilização

A nossa aplicação em Lisp oferece uma interface de menu interativa e fácil de usar, permitindo aos utilizadores explorar diferentes funcionalidades do Jogo do Cavalo. Aqui estão os detalhes de como navegar pelos menus:

## Menu Principal

O Menu Principal apresenta as seguintes opções:

1. Jogar Jogo: Inicia uma nova sessão de jogo.
2. Mostrar Problemas: Exibe os diferentes problemas disponíveis para resolver.
3. Regras do Jogo: Fornece informações sobre as regras e mecânicas do jogo.
4. Sair: Encerra a aplicação.

Para escolher uma opção, insira o número correspondente e pressione Enter.

```Lisp
          ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
          +                                                      +
          +                    JOGO DO CAVALO                    +
          +                                                      +
          +                    1-Jogar Jogo                      +
          +                    2-Mostrar Problemas               +
          +                    3-Regras do Jogo                  +
          +                    4-Sair                            +
          +                                                      +
          ++++++++++++++++++++++++++++++++++++++++++++++++++++++++

 Opção => 
```

## Menu dos Problemas

Depois de selecionar "Mostrar Problemas" no Menu Principal, será apresentado o Menu dos Problemas, que permite escolher entre diferentes desafios, identificados como Problema A até Problema F.

Também existe a opção de voltar ao Menu Principal.

```Lisp
          ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
          +                   ESCOLHER UM PROBLEMA               +
          +                                                      +
          +                    1-Problema A                      +
          +                    2-Problema B                      +
          +                    3-Problema C                      +
          +                    4-Problema D                      +
          +                    5-Problema E                      +
          +                    6-Problema F                      +
          +                    7-Voltar ao Menu Principal        +
          +                                                      +
          ++++++++++++++++++++++++++++++++++++++++++++++++++++++++


 Opção => 
```

## Menu de Cada Problema

Ao selecionar um problema específico, será levado para o Menu do Problema Selecionado, onde poderá escolher entre diferentes ações, como escolher um algoritmo para resolver o problema, configurar o tabuleiro, ou retornar ao menu dos problemas.

```Lisp
          ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
          +                     MODO DE JOGO                     +
          +                                                      +
          +                    1-Escolher um algoritmo           +
          +                    2-Configurar o tabuleiro          +
          +                    3-Voltar ao Menu Problemas        +
          +                                                      +
          ++++++++++++++++++++++++++++++++++++++++++++++++++++++++


 Opção =>
```

## Menu dos Algoritmos

Dentro de cada problema, terá a opção de escolher um algoritmo específico (BFS, DFS, A*) ou configurar o tabuleiro. Cada um desses menus oferece opções relevantes, juntamente com a opção de voltar ao menu anterior.

```Lisp
          ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
          +                    ESCOLHA UM ALGORITMO              +
          +                                                      +
          +                    1-Algoritmo BFS                   +
          +                    2-Algoritmo DFS                   +
          +                    3-Algoritmo A*                    +
          +                    4-Voltar ao Menu Modo Jogo        +
          +                                                      +
          ++++++++++++++++++++++++++++++++++++++++++++++++++++++++


 Opção => 
```

* ### BFS (Breadth First Search)

Ao escolher a opção "1", o algoritmo selecionado irá resolver o problema escolhido.

* ### DFS (Depth First Search)

Ao escolher a opção "2", terá de inserir o valor da profundidade para obter a solucão do problema escolhido.

* ### A*

Ao escolher a opção "3", terá de escolher uma heurística para a solução do problema escolhido.

## Menu Configuração do Tabuleiro - ATUALIZAR

Dentro de cada problema, será possível configurar o tabuleiro, escolhendo a posição do cavalo (apenas na primeira linha).

```Lisp
          ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
          +                ESCOLHA A POSIÇÃO DO CAVALO           +
          +                APENAS NA PRIMEIRA LINHA              +
          +                                                      +
          +                1-Voltar ao Menu Modo Jogo            +
          +                                                      +
          ++++++++++++++++++++++++++++++++++++++++++++++++++++++++


 Opção =>
```

## Menu Regras do Jogo

Ao selecionar "Regras do Jogo" no Menu Principal, será apresentada uma descrição detalhada das regras e estratégias do Jogo do Cavalo.

```txt
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
  
          +  durante o resto do jogo. Em outras palavras, nenhum cavalo pode terminar outra jogada              +
  
          +  nessa casa.                                                                                       +
  
          +  Se um cavalo for colocado numa casa com um número duplo, por exemplo, 66,                          +
  
          +  então qualquer outro número duplo pode ser removido, e o jogador deve escolher                     +
  
          +  com base na sua estratégia (por padrão, remover o de maior valor). Depois de um jogador            +
  
          +  deixar a casa para se movimentar para outra, a casa onde estava fica também inacessível            +
  
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
  
          +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
```

## Encerrar a Aplicação

Para sair da aplicação, selecione "Sair" no Menu Principal. Esta ação encerrará o programa de forma segura.

# 5. Input/Output

## File Input

Nesta aplicação, utilizamos o ficheiro "problemas.dat" para carregar os problemas previamente definidos. Este ficheiro contém os vários problemas que serão integrados na aplicação. A função de leitura do ficheiro processa estas informações para permitir que o utilizador escolha entre os diversos problemas a serem resolvidos.

## File Ouput

Os resultados dos algoritmos implementados, como pontuação total, tempo de execução, número de movimentos, entre outros, são gravados no ficheiro "solucoes.dat". Este ficheiro serve como um registo de todas as soluções encontradas pelos algoritmos BFS, DFS, A*, e é uma ferramenta útil para análise posterior e verificação do desempenho do programa.

## Console Input

O input na consola é o método principal de interação do utilizador com a aplicação. Através da consola, o utilizador pode navegar pelos menus, selecionar opções, escolher problemas, configurar algoritmos e iniciar a resolução de problemas. Este input é essencial para o funcionamento interativo da aplicação.

## Console Ouput

A saída na consola é a representação visual dos menus, tabuleiros de jogo e resultados dos algoritmos. Inclui a exibição dos menus de navegação, os tabuleiros de cada problema selecionado e os detalhes das soluções encontradas pelos algoritmos. É fundamental para fornecer feedback ao utilizador sobre as ações realizadas e os resultados obtidos.
