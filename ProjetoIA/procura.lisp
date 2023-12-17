#|-----------------------------------------------------------------------------------------------------------|#
#|----------------------------------------------------- BFS -------------------------------------------------|#
#|-----------------------------------------------------------------------------------------------------------|#

#|
- Utiliza uma estrutura de dados tipo fila (abertos).
- Explora os nós em um nível antes de passar para o próximo nível.
- Adiciona sucessores no final da fila (nconc abertos (list sucessor)).
|#

(defun bfs-recursivo (fila fechados nos-gerados nos-expandidos pontuacao-maxima melhor-caminho)
  "Função recursiva auxiliar para BFS. Retorna o caminho com a pontuação máxima."
  (if (null fila)
      (progn
        (format t "Busca concluída. Nenhum caminho encontrado.~%")
        (list melhor-caminho pontuacao-maxima nos-gerados nos-expandidos))
      (let* ((no-atual (first fila))
             (resto-fila (rest fila))
             (sucessores (expandir-no no-atual)))
        (format t "Nó expandido. ~%")
        (incf nos-expandidos)
        (dolist (sucessor sucessores)
          (unless (find sucessor fechados :test #'equal)
            (format t "Nó gerado. ~%")
            (incf nos-gerados)
            (if (and (numberp (pontuacao-atual sucessor))
                     (> (pontuacao-atual sucessor) pontuacao-maxima))
                (progn
                  (setf pontuacao-maxima (pontuacao-atual sucessor))
                  (setf melhor-caminho (caminho-ate-raiz sucessor))))))
        (push no-atual fechados)
        (format t "Total de nós gerados até agora: ~A, Nós expandidos até agora: ~A~%" nos-gerados nos-expandidos)
        (bfs-recursivo (append resto-fila sucessores) fechados nos-gerados nos-expandidos pontuacao-maxima melhor-caminho))))



(defun caminho-ate-raiz (no)
  "Retorna o caminho desde o nó até a raiz."
  (if (null (no-pai no))
      (list no)
      (cons no (caminho-ate-raiz (no-pai no)))))


#|
- novos-abertos é uma lista local usada para acumular os sucessores que ainda não foram explorados.
- Utilizamos nconc para concatenar novos-abertos ao final de abertos. Como nconc modifica a lista original, 
usamos reverse em novos-abertos para manter a ordem correta dos sucessores.
- A função então chama a si mesma recursivamente com a nova lista de abertos e a lista de fechados atualizada.
|#

(defun bfs (tabuleiro-problema)
  "Executa a busca em largura (BFS) no problema do tabuleiro para atingir o objetivo."
  (let* ((valor-celula-inicial (celula 0 0 tabuleiro-problema))
         (tabuleiro-inicializado (inicializar-cavalo tabuleiro-problema))
         (pontuacao-inicial (+ (if (numberp valor-celula-inicial) valor-celula-inicial 0) 2))
         (abertos (list (criar-no tabuleiro-inicializado (posicao-cavalo tabuleiro-inicializado) pontuacao-inicial 0 nil)))
         (tempo-execucao (* (medir-tempo-execucao 'bfs-recursivo abertos '() 0 0 0 '()) 1000))) ; Convertendo para milissegundos
    (format t "Tempo de execução: ~A milissegundos~%" tempo-execucao)
    tempo-execucao))



#|-----------------------------------------------------------------------------------------------------------|#
#|----------------------------------------------------- DFS -------------------------------------------------|#
#|-----------------------------------------------------------------------------------------------------------|#

#|
- Utiliza uma estrutura de dados tipo pilha (abertos).
- Explora profundamente cada caminho possível antes de recuar.
- Adiciona sucessores no topo da pilha (push sucessor abertos).
|#

(defun dfs-recursivo (abertos fechados nos-gerados nos-expandidos)
  "Função recursiva auxiliar para DFS. Processa nós de forma recursiva."
  (if abertos
      (let ((no-atual (pop abertos)))
        (format t "Nó expandido. ~%")
        (incf nos-expandidos)
        (dolist (sucessor (expandir-no no-atual))
          (unless (find sucessor fechados :test #'equal)
            (format t "Nó gerado. ~%")
            (incf nos-gerados)
            (if (atingiu-objetivo? sucessor)
                (let ((fator-ramificacao-media (calcular-fator-ramificacao-media nos-gerados nos-expandidos)))
                  (format t "Fator de Ramificação Média (DFS): ~A~%" fator-ramificacao-media)
                  (return (list 'Success sucessor nos-gerados nos-expandidos fator-ramificacao-media)))
                (push sucessor abertos))))
        (push no-atual fechados)
       (format t "Total de nós gerados: ~A, Total de nós expandidos: ~A~%" nos-gerados nos-expandidos)
        (dfs-recursivo abertos fechados nos-gerados nos-expandidos))
      (let ((fator-ramificacao-media (calcular-fator-ramificacao-media nos-gerados nos-expandidos)))
        (format t "Fator de Ramificação Média (DFS): ~A~%" fator-ramificacao-media)
        (list 'Failure nos-gerados nos-expandidos fator-ramificacao-media))))




(defun dfs (tabuleiro-problema)
  "Executa a busca em profundidade (DFS) no problema do tabuleiro para atingir o objetivo."
  (let* ((valor-celula-inicial (celula 0 0 tabuleiro-problema))
         (tabuleiro-inicializado (inicializar-cavalo tabuleiro-problema))
         (pontuacao-inicial (+ (if (numberp valor-celula-inicial) valor-celula-inicial 0) 2))
         (start-time (get-internal-real-time))
         (resultado (dfs-recursivo (list (criar-no tabuleiro-inicializado (posicao-cavalo tabuleiro-inicializado) pontuacao-inicial 0 nil)) '() 0 0))
         (end-time (get-internal-real-time))
         (tempo-execucao (* (/ (- end-time start-time) internal-time-units-per-second) 1000)))
    (format t "Tempo de execução (DFS): ~A milissegundos~%" tempo-execucao)
    resultado))


#|-----------------------------------------------------------------------------------------------------------|#
#|----------------------------------------------------- A* --------------------------------------------------|#
#|-----------------------------------------------------------------------------------------------------------|#

(defun f (no)
  "Calcula a função de avaliação f para o nó usando a heurística escolhida."
  (let ((heuristica (obter-heuristica)))
    (if (functionp heuristica)
        (+ (profundidade no) ; g(n): custo do caminho do nó inicial até n
           (funcall heuristica no)) ; h(n): heurística do nó até o objetivo
        (profundidade no)))
) ; Se não houver heurística, usa apenas g(n)

(defun merge-sort (list)
  (if (small list) list
	  (merge-lists
		(merge-sort (left-half list))
		(merge-sort (right-half list)))))

(defun small (list)
  (or (null list) (null (cdr list))))

(defun right-half (list)
  (last list (ceiling (/ (length list) 2))))
(defun left-half (list)
  (ldiff list (right-half list)))

(defun merge-lists (list1 list2)
  "Funde duas listas de nós ordenadas com base na função de avaliação f."
  (merge 'list list1 list2 (lambda (no1 no2) (< (f no1) (f no2)))))


(defun a-estrela-recursivo (abertos fechados nos-gerados nos-expandidos)
  "Função recursiva auxiliar para o algoritmo A*."
  (block a-estrela-bloco
    (if (null abertos)
        (let ((fator-ramificacao-media (calcular-fator-ramificacao-media nos-gerados nos-expandidos)))
          (format t "Fator de Ramificação Média (A*): ~A~%" fator-ramificacao-media)
          (list 'Failure nos-gerados nos-expandidos fator-ramificacao-media))
        (let* ((no-atual (first abertos))
               (abertos-restantes (rest abertos))
               (sucessores (expandir-no no-atual)))
          (format t "Nó expandido. ~%" )
          (incf nos-expandidos)
          (when (atingiu-objetivo? no-atual)
            (let ((fator-ramificacao-media (calcular-fator-ramificacao-media nos-gerados nos-expandidos)))
              (format t "Fator de Ramificação Média (A*): ~A~%" fator-ramificacao-media)
              (return-from a-estrela-bloco (list 'Success no-atual nos-gerados nos-expandidos fator-ramificacao-media))))
          (dolist (sucessor sucessores)
            (unless (find sucessor fechados :test #'equal)
              (format t "Nó gerado. ~%")
              (push sucessor abertos-restantes)
              (incf nos-gerados)))
          (push no-atual fechados)
        (format t "Total de nós gerados: ~A, Total de nós expandidos: ~A~%" nos-gerados nos-expandidos)
          (a-estrela-recursivo (merge-sort abertos-restantes) fechados nos-gerados nos-expandidos)))))




(defun a-estrela (tabuleiro-problema)
  "Executa o algoritmo A* no problema do tabuleiro para atingir o objetivo."
  (let* ((valor-celula-inicial (celula 0 0 tabuleiro-problema))
         (tabuleiro-inicializado (inicializar-cavalo tabuleiro-problema))
         (pontuacao-inicial (+ (if (numberp valor-celula-inicial) valor-celula-inicial 0) 2))
         (start-time (get-internal-real-time))
         (resultado (a-estrela-recursivo (list (criar-no tabuleiro-inicializado (posicao-cavalo tabuleiro-inicializado) pontuacao-inicial 0 nil)) '() 0 0))
         (end-time (get-internal-real-time))
         (tempo-execucao (* (/ (- end-time start-time) internal-time-units-per-second) 1000)))
    (format t "Tempo de execução (A*): ~A milissegundos~%" tempo-execucao)
    resultado))
