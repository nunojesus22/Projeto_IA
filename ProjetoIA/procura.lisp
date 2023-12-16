#|----- ALGORITMO BFS ----|#
(defun bfs (tabuleiro-inicial)
  (let* 
      (abertos (sucessores-tabuleiro-inicial (criar-no (tabuleiro-inicial) NIL NIL NIL NIL (obter-heuristica)) (valores-disponiveis-primeira-linha (tabuleiro-inicial))))
  )
)