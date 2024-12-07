Aqui, faremos os caminhos da árvore
-----------------------------------


Lidando com os pacotes:

\begin{code}
  module Paths where 

  import Trees
\end{code}  

Vamos adicionar umas árvores padrão para testes:

\begin{code}
  t0 = Nil
  t1 = Tree 1 t0 t0 
  t2 = Tree 2 t0 t1 
  t3 = Tree 3 t1 t2 
  t4 = Tree 4 t2 t3 
  t5 = Tree 5 t3 t4

  -- Os exemplos padrão usados em slide:
  tA :: Tree String 
  tA = Tree "A" tB tC 

  tB :: Tree String 
  tB = Tree { val      = "B"
            , leftSon  = Tree "D" Nil (Tree "J" Nil Nil)
            , rightSon = Tree "H" Nil Nil
            }

  tC :: Tree String 
  tC = Tree { val      = "C"
            , leftSon  = Tree "E" Nil Nil
            , rightSon = Tree "F" (Tree "I" Nil Nil) Nil
            }

\end{code}

Primeiro vamos definir algumas funções auxiliares:

\begin{code}
  height :: Integral i => Tree a -> i
  height Nil          = 0 
  height (Tree _ l r) = 
   1 + max (height l) (height r)

  projectFloor :: Integral i => i -> Tree a -> [a]
  projectFloor _ Nil              = []
  projectFloor num (Tree val tl tr)  
    | num < 0  = error "negative Floor"
    | num == 0 = [val]
    | num > 0  = projectFloor (num - 1) tl 
              ++ projectFloor (num - 1) tr

  showFloor :: (Integral i, Show a) => i -> Tree a -> String
  showFloor 0 Nil = "_"
  showFloor _ Nil = ""
  showFloor num tree@(Tree val l r) 
    | num < 0  = error "num < 0" 
    | num == 0 = show val
    | num > 0  = showFloor (num - 1) l ++ "," ++ showFloor (num - 1) r

\end{code}

Agora podemos definir todos os caminhos pedidos.

\begin{code}
  inPreOrder :: Tree a -> [a]
  inPreOrder Nil            = []
  inPreOrder (Tree val l r) =
    [val] ++ inPreOrder l ++ inPreOrder r

  inSimOrder :: Tree a -> [a]
  inSimOrder Nil            = [] 
  inSimOrder (Tree val l r) = 
    inSimOrder l ++ [val] ++ inSimOrder r


  inPosOrder :: Tree a -> [a]
  inPosOrder Nil            = [] 
  inPosOrder (Tree val l r) = 
    inPosOrder l ++ inPosOrder r ++ [val]


  inLevel :: Tree a -> [a]
  inLevel tree = concat [projectFloor n tree | n <- [0 .. height tree - 1]]
\end{code}


-------------------------------------------------------
Obrigado!
-------------------------------------------------------


Agora, aproveitando o nome do pacote, definirei aqui
algumas funções que mostrar-se-ão bem úteis mais adiante. 

\begin{code}
  data Direction where 
    L :: Direction -- esquerda
    R :: Direction -- direita
    X :: Direction -- achou
    deriving (Eq, Show)

  type Path = [Direction]

  -- como escrever isso usando applicative?
  pathTo :: Eq a => a -> Tree a -> [Path]
  pathTo x Nil = []
  pathTo x tree@(Tree val l r) 
    | x == val  = case (pl, pr) of
                    ([], []) -> [[X]]
                    (_, _) -> map (X:) (pl ++ pr)

    | otherwise = case (pl, pr) of
                    ([], []) -> []
                    (xs, []) -> map (L:) xs 
                    ([], ys) -> map (R:) ys
                    (xs, ys) -> map (L:) xs 
                             ++ map (R:) ys 

    where pl = pathTo x l 
          pr = pathTo x r

\end{code}

Ok, esse código merece melhorias.. Mas se funciona..
Agora faremos algo para separar alguns nós das árvores.

\begin{code}
  splitByPath :: Path -> Tree a -> (Tree a, Tree a)
  splitByPath _ Nil   = (Nil, Nil) 
  splitByPath [] tree = (Nil, tree)
  splitByPath (d : ds) tree@(Tree val l r) =
    case d of
      L -> (Tree val Nil r, snd $ splitByPath ds l)
      R -> (Tree val l Nil, snd $ splitByPath ds r)
      X -> (Nil, tree)
  
\end{code}

