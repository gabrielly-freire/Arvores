Aqui, faremos os caminhos da árvore
-----------------------------------


Lidando com os pacotes:

\begin{code}
  module Paths where 

  import Trees
  import Util
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

  -- essa ideia do "X" só deu trabalho :P
  data Direction where 
    L :: Direction -- esquerda
    R :: Direction -- direita
    X :: Direction -- achou
    deriving (Eq, Show)

  type Path = [Direction]

  -- como escrever isso usando applicative?
  pathsTo :: Eq a => a -> Tree a -> [Path]
  pathsTo x Nil = []
  pathsTo x tree@(Tree val l r) 
    | x == val  = case (pl, pr) of
                    ([], []) -> [[X]]
                    (_, _) -> map (X:) (pl ++ pr)

    | otherwise = case (pl, pr) of
                    ([], []) -> []
                    (xs, []) -> map (L:) xs 
                    ([], ys) -> map (R:) ys
                    (xs, ys) -> map (L:) xs 
                             ++ map (R:) ys 

    where pl = pathsTo x l 
          pr = pathsTo x r

\end{code}

Ok, esse código merece melhorias.. Mas se funciona..
Agora faremos algo para separar alguns nós das árvores.

\begin{code}
  -- entradas bugadas viram a (cons Nil)
  getNode :: Path -> Tree a -> Tree a
  getNode (d : ds) tree@(Tree val l r) = 
    case d of 
      L -> getNode ds l
      R -> getNode ds r
      X -> tree 
  getNode _ _ = Nil 
  
  -- entradas bugadas viram a id
  deleteNode :: Path -> Tree a -> Tree a
  deleteNode _ Nil  = Nil 
  deleteNode [] tree = tree
  deleteNode (d : ds) (Tree val l r) = 
    case d of 
      L -> Tree val (deleteNode ds l) r
      R -> Tree val l (deleteNode ds r)
      X -> Nil 

  splitByPath :: Path -> Tree a -> (Tree a, Tree a)
  splitByPath ds = pairing (deleteNode ds) (getNode ds)
  
  replaceVal :: a -> Path -> Tree a -> Tree a
  replaceVal _ [] tree = tree
  replaceVal _ _ Nil   = Nil
  -- early X's would stop the list iterator
  replaceVal x (d : ds) (Tree val l r) = 
    case d of
      X -> Tree x l r
      L -> Tree val (replaceVal x ds l) r
      R -> Tree val l (replaceVal x ds r)
 
  -- unsafe
  tradeNodes :: Path -> Path -> Tree a -> Tree a
  tradeNodes ds ds' tree =  
    let (x, x') = (val $ getNode ds tree, val $ getNode ds' tree)
    in replaceVal x ds' (replaceVal x' ds tree)

  
  rightest :: Tree a -> Path 
  rightest Nil = []
  rightest (Tree _ l r) =
    case (l, r) of
      (Nil, Nil) -> [X]
      (l, Nil)   -> L : rightest l 
      (_, r)     -> R : rightest r

  
  rightestFromLeft :: Tree a -> Path
  rightestFromLeft Nil            = [] 
  rightestFromLeft (Tree _ Nil _) = [] 
  rightestFromLeft (Tree _ l _)   = 
    (L:) $ rightest l
      

  leftestFromRight :: Tree a -> Path
  leftestFromRight = undefined

  -- assumindo não bugados
  concatPaths :: Path -> Path -> Path
  concatPaths [] [] = [] 
  concatPaths [X] ds' = ds' 
  concatPaths (d:ds) ds' =  d : concatPaths ds ds'






\end{code}


  BKP
  splitByPath :: Path -> Tree a -> (Tree a, Tree a)
  splitByPath _ Nil   = (Nil, Nil) 
  splitByPath [] tree = (Nil, tree)
  splitByPath (d : ds) tree@(Tree val l r) =
    case d of
      L -> (Tree val Nil r, snd $ splitByPath ds l)
      R -> (Tree val l Nil, snd $ splitByPath ds r)
      X -> (Nil, tree)

  Segunda tentativa:

 splitByPath :: Path -> Tree a -> (Tree a, Tree a)
  splitByPath _ Nil    = error "empty Tree" 
  splitByPath [] _     = error "empty Path"
  -- um X prematuro irá encerrar a busca.
  splitByPath (X : ds) tree = (Nil, tree)

  splitByPath (d : d' : ds) tree@(Tree val l r) =
    case (d, d') of
      (L, X) -> ()
      (L, _) ->
      (R, X) ->
      (R, _) -> 

    where ls = splitByPath ds l 
          rs = splitByPath ds r 




