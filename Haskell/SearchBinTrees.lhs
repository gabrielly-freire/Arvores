
Lidando com pacotes..

\begin{code}
  module SearchBinTrees where 

  import Trees
  import Paths
\end{code}  

---------------------------------------------------

Vou trocar os nomes de umas funções, mas estaremos
sempre indicando a qual nome original ela se refere.

----------------------------------------------------

Árvores de busca binária:

\begin{code}
  isBinarySearch :: Ord a => Tree a -> Bool
  isBinarySearch Nil                = True 
  isBinarySearch (Tree rootVal l r) = 
    case (l, r) of 
      (Nil, Nil)                 -> True 
      (Nil, Tree rightVal l2 r2) -> rootVal <= rightVal

                                 && isBinarySearch l2 
                                 && isBinarySearch r2

      (Tree leftVal  l1 r1, Nil) -> leftVal <= rootVal

                                 && isBinarySearch l1
                                 && isBinarySearch r1
      (Tree leftVal  l1 r1, 
       Tree rightVal l2 r2)      -> leftVal <= rootVal
                                 && rootVal <= rightVal
                                 
                                 && isBinarySearch l2 
                                 && isBinarySearch r2
                                
                                 && isBinarySearch l1
                                 && isBinarySearch r1
\end{code}

Uns exemplos pra testes: 

\begin{code}
  exampleTree20 :: Integral i => Tree i
  exampleTree20 =
    Tree { val      = 20
         , leftSon  = exampleTree10
         , rightSon = exampleTree40
         }    

  exampleTree10 :: Integral i => Tree i
  exampleTree10 = 
    Tree { val      = 10 
         , leftSon  = Nil
         , rightSon = leaf 15
         }    

  exampleTree40 :: Integral i => Tree i
  exampleTree40 = 
    Tree { val      = 40
         , leftSon  = leaf 30
         , rightSon = Tree 50 Nil (leaf 60) 
         }    

\end{code}

Algoritmo de Busca: 

\begin{code}
  isIn :: Eq a => a -> Tree a -> Bool
  isIn x Nil = False
  isIn x (Tree val l r) = 
    x == val || x `isIn` l || x `isIn` r
\end{code}

(Seguindo a obs de que o código
não deve inserir valores repetidos)
Inserção de um nó: 

\begin{code}
  with :: Ord a => Tree a -> a -> Tree a
  Nil `with` x     = leaf x
  tree@(Tree val l r) `with` x
    | x == val = tree
    | x <  val = Tree val (l `with` x) r 
    | x >  val = Tree val l (r `with` x) 

  insert :: Ord a => Tree a -> a -> Tree a
  insert = with

\end{code}

-------------------OBS-----------------------
Para desenvolver a parte de remoção, usaremos 
umas funções importadas do Path, mas, p/ ser
mais fiel com o método requerido, usaremos
umas funções menos eficientes em linguagens
declarativas. 
----------------------------------------------

Inicialmente, dados os requisitos para as árvores
binárias de busca, não consideraremos que haverão
elementos repetidos. Nesse sentido, todos os PATH's
serão singletons. 

Vou booleanizar o código apenas para `matchear` com
o que foi requisitado

\begin{code}

  isLeaf :: Tree a -> Bool 
  isLeaf (Tree _ Nil Nil) = True
  isLeaf _                = False
 
  fatherOfTwo :: Tree a -> Bool
  fatherOfTwo (Tree _ (Tree {}) (Tree {})) = True
  fatherOfTwo _                            = False

  fatherOfOne :: Tree a -> Bool
  fatherOfOne (Tree _ l Nil) = True 
  fatherOfOne (Tree _ Nil r) = True
  fatherOfOne _              = False 

  getFirstSon :: Tree a -> Tree a
  getFirstSon (Tree _ Nil r) = r
  getFirstSon (Tree _ l _)   = l
  getFirstSon _              = Nil 

  isNull :: Tree a -> Bool
  isNull = undefined  

  -- e deixa com os avós (assumindo que 
  -- o node tera exatamente 1 filho)
  killNode :: Path -> Tree a -> Tree a
  killNode [] tree = tree
  killNode _ Nil   = Nil
  killNode (d : ds) tree@(Tree val l r) = 
    case d of 
      X -> getFirstSon tree
      L -> Tree val (killNode ds l) r
      R -> Tree val l (killNode ds r)



  -- booleanismo: 
  removeNode :: Path -> Tree a -> Tree a
  removeNode _ Nil   = Nil
  removeNode [] tree = tree
  removeNode ds tree@(Tree x l r)
    | isLeaf      node = deleteNode ds tree
    | fatherOfOne node = killNode ds tree
    | fatherOfTwo node 
      = deleteNode safeLeafPath $
        tradeNodes ds safeLeafPath tree
    | otherwise        = Nil
    where node         = getNode ds tree
          safeLeafPath = concatPaths ds 
                          (rightestFromLeft node)
\end{code}

  Por fim, vamos fazer uma função que recebe uma lista
  e retorna uma Árvore de busca binária. 

\begin{code}
  fromList :: Ord a => [a] -> Tree a
  fromList = foldl insert Nil 
\end{code}

  O pior caso para inserir será o da lista estar ordenada, 
pois cada elemento seria maior que o maior da árvore até
então, criando uma árvore com apenas filhos direitos.

  Nesse cenário, para inserir o i-ésimo elemento, teria
que conferir se ele é maior que (i - 1) filhos. 
  Desse modo, considere que a? = a + (a-1) + (a-2) + .. + 1.
Nesse sentido, teremos 1? + 2? + 3? + 4? + .. + n? iterações,
  Depois de muitos cálculos, chegamos em [n(n + 1)(n + 2)] / 6









