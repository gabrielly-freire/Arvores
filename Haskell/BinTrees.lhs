
Lidando com pacotes..

\begin{code}
  module BinTrees where 

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
      (Nil, Tree rightVal l2 r2) -> rootVal < rightVal

                                 && isBinarySearch l2 
                                 && isBinarySearch r2

      (Tree leftVal  l1 r1, Nil) -> leftVal < rootVal

                                 && isBinarySearch l1
                                 && isBinarySearch r1
      (Tree leftVal  l1 r1, 
       Tree rightVal l2 r2)      -> leftVal < rootVal
                                 && rootVal < rightVal
                                 
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

\end{code}





