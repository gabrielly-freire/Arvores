Vou tentar uma abordagem sem ponteiros 
e nem hashs, mantendo simples, 
ainda que menos eficiente. 

\begin{code}
  module Trees where 

  import StringFormatters
 -- import Data.IORef
 -- import Control.Monad.IO.Class (liftIO) 
\end{code}
     

\usepackage{listings}
\lstloadlanguages{Haskell}
\lstnewenvironment{code}
    {\lstset{}%
      \csname lst@SetFirstLabel\endcsname}
    {\csname lst@SaveFirstLabel\endcsname}
    \lstset{
      basicstyle=\small\ttfamily,
      flexiblecolumns=false,
      basewidth={0.5em,0.45em},
      literate={+}{{$+$}}1 {/}{{$/$}}1 {*}{{$*$}}1 {=}{{$=$}}1
               {>}{{$>$}}1 {<}{{$<$}}1 {\\}{{$\lambda$}}1
               {\\\\}{{\char`\\\char`\\}}1
               {->}{{$\rightarrow$}}2 {>=}{{$\geq$}}2 {<-}{{$\leftarrow$}}2
               {<=}{{$\leq$}}2 {=>}{{$\Rightarrow$}}2 
               {\ .}{{$\circ$}}2 {\ .\ }{{$\circ$}}2
               {>>}{{>>}}2 {>>=}{{>>=}}2
               {|}{{$\mid$}}1               
    }


Certo, vamos começar.
Primeiro, definamos as árvores
de maneira generalista p/ α. 

\begin{code}
  data Tree a where
   Nil  :: Tree a 
   Tree :: { val      :: a
           , leftSon  :: Tree a
           , rightSon :: Tree a
           }          -> Tree a
    deriving Eq  
\end{code}


Talvez essa sintaxe ainda não pareça muito amigável, mas é questão de costume
Agora vamos definir como exibir listas (`traduzí-las` em String) de uma maneira padrão. 

Vamos adicionar umas árvores padrão para testes:

\begin{code}
  t0 = Nil
  t1 = Tree 1 t0 t0 
  t2 = Tree 2 t0 t1 
  t3 = Tree 3 t1 t2 
  t4 = Tree 4 t2 t3 
  t5 = Tree 5 t3 t4
\end{code}


Vamos definir algumas funções auxiliares:

\begin{code}
  height :: Integral i => Tree a -> i
  height Nil          = 0 
  height (Tree _ l r) = 
   1 + max (height l) (height r)

  showFloor :: (Integral i, Show a) => i -> Tree a -> String
  showFloor 0 Nil = "_"
  showFloor _ Nil = ""
  showFloor num tree@(Tree val l r) 
    | num < 0  = error "num < 0" 
    | num == 0 = show val
    | num > 0  = showFloor (num - 1) l ++ "," ++ showFloor (num - 1) r


  showFormattedFloor :: (Integral i, Show a) => i -> Tree a -> String
  showFormattedFloor num tree = formatTreeStr (showFloor num tree)
    
    {-
      case (l,r) of 
         (Nil, Nil) -> "" 
         (Nil, r)   -> showFloor (num - 1) r 
         (l,  Nil)  -> showFloor (num - 1) l 
         (l,  r)    -> showFloor (num - 1) l ++ ", " ++ showFloor (num - 1) r  
    -}
  


  showHeight :: (Show a, Integral i) => Tree a -> i -> [String]
  showHeight Nil _ = [] 
  showHeight tree@(Tree val leftSon rightSon) num
    | num < 0  = error "Numero < 0"
    | num == 0 = [show val]
    | num == 1 = showHeight leftSon (num - 1)  ++ showHeight rightSon (num - 1)
    | num > 1  = showHeight tree (num - 1)
     
    
\end{code}


\begin{code}
  instance Show (Tree a) where 
    show :: Tree a -> String
    show Nil = "Nil"
    show (Tree val leftSon rightSon) = 
      case leftSon of 
        Nil  -> ""
        tree -> ""

\end{code}

  Esse show funfará p/ Árvores pequenas


\begin{code}
  getVal :: Tree a -> Maybe a
  getVal Nil = Nothing
  getVal Tree { val } = Just val
  
  getLeftSon :: Tree a -> Tree a
  getLeftSon Nil = Nil
  getLeftSon Tree { leftSon } = leftSon
  
  getRightSon :: Tree a -> Tree a
  getRightSon Nil = Nil
  getRightSon Tree { rightSon } = rightSon
\end{code}





/begin{code}
   
/end{code}








