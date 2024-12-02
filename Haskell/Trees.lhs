Vou tentar uma abordagem sem ponteiros 
e nem hashs, mantendo simples, 
ainda que menos eficiente. 

\begin{code}
 module Trees where 

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


djisdj





