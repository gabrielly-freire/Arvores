Vou tentar uma abordagem sem ponteiros 
e nem hashs, mantendo simples, 
ainda que menos eficiente. 

\begin{code}
  module Trees where 
  --import qualified Data.Tree as T
  import StringFormatters
  import PrintTrees
--  import Data.Time (getCurrentTime, diffUTCTime, NominalDiffTime)
 -- import Data.IORef
 -- import Control.Monad.IO.Class (liftIO) 
\end{code}  


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
    deriving (Eq)  

  -- Agora podemos exibir as árvores:

  instance Show a => Show (Tree a) where 
    show :: Tree a -> String
    show Nil     = "" 
    show Tree {val, leftSon, rightSon} = 
      case (leftSon, rightSon) of
        (Nil, Nil) -> formatVal val
        (Nil, r)   -> formatVal val ++ formatTree r "R" 
        (l, Nil)   -> formatVal val ++ formatTree l "L" 
        (l, r)     -> formatVal val ++ formatTree l "L" ++ formatTree r "R"
      where formatVal v      = "(" ++ show v ++ ")"
            formatTree t str = " [" ++ str ++ ": " ++ show t ++ "]"


  mapTree :: (a -> b) -> (Tree a -> Tree b)
  mapTree = undefined 
  
  rev :: Tree a -> Tree a
  rev Nil = Nil 
  rev (Tree val l r) = (Tree val (rev r) (rev l))

  -- Definicao útil
  leaf :: a -> Tree a 
  leaf x = Tree x Nil Nil
    
\end{code}


  timed :: (a -> b) -> a -> IO (b, NominalDiffTime)
  timed f x =
    do 
      start <- getCurrentTime
      let result = f x
      end <- getCurrentTime
      let duration = diffUTCTime end start
      return (result, duration)


 Não sei oq essas coisas estão fazendo aqui!

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



  toGeneralTree :: Tree a -> T.Tree a
  toGeneralTree Nil            = error "Void Tree" 
  toGeneralTree (Tree val l r) = 
    case (l, r) of 
      (Nil, Nil)               -> T.Node val []

      (Tree {}, Nil) -> T.Node val [toGeneralTree l]

      (Nil, Tree {}) -> T.Node val [toGeneralTree r]

      (Tree {},
       Tree {})       -> T.Node val [toGeneralTree l,
                                     toGeneralTree r]
      
  instance Show a => Show (Tree a) where
    show :: Show a => Tree a -> String
    show tree =  show $ toGeneralTree tree
    


--  instance Show a => Show (Tree a) where
--    show :: Show a => Tree a -> String
--    show tree =  show $ toGeneralTree tree

  toGeneralTree :: Tree a -> T.Tree a
  toGeneralTree Nil            = error "Void Tree" 
  toGeneralTree (Tree val l r) = 
    case (l, r) of 
      (Nil, Nil)               -> T.Node val []

      (Tree {}, Nil) -> T.Node val [toGeneralTree l]

      (Nil, Tree {}) -> T.Node val [toGeneralTree r]

      (Tree {},
       Tree {})       -> T.Node val [toGeneralTree l,
                                     toGeneralTree r]
      
 
  p = pretty 
  pretty tree = putStrLn $ T.drawTree $ toGeneralTree tree
