module Teste where 

newtype Rev a = Rev a 
  deriving (Eq, Show)

instance Ord a => (Ord (Rev a)) where 
  Rev x <= Rev y = y <= x

instance Num String where 
  (+) = (++)
  (*) = (++)
  negate = id
  -- signum = 0 
  abs = const ""


