{-# LANGUAGE GADTs #-}

data Expr where
  Lit :: Integer -> Expr
  Add :: Expr -> Expr -> Expr
  Sub :: Expr -> Expr -> Expr
  Mul :: Expr -> Expr -> Expr
  Var :: String -> Expr    -- new constructor
  deriving Show


f :: Expr -> Expr
f x = x + 1

g :: Expr -> Expr -> Expr
g x y = x * x + y + 2

instance Num Expr where
  fromInteger n = Lit n
  e1 + e2 = Add e1 e2
  e1 - e2 = Sub e1 e2
  e1 * e2 = Mul e1 e2
