module Syntax where

newtype TermMember = TermMember {tmbm :: String}

instance Show TermMember where
  show tm = "#" ++ tmbm tm

newtype TypeMember = TypeMember {tybm :: String}

instance Show TypeMember where
  show ty = "#" ++ tybm ty

newtype Variable = Variable {var :: String}

instance Show Variable where
  show v = "$" ++ var v

data Value
  = Object Variable Type Definition
  | Lambda Variable Type Term

instance Show Value where
  show (Object v t d) = "object (" ++ show v ++ ":" ++ show t ++ ")" ++ show d
  show (Lambda v t d) = "\\(" ++ show v ++ ":" ++ show t ++ ")" ++ show d

data Term
  = Var Variable
  | Val Value
  | App Variable Variable
  | Let Variable Term Term
  | Sel Variable TermMember

instance Show Term where
  show (Var v    ) = show v
  show (Val v    ) = show v
  show (App v1 v2) = show v1 ++ " " ++ show v2
  show (Let v t u) = "let " ++ show v ++ " = " ++ show t ++ " in " ++ show u
  show (Sel x a  ) = show x ++ "." ++ show a

data Type
  = Top
  | Bot
  | TypeDelc TypeMember Type Type
  | TypeProj Variable TypeMember
  | Func Variable Type Type
  | FieldDelc TermMember Type
  | Intersect Type Type           -- ^ t1 ^ t2
  | Mu Variable Type              -- ^ µ(x : T)

instance Show Type where
  show Top = "Top"
  show Bot = "Bot"
  show (TypeDelc a s u) =
    "{" ++ show a ++ ":" ++ show s ++ ".." ++ show u ++ "}"
  show (TypeProj v a ) = show v ++ "." ++ show a
  show (Func x t s   ) = "\\(" ++ show x ++ ":" ++ show t ++ ")" ++ show s
  show (FieldDelc a t) = "{" ++ show a ++ ":" ++ show t ++ "}"
  show (Intersect s u) = show s ++ "^" ++ show u
  show (Mu        v t) = "µ(" ++ show v ++ ":" ++ show t ++ ")"

data Definition
  = TypeDef TypeMember Type
  | FieldDef TermMember Term
  | AggrDef Definition Definition   -- ^ d1 ^ d2

instance Show Definition where
  show (TypeDef  a  t ) = "{" ++ show a ++ "=" ++ show t ++ "}"
  show (FieldDef a  t ) = "{" ++ show a ++ "=" ++ show t ++ "}"
  show (AggrDef  d1 d2) = show d1 ++ "^" ++ show d2
