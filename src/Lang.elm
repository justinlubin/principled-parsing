module Lang exposing (..)

type Exp
  = Var String
  | Num Int
  | Plus Exp Exp
  | OperandHole
  | OperatorHole Exp Exp

type alias VSA =
  List VSATree

type VSATree
  = VVar (List String)
  | VNum (List Int)
  | VPlus VSA VSA
  | VOperandHole
  | VOperatorHole VSA VSA

type alias Shape =
  (Side, Side)

type Token
  = LPAREN
  | RPAREN
  | VAR String
  | NUM Int
  | PLUS
  | OPERAND_HOLE
  | OPERATOR_HOLE

type Side
  = Left
  | Right
