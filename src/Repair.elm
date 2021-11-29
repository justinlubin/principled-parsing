module Repair exposing
  ( shape
  , balance
  )

import Shape exposing (Side(..))
import Token exposing (Token(..))
import Exp exposing (Exp(..))
import Translation

import Utils

--------------------------------------------------------------------------------
-- Shape repair

glue : Token -> Token -> List Token
glue tok1 tok2 =
  case (Tuple.second (Token.shape tok1), Tuple.first (Token.shape tok2)) of
    (Left, Left) ->
      []

    (Right, Right) ->
      []

    (Left, Right) ->
      [OPERAND_HOLE]

    (Right, Left) ->
      [OPERATOR_HOLE]

innerShapeRepair : List Token -> List Token
innerShapeRepair toks =
  case toks of
    first :: second :: rest ->
      first
        :: glue first second
        ++ innerShapeRepair (second :: rest)

    _ ->
      toks

outerShapeRepair : List Token -> List Token
outerShapeRepair toks =
  case Utils.shell toks of
    Nothing ->
      []

    Just (first, last) ->
      let
        prefix =
          case Tuple.first (Token.shape first) of
            Left ->
              []

            Right ->
              [OPERAND_HOLE]

        suffix =
          case Tuple.second (Token.shape last) of
            Left ->
              [OPERAND_HOLE]

            Right ->
              []
      in
      prefix ++ toks ++ suffix

shape : List Token -> List Token
shape =
  innerShapeRepair >> outerShapeRepair

--------------------------------------------------------------------------------
-- Balance repair

parensNeeded : a -> a -> Int -> List a -> Int
parensNeeded left right depth xs =
  case xs of
    [] ->
      depth

    head :: tail ->
      let
        newDepth =
          if head == left then
            depth + 1
          else if head == right then
            depth - 1
          else
            depth
      in
      parensNeeded left right newDepth tail

--  ( ) a ) a ( a ( ) a (
-------------------------

wellShapedRightInsertions : List Token -> List (List Token)
wellShapedRightInsertions =
  Utils.rightInsertions
    ( \left maybeRight ->
        case maybeRight of
          Nothing ->
            Token.fits left RPAREN

          Just right ->
            Token.fits left RPAREN && Token.fits RPAREN right
    )
    RPAREN

rightConvexChunks : List Token -> List (List Token)
rightConvexChunks =
  Utils.groupBy
    ( \_ right ->
        Tuple.second (Token.shape right) == Right
    )

balanceForward : List Token -> List (List Token)
balanceForward toks =
  toks
    |> rightConvexChunks
    |> Utils.insertions
         (Utils.unmatchedLefts LPAREN RPAREN toks)
         [RPAREN]
    |> List.map List.concat

balance : List Token -> List (List Token)
balance =
  balanceForward
    >> List.concatMap
        ( Token.reverseMany
            >> balanceForward
            >> List.map Token.reverseMany
        )
    >> Utils.deduplicate
    >> List.filter (Translation.parse >> Utils.isJust)
