module Repair exposing
  ( shape
  , balance
  )

import Shape exposing (Side(..))
import Token exposing (Token(..))
import Exp exposing (Exp(..))

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

wellShapedRightInsertions : Token -> List Token -> List (List Token)
wellShapedRightInsertions tok toks =
  case toks of
    [] ->
      [[]]

    [head] ->
      if Token.fits head tok then
        [[head, tok]]
      else
        [[head]]

    first :: second :: rest ->
      let
        extra =
          if Token.fits first tok && Token.fits tok second && second /= tok then
            [first :: tok :: second :: rest]
          else
            []
      in
        extra
          ++ List.map
               ((::) first)
               (wellShapedRightInsertions tok (second :: rest))

checkForwardBalanced : Token -> Token -> Int -> List Token -> Bool
checkForwardBalanced left right depth toks =
  case toks of
    [] ->
      depth <= 0

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
      checkForwardBalanced left right newDepth tail

forwardRepair : Token -> Token -> Int -> List Token -> List (List Token)
forwardRepair left right depth toks =
  case toks of
    [] ->
      [[]]

    head :: tail ->
      let
        newDepth =
          if head == left then
            depth + 1
          else
            depth

        partiallyRepairedTails =
          if head /= left || checkForwardBalanced left right newDepth tail then
            [tail]
          else
            -- wellShapedInsertions right tail
            [tail]
      in
      List.map
        ((::) head)
        ( List.concatMap
            (forwardRepair left right newDepth)
            partiallyRepairedTails
        )

balance : List Token -> List (List Token)
balance =
  forwardRepair LPAREN RPAREN 0
    >> List.concatMap
      ( Token.reverseMany
          >> forwardRepair LPAREN RPAREN 0
          >> List.map Token.reverseMany
          )
    >> Utils.deduplicate
