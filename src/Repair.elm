module Repair exposing
  ( shape
  , balance
  )

import Lang exposing (..)
import Token

import Utils

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
      let
        (_, firstRight) =
          Token.shape first

        (secondLeft, _) =
          Token.shape second
      in
      first
        :: glue first second
        ++ innerShapeRepair (second :: rest)

    _ ->
      toks

outerShapeRepair : List Token -> List Token
outerShapeRepair toks =
  case toks of
    [] ->
      []

    head :: tail ->
      let
        prefix =
          case Tuple.first (Token.shape head) of
            Left ->
              []

            Right ->
              [OPERAND_HOLE]

        suffix =
          case
            tail
              |> List.reverse
              |> List.head
              |> Maybe.withDefault head
              |> Token.shape
              |> Tuple.second
          of
            Left ->
              [OPERAND_HOLE]

            Right ->
              []
      in
      prefix ++ toks ++ suffix

shape : List Token -> List Token
shape =
  innerShapeRepair >> outerShapeRepair

wellShapedInsertions : Token -> List Token -> List (List Token)
wellShapedInsertions tok toks =
  case toks of
    [] ->
      [[tok]]

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
               (wellShapedInsertions tok (second :: rest))

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
            wellShapedInsertions right tail
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
    {- >> List.concatMap
         ( reverseTokens
             >> forwardRepair LPAREN RPAREN 0
             >> List.map reverseTokens
             ) -}
    >> Utils.deduplicate
