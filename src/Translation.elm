module Translation exposing
  ( tokenize
  , parse
  )

import Lang exposing (..)

tokenize : String -> List Token
tokenize =
  let
    helper : List Token -> List Char -> List Token
    helper acc cs =
      case cs of
        [] ->
          List.reverse acc

        ' ' :: rest ->
          helper acc rest

        '(' :: rest ->
          helper (LPAREN :: acc) rest

        ')' :: rest ->
          helper (RPAREN :: acc) rest

        '+' :: rest ->
          helper (PLUS :: acc) rest

        x :: rest ->
          let
            s =
              String.fromChar x

            token =
              case String.toInt s of
                Just n ->
                  NUM n

                Nothing ->
                  VAR s
          in
          helper (token :: acc) rest
  in
  String.toList >> helper []

consume : Token -> List Token -> Maybe (List Token)
consume tok toks =
  case toks of
    [] ->
      Nothing

    head :: tail ->
      if head == tok then
        Just tail
      else
        Nothing

parseBase : List Token -> Maybe (Exp, List Token)
parseBase toks =
  case toks of
    LPAREN :: toks2 ->
      parseTerm toks2 |> Maybe.andThen (\(e, toks3) ->
      consume RPAREN toks3 |> Maybe.map (\toks4 ->
        (e, toks4)
      ))

    NUM n :: toks2 ->
      Just (Num n, toks2)

    VAR x :: toks2 ->
      Just (Var x, toks2)

    OPERAND_HOLE :: toks2 ->
      Just (OperandHole, toks2)

    _ ->
      Nothing

parseTerm : List Token -> Maybe (Exp, List Token)
parseTerm toks =
  parseBase toks |> Maybe.andThen (\(left, toks2) ->
    case toks2 of
      PLUS :: toks3 ->
        parseTerm toks3 |> Maybe.map (\(right, toks4) ->
          (Plus left right, toks4)
        )

      OPERATOR_HOLE :: toks3 ->
        parseTerm toks3 |> Maybe.map (\(right, toks4) ->
          (OperatorHole left right, toks4)
        )

      _ ->
        Just (left, toks2)
  )

parse : List Token -> Maybe Exp
parse toks =
  case parseTerm toks of
    Just (e, []) ->
      Just e

    _ ->
      Nothing
