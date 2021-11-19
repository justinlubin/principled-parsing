module View exposing
  ( view
  )

import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE

import Lang exposing (..)
import Token
import Translation
import Repair
import VersionSpaceAlgebra as VSA

import Model exposing (Model)
import Update exposing (Msg(..))

viewSide : Side -> Html Msg
viewSide s =
  case s of
    Left ->
      H.span [ HA.class "left" ] []

    Right ->
      H.span [ HA.class "right" ] []

tokenClass : Token -> String
tokenClass tok =
  case tok of
    LPAREN ->
      "paren"

    RPAREN ->
      "paren"

    VAR _ ->
      "var"

    NUM _ ->
      "num"

    PLUS ->
      "op"

    OPERAND_HOLE ->
      "hole"

    OPERATOR_HOLE ->
      "hole"

viewToken : Token -> Html Msg
viewToken tok =
  let
    (left, right) =
      Token.shape tok
  in
  H.span
    [ HA.class "tile"
    , HA.class (tokenClass tok)]
    [ viewSide left
    , H.span [ HA.class "contents" ] [ H.text (Token.contents tok) ]
    , viewSide right
    ]

sequenceMaybe : List (Maybe a) -> Maybe (List a)
sequenceMaybe xs =
  case xs of
    [] ->
      Just []

    Just x :: tail ->
      Maybe.map ((::) x) (sequenceMaybe tail)

    Nothing :: tail ->
      Nothing

view : Model -> Html Msg
view model =
  let
    tokens =
      Translation.tokenize model.input

    shapeRepairedTokens =
      Repair.shape tokens

    possibleBalanceRepairedTokens =
      Repair.balance shapeRepairedTokens

    maybePossibleTrees =
      possibleBalanceRepairedTokens
        |> List.map Translation.parse
        |> sequenceMaybe
  in
  H.div
    []
    [ H.h2
        []
        [ H.text "Input" ]
    , H.input
        [ HA.class "main-input"
        , HA.type_ "text"
        , HE.onInput InputChanged
        ]
        []
    , H.h2
        []
        [ H.text "Tokens" ]
    , H.div
        [ HA.class "tiles" ]
        ( List.map viewToken tokens
        )
    , H.h2
        []
        [ H.text "Shape-repaired tokens" ]
    , H.div
        [ HA.class "tiles" ]
        ( List.map viewToken shapeRepairedTokens
        )
    , H.h2
        []
        [ H.text "Balance-repaired possibilities" ]
    , H.div
        []
        ( List.map
            ( \toks ->
                H.div
                  []
                  [ H.text <|
                      case Translation.parse toks of
                        Just e ->
                          expDebugString e

                        Nothing ->
                          "Did not parse!"
                  , H.div
                      [ HA.class "tiles" ]
                      (List.map viewToken toks)
                  , H.br [] []
                  ]
            )
            possibleBalanceRepairedTokens
        )
    , H.h2
        []
        [ H.text "Version space algebra" ]
    , H.div
        []
        [ case maybePossibleTrees of
            Just [] ->
              H.text "No possible trees!"

            Just (head :: tail) ->
              H.text <| VSA.debugString <|
                List.foldl
                  (\e -> VSA.merge (VSA.fromExp e))
                  (VSA.fromExp head)
                  tail

            Nothing ->
              H.text "A possible list of tokens failed to parse!"
        ]
    ]

expDebugString : Exp -> String
expDebugString e =
  case e of
    Var x ->
      x

    Num n ->
      String.fromInt n

    Plus e1 e2 ->
      "(+ " ++ expDebugString e1 ++ " " ++ expDebugString e2 ++ ")"

    OperandHole ->
      "?"

    OperatorHole e1 e2 ->
      "(?op " ++ expDebugString e1 ++ " " ++ expDebugString e2 ++ ")"
