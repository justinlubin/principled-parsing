module View exposing
  ( view
  )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import BinaryTreeDiagram as BTD

import Shape exposing (Shape, Side)
import Token exposing (Token(..))
import Exp exposing (Exp(..))
import Translation
import Repair
import ParseForest exposing (ParseForest(..))

import Model exposing (Model)
import Update exposing (Msg(..))

viewSide : Side -> Html Msg
viewSide s =
  case s of
    Shape.Left ->
      span [ class "left" ] []

    Shape.Right ->
      span [ class "right" ] []

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
  span
    [ class "tile"
    , class (tokenClass tok)]
    [ viewSide left
    , span [ class "contents" ] [ text (Token.contents tok) ]
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

binaryTreeFromExp : Exp -> BTD.BinaryTree (String, String)
binaryTreeFromExp e =
  case e of
    Var s ->
      BTD.Node ("#99FF99", s) BTD.Empty BTD.Empty

    Num n ->
      BTD.Node ("#99FF99", String.fromInt n) BTD.Empty BTD.Empty

    Plus e1 e2 ->
      BTD.Node ("#99FFFF", "+") (binaryTreeFromExp e1) (binaryTreeFromExp e2)

    OperandHole ->
      BTD.Node ("#FF9999", "?") BTD.Empty BTD.Empty

    OperatorHole e1 e2 ->
      BTD.Node ("#99FFFF", "?") (binaryTreeFromExp e1) (binaryTreeFromExp e2)

viewParseForest : ParseForest -> Html Msg
viewParseForest (P exps) =
  div
    [ class "parse-forest"
    ]
    ( List.map
        (binaryTreeFromExp >> BTD.diagramView Tuple.first Tuple.second)
        exps
    )

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
  div
    []
    [ h1
        []
        [ text "Principled Parsing of Incomplete Programs"
        ]
    , input
        [ class "main-input"
        , type_ "text"
        , onInput InputChanged
        ]
        []
    , h2
        []
        [ text "Tokens" ]
    , div
        [ class "tiles" ]
        ( List.map viewToken tokens
        )
    , h2
        []
        [ text "Shape-repaired tokens" ]
    , div
        [ class "tiles" ]
        ( List.map viewToken shapeRepairedTokens
        )
    , h2
        []
        [ text "Balance-repaired tokens" ]
    , div
        []
        ( List.map
            ( \toks ->
                div
                  [ class "tiles" ]
                  (List.map viewToken toks)
            )
            possibleBalanceRepairedTokens
        )
    , h2
        []
        [ text "Parse forest" ]
    , div
        []
        ( case maybePossibleTrees of
            Just possibleTrees ->
              [ possibleTrees
                  |> ParseForest.fromList
                  |> viewParseForest
              ]

            Nothing ->
              [ text "A possible list of tokens failed to parse!"
              ]
        )
    ]
