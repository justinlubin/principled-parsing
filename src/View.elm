module View exposing
  ( view
  )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Shape exposing (Shape, Side)
import Token exposing (Token(..))
import Exp exposing (Exp(..))
import Translation
import Repair
import Vsa

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
    [ h2
        []
        [ text "Input" ]
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
        [ text "Balance-repaired possibilities" ]
    , div
        []
        ( List.map
            ( \toks ->
                div
                  []
                  [ text <|
                      case Translation.parse toks of
                        Just e ->
                          expDebugString e

                        Nothing ->
                          "Did not parse!"
                  , div
                      [ class "tiles" ]
                      (List.map viewToken toks)
                  , br [] []
                  ]
            )
            possibleBalanceRepairedTokens
        )
    , h2
        []
        [ text "Version space algebra" ]
    , div
        []
        [ case maybePossibleTrees of
            Just [] ->
              text "No possible trees!"

            Just (head :: tail) ->
              text <| Vsa.debugString <|
                List.foldl
                  (\e -> Vsa.merge (Vsa.fromExp e))
                  (Vsa.fromExp head)
                  tail

            Nothing ->
              text "A possible list of tokens failed to parse!"
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
