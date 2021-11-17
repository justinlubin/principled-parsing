module Main exposing (..)

import Browser
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE

type Exp
  = Var String
  | Plus Exp Exp

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

type alias Shape =
  (Side, Side)

shape : Token -> Shape
shape tok =
  case tok of
    LPAREN ->
      (Left, Left)

    RPAREN ->
      (Right, Right)

    VAR _ ->
      (Left, Right)

    NUM _ ->
      (Left, Right)

    PLUS ->
      (Right, Left)

    OPERAND_HOLE ->
      (Left, Right)

    OPERATOR_HOLE ->
      (Right, Left)

contents : Token -> String
contents tok =
  case tok of
    LPAREN ->
      "("

    RPAREN ->
      ")"

    VAR x ->
      x

    NUM n ->
      String.fromInt n

    PLUS ->
      "+"

    OPERAND_HOLE ->
      "?"

    OPERATOR_HOLE ->
      "?"

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

-- Model

type alias Model =
  { input : String
  }

init : Model
init =
  { input =
      "1 + (2 + 3)"
  }


-- Update

type Msg =
  InputChanged String

update : Msg -> Model -> Model
update msg model =
  case msg of
    InputChanged newInput ->
      { model | input = newInput }

-- View

viewSide : Side -> Html Msg
viewSide s =
  case s of
    Left ->
      H.span [ HA.class "left" ] []

    Right ->
      H.span [ HA.class "right" ] []

viewToken : Token -> Html Msg
viewToken tok =
  let
    (left, right) =
      shape tok
  in
  H.span
    [ HA.class "tile" ]
    [ viewSide left
    , H.span [ HA.class "contents" ] [ H.text (contents tok) ]
    , viewSide right
    ]

view : Model -> Html Msg
view model =
  H.div
    []
    [ H.div
        [ HA.class "tiles" ]
        ( model.input
            |> tokenize
            |> List.map viewToken
        )
    , H.input
        [ HA.class "main-input"
        , HA.type_ "text"
        , HE.onInput InputChanged
        ]
        []
    ]

-- Main

main : Program () Model Msg
main =
  Browser.sandbox
    { init = init
    , update = update
    , view = view
    }

