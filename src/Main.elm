module Main exposing (..)

import Browser
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE

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

emptyVsa : VSA
emptyVsa =
  []

mergeTree : VSATree -> VSATree -> VSA
mergeTree t1 t2 =
  case (t1, t2) of
    (VVar xs1, VVar xs2) ->
      [VVar (deduplicate (xs1 ++ xs2))]

    (VNum ns1, VNum ns2) ->
      [VNum (deduplicate (ns1 ++ ns2))]

    (VPlus vsa11 vsa12, VPlus vsa21 vsa22) ->
      [VPlus (merge vsa11 vsa21) (merge vsa12 vsa22)]

    (VOperandHole, VOperandHole) ->
      [VOperandHole]

    (VOperatorHole vsa11 vsa12, VOperatorHole vsa21 vsa22) ->
      [VOperatorHole (merge vsa11 vsa21) (merge vsa12 vsa22)]

    _ ->
      [t1, t2]

-- TODO: definitely not correct! consider a+b merged with c+d
merge : VSA -> VSA -> VSA
merge vts1 vts2 =
  List.concatMap
    (\vt1 -> List.concatMap (mergeTree vt1) vts2)
    vts1

toVsa : Exp -> VSA
toVsa e =
  case e of
    Var x ->
      [VVar [x]]

    Num n ->
      [VNum [n]]

    Plus e1 e2 ->
      [VPlus (toVsa e1) (toVsa e2)]

    OperandHole ->
      [VOperandHole]

    OperatorHole e1 e2 ->
      [VOperatorHole (toVsa e1) (toVsa e2)]

vsaTreeDebugString : VSATree -> String
vsaTreeDebugString vsaTree =
  case vsaTree of
    VVar xs ->
      String.join ", " xs

    VNum ns ->
      String.join ", " (List.map String.fromInt ns)

    VPlus vsa1 vsa2 ->
      "(+ "
        ++ vsaDebugString vsa1
        ++ " "
        ++ vsaDebugString vsa2
        ++ ")"

    VOperandHole ->
      "?"

    VOperatorHole vsa1 vsa2 ->
      "(?op "
        ++ vsaDebugString vsa1
        ++ " "
        ++ vsaDebugString vsa2
        ++ ")"

vsaDebugString : VSA -> String
vsaDebugString vsa =
  "{" ++ String.join ", " (List.map vsaTreeDebugString vsa) ++ "}"

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

invert : Side -> Side
invert s =
  case s of
    Left ->
      Right

    Right ->
      Left

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

glue : Token -> Token -> List Token
glue tok1 tok2 =
  case (Tuple.second (shape tok1), Tuple.first (shape tok2)) of
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
          shape first

        (secondLeft, _) =
          shape second
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
          case Tuple.first (shape head) of
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
              |> shape
              |> Tuple.second
          of
            Left ->
              [OPERAND_HOLE]

            Right ->
              []
      in
      prefix ++ toks ++ suffix

shapeRepair : List Token -> List Token
shapeRepair =
  innerShapeRepair >> outerShapeRepair

fits : Token -> Token -> Bool
fits tok1 tok2 =
  Tuple.second (shape tok1) == Tuple.first (shape tok2)

deduplicate : List a -> List a
deduplicate xs =
  case xs of
    [] ->
      []

    head :: tail ->
      head :: deduplicate (List.filter ((/=) head) tail)

wellShapedInsertions : Token -> List Token -> List (List Token)
wellShapedInsertions tok toks =
  case toks of
    [] ->
      [[tok]]

    [head] ->
      if fits head tok then
        [[head, tok]]
      else
        [[head]]

    first :: second :: rest ->
      let
        extra =
          if fits first tok && fits tok second && second /= tok then
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

balanceRepair : List Token -> List (List Token)
balanceRepair =
  forwardRepair LPAREN RPAREN 0
    {- >> List.concatMap
         ( reverseTokens
             >> forwardRepair LPAREN RPAREN 0
             >> List.map reverseTokens
             ) -}
    >> deduplicate

reverseToken : Token -> Token
reverseToken tok =
  case tok of
    LPAREN ->
      RPAREN

    RPAREN ->
      LPAREN

    _ ->
      tok

reverseTokens : List Token -> List Token
reverseTokens toks =
  case toks of
    [] ->
      []

    head :: tail ->
      reverseTokens tail ++ [reverseToken head]

-- Model

type alias Model =
  { input : String
  }

init : Model
init =
  { input =
      "(1) + ("
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
      shape tok
  in
  H.span
    [ HA.class "tile"
    , HA.class (tokenClass tok)]
    [ viewSide left
    , H.span [ HA.class "contents" ] [ H.text (contents tok) ]
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
      tokenize model.input

    shapeRepairedTokens =
      shapeRepair tokens

    possibleBalanceRepairedTokens =
      balanceRepair shapeRepairedTokens

    maybePossibleTrees =
      possibleBalanceRepairedTokens
        |> List.map parse
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
                      case parse toks of
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
              H.text <| vsaDebugString <|
                List.foldl
                  (\e -> merge (toVsa e))
                  (toVsa head)
                  tail

            Nothing ->
              H.text "A possible list of tokens failed to parse!"
        ]
    ]

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

-- Main

main : Program () Model Msg
main =
  Browser.sandbox
    { init = init
    , update = update
    , view = view
    }
