module Utils exposing
  ( ..
  )

deduplicate : List a -> List a
deduplicate xs =
  case xs of
    [] ->
      []

    head :: tail ->
      head :: deduplicate (List.filter ((/=) head) tail)

shell : List a -> Maybe (a, a)
shell xs =
  case xs of
    [] ->
      Nothing

    head :: tail ->
      Just
        ( head
        , tail
            |> List.reverse
            |> List.head
            |> Maybe.withDefault head
        )

suffixAfter : (a -> Bool) -> List a -> Maybe (List a)
suffixAfter p xs =
  case xs of
    [] ->
      Nothing

    head :: tail ->
      if p head then
        Just tail
      else
        suffixAfter p tail

subsequence : List a -> List a -> Bool
subsequence smaller larger =
  case smaller of
    [] ->
      True

    head :: tail ->
      case suffixAfter ((==) head) larger of
        Nothing ->
          False

        Just suffix ->
          subsequence tail suffix

listIterate : Int -> (a -> List a) -> a -> List a
listIterate n f x =
  if n <= 0 then
    [x]
  else
    List.concatMap (listIterate (n - 1) f) (f x)

rightInsertions : (a -> Maybe a -> Bool) -> a -> List a -> List (List a)
rightInsertions shouldInsert item xs =
  case xs of
    [] ->
      []

    [head] ->
      if shouldInsert head Nothing then
        [[head, item]]
      else
        []

    left :: right :: rest ->
      let
        extra =
          if shouldInsert left (Just right) then
            [left :: item :: right :: rest]
          else
            []
      in
        extra
          ++ List.map
               ((::) left)
               (rightInsertions shouldInsert item (right :: rest))
