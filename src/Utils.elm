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

insertions : Int -> a -> List a -> List (List a)
insertions k item xs =
  if k <= 0 then
    [xs]
  else
    let
      insertNow =
        List.map ((::) item) (insertions (k - 1) item xs)

      insertLater =
        case xs of
          [] ->
            []

          head :: tail ->
            List.map ((::) head) (insertions k item tail)
    in
    insertNow ++ insertLater

productRange : Int -> Int -> Int
productRange low high =
  let
    helper : Int -> Int -> Int
    helper acc current =
      if current > high then
        acc
      else
        helper (current * acc) (current + 1)
  in
  helper 1 low

choose : Int -> Int -> Int
choose n k =
  productRange (k + 1) n // productRange 1 (n - k)

unmatchedRights : a -> a -> List a -> Int
unmatchedRights left right =
  let
    helper : Int -> Int -> List a -> Int
    helper depth count xs =
      case xs of
        [] ->
          count

        head :: tail ->
          if head == left then
            helper (depth + 1) count tail
          else if head == right then
            if depth > 0 then
              helper (depth - 1) count tail
            else
              helper depth (count + 1) tail
          else
            helper depth count tail
  in
  helper 0 0

unmatchedLefts : a -> a -> List a -> Int
unmatchedLefts left right =
  List.reverse >> unmatchedRights right left

span : (a -> Bool) -> List a -> (List a, List a)
span pred xs =
  case xs of
    [] ->
      ([], [])

    head :: tail ->
      if pred head then
        let
          (prefix, suffix) =
            span pred tail
        in
        (head :: prefix, suffix)
      else
        ([], xs)

groupBy : (a -> a -> Bool) -> List a -> List (List a)
groupBy pred xs =
  case xs of
    [] ->
      []

    head :: tail ->
      let
        (prefix, suffix) =
          span (pred head) tail
      in
      (head :: prefix) :: groupBy pred suffix

isJust : Maybe a -> Bool
isJust mx =
  case mx of
    Nothing ->
      False

    Just _ ->
      True
