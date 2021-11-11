type tile_edge =
  | In
  | Out

type tile =
  tile_edge * tile_edge

type token =
  | TVar of string
  | TPlus
  | TLParen
  | TRParen
  | THole

type token_tile =
  token * tile

type exp =
  | EVar of string
  | EPlus of exp * exp
  | EHole

let tile_checks : tile list -> bool =
  let local_check : tile_edge -> tile_edge -> bool =
    (<>)
  in
  let rec inner_checks : tile list -> bool =
    fun ts ->
      begin match ts with
        | first :: second :: rest ->
            local_check (snd first) (fst second)
              && inner_checks (second :: rest)

        | _ ->
            true
      end
  in
  let outer_checks : tile list -> bool =
    fun ts ->
      begin match ts with
        | first :: tail ->
            let last =
              List.nth_opt ts (List.length ts - 1)
                |> Option.value ~default:first
            in
            fst first = Out && snd last = Out

        | [] ->
            true
      end
  in
  fun ts ->
    inner_checks ts && outer_checks ts

(* Should always succeed *)
let rec fix_shapes : token list -> token_tile list =
  fun toks ->
    failwith "TODO"

let ok =
  [ (Out, In); (Out, Out); (In, In); (Out, Out); (In, Out) ]

let not_ok =
  [ (Out, In); (Out, Out); (Out, In); (Out, Out); (In, Out) ]

let not_ok2 =
  [ (In, In); (Out, Out); (In, In); (Out, Out); (In, Out) ]

;;

print_endline (string_of_bool (tile_checks ok));
print_endline (string_of_bool (tile_checks not_ok));
print_endline (string_of_bool (tile_checks not_ok2))
