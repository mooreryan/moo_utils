open! Base

open Moo_utils_iter.Import

(* module Iter = Moo_utils_iter.Import.Import.IterLabels *)

let x = Iter.of_list [1]

let%test_unit _ =
  let s = Set.of_list (module String) ["b"; "b"; "a"] in
  let s' = Iter.(s |> of_set |> to_set ~m:(module String)) in
  assert (Set.equal s s')

let () = ()
