open! Base
open Stdio
module Q = Base_quickcheck

open Moo_utils_iter.Import

let string_no_whitespace =
  let whitespace = Re.compile @@ Re.space in
  let is_whitespace s = Re.execp whitespace s in
  let is_not_whitespace = Fn.non is_whitespace in
  Q.Generator.string |> Q.Generator.filter ~f:is_not_whitespace

module String_list_qc = struct
  type t = string list [@@deriving sexp_of]
  let quickcheck_generator = Q.Generator.list Q.Generator.string
  let quickcheck_shrinker = Q.Shrinker.list Q.Shrinker.string
end

let%test_unit "maps round trip" =
  let _ =
    Q.Test.run_exn
      ~f:(fun kv_pairs ->
        let expect =
          (* Our keys aren't necessarily unique, but it's easier to just get rid
             of the duplicates here than it is to generate unique keys in
             quickcheck. *)
          let use_new_val _ v = v in
          Map.of_alist_fold ~init:"" ~f:use_new_val (module String) kv_pairs
        in
        let result = Iter.(expect |> of_map |> to_map_exn ~m:(module String)) in
        [%test_result: string Map.M(String).t] result ~expect )
      ( module struct
        type t = (string * string) list [@@deriving sexp_of]

        let quickcheck_generator =
          let open Q.Generator.Let_syntax in
          let kv_pair =
            let%bind k = Q.Generator.string in
            let%bind v = Q.Generator.string in
            return (k, v)
          in
          Q.Generator.list kv_pair

        let quickcheck_shrinker =
          let kv_pair = Q.Shrinker.both Q.Shrinker.string Q.Shrinker.string in
          Q.Shrinker.list kv_pair
      end )
  in
  ()

let%test_unit "sets round trip" =
  Q.Test.run_exn
    ~f:(fun strings ->
      let expect = Set.of_list (module String) strings in
      let result = Iter.(expect |> of_set |> to_set ~m:(module String)) in
      [%test_result: Set.M(String).t] result ~expect )
    (module String_list_qc)

let%expect_test "okay usage of tap" =
  (* Before realizing the iter, nothing is printed. *)
  let it = Iter.(["apple"; "pie"] |> of_list |> tap ~f:print_endline) in
  [%expect {| |}] ;
  (* Now realize the seq, and you will see stuff printed. *)
  let l = Iter.to_list it in
  [%expect {|
    apple
    pie |}] ;
  (* Finally, show iter as a list. *)
  l |> [%sexp_of: string list] |> print_s ;
  [%expect {| (apple pie) |}]

let%expect_test "be careful with side-effects" =
  let l = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10] in
  let l_take_before = ref [] in
  let l_take_after = ref [] in
  let (_ : int list) =
    Iter.(
      l |> of_list |> take 5
      |> tap ~f:(fun s -> l_take_before := s :: !l_take_before)
      |> to_list )
  in
  let (_ : int list) =
    Iter.(
      l |> of_list
      |> tap ~f:(fun s -> l_take_after := s :: !l_take_after)
      |> take 5 |> to_list )
  in
  !l_take_before |> List.rev |> [%sexp_of: int list] |> print_s ;
  [%expect {| (1 2 3 4 5) |}] ;
  !l_take_after |> List.rev |> [%sexp_of: int list] |> print_s ;
  [%expect {| (1 2 3 4 5 6) |}]

let%test_unit "using tap to build original iter contents" =
  Q.Test.run_exn
    ~f:(fun strings ->
      let result = ref [] in
      let (_ : string list) =
        Iter.(
          strings |> of_list
          |> tap ~f:(fun s -> result := s :: !result)
          |> to_list )
      in
      [%test_result: string list] (List.rev !result) ~expect:strings )
    (module String_list_qc)

let%expect_test "okay usage of tapi" =
  (* Before realizing the iter, nothing is printed. *)
  let it =
    Iter.(
      ["apple"; "pie"] |> of_list
      |> tapi ~f:(fun i s -> print_endline [%string "%{i#Int}: %{s}"]) )
  in
  [%expect {| |}] ;
  (* Now realize the seq, and you will see stuff printed. *)
  let l = Iter.to_list it in
  [%expect {|
    0: apple
    1: pie |}] ;
  (* Finally, show iter as a list. *)
  l |> [%sexp_of: string list] |> print_s ;
  [%expect {| (apple pie) |}]

let%test_unit "Iter.filter_opt matches List.filter_opt" =
  Q.Test.run_exn
    ~f:(fun strings ->
      let expect = List.filter_opt strings in
      let result = Iter.(strings |> of_list |> filter_opt |> to_list) in
      [%test_result: string list] result ~expect )
    ( module struct
      type t = string option list [@@deriving sexp_of]

      let quickcheck_generator =
        Q.Generator.list @@ Q.Generator.option Q.Generator.string

      let quickcheck_shrinker =
        Q.Shrinker.list @@ Q.Shrinker.option Q.Shrinker.string
    end )

let%test_unit "Iter.split matches String.split" =
  Q.Test.run_exn
    ~f:(fun l ->
      let expect = List.map l ~f:(String.split ~on:'\t') in
      let result = Iter.(l |> of_list |> split ~on:'\t' |> to_list) in
      [%test_result: string list list] result ~expect )
    ( module struct
      type t = string list [@@deriving sexp_of]

      let quickcheck_generator =
        let open Q.Generator.Let_syntax in
        let s =
          let%bind s1 = Q.Generator.string in
          let%bind s2 = Q.Generator.string in
          return [%string "%{s1}\t%{s2}"]
        in
        Q.Generator.list s

      let quickcheck_shrinker = Q.Shrinker.atomic
    end )

let%test_unit "Iter.split' matches String.split" =
  Q.Test.run_exn
    ~f:(fun l ->
      let expect =
        List.map l ~f:(fun s -> String.split ~on:'\t' s |> Array.of_list)
      in
      let result = Iter.(l |> of_list |> split' ~on:'\t' |> to_list) in
      [%test_result: string array list] result ~expect )
    ( module struct
      type t = string list [@@deriving sexp_of]

      let quickcheck_generator =
        let open Q.Generator.Let_syntax in
        let s =
          let%bind s1 = Q.Generator.string in
          let%bind s2 = Q.Generator.string in
          return [%string "%{s1}\t%{s2}"]
        in
        Q.Generator.list s

      let quickcheck_shrinker = Q.Shrinker.atomic
    end )

let%test_unit "Iter.split_strip is good" =
  Q.Test.run_exn
    ~f:(fun l ->
      let with_spaces =
        List.map l ~f:(fun (a, b) -> [%string "  %{a}  \t  %{b} \t  "])
      in
      let expect = List.map l ~f:(fun (a, b) -> [a; b; ""]) in
      let result =
        Iter.(with_spaces |> of_list |> split_strip ~on:'\t' |> to_list)
      in
      [%test_result: string list list] result ~expect )
    ( module struct
      type t = (string * string) list [@@deriving sexp_of]

      let quickcheck_generator =
        let open Q.Generator.Let_syntax in
        let s =
          let%bind s1 = string_no_whitespace in
          let%bind s2 = string_no_whitespace in
          return (s1, s2)
        in
        Q.Generator.list s

      let quickcheck_shrinker = Q.Shrinker.atomic
    end )

let%test_unit "Iter.split_strip' is good" =
  Q.Test.run_exn
    ~f:(fun l ->
      let with_spaces =
        List.map l ~f:(fun (a, b) -> [%string "  %{a}  \t  %{b} \t  "])
      in
      let expect = List.map l ~f:(fun (a, b) -> [|a; b; ""|]) in
      let result =
        Iter.(with_spaces |> of_list |> split_strip' ~on:'\t' |> to_list)
      in
      [%test_result: string array list] result ~expect )
    ( module struct
      type t = (string * string) list [@@deriving sexp_of]

      let quickcheck_generator =
        let open Q.Generator.Let_syntax in
        let s =
          let%bind s1 = string_no_whitespace in
          let%bind s2 = string_no_whitespace in
          return (s1, s2)
        in
        Q.Generator.list s

      let quickcheck_shrinker = Q.Shrinker.atomic
    end )
