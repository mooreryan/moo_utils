open! Base
open Moo_utils_base.Import
module Quickcheck = Base_quickcheck
let print_s = Stdio.print_s

(* TODO: make this available outside the test package. *)
let or_error_option_quickcheck_generator :
       'a Base_quickcheck.Generator.t
    -> 'a Or_error.Option.t Base_quickcheck.Generator.t =
 fun a_gen ->
  let open Quickcheck.Generator.Let_syntax in
  let ok_gen = Quickcheck.quickcheck_generator_option a_gen in
  let err_gen =
    let%map s = Quickcheck.quickcheck_generator_string in
    Error.of_string s
  in
  let%map either = Quickcheck.Generator.either ok_gen err_gen in
  Result.of_either either

let%expect_test "of_option" =
  let print x = x |> [%sexp_of: int Or_error.t] |> print_s in
  None |> Or_error.of_option ~msg:"Got none" |> print ;
  [%expect {| (Error "Got none") |}] ;
  Some 123 |> Or_error.of_option ~msg:"Got none" |> print ;
  [%expect {| (Ok 123) |}]

(* or error option type tests *)
let%expect_test "return" =
  1 |> Or_error.Option.return |> [%sexp_of: int Or_error.Option.t] |> print_s ;
  [%expect {| (Ok (1)) |}] ;
  Some "A" |> Or_error.Option.return
  |> [%sexp_of: string option Or_error.Option.t] |> print_s ;
  [%expect {| (Ok ((A))) |}] ;
  None |> Or_error.Option.return |> [%sexp_of: bool option Or_error.Option.t]
  |> print_s ;
  [%expect {| (Ok (())) |}]

let%expect_test "bind" =
  let module M = Or_error.Option in
  let f x = M.return (x + 1) in
  1 |> M.return |> M.bind ~f |> [%sexp_of: int M.t] |> print_s ;
  [%expect {| (Ok (2)) |}] ;
  None |> Or_error.return |> M.bind ~f |> [%sexp_of: int M.t] |> print_s ;
  [%expect {| (Ok ()) |}] ;
  Or_error.error_string "yo" |> M.bind ~f |> [%sexp_of: int M.t] |> print_s ;
  [%expect {| (Error yo) |}]

let%expect_test "apply" =
  let module M = Or_error.Option in
  let print t = t |> [%sexp_of: int M.t] |> print_s in
  let f x = x + 1 in
  let x = 1 in
  M.(apply (Ok (Some f)) (Ok (Some x))) |> print ;
  [%expect {| (Ok (2)) |}] ;
  M.(apply (Ok None) (Ok (Some x))) |> print ;
  [%expect {| (Ok ()) |}] ;
  M.(apply (Ok (Some f)) (Ok None)) |> print ;
  [%expect {| (Ok ()) |}] ;
  M.(apply (Ok None) (Ok None)) |> print ;
  [%expect {| (Ok ()) |}] ;
  M.(apply (Or_error.error_string "e1") (Or_error.error_string "e2")) |> print ;
  [%expect {| (Error (e1 e2)) |}] ;
  M.(apply (Or_error.error_string "e1") (Ok (Some x))) |> print ;
  [%expect {| (Error e1) |}] ;
  M.(apply (Or_error.error_string "e1") (Ok None)) |> print ;
  [%expect {| (Error e1) |}] ;
  M.(apply (Ok (Some f)) (Or_error.error_string "e2")) |> print ;
  [%expect {| (Error e2) |}] ;
  M.(apply (Ok None) (Or_error.error_string "e2")) |> print ;
  [%expect {| (Error e2) |}]

let%expect_test "map" =
  let module M = Or_error.Option in
  let f x = x + 1 in
  Ok (Some 1) |> M.map ~f |> [%sexp_of: int M.t] |> print_s ;
  [%expect {| (Ok (2)) |}] ;
  Ok None |> M.map ~f |> [%sexp_of: int M.t] |> print_s ;
  [%expect {| (Ok ()) |}] ;
  Or_error.error_string "yo" |> M.map ~f |> [%sexp_of: int M.t] |> print_s ;
  [%expect {| (Error yo) |}]

let%expect_test "infix map (<$>)" =
  let module M = Or_error.Option in
  let ( <$> ) = M.Applicative_infix.( <$> ) in
  let f x = x + 1 in
  f <$> Ok (Some 1) |> [%sexp_of: int M.t] |> print_s ;
  [%expect {| (Ok (2)) |}] ;
  f <$> Ok None |> [%sexp_of: int M.t] |> print_s ;
  [%expect {| (Ok ()) |}] ;
  f <$> Or_error.error_string "yo" |> [%sexp_of: int M.t] |> print_s ;
  [%expect {| (Error yo) |}]

let%expect_test "of_or_error" =
  let module M = Or_error.Option in
  1 |> Or_error.return |> M.of_or_error |> [%sexp_of: int M.t] |> print_s ;
  [%expect {| (Ok (1)) |}] ;
  "yo" |> Or_error.error_string |> M.of_or_error |> [%sexp_of: int M.t]
  |> print_s ;
  [%expect {| (Error yo) |}]

let%expect_test "of_option" =
  let module M = Or_error.Option in
  1 |> Option.return |> M.of_option |> [%sexp_of: int M.t] |> print_s ;
  [%expect {| (Ok (1)) |}] ;
  None |> M.of_option |> [%sexp_of: int M.t] |> print_s ;
  [%expect {| (Ok ()) |}]

(* See https://wiki.haskell.org/Typeclassopedia *)

module type APPLICATIVE = sig
  type 'a t
  val return : 'a -> 'a t
  val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
end

module Test_applicative (M : APPLICATIVE) = struct
  let ( <.> ) f g x = f (g x) (* <- function composition (haskell `.`) *)

  (* identity law: pure id <*> v = v *)
  let ident ~x ~eq = eq M.(return Fn.id <*> x) x

  (* homomorphism: pure f <*> pure x = pure (f x) *)
  let homomorphism ~f ~x ~eq = eq M.(return f <*> return x) M.(return (f x))

  (* interchange: u <*> pure y = pure (\f -> f y) <*> u *)
  let interchange ~u ~y ~eq =
    eq M.(u <*> return y) M.(return (fun f -> f y) <*> u)

  (* composition: u <*> (v <*> w) = pure (.) <*> u <*> v <*> w *)
  let composition ~u ~v ~w ~eq =
    eq M.(u <*> (v <*> w)) M.(return ( <.> ) <*> u <*> v <*> w)
end

let%test_module "Or_error.Option (applicative laws)" =
  ( module struct
    module M = Or_error.Option
    module Test = Test_applicative (M)

    let f x = x + 1
    let g x = x - 10

    let eq = M.equal Int.equal

    let%test _ = Test.ident ~x:(Ok (Some 1)) ~eq
    let%test _ = Test.ident ~x:(Ok None) ~eq
    let%test _ = Test.ident ~x:(Or_error.error_string "yo") ~eq

    let%test _ = Test.homomorphism ~f ~x:1 ~eq

    let%test _ = Test.interchange ~u:(Ok (Some f)) ~y:1 ~eq
    let%test _ = Test.interchange ~u:(Ok None) ~y:1 ~eq
    let%test _ = Test.interchange ~u:(Or_error.error_string "yo") ~y:1 ~eq

    (* Technically you could do all combinations. *)
    let%test _ =
      Test.composition ~u:(M.return f) ~v:(M.return g) ~w:(M.return 1) ~eq
    let%test _ =
      Test.composition ~u:(Ok None) ~v:(M.return g) ~w:(M.return 1) ~eq
    let%test _ =
      Test.composition
        ~u:(Or_error.error_string "u error")
        ~v:(M.return g) ~w:(M.return 1) ~eq
  end )

module type MONAD = sig
  type 'a t
  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
end

(* Meant to be used with the [%test_eq] of ppx_assert. *)
module Test_monad (M : MONAD) = struct
  (* return a >>= k = k a *)
  let test_left_ident f x = (M.(return x >>= f), f x)

  (* m >>= return = m *)
  let test_right_ident m = (M.(m >>= return), m)

  (* (m >>= f) >>= g behaves the same as m >>= (f >>= g) *)
  (* m >>= (\x -> k x >>= h) = (m >>= k) >>= h *)
  (* (g >=> h) >=> k =  g >=> (h >=> k) *)
  let test_assoc m ~f ~g = (M.(m >>= fun x -> f x >>= g), M.(m >>= f >>= g))
end

let%test_module "Or_error.Option monad laws" =
  ( module struct
    module M = Or_error.Option
    module Test = Test_monad (M)
    open Quickcheck.Generator.Let_syntax

    let int_t_gen : int Or_error.Option.t Quickcheck.Generator.t =
      or_error_option_quickcheck_generator Quickcheck.Generator.int

    module Int_t_quickcheck_test = struct
      type t = int M.t [@@deriving sexp_of]
      let quickcheck_generator = int_t_gen
      let quickcheck_shrinker = Quickcheck.Shrinker.atomic
    end

    let fn_gen = Quickcheck.Generator.fn Quickcheck.Observer.int int_t_gen

    let%test_unit "left identity" =
      Quickcheck.Test.run_exn
        ( module struct
          type t = (int -> int M.t) * int [@@deriving sexp_of]
          let quickcheck_generator =
            Quickcheck.Generator.both fn_gen Quickcheck.Generator.int
          let quickcheck_shrinker = Quickcheck.Shrinker.atomic
        end )
        ~f:(fun (f, x) ->
          let a, b = Test.test_left_ident f x in
          [%test_eq: int M.t] a b )

    let%test_unit "right identity" =
      Quickcheck.Test.run_exn
        (module Int_t_quickcheck_test)
        ~f:(fun m ->
          let a, b = Test.test_right_ident m in
          [%test_eq: int M.t] a b )

    let%test_unit "right identity" =
      Quickcheck.Test.run_exn
        (module Int_t_quickcheck_test)
        ~f:(fun m ->
          let a, b = Test.test_right_ident m in
          [%test_eq: int M.t] a b )

    let%test_unit "associativity" =
      Quickcheck.Test.run_exn
        ( module struct
          type t = int M.t * (int -> int M.t) * (int -> int M.t)
          [@@deriving sexp_of]

          let quickcheck_generator =
            let%bind m = int_t_gen and f = fn_gen and g = fn_gen in
            return (m, f, g)

          let quickcheck_shrinker = Quickcheck.Shrinker.atomic
        end )
        ~f:(fun (m, f, g) ->
          let a, b = Test.test_assoc m ~f ~g in
          [%test_eq: int M.t] a b )
  end )
