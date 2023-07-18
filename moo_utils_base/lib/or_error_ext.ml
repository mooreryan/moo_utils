open! Base

let of_option : 'a option -> msg:string -> 'a Or_error.t =
 fun opt ~msg ->
  match opt with
  | None ->
      Or_error.error_string msg
  | Some v ->
      Or_error.return v

module Option = struct
  module T = struct
    [@@@coverage off]
    type 'a t = 'a Option.t Or_error.t [@@deriving compare, equal, sexp]
    [@@@coverage on]

    let return : 'a -> 'a t = fun a -> Ok (Some a)

    let map : 'a t -> f:('a -> 'b) -> 'b t =
     fun t ~f -> Or_error.map t ~f:(Option.map ~f)

    let map = `Custom map
  end

  include T

  include Monad.Make (struct
    include T

    let bind : 'a t -> f:('a -> 'b t) -> 'b t =
     fun t ~f ->
      let open Or_error.Let_syntax in
      match%bind t with None -> Ok None | Some a -> f a
  end)

  include Applicative.Make (struct
    include T

    let apply : ('a -> 'b) t -> 'a t -> 'b t =
     fun mf mv ->
      match (mf, mv) with
      | Ok (Some f), Ok (Some v) ->
          Ok (Some (f v))
      | Ok None, Ok (Some _) | Ok (Some _), Ok None | Ok None, Ok None ->
          Ok None
      | Error e1, Error e2 ->
          Error (Error.of_list [e1; e2])
      | (Error _ as e), Ok (Some _)
      | (Error _ as e), Ok None
      | Ok (Some _), (Error _ as e)
      | Ok None, (Error _ as e) ->
          e
  end)

  module Applicative_infix = struct
    include Applicative_infix

    let ( <$> ) : ('a -> 'b) -> 'a t -> 'b t = fun f x -> map x ~f
  end

  let of_or_error : 'a Or_error.t -> 'a t = Or_error.map ~f:Option.return
  let of_option : 'a option -> 'a t = Or_error.return
end
