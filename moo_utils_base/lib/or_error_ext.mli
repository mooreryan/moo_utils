open! Base

val of_option : 'a option -> msg:string -> 'a Or_error.t

module Option : sig
  type 'a t = 'a Option.t Or_error.t [@@deriving compare, equal, sexp]

  include Monad.S with type 'a t := 'a t
  include Applicative.S with type 'a t := 'a t

  module Applicative_infix : sig
    include Base.Applicative.Applicative_infix with type 'a t := 'a t

    val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t
    (** A different Infix map *)
  end

  val of_or_error : 'a Or_error.t -> 'a t
  val of_option : 'a option -> 'a t
end
