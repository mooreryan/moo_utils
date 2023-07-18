open! Base

(** For easy including in other files. *)

module Or_error : sig
  include module type of struct
    include Or_error
  end

  include module type of struct
    include Or_error_ext
  end
end
