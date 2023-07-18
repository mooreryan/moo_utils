module IterLabels : sig

  (** Below here is included from Iter_labels_ext. *)
  
  include module type of struct
    include IterLabels
  end

  include module type of struct
    include Iter_labels_ext
  end

  (** [Map] and [Set] contents are "hidden". They will still show up, so don't
      do global opens as it will mess with [Base] stuff. *)

  module Map : sig end
  module Set : sig end
end

module Iter = IterLabels
