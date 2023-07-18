val tap : 'a IterLabels.t -> f:('a -> unit) -> 'a IterLabels.t
(** tap/tee like functions that run a side-effecting function on each value of
    the iterator, and return that value unchanged. Unlike ruby or scala tap,
    doesn't pass the FULL iterator at that time, rather just sticks a
    side-effecting function in there. *)

val tapi : 'a IterLabels.t -> f:(int -> 'a -> unit) -> 'a IterLabels.t

val to_map_exn :
     ('key * 'value) IterLabels.t
  -> m:('key, 'cmp) Base.Comparator.Module.t
  -> ('key, 'value, 'cmp) Base.Map.t
(** Uses [Map.add_exn], so it will throw if there are any duplicated keys. *)

val of_map : ('key, 'value, 'cmp) Base.Map.t -> ('key * 'value) IterLabels.t

val to_set :
     'a IterLabels.t
  -> m:('a, 'cmp) Base.Comparator.Module.t
  -> ('a, 'cmp) Base.Set.t

val of_set : ('a, 'cmp) Base.Set.t -> 'a IterLabels.t

val filter_opt : 'a option IterLabels.t -> 'a IterLabels.t

val split : string IterLabels.t -> on:char -> string list IterLabels.t

val split' : string IterLabels.t -> on:char -> string array IterLabels.t

val split_strip : string IterLabels.t -> on:char -> string list IterLabels.t

val split_strip' : string IterLabels.t -> on:char -> string array IterLabels.t
