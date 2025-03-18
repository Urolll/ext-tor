module type RING = sig
  type t
  val add_id: t
  val mult_id: t
  val add: t -> t -> t
  val mult: t -> t -> t
  val neg: t -> t
  val to_string: t -> string
  val eq: t -> t -> bool
  val is_finite: bool
  val elements: t list
  val of_int: int -> t
end

module type MODULE = sig
  module R: RING
  type m
  val zero: m
  val add: m -> m -> m
  val neg: m -> m
  val smul: R.t -> m -> m
  val eq: m -> m -> bool
  val to_string: m -> string
  val make: R.t -> R.t -> m
end

