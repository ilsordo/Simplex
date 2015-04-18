module type FIELD = sig
  type t

  val of_string : string -> t option
  val of_int : int -> t
  val to_string : t -> string
  val print : out_channel -> t -> unit

  val zero : t
  val one : t

  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( / ) : t -> t -> t (* Can raise Division_by_zero *)

  val neg : t -> t
  val inv : t -> t (* Can raise Division_by_zero *)

  val compare : t -> t -> int
end
