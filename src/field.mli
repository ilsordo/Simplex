module type FIELD = sig
  type t

  val of_string : string -> t option
  val of_int : int -> t
  val print : out_channel -> t -> unit

  val zero : t
  val one : t

  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( / ) : t -> t -> t

  val compare : t -> t -> int
end
