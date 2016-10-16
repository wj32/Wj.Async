namespace Wj.Async

module RegistrationList =
  type 'a T

  val create : unit -> 'a T

  val isEmpty : 'a T -> bool
  val addOnEmptyCallback : 'a T -> f : (unit -> unit) -> unit

  val add : 'a T -> value : 'a -> unit
  val register : 'a T -> value : 'a -> IRegistration
  val moveFrom : 'a T -> from : 'a T -> unit
  val clear : 'a T -> unit

  val ofList : 'a list -> 'a T
  val toList : 'a T -> 'a list
