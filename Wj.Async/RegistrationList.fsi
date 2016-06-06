namespace Wj.Async

module RegistrationList =
  type 'a T

  val create : unit -> 'a T
  val add : 'a T -> value : 'a -> unit
  val register : 'a T -> value : 'a -> IRegistration
  val moveFrom : 'a T -> from : 'a T -> unit
  val clear : 'a T -> unit

  val toList : 'a T -> 'a list
