namespace Wj.Async

open System

module Result =
  type T<'a, 'error> =
    | Success of 'a
    | Failure of 'error

  // General

  val mapError : T<'a, 'error> -> ('error -> 'error2) -> T<'a, 'error2>
  val ofSuccess : 'a -> T<'a, 'error>
  val ofFailure : 'error -> T<'a, 'error>
  val ofChoice : Choice<'a, 'error> -> T<'a, 'error>

  // Try with

  val tryWith : (unit -> 'a) -> T<'a, exn>

  // Monad

  val ``return`` : 'a -> T<'a, 'error> // same as of Success
  val bind : T<'a, 'error> -> ('a -> T<'b, 'error>) -> T<'b, 'error>
  val map : T<'a, 'error> -> ('a -> 'b) -> T<'b, 'error>
  val join : T<T<'a, 'error>, 'error> -> T<'a, 'error>
  val forget : T<'a, 'error> -> T<unit, 'error>
  val all : T<'a, 'error> list -> T<'a list, 'error>
  val allForget : T<unit, 'error> list -> T<unit, 'error>

  // Infix operators

  module Infix =
    val (>>=) : T<'a, 'error> -> ('a -> T<'b, 'error>) -> T<'b, 'error>
    val (>>|) : T<'a, 'error> -> ('a -> 'b) -> T<'b, 'error>
