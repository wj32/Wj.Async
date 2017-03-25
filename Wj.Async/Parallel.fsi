namespace Wj.Async

open System

module Parallel =
  module Pool =
    [<Sealed>]
    type T

    module Config =
      type T =
        { minThreadCount : int;
          maxThreadCount : int;
          threadTimeout : TimeSpan option; }

      val ``default`` : T

    val create : Config.T -> T
    val ``default`` : T

  val parallelDefault : IConcurrency
  val parallelAtMost : maxThreadCount : int -> IConcurrency
