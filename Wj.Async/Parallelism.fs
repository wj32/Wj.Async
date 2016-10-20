namespace Wj.Async

open System

module Parallelism =
  type T =
    | Sequential
    | Parallel
    | Throttle of throttle : IThrottle
