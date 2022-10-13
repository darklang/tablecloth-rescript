type t<'a> = option<'a>

let some = a => Some(a)

let isSome = Belt.Option.isSome

let isNone = Belt.Option.isNone

let or_ = (ta, tb) => isSome(ta) ? ta : tb

let orElse = (ta, tb) => isSome(tb) ? tb : ta

let and_ = (ta, tb) => isSome(ta) ? tb : ta

let andThen = (t, ~f) =>
  switch t {
  | None => None
  | Some(x) => f(x)
  }

let flatten = x =>
  switch x {
  | Some(option) => option
  | None => None
  }

let both = (a, b) =>
  switch (a, b) {
  | (Some(a), Some(b)) => Some(a, b)
  | _ => None
  }

let map = (t, ~f) => Belt.Option.map(t, f)

let map2 = (a, b, ~f) =>
  switch (a, b) {
  | (Some(a), Some(b)) => Some(f(a, b))
  | _ => None
  }

let unwrap = (t, ~default) => Belt.Option.getWithDefault(t, default)

let unwrapOrFailWith = (t, ~exn) =>
  switch t {
  | Some(value) => value
  | None => raise(exn)
  }

let unwrapUnsafe = unwrapOrFailWith(~exn=Invalid_argument("Option.unwrapUnsafe called with None"))

let toArray = t =>
  switch t {
  | None => []
  | Some(value) => [value]
  }

let toList = t =>
  switch t {
  | None => list{}
  | Some(value) => list{value}
  }

let tap = (t, ~f) =>
  switch t {
  | None => ()
  | Some(x) => f(x)
  }

let equal = (a, b, equal) =>
  switch (a, b) {
  | (None, None) => true
  | (Some(a'), Some(b')) => equal(a', b')
  | _ => false
  }

let compare = (a, b, ~f as compare) =>
  switch (a, b) {
  | (None, None) => 0
  | (Some(a'), Some(b')) => compare(a', b')
  | (None, Some(_)) => -1
  | (Some(_), None) => 1
  }

