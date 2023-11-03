type t = bool

let fromInt = i =>
  switch i {
  | 0 => Some(false)
  | 1 => Some(true)
  | _ => None
  }

let fromString = string =>
  switch string {
  | "false" => Some(false)
  | "true" => Some(true)
  | _ => None
  }

let xor = (a, b) => (a && !b) || (!a && b)

let not = not

let and_ = (a, b) => a && b

@send external toString: bool => string = "toString"

let toInt = t => t ? 1 : 0

let compare = compare

let equal = \"="
