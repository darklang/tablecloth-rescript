type t = int

include TableclothComparator.Make({
  type t = t

  let compare = compare
})

let minimumValue = Js.Int.min

let maximumValue = Js.Int.max

let zero = 0

let one = 1

let fromString = s =>
  switch int_of_string(s) {
  | i => Some(i)
  | exception Failure(_) => None
  }

let add = \"+"

let subtract = \"-"

let multiply = \"*"

let divide = (n, ~by) => n / by

let divideFloat = (n, ~by) => Js.Int.toFloat(n) /. Js.Int.toFloat(by)

let power = (~base, ~exponent) => {
  let result = Js.Math.pow_float(~base=Js.Int.toFloat(base), ~exp=Js.Int.toFloat(exponent))

  let result = if result > TableclothFloat.maximumSafeInteger {
    TableclothFloat.maximumSafeInteger
  } else if result < TableclothFloat.minimumSafeInteger {
    TableclothFloat.minimumSafeInteger
  } else {
    result
  }

  Js.Math.unsafe_trunc(result)
}

let negate = \"~-"

let remainder = (n, ~by) => mod(n, by)

let modulo = (n, ~by) =>
  mod(
    if n <= 0 {
      abs(n) * 2
    } else {
      n
    },
    by,
  )

let maximum = Js.Math.max_int

let minimum = Js.Math.min_int

let absolute = abs

let isEven = n => mod(n, 2) == 0

let isOdd = n => mod(n, 2) != 0

let clamp = (n, ~lower, ~upper) =>
  if upper < lower {
    raise(Invalid_argument("~lower must be less than or equal to ~upper"))
  } else {
    max(lower, min(upper, n))
  }

let inRange = (n, ~lower, ~upper) =>
  if upper < lower {
    raise(Invalid_argument("~lower must be less than or equal to ~upper"))
  } else {
    n >= lower && n < upper
  }

let toFloat = Js.Int.toFloat

let toString = Js.Int.toString

let equal = \"="

let compare = compare

