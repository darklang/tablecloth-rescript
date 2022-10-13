type t = float

let fromInt = Js.Int.toFloat

let fromString = string =>
  switch string |> String.lowercase_ascii {
  | "nan" => Some(Js.Float._NaN)
  | _ =>
    switch Js.Float.fromString(string) {
    | result if result |> Js.Float.isNaN => None
    | result => Some(result)
    }
  }

let add = \"+."

let subtract = \"-."

let multiply = \"*."

let divide = (n, ~by) => n /. by

let power = (~base, ~exponent) => Js.Math.pow_float(~base, ~exp=exponent)

let negate = \"~-."

let absolute = Js.Math.abs_float

let clamp = (n, ~lower, ~upper) =>
  if upper < lower {
    raise(
      Invalid_argument(
        "~lower:" ++
        (Js.Float.toString(lower) ++
        (" must be less than or equal to ~upper:" ++ Js.Float.toString(upper))),
      ),
    )
  } else if Js.Float.isNaN(lower) || (Js.Float.isNaN(upper) || Js.Float.isNaN(n)) {
    nan
  } else {
    max(lower, min(upper, n))
  }

let inRange = (n, ~lower, ~upper) =>
  if upper < lower {
    raise(
      Invalid_argument(
        "~lower:" ++
        (Js.Float.toString(lower) ++
        (" must be less than or equal to ~upper:" ++ Js.Float.toString(upper))),
      ),
    )
  } else {
    n >= lower && n < upper
  }

let squareRoot = sqrt

let log = (n, ~base) => Js.Math.log(n) /. Js.Math.log(base)

let zero = 0.0

let one = 1.0

let nan = Js.Float._NaN

let infinity = infinity

let negativeInfinity = neg_infinity

let e = Js.Math._E

let pi = Js.Math._PI

let epsilon = epsilon_float

@scope("Number") @val external largestValue: t = "MAX_VALUE"

@scope("Number") @val external smallestValue: t = "MIN_VALUE"

@scope("Number") @val external maximumSafeInteger: t = "MAX_SAFE_INTEGER"

@scope("Number") @val external minimumSafeInteger: t = "MIN_SAFE_INTEGER"

let isNaN = Js.Float.isNaN

let isFinite = Js.Float.isFinite

let isInfinite = n => !Js.Float.isFinite(n) && !isNaN(n)

@scope("Number") @val external isInteger: t => bool = "isInteger"

@scope("Number") @val external isSafeInteger: t => bool = "isSafeInteger"

let maximum = (x, y) =>
  if isNaN(x) || isNaN(y) {
    nan
  } else if y > x {
    y
  } else {
    x
  }

let minimum = (x, y) =>
  if isNaN(x) || isNaN(y) {
    nan
  } else if y < x {
    y
  } else {
    x
  }

let hypotenuse = Js.Math.hypot

type radians = float

let degrees = n => n *. (pi /. 180.0)

external radians: float => float = "%identity"

let turns = n => n *. 2. *. pi

let cos = Js.Math.cos

let acos = Js.Math.acos

let sin = Js.Math.sin

let asin = Js.Math.asin

let tan = Js.Math.tan

let atan = Js.Math.atan

let atan2 = (~y, ~x) => Js.Math.atan2(~y, ~x, ())

type direction = [
  | #Zero
  | #AwayFromZero
  | #Up
  | #Down
  | #Closest([#Zero | #AwayFromZero | #Up | #Down | #ToEven])
]

let round = (~direction=#Closest(#Up), n) =>
  switch direction {
  | #Up => Js.Math.ceil_float(n)
  | #Down => Js.Math.floor_float(n)
  | #Zero => Js.Math.trunc(n)
  | #AwayFromZero =>
    if n > 0. {
      Js.Math.ceil_float(n)
    } else {
      Js.Math.floor_float(n)
    }
  | #Closest(#Zero) =>
    if n > 0. {
      Js.Math.ceil_float(n -. 0.5)
    } else {
      Js.Math.floor_float(n +. 0.5)
    }
  | #Closest(#AwayFromZero) =>
    if n > 0. {
      Js.Math.floor_float(n +. 0.5)
    } else {
      Js.Math.ceil_float(n -. 0.5)
    }
  | #Closest(#Down) => Js.Math.ceil_float(n -. 0.5)
  | #Closest(#Up) => Js.Math.round(n)
  | #Closest(#ToEven) =>
    let roundNearestLowerBound = -.(2. ** 52.)
    let roundNearestUpperBound = 2. ** 52.
    if n <= roundNearestLowerBound || n >= roundNearestUpperBound {
      n +. 0.
    } else {
      let floor = floor(n)
      let ceil_or_succ = floor +. 1.
      let diff_floor = n -. floor
      let diff_ceil = ceil_or_succ -. n
      if diff_floor < diff_ceil {
        floor
      } else if diff_floor > diff_ceil {
        ceil_or_succ
      } else if mod_float(floor, 2.) == 0. {
        floor
      } else {
        ceil_or_succ
      }
    }
  }

let floor = Js.Math.floor_float

let ceiling = Js.Math.ceil_float

let truncate = Js.Math.trunc

let fromPolar = ((r, theta)) => (r *. cos(theta), r *. sin(theta))

let toPolar = ((x, y)) => (hypotenuse(x, y), atan2(~x, ~y))

let toInt = f =>
  if Js.Float.isFinite(f) {
    Some(Js.Math.unsafe_trunc(f))
  } else {
    None
  }

let toString = Js.Float.toString

let equal = \"="

let compare = compare

