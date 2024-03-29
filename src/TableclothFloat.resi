@@ocaml.text(" ")

@@ocaml.text(" A module for working with {{: https://en.wikipedia.org/wiki/Floating-point_arithmetic } floating-point numbers}.

    Valid syntax for [float]s includes:
    {[
      0.
      42.
      42.0
      3.14
      -0.1234
      123_456.123_456
      6.022e23   // = (6.022 * 10^23)
      6.022e+23  // = (6.022 * 10^23)
      1.602e-19  // = (1.602 * 10^-19)
      1e3        // = (1 * 10 ** 3) = 1000.
   ]}

    {b Historical Note: } The particular details of floats (e.g. [NaN]) are
    specified by {{: https://en.wikipedia.org/wiki/IEEE_754 } IEEE 754 } which is literally hard-coded into almost all
    CPUs in the world.
")

type t = float

@@ocaml.text(" {1 Constants} ")

@ocaml.doc(" The literal [0.0] as a named value. ")
let zero: t

@ocaml.doc(" The literal [1.0] as a named value. ")
let one: t

@ocaml.doc(" [NaN] as a named value. NaN stands for {{: https://en.wikipedia.org/wiki/NaN } not a number}.

    {b Note } comparing values with {!Float.nan} will {b always return } [false] even if the value you are comparing against is also [NaN].

    For detecting [NaN] you should use {!Float.isNaN}

    {2 Examples}

    {[
      let isNotANumber = x => Float.equal(x, nan)

      isNotANumber(nan) == false
    ]}
")
let nan: t

@ocaml.doc(" Positive {{: https://en.wikipedia.org/wiki/IEEE_754-1985#Positive_and_negative_infinity } infinity}.

    {[
      Float.divide(Float.pi, ~by=0.0) == Float.infinity
    ]}
")
let infinity: t

@ocaml.doc(" Negative infinity, see {!Float.infinity}. ")
let negativeInfinity: t

@ocaml.doc(
  " An approximation of {{: https://en.wikipedia.org/wiki/E_(mathematical_constant) } Euler's number}. "
)
let e: t

@ocaml.doc(" An approximation of {{: https://en.wikipedia.org/wiki/Pi } pi}. ")
let pi: t

@ocaml.doc(" The smallest interval between two representable numbers. ")
let epsilon: t

@ocaml.doc(" The largest (furthest from zero) representable positive [float].
    Has a value of approximately [1.79E+308], or 1.7976931348623157 * 10^308.
    Values larger than [largestValue] are represented as Infinity.
")
let largestValue: t

@ocaml.doc(" The smallest representable positive [float].
    The closest to zero without actually being zero.
    Has a value of approximately [5E-324], in browsers and in Node.js is 2^-1074
  ")
let smallestValue: t

@ocaml.doc(" Represents the maximum safe integer in JS. [Number]s in JS are represented as a 64-bit floating point
    {{: https://en.wikipedia.org/wiki/IEEE_754 } IEEE 754 } numbers.
    [maximumSafeInteger] has a value of 2^53 - 1 === [9_007_199_254_740_991.0].
    Values larger cannot be represented exactly and cannot be correctly compared.

    Defined as Float since integers in Rescript are limited to 32-bits, their max value is 2^31 - 1 === [2_147_483_647]
    See also: {!Int.maximumValue}
")
let maximumSafeInteger: t

@ocaml.doc(" Represents the minimum safe integer in JS. [Number]s in JS are represented as a 64-bit floating point
    {{: https://en.wikipedia.org/wiki/IEEE_754 } IEEE 754 } numbers.
    [minimumSafeInteger] has a value of -2^53 - 1 === [-9_007_199_254_740_991.0].
    Values larger cannot be represented exactly and cannot be correctly compared.

    Defined as Float since Rescript integers are limited to 32-bits, their min value is -2^31 - 1 === [-2_147_483_647]
    See also: {!Int.minimumValue}
")
let minimumSafeInteger: t

@@ocaml.text(" {1 Create} ")

@ocaml.doc(" Convert an {!Int} to a [float].

    {2 Examples}

    {[
      Float.fromInt(5) == 5.0
      Float.fromInt(0) == 0.0
      Float.fromInt(-7) == -7.0
    ]}
")
let fromInt: int => t

@ocaml.doc(" Convert a {!String} to a [float].
    The behaviour of this function is platform specific.
    Parses [Infinity] case-sensitive, [NaN] is case-insensitive.

    {2 Examples}

    {[
      Float.fromString(\"4.667\") == Some(4.667)
      Float.fromString(\"-4.667\") == Some(-4.667)
      Float.fromString(\"Hamster\") == None
      Float.fromString(\"NaN\") == Some(Float.nan)
      Float.fromString(\"nan\") == Some(Float.nan)
      Float.fromString(\"Infinity\") == Some(Float.infinity)
    ]}
")
let fromString: string => option<t>

@@ocaml.text(" {1 Basic arithmetic and operators} ")

@ocaml.doc(" Addition for floating point numbers.

    Although [int]s and [float]s support many of the same basic operations such as
    addition and subtraction you {b cannot} [add] an [int] and a [float] directly which
    means you need to use functions like {!Int.toFloat} to convert both values to the same type.

    So if you needed to add a {!Array.length} to a [float] for some reason, you
    could:

    {[
      [1, 2, 3]->Array.length->Int.toFloat->Float.add(3.5) == 6.5
    ]}

    Languages like Java and JavaScript automatically convert [int] values
    to [float] values when you mix and match. This can make it difficult to be sure
    exactly what type of number you are dealing with and cause unexpected behavior.

    Rescript has opted for a design that makes all conversions explicit.

    {2 Examples}

    {[
      Float.add(3.14, 3.14) == 6.28

      3.2
      ->Float.round
      ->Float.toInt
      ->Option.map(~f=int => int + Array.length([1, 2, 3])) == Some(6)
   ]}
")
let add: (t, t) => t

@ocaml.doc(" Subtract numbers.

    {2 Examples}

    {[
      Float.subtract(4.0, 3.0) == 1.0
    ]}
")
let subtract: (t, t) => t

@ocaml.doc(" Multiply numbers.

    {2 Examples}

    {[
      Float.multiply(2.0, 7.0) == 14.0
    ]}
")
let multiply: (t, t) => t

@ocaml.doc(" Floating-point division.

    {2 Examples}

    {[
      Float.divide(3.14, ~by=2.0) == 1.57
    ]}
")
let divide: (t, ~by: t) => t

@ocaml.doc(" Exponentiation, takes the base first, then the exponent.

    {2 Examples}

    {[
      Float.power(~base=7.0, ~exponent=3.0) == 343.0
    ]}
")
let power: (~base: t, ~exponent: t) => t

@ocaml.doc(" Flips the 'sign' of a [float] so that positive floats become negative and negative integers become positive. Zero stays as it is.

    {2 Examples}

    {[
      Float.negate(8.) == -8.
      Float.negate(-7.) == 7.
      Float.negate(0.) == 0.
   ]}
")
let negate: t => t

@ocaml.doc(" Get the {{: https://en.wikipedia.org/wiki/Absolute_value } absolute value} of a number.

    {2 Examples}

    {[
      Float.absolute(8.) == 8.
      Float.absolute(-7) = 7
      Float.absolute(0) == 0
   ]}
")
let absolute: t => t

@ocaml.doc(" Returns the larger of two [float]s, if both arguments are equal, returns the first argument

    If either (or both) of the arguments are [NaN], returns [NaN]

    {2 Examples}

    {[
      Float.maximum(7., 9.) == 9.
      Float.maximum(-4., -1.) == -1.
      Float.maximum(7., Float.nan)->Float.isNaN == true
    ]}
")
let maximum: (t, t) => t

@ocaml.doc(" Returns the smaller of two [float]s, if both arguments are equal, returns the first argument.

    If either (or both) of the arguments are [NaN], returns [NaN].

    {2 Examples}

    {[
      Float.minimum(7.0, 9.0) == 7.0
      Float.minimum(-4.0, -1.0) == -4.0
      Float.minimum(7., Float.nan)->Float.isNaN == true
    ]}
")
let minimum: (t, t) => t

@ocaml.doc(" Clamps [n] within the inclusive [lower] and [upper] bounds.

    {3 Exceptions}

    Throws an [Invalid_argument] exception if [lower > upper].

    {2 Examples}

    {[
      Float.clamp(5.0, ~lower=0., ~upper=8.) == 5.
      Float.clamp(9.0, ~lower=0., ~upper=8.) == 8.
      Float.clamp(5.0, ~lower=-10., ~upper=-5.) == -5.
    ]}
")
let clamp: (t, ~lower: t, ~upper: t) => t

@@ocaml.text(" {1 Fancier math} ")

@ocaml.doc(" Take the square root of a number.

    [squareRoot] returns [NaN] when its argument is negative. See {!Float.nan} for more.

    {2 Examples}

    {[
      Float.squareRoot(4.0) == 2.0
      Float.squareRoot(9.0) == 3.0
    ]}
")
let squareRoot: t => t

@ocaml.doc(" Calculate the logarithm of a number with a given base.

    {2 Examples}

    {[
      Float.log(100., ~base=10.) == 2.
      Float.log(256., ~base=2.) == 8.
    ]}
")
let log: (t, ~base: t) => t

@@ocaml.text(" {1 Query} ")

@ocaml.doc(" Determine whether a [float] is an [undefined] or unrepresentable number.

    {b Note: } this function is more useful than it might seem since [NaN] {b does not } equal [NaN]:

    {[
      (Float.nan == Float.nan) == false
    ]}

    {2 Examples}

    {[
      Float.isNaN(0.0 /. 0.0) == true
      Float.squareRoot(-1.0)->Float.isNaN == true
      Float.isNaN(1.0 /. 0.0) == false  (* Float.infinity {b is} a number *)
      Float.isNaN(1.) == false
    ]}
")
let isNaN: t => bool

@ocaml.doc(" Determine whether a float is finite number. True for any float except [Infinity], [-Infinity] or [NaN]

    Notice that [NaN] is not finite!

    {2 Examples}

    {[
      Float.isFinite(0. /. 0.) == false
      Float.squareRoot(-1.)->Float.isFinite == false
      Float.isFinite(1. /. 0.) == false
      Float.isFinite(1.) == true
      Float.nan->Float.isFinite == false
    ]}
")
let isFinite: t => bool

@ocaml.doc(" Determine whether a float is positive or negative infinity.

    {2 Examples}

    {[
      Float.isInfinite(0. /. 0.) == false
      Float.squareRoot(-1.)->Float.isInfinite == false
      Float.isInfinite(1. /. 0.) == true
      Float.isInfinite(1.) == false
      Float.nan->Float.isInfinite == false
    ]}
")
let isInfinite: t => bool

@ocaml.doc(" Determine whether the passed value is an integer.

    {2 Examples}

    {[
      Float.isInteger(4.0) == true
      Float.pi->Float.isInteger == false
    ]}
")
let isInteger: t => bool

@ocaml.doc(" Determine whether the passed value is a safe integer (number between -(2**53 - 1) and 2**53 - 1).

    {2 Examples}

    {[
      Float.isSafeInteger(4.0) == true
      Float.isSafeInteger(Float.pi) == false
      Float.isSafeInteger(Float.maximumSafeInteger +. 1.) == false
    ]}
")
let isSafeInteger: t => bool

@ocaml.doc(" Checks if a float is between [lower] and up to, but not including, [upper].

    If [lower] is not specified, it's set to to [0.0].

    {3 Exceptions}

    Throws an [Invalid_argument] exception if [lower > upper]

    {2 Examples}

    {[
      Float.inRange(3., ~lower=2., ~upper=4.) == true
      Float.inRange(2., ~lower=1., ~upper=2.) == false
      Float.inRange(9.6, ~lower=5.2, ~upper=7.9) == false
    ]}
")
let inRange: (t, ~lower: t, ~upper: t) => bool

@@ocaml.text(" {1 Angles} ")

@ocaml.doc(" This type is just an alias for [float].

    Its purpose is to make understanding the signatures of the following
    functions a little easier.
")
type radians = float

@ocaml.doc(" [hypotenuse x y] returns the length of the hypotenuse of a right-angled triangle with sides of length [x] and [y], or, equivalently, the distance of the point [(x, y)] to [(0, 0)].

    {2 Examples}

    {[
      Float.hypotenuse(3., 4.) == 5.
    ]}
")
let hypotenuse: (t, t) => t

@ocaml.doc(" Converts an angle in {{: https://en.wikipedia.org/wiki/Degree_(angle) } degrees} to {!Float.radians}.

    {2 Examples}

    {[
      Float.degrees(180.) == Float.pi
      Float.degrees(360.) == Float.pi *. 2.
      Float.degrees(90.) == Float.pi /. 2.
    ]}
")
let degrees: t => radians

@ocaml.doc(" Convert a {!Float.t} to {{: https://en.wikipedia.org/wiki/Radian } radians}.

    {b Note } This function doesn't actually do anything to its argument, but can be useful to indicate intent when inter-mixing angles of different units within the same function.

    {2 Examples}

    {[
      Float.pi->Float.radians == 3.141592653589793
    ]}
")
let radians: t => radians

@ocaml.doc(" Convert an angle in {{: https://en.wikipedia.org/wiki/Turn_(geometry) } turns} into {!Float.radians}.

    One turn is equal to 360 degrees.

    {2 Examples}

    {[
      Float.turns(1. /. 2.) == Float.pi
      Float.turns(1.) ==  Float.degrees(360.)
    ]}
")
let turns: t => radians

@@ocaml.text(" {1 Polar coordinates} ")

@ocaml.doc(" Convert {{: https://en.wikipedia.org/wiki/Polar_coordinate_system } polar coordinates } (radius, radians) to {{: https://en.wikipedia.org/wiki/Cartesian_coordinate_system } Cartesian coordinates } (x,y).

    {2 Examples}

    {[
      Float.fromPolar((Float.squareRoot(2.), Float.degrees(45.))) == (1.0000000000000002, 1.)
    ]}
")
let fromPolar: ((float, radians)) => (float, float)

@ocaml.doc(" Convert {{: https://en.wikipedia.org/wiki/Cartesian_coordinate_system } Cartesian coordinates } [(x, y)] to {{: https://en.wikipedia.org/wiki/Polar_coordinate_system } polar coordinates } [(radius, radians)].

    {2 Examples}

    {[
      Float.toPolar((-1.0, 0.0)) == (1.0, Float.pi)
      Float.toPolar((3.0, 4.0)) == (5.0, 0.9272952180016122)
      Float.toPolar((5.0, 12.0)) == (13.0, 1.1760052070951352)
    ]}
")
let toPolar: ((float, float)) => (float, radians)

@ocaml.doc(" Figure out the cosine given an angle in {{: https://en.wikipedia.org/wiki/Radian } radians}.

    {2 Examples}

    {[
      Float.degrees(60.)->Float.cos == 0.5000000000000001
      (Float.pi /. 3.)->Float.radians->Float.cos == 0.5000000000000001
    ]}
")
let cos: radians => t

@ocaml.doc(" Figure out the arccosine for [adjacent / hypotenuse] in {{: https://en.wikipedia.org/wiki/Radian } radians}:

    {2 Examples}

    {[
      (Float.radians(1.0) /. 2.0)->Float.acos == Float.radians(1.0471975511965979) // 60 degrees or pi/3 radians
    ]}
")
let acos: radians => t

@ocaml.doc(" Figure out the sine given an angle in {{: https://en.wikipedia.org/wiki/Radian } radians}.

    {2 Examples}

    {[
      Float.degrees(30.)->Float.sin == 0.49999999999999994
      (Float.pi /. 6.)->Float.radians->Float.sin == 0.49999999999999994
    ]}
")
let sin: radians => t

@ocaml.doc(" Figure out the arcsine for [opposite / hypotenuse] in {{: https://en.wikipedia.org/wiki/Radian } radians}:

    {2 Examples}

    {[
      Float.asin(1.0 /. 2.0) == 0.5235987755982989 (* 30 degrees or pi / 6 radians *)
    ]}
")
let asin: radians => t

@ocaml.doc(" Figure out the tangent given an angle in radians.

    {2 Examples}

    {[
      Float.degrees(45.)->Float.tan == 0.9999999999999999
      (Float.pi /. 4.)->Float.radians->Float.tan == 0.9999999999999999
      (Float.pi /. 4.)->Float.tan == 0.9999999999999999
    ]}
")
let tan: radians => t

@ocaml.doc(" This helps you find the angle (in radians) to an [(x, y)] coordinate, but
    in a way that is rarely useful in programming.

    {b You probably want} {!atan2} instead!

    This version takes [y / x] as its argument, so there is no way to know whether
    the negative signs comes from the [y] or [x] value. So as we go counter-clockwise
    around the origin from point [(1, 1)] to [(1, -1)] to [(-1,-1)] to [(-1,1)] we do
    not get angles that go in the full circle:

    Notice that everything is between [pi / 2] and [-pi/2]. That is pretty useless
    for figuring out angles in any sort of visualization, so again, check out
    {!Float.atan2} instead!

    {2 Examples}

    {[
      Float.atan(1. /. 1.) == 0.7853981633974483  (* 45 degrees or pi/4 radians *)
      Float.atan(1. /. -1.) == -0.7853981633974483  (* 315 degrees or 7 * pi / 4 radians *)
      Float.atan(-1. /. -1.) == 0.7853981633974483 (* 45 degrees or pi/4 radians *)
      Float.atan(-1. /. 1.) == -0.7853981633974483 (* 315 degrees or 7 * pi/4 radians *)
    ]}
")
let atan: t => radians

@ocaml.doc(" This helps you find the angle (in radians) to an [(x, y)] coordinate.

    So rather than [Float.(atan (y / x))] you can [Float.atan2 ~y ~x] and you can get a full range of angles:

    {2 Examples}

    {[
      Float.atan2(~y=1., ~x=1.) == 0.7853981633974483  (* 45 degrees or pi/4 radians *)
      Float.atan2(~y=1., ~x=-1.) == 2.3561944901923449  (* 135 degrees or 3 * pi/4 radians *)
      Float.atan2(~y=-1., ~x=-1.) == -2.3561944901923449 (* 225 degrees or 5 * pi/4 radians *)
      Float.atan2(~y=-1., ~x=1.) == -0.7853981633974483 (* 315 degrees or 7 * pi/4 radians *)
    ]}
")
let atan2: (~y: t, ~x: t) => radians

@@ocaml.text(" {1 Rounding} ")

@ocaml.doc(" The possible [direction]s availible when doing {!Float.round}.

    See {!Float.round} for what each variant represents.
 ")
type direction = [
  | #Zero
  | #AwayFromZero
  | #Up
  | #Down
  | #Closest([#Zero | #AwayFromZero | #Up | #Down | #ToEven])
]

@ocaml.doc(" Round a number, by default to the to the closest [int] with halves rounded [#Up] (towards positive infinity).

    Other rounding strategies are available by using the optional [~direction] labelelled.

    {2 Examples}

    {[
      Float.round(1.2) == 1.0
      Float.round(1.5) == 2.0
      Float.round(1.8) == 2.0
      Float.round(-1.2) == -1.0
      Float.round(-1.5) == -1.0
      Float.round(-1.8) == -2.0
   ]}

    {3 Towards zero}

    {[
      Float.round(1.2, ~direction=#Zero) == 1.0
      Float.round(1.5, ~direction=#Zero) == 1.0
      Float.round(1.8, ~direction=#Zero) == 1.0
      Float.round(-1.2, ~direction=#Zero) == -1.0
      Float.round(-1.5, ~direction=#Zero) == -1.0
      Float.round(-1.8, ~direction=#Zero) == -1.0
   ]}

    {3 Away from zero}

    {[
      Float.round(1.2, ~direction=#AwayFromZero) == 2.0
      Float.round(1.5, ~direction=#AwayFromZero) == 2.0
      Float.round(1.8, ~direction=#AwayFromZero) == 2.0
      Float.round(-1.2, ~direction=#AwayFromZero) == -2.0
      Float.round(-1.5, ~direction=#AwayFromZero) == -2.0
      Float.round(-1.8, ~direction=#AwayFromZero) == -2.0
   ]}

    {3 Towards infinity}

    This is also known as {!Float.ceiling}.

    {[
      Float.round(1.2, ~direction=#Up) == 2.0
      Float.round(1.5, ~direction=#Up) == 2.0
      Float.round(1.8, ~direction=#Up) == 2.0
      Float.round(-1.2, ~direction=#Up) == -1.0
      Float.round(-1.5, ~direction=#Up) == -1.0
      Float.round(-1.8, ~direction=#Up) == -1.0
   ]}

    {3 Towards negative infinity}

    This is also known as {!Float.floor}.

    {[
      Array.map(
      [-1.8, -1.5, -1.2, 1.2, 1.5, 1.8],
      ~f=Float.round(~direction=#Down),
      ) == [-2.0, -2.0, -2.0, 1.0, 1.0, 1.0]
   ]}

    {3 To the closest integer}

    Rounding a number [x] to the closest integer requires some tie-breaking for when the [fraction] part of [x] is exactly [0.5].

    {3 Halves rounded towards zero}

    {[
      Array.map(
      [-1.8, -1.5, -1.2, 1.2, 1.5, 1.8],
      ~f=Float.round(~direction=#Closest(#Zero)),
      ) == [-2.0, -1.0, -1.0, 1.0, 1.0, 2.0]
   ]}

    {3 Halves rounded away from zero}

    This method is often known as {b commercial rounding}.

    {[
      Array.map(
      [-1.8, -1.5, -1.2, 1.2, 1.5, 1.8],
      ~f=Float.round(~direction=#Closest(#AwayFromZero)),
      ) == [-2.0, -2.0, -1.0, 1.0, 2.0, 2.0]
   ]}

    {3 Halves rounded down}

    {[
      Array.map(
      [-1.8, -1.5, -1.2, 1.2, 1.5, 1.8],
      ~f=Float.round(~direction=#Closest(#Down)),
      ) == [-2.0, -2.0, -1.0, 1.0, 1.0, 2.0]
   ]}

    {3 Halves rounded up}

    This is the default.

    [Float.round(1.5)] is the same as [Float.round(1.5, ~direction=#Closest(#Up))]

    {3 Halves rounded towards the closest even number}

    {[
      Float.round(-1.5, ~direction=#Closest(#ToEven)) == -2.0
      Float.round(-2.5, ~direction=#Closest(#ToEven)) == -2.0
   ]}
")
let round: (~direction: direction=?, t) => t

@ocaml.doc(" Floor function, equivalent to [Float.round(~direction=#Down)].

    {2 Examples}

    {[
      Float.floor(1.2) == 1.0
      Float.floor(1.5) == 1.0
      Float.floor(1.8) == 1.0
      Float.floor(-1.2) == -2.0
      Float.floor(-1.5) == -2.0
      Float.floor(-1.8) == -2.0
   ]}
")
let floor: t => t

@ocaml.doc(" Ceiling function, equivalent to [Float.round(~direction=#Up)].

    {2 Examples}

    {[
      Float.ceiling(1.2) == 2.0
      Float.ceiling(1.5) == 2.0
      Float.ceiling(1.8) == 2.0
      Float.ceiling(-1.2) == -1.0
      Float.ceiling(-1.5) == -1.0
      Float.ceiling(-1.8) == -1.0
   ]}
")
let ceiling: t => t

@ocaml.doc(" Ceiling function, equivalent to [Float.round(~direction=#Zero)].

    {2 Examples}

    {[
      Float.truncate(1.0) == 1.
      Float.truncate(1.2) == 1.
      Float.truncate(1.5) == 1.
      Float.truncate(1.8) == 1.
      Float.truncate(-1.2) == -1.
      Float.truncate(-1.5) == -1.
      Float.truncate(-1.8) == -1.
   ]}
")
let truncate: t => t

@@ocaml.text(" {1 Convert} ")

@ocaml.doc(" Converts a [float] to an {!Int} by {b ignoring the decimal portion}. See {!Float.truncate} for examples.

    Returns [None] when trying to round a [float] which can't be represented as an [int] such as {!Float.nan} or {!Float.infinity} or numbers which are too large or small.

    You probably want to use some form of {!Float.round} prior to using this function.

    {2 Examples}

    {[
      Float.toInt(1.6) == Some(1)
      Float.toInt(2.0) == Some(2))
      Float.toInt(5.683) == Some(5)
      Float.nan->Float.toInt == None
      Float.infinity->Float.toInt == None
      Float.round(1.6)->Float.toInt) = Some(2)
    ]}
")
let toInt: t => option<int>

@ocaml.doc(" Convert a [float] to a {!String}
    The behaviour of this function is platform specific

    Returns a string representation of the float in base 10.
")
let toString: t => string

@@ocaml.text(" {1 Compare} ")

@ocaml.doc(" Test two floats for equality. ")
let equal: (t, t) => bool

@ocaml.doc(" Compare two floats. ")
let compare: (t, t) => int
