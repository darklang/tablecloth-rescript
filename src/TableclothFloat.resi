/** A module for working with [ floating-point numbers](https://en.wikipedia.org/wiki/Floating-point_arithmetic).

  Valid syntax for `float`s includes:
  ```rescript
  0.
  42.
  42.0
  3.14
  -0.1234
  123_456.123_456
  6.022e23   // = (6.022 * 10^23)
  6.022e+23  // = (6.022 * 10^23)
  1.602e-19  // = (1.602 * 10^-19)
  1e3  // = (1 * 10 ** 3) = 1000.
   ```

  *Historical Note: * The particular details of floats (e.g. `NaN`) are
  specified by [ IEEE 754 ](https://en.wikipedia.org/wiki/IEEE_754) which is literally hard-coded into almost all
  CPUs in the world.
*/
type t = float

/** The literal `0.0` as a named value. */
let zero: t

/** The literal `1.0` as a named value. */
let one: t

/** `NaN` as a named value. NaN stands for [ not a number](https://en.wikipedia.org/wiki/NaN).

  *Note:* comparing values with [Float.nan](Float.mdx#nan) will *always return* `false` even if the value you are comparing against is also `NaN`.

  For detecting `NaN` you should use [Float.isNaN](Float.mdx#isNaN)

  ## Examples

  ```rescript
  let isNotANumber = x => Float.equal(x, nan)

  isNotANumber(nan) == false
  ```
*/
let nan: t

/** Positive [ infinity](https://en.wikipedia.org/wiki/IEEE_754-1985#Positive_and_negative_infinity).

  ```rescript
  Float.divide(Float.pi, ~by=0.0) == Float.infinity
  ```
*/
let infinity: t

/** Negative infinity, see [Float.infinity](Float.mdx#infinity). */
let negativeInfinity: t

/** An approximation of [ Euler's number](https://en.wikipedia.org/wiki/E_(mathematical_constant)). */
let e: t

/** An approximation of [ pi](https://en.wikipedia.org/wiki/Pi). */
let pi: t

/** The smallest interval between two representable numbers. */
let epsilon: t

/** The largest (furthest from zero) representable positive `float`.
  Has a value of approximately `1.79E+308`, or 1.7976931348623157 * 10^308.
  Values larger than `largestValue` are represented as Infinity.
*/
let largestValue: t

/** The smallest representable positive `float`.
  The closest to zero without actually being zero.
  Has a value of approximately `5E-324`, in browsers and in Node.js is 2^-1074
  */
let smallestValue: t

/** Represents the maximum safe integer in JS. [Number]s in JS are represented as a 64-bit floating point
  [ IEEE 754 ](https://en.wikipedia.org/wiki/IEEE_754) numbers.
  `maximumSafeInteger` has a value of 2^53 - 1 === `9_007_199_254_740_991.0`.
  Values larger cannot be represented exactly and cannot be correctly compared.

  Defined as Float since integers in Rescript are limited to 32-bits, their max value is 2^31 - 1 === `2_147_483_647`
  See also: [Int.maximumValue](Int.mdx#maximumValue)
*/
let maximumSafeInteger: t

/** Represents the minimum safe integer in JS. [Number]s in JS are represented as a 64-bit floating point
  [ IEEE 754 ](https://en.wikipedia.org/wiki/IEEE_754) numbers.
  `minimumSafeInteger` has a value of -2^53 - 1 === `-9_007_199_254_740_991.0`.
  Values larger cannot be represented exactly and cannot be correctly compared.

  Defined as Float since Rescript integers are limited to 32-bits, their min value is -2^31 - 1 === `-2_147_483_647`
  See also: [Int.minimumValue](Int.mdx#minimumValue)
*/
let minimumSafeInteger: t

/** Convert an [Int](Int.mdx#) to a `float`.

  ## Examples

  ```rescript
  Float.fromInt(5) == 5.0
  Float.fromInt(0) == 0.0
  Float.fromInt(-7) == -7.0
  ```
*/
let fromInt: int => t

/** Convert a [String](String.mdx#) to a `float`.
  Parses `Infinity` case-sensitive, `NaN` is case-insensitive.

  ## Examples

  ```rescript
  Float.fromString("4.667") == Some(4.667)
  Float.fromString("-4.667") == Some(-4.667)
  Float.fromString("Hamster") == None
  Float.fromString("NaN") == Some(Float.nan)
  Float.fromString("nan") == Some(Float.nan)
  Float.fromString("Infinity") == Some(Float.infinity)
  ```
*/
let fromString: string => option<t>

/** Addition for floating point numbers.

  Although `int`s and `float`s support many of the same basic operations such as
  addition and subtraction you *cannot* `add` an `int` and a `float` directly which
  means you need to use functions like [Int.toFloat](Int.mdx#toFloat) to convert both values to the same type.

  So if you needed to add a [Array.length](Array.mdx#length) to a `float` for some reason, you
  could:

  ```rescript
  [1, 2, 3]->Array.length->Int.toFloat->Float.add(3.5) == 6.5
  ```

  Languages like Java and JavaScript automatically convert `int` values
  to `float` values when you mix and match. This can make it difficult to be sure
  exactly what type of number you are dealing with and cause unexpected behavior.

  Rescript has opted for a design that makes all conversions explicit.

  ## Examples

  ```rescript
  Float.add(3.14, 3.14) == 6.28

  3.2
  ->Float.round
  ->Float.toInt
  ->Option.map(~f=int => int + Array.length([1, 2, 3])) == Some(6)
   ```
*/
let add: (t, t) => t

/** Subtract numbers.

  ## Examples

  ```rescript
  Float.subtract(4.0, 3.0) == 1.0
  ```
*/
let subtract: (t, t) => t

/** Multiply numbers.

  ## Examples

  ```rescript
  Float.multiply(2.0, 7.0) == 14.0
  ```
*/
let multiply: (t, t) => t

/** Floating-point division.

  ## Examples

  ```rescript
  Float.divide(3.14, ~by=2.0) == 1.57
  ```
*/
let divide: (t, ~by: t) => t

/** Exponentiation, takes the base first, then the exponent.

  ## Examples

  ```rescript
  Float.power(~base=7.0, ~exponent=3.0) == 343.0
  ```
*/
let power: (~base: t, ~exponent: t) => t

/** Flips the 'sign' of a `float` so that positive floats become negative and negative integers become positive. Zero stays as it is.

  ## Examples

  ```rescript
  Float.negate(8.) == -8.
  Float.negate(-7.) == 7.
  Float.negate(0.) == 0.
   ```
*/
let negate: t => t

/** Get the [ absolute value](https://en.wikipedia.org/wiki/Absolute_value) of a number.

  ## Examples

  ```rescript
  Float.absolute(8.) == 8.
  Float.absolute(-7) = 7
  Float.absolute(0) == 0
   ```
*/
let absolute: t => t

/** Returns the larger of two `float`s, if both arguments are equal, returns the first argument

  If either (or both) of the arguments are `NaN`, returns `NaN`

  ## Examples

  ```rescript
  Float.maximum(7., 9.) == 9.
  Float.maximum(-4., -1.) == -1.
  Float.maximum(7., Float.nan)->Float.isNaN == true
  ```
*/
let maximum: (t, t) => t

/** Returns the smaller of two `float`s, if both arguments are equal, returns the first argument.

  If either (or both) of the arguments are `NaN`, returns `NaN`.

  ## Examples

  ```rescript
  Float.minimum(7.0, 9.0) == 7.0
  Float.minimum(-4.0, -1.0) == -4.0
  Float.minimum(7., Float.nan)->Float.isNaN == true
  ```
*/
let minimum: (t, t) => t

/** Clamps `n` within the inclusive `lower` and `upper` bounds.

 ### Exceptions

  Throws an `Invalid_argument` exception if `lower > upper`.

  ## Examples

  ```rescript
  Float.clamp(5.0, ~lower=0., ~upper=8.) == 5.
  Float.clamp(9.0, ~lower=0., ~upper=8.) == 8.
  Float.clamp(5.0, ~lower=-10., ~upper=-5.) == -5.
  ```
*/
let clamp: (t, ~lower: t, ~upper: t) => t

/** Take the square root of a number.

  `squareRoot` returns `NaN` when its argument is negative. See [Float.nan](Float.mdx#nan) for more.

  ## Examples

  ```rescript
  Float.squareRoot(4.0) == 2.0
  Float.squareRoot(9.0) == 3.0
  ```
*/
let squareRoot: t => t

/** Calculate the logarithm of a number with a given base.

  ## Examples

  ```rescript
  Float.log(100., ~base=10.) == 2.
  Float.log(256., ~base=2.) == 8.
  ```
*/
let log: (t, ~base: t) => t

/** Determine whether a `float` is an `undefined` or unrepresentable number.

  *Note:* this function is more useful than it might seem since `NaN` *does not* equal `NaN`:

  ```rescript
  (Float.nan == Float.nan) == false
  ```

  ## Examples

  ```rescript
  Float.isNaN(0.0 /. 0.0) == true
  Float.squareRoot(-1.0)->Float.isNaN == true
  Float.isNaN(1.0 /. 0.0) == false  (* Float.infinity *is* a number *)
  Float.isNaN(1.) == false
  ```
*/
let isNaN: t => bool

/** Determine whether a float is finite number. True for any float except `Infinity`, `-Infinity` or `NaN`

  Notice that `NaN` is not finite!

  ## Examples

  ```rescript
  Float.isFinite(0. /. 0.) == false
  Float.squareRoot(-1.)->Float.isFinite == false
  Float.isFinite(1. /. 0.) == false
  Float.isFinite(1.) == true
  Float.nan->Float.isFinite == false
  ```
*/
let isFinite: t => bool

/** Determine whether a float is positive or negative infinity.

  ## Examples

  ```rescript
  Float.isInfinite(0. /. 0.) == false
  Float.squareRoot(-1.)->Float.isInfinite == false
  Float.isInfinite(1. /. 0.) == true
  Float.isInfinite(1.) == false
  Float.nan->Float.isInfinite == false
  ```
*/
let isInfinite: t => bool

/** Determine whether the passed value is an integer.

  ## Examples

  ```rescript
  Float.isInteger(4.0) == true
  Float.pi->Float.isInteger == false
  ```
*/
let isInteger: t => bool

/** Determine whether the passed value is a safe integer (number between -(2^53 - 1) and 2^53 - 1).

  ## Examples

  ```rescript
  Float.isSafeInteger(4.0) == true
  Float.isSafeInteger(Float.pi) == false
  Float.isSafeInteger(Float.maximumSafeInteger +. 1.) == false
  ```
*/
let isSafeInteger: t => bool

/** Checks if a float is between `lower` and up to, but not including, `upper`.

  If `lower` is not specified, it's set to to `0.0`.

 ### Exceptions

  Throws an `Invalid_argument` exception if `lower > upper`

  ## Examples

  ```rescript
  Float.inRange(3., ~lower=2., ~upper=4.) == true
  Float.inRange(2., ~lower=1., ~upper=2.) == false
  Float.inRange(9.6, ~lower=5.2, ~upper=7.9) == false
  ```
*/
let inRange: (t, ~lower: t, ~upper: t) => bool

/** This type is just an alias for [float].

  Its purpose is to make understanding the signatures of the following
  functions a little easier.
*/
type radians = float

/** `hypotenuse(x,y)` returns the length of the hypotenuse of a right-angled triangle with sides of length `x` and `y`, or, equivalently, the distance of the point `(x, y)` to `(0, 0)`.

  ## Examples

  ```rescript
  Float.hypotenuse(3., 4.) == 5.
  ```
*/
let hypotenuse: (t, t) => t

/** Converts an angle in [ degrees](https://en.wikipedia.org/wiki/Degree_(angle)) to [Float.radians](Float.mdx#radians).

  ## Examples

  ```rescript
  Float.degrees(180.) == Float.pi
  Float.degrees(360.) == Float.pi *. 2.
  Float.degrees(90.) == Float.pi /. 2.
  ```
*/
let degrees: t => radians

/** Convert a [Float.t](Float.t.mdx#) to [ radians](https://en.wikipedia.org/wiki/Radian).

  *Note:* This function doesn't actually do anything to its argument, but can be useful to indicate intent when inter-mixing angles of different units within the same function.

  ## Examples

  ```rescript
  Float.pi->Float.radians == 3.141592653589793
  ```
*/
let radians: t => radians

/** Convert an angle in [ turns](https://en.wikipedia.org/wiki/Turn_(geometry)) into [Float.radians](Float.mdx#radians).

  One turn is equal to 360 degrees.

  ## Examples

  ```rescript
  Float.turns(1. /. 2.) == Float.pi
  Float.turns(1.) ==  Float.degrees(360.)
  ```
*/
let turns: t => radians

/** Convert [ polar coordinates ](https://en.wikipedia.org/wiki/Polar_coordinate_system) `(radius, radians)` to [ Cartesian coordinates ](https://en.wikipedia.org/wiki/Cartesian_coordinate_system) `(x,y)`.

  ## Examples

  ```rescript
  Float.fromPolar((Float.squareRoot(2.), Float.degrees(45.))) == (1.0000000000000002, 1.)
  ```
*/
let fromPolar: ((float, radians)) => (float, float)

/** Convert [ Cartesian coordinates ](https://en.wikipedia.org/wiki/Cartesian_coordinate_system) `(x, y)` to [ polar coordinates ](https://en.wikipedia.org/wiki/Polar_coordinate_system) `(radius, radians)`.

  ## Examples

  ```rescript
  Float.toPolar((-1.0, 0.0)) == (1.0, Float.pi)
  Float.toPolar((3.0, 4.0)) == (5.0, 0.9272952180016122)
  Float.toPolar((5.0, 12.0)) == (13.0, 1.1760052070951352)
  ```
*/
let toPolar: ((float, float)) => (float, radians)

/** Figure out the cosine given an angle in [ radians](https://en.wikipedia.org/wiki/Radian).

  ## Examples

  ```rescript
  Float.degrees(60.)->Float.cos == 0.5000000000000001
  (Float.pi /. 3.)->Float.radians->Float.cos == 0.5000000000000001
  ```
*/
let cos: radians => t

/** Figure out the arccosine for `adjacent / hypotenuse` in [ radians](https://en.wikipedia.org/wiki/Radian):

  ## Examples

  ```rescript
  (Float.radians(1.0) /. 2.0)->Float.acos == Float.radians(1.0471975511965979) // 60 degrees or pi/3 radians
  ```
*/
let acos: radians => t

/** Figure out the sine given an angle in [ radians](https://en.wikipedia.org/wiki/Radian).

  ## Examples

  ```rescript
  Float.degrees(30.)->Float.sin == 0.49999999999999994
  (Float.pi /. 6.)->Float.radians->Float.sin == 0.49999999999999994
  ```
*/
let sin: radians => t

/** Figure out the arcsine for `opposite / hypotenuse` in [ radians](https://en.wikipedia.org/wiki/Radian):

  ## Examples

  ```rescript
  Float.asin(1.0 /. 2.0) == 0.5235987755982989 (* 30 degrees or pi / 6 radians *)
  ```
*/
let asin: radians => t

/** Figure out the tangent given an angle in radians.

  ## Examples

  ```rescript
  Float.degrees(45.)->Float.tan == 0.9999999999999999
  (Float.pi /. 4.)->Float.radians->Float.tan == 0.9999999999999999
  (Float.pi /. 4.)->Float.tan == 0.9999999999999999
  ```
*/
let tan: radians => t

/** This helps you find the angle (in radians) to an `(x, y)` coordinate, but
  in a way that is rarely useful in programming.

  *You probably want* [atan2](#atan2) instead!

  This version takes `y / x` as its argument, so there is no way to know whether
  the negative signs comes from the `y` or `x` value. So as we go counter-clockwise
  around the origin from point `(1, 1)` to `(1, -1)` to `(-1,-1)` to `(-1,1)` we do
  not get angles that go in the full circle:

  Notice that everything is between `pi / 2` and `-pi/2`. That is pretty useless
  for figuring out angles in any sort of visualization, so again, check out
  [Float.atan2](Float.mdx#atan2) instead!

  ## Examples

  ```rescript
  Float.atan(1. /. 1.) == 0.7853981633974483  (* 45 degrees or pi/4 radians *)
  Float.atan(1. /. -1.) == -0.7853981633974483  (* 315 degrees or 7 * pi / 4 radians *)
  Float.atan(-1. /. -1.) == 0.7853981633974483 (* 45 degrees or pi/4 radians *)
  Float.atan(-1. /. 1.) == -0.7853981633974483 (* 315 degrees or 7 * pi/4 radians *)
  ```
*/
let atan: t => radians

/** This helps you find the angle (in radians) to an `(x, y)` coordinate.

  So rather than `Float.(atan (y / x))` you can `Float.atan2(~y, ~x)` and you can get a full range of angles:

  ## Examples

  ```rescript
  Float.atan2(~y=1., ~x=1.) == 0.7853981633974483  (* 45 degrees or pi/4 radians *)
  Float.atan2(~y=1., ~x=-1.) == 2.3561944901923449  (* 135 degrees or 3 * pi/4 radians *)
  Float.atan2(~y=-1., ~x=-1.) == -2.3561944901923449 (* 225 degrees or 5 * pi/4 radians *)
  Float.atan2(~y=-1., ~x=1.) == -0.7853981633974483 (* 315 degrees or 7 * pi/4 radians *)
  ```
*/
let atan2: (~y: t, ~x: t) => radians

/** The possible `direction`s availible when doing [Float.round](Float.mdx#round).

  See [Float.round](Float.mdx#round) for what each variant represents.
 */
type direction = [
  | #Zero
  | #AwayFromZero
  | #Up
  | #Down
  | #Closest([#Zero | #AwayFromZero | #Up | #Down | #ToEven])
]

/** Round a number, by default to the to the closest `int` with halves rounded `#Up` (towards positive infinity).

  Other rounding strategies are available by using the optional `~direction` labelelled.

  ## Examples

  ```rescript
  Float.round(1.2) == 1.0
  Float.round(1.5) == 2.0
  Float.round(1.8) == 2.0
  Float.round(-1.2) == -1.0
  Float.round(-1.5) == -1.0
  Float.round(-1.8) == -2.0
   ```

  ### Towards zero

  ```rescript
  Float.round(1.2, ~direction=#Zero) == 1.0
  Float.round(1.5, ~direction=#Zero) == 1.0
  Float.round(1.8, ~direction=#Zero) == 1.0
  Float.round(-1.2, ~direction=#Zero) == -1.0
  Float.round(-1.5, ~direction=#Zero) == -1.0
  Float.round(-1.8, ~direction=#Zero) == -1.0
   ```

  ### Away from zero

  ```rescript
  Float.round(1.2, ~direction=#AwayFromZero) == 2.0
  Float.round(1.5, ~direction=#AwayFromZero) == 2.0
  Float.round(1.8, ~direction=#AwayFromZero) == 2.0
  Float.round(-1.2, ~direction=#AwayFromZero) == -2.0
  Float.round(-1.5, ~direction=#AwayFromZero) == -2.0
  Float.round(-1.8, ~direction=#AwayFromZero) == -2.0
   ```

  ### Towards infinity

  This is also known as [Float.ceiling](Float.mdx#ceiling).

  ```rescript
  Float.round(1.2, ~direction=#Up) == 2.0
  Float.round(1.5, ~direction=#Up) == 2.0
  Float.round(1.8, ~direction=#Up) == 2.0
  Float.round(-1.2, ~direction=#Up) == -1.0
  Float.round(-1.5, ~direction=#Up) == -1.0
  Float.round(-1.8, ~direction=#Up) == -1.0
   ```

  ### Towards negative infinity

  This is also known as [Float.floor](Float.mdx#floor).

  ```rescript
  Array.map(
  [-1.8, -1.5, -1.2, 1.2, 1.5, 1.8],
  ~f=Float.round(~direction=#Down),
  ) == [-2.0, -2.0, -2.0, 1.0, 1.0, 1.0]
   ```

  ### To the closest integer

  Rounding a number `x` to the closest integer requires some tie-breaking for when the [fraction] part of `x` is exactly [0.5].

  ### Halves rounded towards zero

  ```rescript
  Array.map(
  [-1.8, -1.5, -1.2, 1.2, 1.5, 1.8],
  ~f=Float.round(~direction=#Closest(#Zero)),
  ) == [-2.0, -1.0, -1.0, 1.0, 1.0, 2.0]
   ```

  ### Halves rounded away from zero

  This method is often known as *commercial rounding*.

  ```rescript
  Array.map(
  [-1.8, -1.5, -1.2, 1.2, 1.5, 1.8],
  ~f=Float.round(~direction=#Closest(#AwayFromZero)),
  ) == [-2.0, -2.0, -1.0, 1.0, 2.0, 2.0]
   ```

  ### Halves rounded down

  ```rescript
  Array.map(
  [-1.8, -1.5, -1.2, 1.2, 1.5, 1.8],
  ~f=Float.round(~direction=#Closest(#Down)),
  ) == [-2.0, -2.0, -1.0, 1.0, 1.0, 2.0]
   ```

  ### Halves rounded up

  This is the default.

  `Float.round(1.5)` is the same as `Float.round(1.5, ~direction=#Closest(#Up))`

  ### Halves rounded towards the closest even number

  ```rescript
  Float.round(-1.5, ~direction=#Closest(#ToEven)) == -2.0
  Float.round(-2.5, ~direction=#Closest(#ToEven)) == -2.0
   ```
*/
let round: (~direction: direction=?, t) => t

/** Floor function, equivalent to `Float.round(~direction=#Down)`.

  ## Examples

  ```rescript
  Float.floor(1.2) == 1.0
  Float.floor(1.5) == 1.0
  Float.floor(1.8) == 1.0
  Float.floor(-1.2) == -2.0
  Float.floor(-1.5) == -2.0
  Float.floor(-1.8) == -2.0
   ```
*/
let floor: t => t

/** Ceiling function, equivalent to `Float.round(~direction=#Up)`.

  ## Examples

  ```rescript
  Float.ceiling(1.2) == 2.0
  Float.ceiling(1.5) == 2.0
  Float.ceiling(1.8) == 2.0
  Float.ceiling(-1.2) == -1.0
  Float.ceiling(-1.5) == -1.0
  Float.ceiling(-1.8) == -1.0
   ```
*/
let ceiling: t => t

/** Ceiling function, equivalent to `Float.round(~direction=#Zero)`.

  ## Examples

  ```rescript
  Float.truncate(1.0) == 1.
  Float.truncate(1.2) == 1.
  Float.truncate(1.5) == 1.
  Float.truncate(1.8) == 1.
  Float.truncate(-1.2) == -1.
  Float.truncate(-1.5) == -1.
  Float.truncate(-1.8) == -1.
   ```
*/
let truncate: t => t

/** Converts a `float` to an [Int](Int.mdx#) by *ignoring the decimal portion*. See [Float.truncate](Float.mdx#truncate) for examples.

  Returns `None` when trying to round a `float` which can't be represented as an `int` such as [Float.nan](Float.mdx#nan) or [Float.infinity](Float.mdx#infinity) or numbers which are too large or small.

  You probably want to use some form of [Float.round](Float.mdx#round) prior to using this function.

  ## Examples

  ```rescript
  Float.toInt(1.6) == Some(1)
  Float.toInt(2.0) == Some(2))
  Float.toInt(5.683) == Some(5)
  Float.nan->Float.toInt == None
  Float.infinity->Float.toInt == None
  Float.round(1.6)->Float.toInt) = Some(2)
  ```
*/
let toInt: t => option<int>

/** Convert a `float` to a [String](String.mdx#)
  The behaviour of this function is platform specific

  Returns a string representation of the float in base 10.
*/
let toString: t => string

/** Test two floats for equality. */
let equal: (t, t) => bool

/** Compare two floats. */
let compare: (t, t) => int
