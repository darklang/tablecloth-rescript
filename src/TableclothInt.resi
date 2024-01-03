/** An `int` is a whole number.

  Rescript's has a 32-bit [ signed ](https://en.wikipedia.org/wiki/Signed_number_representations) [ integer](https://en.wikipedia.org/wiki/Integer).
  Largegest `int` value is 2^31 - 1 === [2_147_483_647], and smallest is -2^31 - 1 === [-2_147_483_647]

  `int`s are subject to [ overflow ](https://en.wikipedia.org/wiki/Integer_overflow), meaning that `Int.maximumValue + 1 == Int.minimumValue`.

  If you work with integers larger than [minimumValue](#minimumValue) and smaller than [maximumValue](#maximumValue) you can use the [Int](#Int) module.
  If you need to work with larger numbers, concider using [Float](Float.mdx#) since they are signed 64-bit `float`s and limited by
  `1.79E+308`, or 1.7976931348623157 * 10^308 at the upper end and `5E-324` at the lower.

  Valid syntax for `int`s includes:
  ```rescript
  0
  42
  9000
  1_000_000
  1_000_000
  0xFF // 255 in hexadecimal
  0x000A // 10 in hexadecimal
   ```

  ~Historical Note:~ The name `int` comes from the term [ integer](https://en.wikipedia.org/wiki/Integer). It appears
  that the `int` abbreviation was introduced in the programming language ALGOL 68.

  Today, almost all programming languages use this abbreviation.
*/
type t = int

/** The literal `0` as a named value. */
let zero: t

/** The literal `1` as a named value. */
let one: t

/** The maximum representable `int` on the current platform. */
let maximumValue: t

/** The minimum representable `int` on the current platform. */
let minimumValue: t

/** Attempt to parse a `string` into a `int`.

  ## Examples

  ```rescript
  Int.fromString("0") == Some(0)
  Int.fromString("42") == Some(42)
  Int.fromString("-3") == Some(-3)
  Int.fromString("123_456") == Some(123_456)
  Int.fromString("0xFF") == Some(255)
  Int.fromString("0x00A") == Some(10)
  Int.fromString("Infinity") == None
  Int.fromString("NaN") == None
  ```
*/
let fromString: string => option<t>

/** Add two [Int](Int.mdx#) numbers.

  You *cannot* add an `int` and a `float` directly though.

  See [Float.add](Float.mdx#add) for why, and how to overcome this limitation.

  ## Examples

  ```rescript
  Int.add(3002, 4004) == 7006
  ```
*/
let add: (t, t) => t

/** Subtract numbers.
  
  ## Examples

  ```rescript
  Int.subtract(4, 3) == 1
  ```
*/
let subtract: (t, t) => t

/** Multiply `int`s.
  
  ## Examples

  ```rescript
  Int.multiply(2, 7) == 14
  ```
*/
let multiply: (t, t) => t

/** Integer division.

  Notice that the remainder is discarded.

 ### Exceptions

  Throws [Division_by_zero] when the divisor is `0`.

  ## Examples

  ```rescript
  Int.divide(3, ~by=2) == 1
  ```
*/
let divide: (t, ~by: t) => t

/** Floating point division

  ## Examples

  ```rescript
  Int.divideFloat(3, ~by=2) == 1.5
  Int.divideFloat(27, ~by=5) == 5.25
  Int.divideFloat(8, ~by=4) == 2.0
  ```
*/
let divideFloat: (t, ~by: t) => float

/** Exponentiation, takes the base first, then the exponent.

  ## Examples

  ```rescript
  Int.power(~base=7, ~exponent=3) == 343
  ```
*/
let power: (~base: t, ~exponent: t) => t

/** Flips the 'sign' of an integer so that positive integers become negative and negative integers become positive. Zero stays as it is.

  ## Examples

  ```rescript
  Int.negate(8) == -8
  Int.negate(-7) == 7
  Int.negate(0) == 0
  ```
*/
let negate: t => t

/** Get the [ absolute value ](https://en.wikipedia.org/wiki/Absolute_value) of a number.

  ## Examples

  ```rescript
  Int.absolute(8) == 8
  Int.absolute(-7) == 7
  Int.absolute(0) == 0
  ```
*/
let absolute: t => t

/** Perform [ modular arithmetic ](https://en.wikipedia.org/wiki/Modular_arithmetic).

  *Note:* [modulo](#modulo) is not `%` JS operator. If you want `%`, use [remainder](#remainder)

  If you intend to use `modulo` to detect even and odd numbers consider using [Int.isEven](Int.mdx#isEven) or [Int.isOdd](Int.mdx#isOdd).

  The `modulo` function works in the typical mathematical way when you run into negative numbers

  Use [Int.remainder](Int.mdx#remainder) for a different treatment of negative numbers.

  ## Examples

  ```rescript
  Int.modulo(-4, ~by=3) == 2
  Int.modulo(-3, ~by=3) == 0
  Int.modulo(-2, ~by=3) = 1
  Int.modulo(-1, ~by=3) == 2
  Int.modulo(0, ~by=3) == 0
  Int.modulo(1, ~by=3) == 1
  Int.modulo(2, ~by=3) == 2
  Int.modulo(3, ~by=3) == 0
  Int.modulo(4, ~by=3) == 1
  ```
*/
let modulo: (t, ~by: t) => t

/** Get the remainder after division. Works the same as `%` JS operator

  Use [Int.modulo](Int.mdx#modulo) for a different treatment of negative numbers.

  The sign of the result is the same as the sign 
  of the dividend (`~by`) while with a [modulo](#modulo) the sign 
  of the result is the same as the divisor (`t`).

  ## Examples

  ```rescript
  Array.map([-5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5], ~f=Int.remainder(~by=4)) ==
  [-1, 0, -3, -2, -1, 0, 1, 2, 3, 0, 1]
   ```
*/
let remainder: (t, ~by: t) => t

/** Returns the larger of two `int`s.

  ## Examples

  ```rescript
  Int.maximum(7, 9) == 9
  Int.maximum(-4, -1) == -1
  ```
*/
let maximum: (t, t) => t

/** Returns the smaller of two `int`s.

  ## Examples

  ```rescript
  Int.minimum(7, 9) == 7
  Int.minimum(-4, -1) == -4
  ```
*/
let minimum: (t, t) => t

/** Check if an `int` is even.

  ## Examples

  ```rescript
  Int.isEven(8) == true
  Int.isEven(7) == false
  Int.isEven(0) == true
  ```
*/
let isEven: t => bool

/** Check if an `int` is odd.

  ## Examples

  ```rescript
  Int.isOdd(7) == true
  Int.isOdd(8) == false
  Int.isOdd(0) == false
  ```
*/
let isOdd: t => bool

/** Clamps `n` within the inclusive `lower` and `upper` bounds.

 ### Exceptions

  Throws an `Invalid_argument` exception if `lower > upper`

  ## Examples

  ```rescript
  Int.clamp(5, ~lower=0, ~upper=8) == 5
  Int.clamp(9, ~lower=0, ~upper=8) == 8
  Int.clamp(5, ~lower=-10, ~upper=-5) == -5
  ```
*/
let clamp: (t, ~lower: t, ~upper: t) => t

/** Checks if `n` is between `lower` and up to, but not including, `upper`.

 ### Exceptions

  Throws an `Invalid_argument` exception if `lower > upper`

  ## Examples

  ```rescript
  Int.inRange(3, ~lower=2, ~upper=4) == true
  Int.inRange(4, ~lower=5, ~upper=8) == false
  Int.inRange(-3, ~lower=-6, ~upper=-2) == true
  ```

*/
let inRange: (t, ~lower: t, ~upper: t) => bool

/** Convert an `int` into a `float`. Useful when mixing [Int](Int.mdx#) and [Float](Float.mdx#) values like this:

  ## Examples

  ```rescript
  let halfOf = (number: int): float => Int.toFloat(number) /. 2.

  halfOf(7) == 3.5
   ```
*/
let toFloat: t => float

/** Convert an `int` into a `string` representation.

  Guarantees that

  ```rescript
  n->Int.toString->Int.fromString == Some(n)
  ```

  ## Examples

  ```rescript
  Int.toString(3) == "3"
  Int.toString(-3) == "-3"
  Int.toString(0) == "0"
  ```
*/
let toString: t => string

/** Test two `int`s for equality. */
let equal: (t, t) => bool

/** Compare two `int`s. */
let compare: (t, t) => int

/** The unique identity for [Comparator](Comparator.mdx#). */
type identity

let comparator: TableclothComparator.t<t, identity>
