/** Functions for working with boolean values.

  Booleans in Rescript are represented by the `true` and `false` literals.

  Whilst a bool isnt a variant, you will get warnings if you haven't
  exhaustively pattern match on them:

  ```rescript
  let bool = false
  let string = switch bool {
  | false => "false"
  }

  (*
  Warning number 8
  You forgot to handle a possible case here, for example: 
  true
  *)
  ```
*/
type t = bool

/** # Create */
/** Convert an {!Int} into a {!Bool}.

  ## Examples

  ```rescript
  Bool.fromInt(0) == Some(false)
  Bool.fromInt(1) == Some(true)
  Bool.fromInt(8) == None
  Bool.fromInt(-3) == None
  ```
*/
let fromInt: int => option<bool>

/** Convert a [String](String.mdx#) into a {!Bool}.

  ## Examples

  ```rescript
  Bool.fromString("true") == Some(true)
  Bool.fromString("false") == Some(false)
  Bool.fromString("True") == None
  Bool.fromString("False") == None
  Bool.fromString("0") == None
  Bool.fromString("1") == None
  Bool.fromString("Not even close") == None
  ```
*/
let fromString: string => option<bool>

/** # Basic operations */
/** The exclusive or operator.

  Returns `true` if *exactly one* of its operands is `true`.

  ## Examples

  ```rescript
  Bool.xor(true, true) == false
  Bool.xor(true, false) == true
  Bool.xor(false, true) == true
  Bool.xor(false, false) == false
  ```
*/
let xor: (bool, bool) => bool

/** Negate a [bool].

  ## Examples

  ```rescript
  Bool.not(false) == true
  Bool.not(true) == false
  ```
*/
let not: t => bool

/** The logical conjunction [AND] operator.

  Returns `true` if *both* of its operands are `true`.
  If the 'left' operand evaluates to `false`, the 'right' operand is not evaluated.

  ## Examples

  ```rescript
  Bool.and_(true, true) == true
  Bool.and_(true, false) == false
  Bool.and_(false, true) == false
  Bool.and_(false, false) == false
  ```
*/
let and_: (bool, bool) => bool

/** # Convert */
/** Convert a [bool] to a [String](String.mdx#)

  ## Examples

  ```rescript
  Bool.toString(true) == "true"
  Bool.toString(false) == "false"
  ```
*/
let toString: bool => string

/** Convert a [bool] to an {!Int}.

  ## Examples

  ```rescript
  Bool.toInt(true) == 1
  Bool.toInt(false) == 0
  ```
*/
let toInt: bool => int

/** {1 Compare} */
/** Test for the equality of two [bool] values.

  ## Examples

  ```rescript
  Bool.equal(true, true) == true
  Bool.equal(false, false) == true
  Bool.equal(false, true) == false
  ```
*/
let equal: (bool, bool) => bool

/** Compare two [bool] values.

  ## Examples

  ```rescript
  Bool.compare(true, false) == 1
  Bool.compare(false, true) == -1
  Bool.compare(true, true) == 0
  Bool.compare(false, false) == 0
  ```
*/
let compare: (bool, bool) => int
