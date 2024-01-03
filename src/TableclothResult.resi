/** A [Result](Result.mdx#) is used to represent a computation which may fail.

  A [Result](Result.mdx#) is a variant, which has a constructor for successful results
  `Ok('ok)`, and one for unsuccessful results `[Error('error)]`.

  ```rescript
  type t<'ok, 'error> =
  | Ok('ok)
  | Error('error)
  ```

  Here is how you would annotate a [Result](Result.mdx#) variable whose `Ok`
  variant is an integer and whose `Error` variant is a string:

  ```rescript
  let ok: Result.t<int, string> = Ok(3)
  let error: Result.t<int, string> = Error("This computation failed!")
  ```

  *Note:* The `'error` case can be of *any* type and while `string` is very common you could also use:
  - `Array.t(string)` to allow errors to be accumulated
  - `exn`, in which case the result type just makes exceptions explicit in the return type
  - A variant or polymorphic variant, with one case per possible error. This is means each error can be dealt with explicitly. See [ this excellent article](https://dev.to/kevanstannard/exploring-rescript-exception-handling-57o3) for more information on this approach.

  If the function you are writing can only fail in a single obvious way, maybe you want an [Option](Option.mdx#) instead.
*/
type t<'ok, 'error> = result<'ok, 'error>

/** A function alternative to the `Ok` constructor which can be used in places where
  the constructor isn't permitted or functions like [List.map](List.mdx#map).

  ## Examples

  ```rescript
  String.reverse("desserts") ->Result.ok == Ok("stressed")
  Array.map([1, 2, 3], ~f=Result.ok) == [Ok(1), Ok(2), Ok(3)]
  ```
*/
let ok: 'ok => t<'ok, 'error>

/** A function alternative to the `Error` constructor which can be used in places where
  the constructor isn't permitted such as at the of a [Fun.pipe](Fun.mdx#pipe) or functions like [List.map](List.mdx#map).

  *Note*

  In Rescript you *can* use constructors with the fast pipe (`->`).

  ```rescript
  5->Ok == Ok(5)
  ```

  See the [Rescript docs](https://reasonml.github.io/docs/en/pipe-first#pipe-into-variants) for more.

  ## Examples

  ```rescript
  Int.negate(3)->Result.error == Error(-3)
  Array.map([1, 2, 3], ~f=Result.error) == [Error(1), Error(2), Error(3)]
  ```
*/
let error: 'error => t<'ok, 'error>

/** Run the provided function and wrap the returned value in a [Result](Result.mdx#), catching any exceptions raised.

  ## Examples

  ```rescript
  Result.attempt(() => 5 / 0) // returns Error(Division_by_zero)

  Array.map([1, 2, 3], ~f=Result.ok) == [Ok(1), Ok(2), Ok(3)]

  let numbers = [1, 2, 3]
  Result.attempt(() => numbers[3]) // returns Error(Assert_failure)
  ```
*/
let attempt: (unit => 'ok) => t<'ok, exn>

/** Convert an [Option](Option.mdx#) to a [Result](Result.mdx#) where a `Some(value)` becomes `Ok(value)` and a `None` becomes `Error(error)`.

  ## Examples

  ```rescript
  Result.fromOption(Some(84), ~error="Greater than 100") == Ok(84)

  Result.fromOption(None, ~error="Greater than 100") == Error("Greater than 100")
  ```
*/
let fromOption: (option<'ok>, ~error: 'error) => t<'ok, 'error>

/** Check if a [Result](Result.mdx#) is an `Ok`.

  Useful when you want to perform some side effect based on the presence of
  an `Ok` like logging.

  *Note:* if you need access to the contained value rather than doing
  `Result.isOk` followed by [Result.unwrapUnsafe](Result.mdx#unwrapUnsafe) its safer and just as
  convenient to use pattern matching directly or use one of [Result.andThen](Result.mdx#andThen)
  or [Result.map](Result.mdx#map)

  ## Examples

  ```rescript
  Result.isOk(Ok(3)) == true
  Result.isOk(Error(3)) == false
  ```
*/
let isOk: t<_, _> => bool

/** Check if a [Result](Result.mdx#) is an `Error`.

  Useful when you want to perform some side effect based on the presence of
  an `Error` like logging.

  *Note:* if you need access to the contained value rather than doing
  [Result.isOk](Result.mdx#isOk) followed by [Result.unwrapUnsafe](Result.mdx#unwrapUnsafe) its safer and just as
  convenient to use pattern matching directly or use one of [Result.andThen](Result.mdx#andThen)
  or [Result.map](Result.mdx#map)

  ## Examples

  ```rescript
  Result.isError(Ok(3)) == false
  Result.isError(Error(3)) == true
  ```
*/
let isError: t<_, _> => bool

/** Returns the first argument if it [isError](#isError), otherwise return the second argument.

  Unlike the [Bool.and_](Bool.mdx#and_) operator, the `and_` function does not short-circuit.
  When you call `and_`, both arguments are evaluated before being passed to the function.

  ## Examples

  ```rescript
  Result.and_(Ok("Antelope"), Ok("Salmon")) == Ok("Salmon")

   Result.and_(Error(#UnexpectedBird("Finch")), Ok("Salmon"))
   == Error(#UnexpectedBird("Finch"))

  Result.and_(Ok("Antelope"), Error(#UnexpectedBird("Finch")))
  == Error(#UnexpectedBird("Finch"))

  Result.and_(Error(#UnexpectedInvertebrate("Honey Bee")), Error(#UnexpectedBird("Finch")))
  == Error(#UnexpectedInvertebrate("Honey Bee"))
  ```
*/
let and_: (t<'ok, 'error>, t<'ok, 'error>) => t<'ok, 'error>

/** Return the first argument if it [isOk](#isOk), otherwise return the second.

  Unlike the built in `||` operator, the `or_` function does not short-circuit.
  When you call `or_`, both arguments are evaluated before being passed to the function.

  ## Examples

  ```rescript
  Result.or_(Ok("Boar"), Ok("Gecko")) == Ok("Boar")
  Result.or_(Error(#UnexpectedInvertebrate("Periwinkle")), Ok("Gecko")) == Ok("Gecko")
  Result.or_(Ok("Boar"), Error(#UnexpectedInvertebrate("Periwinkle"))) == Ok("Boar")

  Result.or_(Error(#UnexpectedInvertebrate("Periwinkle")), Error(#UnexpectedBird("Robin")))
  == Error(#UnexpectedBird("Robin"))
   ```
*/
let or_: (t<'ok, 'error>, t<'ok, 'error>) => t<'ok, 'error>

/** Return the second argument if it [isOk](#isOk), otherwise return the first.

  Like [or_](#or_) but in reverse. Useful when using the `|>` operator

  ## Examples

  ```rescript
  Result.orElse (Ok "Boar") (Ok "Gecko") = (Ok "Gecko")
  ```

  ```rescript
  Result.orElse (Error (`UnexpectedInvertabrate "Periwinkle")) (Ok "Gecko") = (Ok "Gecko")
  ```

  ```rescript
  Result.orElse (Ok "Boar") (Error (`UnexpectedInvertabrate "Periwinkle")) = (Ok "Boar") 
  ```

  ```rescript
  Result.orElse (Error (`UnexpectedInvertabrate "Periwinkle")) (Error (`UnexpectedBird "Robin")) = (Error (`UnexpectedInvertabrate "Periwinkle"))
  ```
*/
let orElse: (t<'ok, 'error>, t<'ok, 'error>) => t<'ok, 'error>

let or_else: (t<'ok, 'error>, t<'ok, 'error>) => t<'ok, 'error>

/** Combine two results, if both are `Ok` returns an `Ok` containing a [Tuple2](Tuple2.mdx#) of the values.

  If either is an `Error`, returns the first `Error`.

  The same as writing `Result.map2(~f=Tuple2.make)`.

  ## Examples

  ```rescript
  Result.both(Ok("Badger"), Ok("Rhino")) == Ok("Dog", "Rhino")

  Result.both(Error(#UnexpectedBird("Flamingo")), Ok("Rhino"))
  == Error(#UnexpectedBird("Flamingo"))

  Result.both(Ok("Badger"), Error(#UnexpectedInvertebrate("Blue ringed octopus")))
  == Error(#UnexpectedInvertebrate("Blue ringed octopus"))

  Result.both(
  Error(#UnexpectedBird("Flamingo")),
  Error(#UnexpectedInvertebrate("Blue ringed octopus")),
  ) == Error(#UnexpectedBird("Flamingo"))
  ```
*/
let both: (t<'a, 'error>, t<'b, 'error>) => t<('a, 'b), 'error>

/** Collapse a nested result, removing one layer of nesting.

  ## Examples

  ```rescript
  Result.flatten(Ok(Ok(2))) == Ok(2)

  Result.flatten(Ok(Error(#UnexpectedBird("Peregrin falcon"))))
  == Error(#UnexpectedBird("Peregrin falcon"))

  Result.flatten(Error(#UnexpectedInvertebrate("Woodlouse")))
  == Error(#UnexpectedInvertebrate("Woodlouse"))
  ```
*/
let flatten: t<t<'ok, 'error>, 'error> => t<'ok, 'error>

/** Unwrap a Result using the `~default` value in case of an `Error`.

  ## Examples

  ```rescript
  Result.unwrap(Ok(12), ~default=0) == 12
  Result.unwrap(Error(#UnexpectedBird("Ostrich")), ~default=0) == 0
  ```
*/
let unwrap: (t<'ok, 'error>, ~default: 'ok) => 'ok

/** Unwrap a Result using the `Lazy.force(default)` value in case of an `Error`

  ## Examples

  ```rescript
  Result.unwrapLazy ~default:(lazy 0) (Ok 12) = 12
  ```

  ```rescript
  Result.unwrapLazy ~default:(lazy 0) ((Error (`UnexpectedBird "Ostrich"))) = 0
  ```
*/
let unwrapLazy: (t<'ok, 'error>, ~default: Lazy.t<'ok>) => 'ok

/** Unwrap a Result, raising an exception in case of an `Error`.

 ### Exceptions

  Raises an `Not_found` exception.

  ## Examples

  ```rescript
  Result.unwrapUnsafe(Ok(12)) == 12
  Result.unwrapUnsafe(Error("bad")) // raises Not_found
  ```
*/
let unwrapUnsafe: t<'ok, _> => 'ok

/** Like [Result.unwrap](Result.mdx#unwrap) but unwraps an `Error` value instead.

  ## Examples

  ```rescript
  Result.unwrapError(
  Error(#UnexpectedBird("Swallow")),
  ~default=#UnexpectedInvertebrate("Ladybird"),
  ) == #UnexpectedBird("Swallow")

  Result.unwrapError(Ok(5), ~default=#UnexpectedInvertebrate("Ladybird"))
  == #UnexpectedInvertebrate("Ladybird")
  ```
*/
let unwrapError: (t<'ok, 'error>, ~default: 'error) => 'error

/** Combine two Results.

  If one of the results is an `Error`, that becomes the return result.

  If both are `Error` values, returns its first.

  ## Examples

  ```rescript
  Result.map2(Ok(7), Ok(3), ~f=Int.add) == Ok(10)
  Result.map2(Error("A"), Ok(3), ~f=Int.add) == Error("A")
  Result.map2(Ok(7), Error("B"), ~f=Int.add) == Error("B")
  Result.map2(Error("A"), Error("B"), ~f=Int.add) == Error("A")
  ```
*/
let map2: (t<'a, 'error>, t<'b, 'error>, ~f: ('a, 'b) => 'c) => t<'c, 'error>

/** If all of the elements of a list are `Ok`, returns an `Ok` of the the list of unwrapped values.

  If *any* of the elements are an `Error`, the first one encountered is returned.

  ## Examples

  ```rescript
  Result.values(list{Ok(1), Ok(2), Ok(3), Ok(4)}) == Ok(list{1, 2, 3, 4})
  Result.values(list{Ok(1), Error("two"), Ok(3), Error("four")}) == Error("two")
  ```
*/
let values: list<t<'ok, 'error>> => t<list<'ok>, 'error>

/**
  `Result.combine(results)` takes a list of [Result](Result.mdx#) values. If all
  the elements in `results` are of the form `Ok x`, then [Result.combine]
  creates a list `xs` of all the values extracted from their `Ok`s, and returns
  `Ok(xs)`

  If any of the elements in `results` are of the form `Error(err)`,
  the first of them is returned as the result of `Result.combine`.

  ## Examples

  ```rescript
  Result.combine(list{Ok(1), Ok(2), Ok(3), Ok(4)}) == Ok(list{1, 2, 3, 4})
  Result.combine(list{Ok(1), Error("two"), Ok(3), Error("four")}) == Error("two")
  ```
  */
let combine: list<result<'ok, 'error>> => result<list<'ok>, 'error>

/** Transforms the `'ok` in a result using `f`. Leaves the `'error` untouched.

  ## Examples

  ```rescript
  Result.map(Ok(3), ~f=Int.add(1)) == Ok(9)
  Result.map(Error("three"), ~f=Int.add(1)) == Error("three")
  ```
*/
let map: (t<'a, 'error>, ~f: 'a => 'b) => t<'b, 'error>

/** Transforms the value in an `Error` using `f`. Leaves an `Ok` untouched.

  ## Examples

  ```rescript
  Result.mapError(Ok(3), ~f=String.reverse) == Ok(3)
  Result.mapError(Error("bad"), ~f=String.reverse) == Error("dab")
  ```
*/
let mapError: (t<'ok, 'a>, ~f: 'a => 'b) => t<'ok, 'b>

/** Run a function which may fail on a result.

  Short-circuits of called with an `Error`.

  ## Examples

  ```rescript
  let reciprical = (x: float): Result.t<float, string> =>
  if x == 0.0 {
    Error("Divide by zero")
  } else {
    Ok(1.0 /. x)
  }

  let root = (x: float): Result.t<float, string> =>
  if x < 0.0 {
    Error("Cannot be negative")
  } else {
    Ok(Float.squareRoot(x))
  }

  Result.andThen(Ok(4.0), ~f=reciprical) == Ok(0.25)
  Result.andThen(Error("Missing number!"), ~f=reciprical) == Error("Missing number!")
  Result.andThen(Ok(0.0), ~f=reciprical) == Error("Divide by zero")
  Result.andThen(Ok(4.0), ~f=root)->Result.andThen(~f=reciprical) == Ok(0.5)
  Result.andThen(Ok(-2.0), ~f=root)->Result.andThen(~f=reciprical) == Error("Cannot be negative")
  Result.andThen(Ok(0.0), ~f=root)->Result.andThen(~f=reciprical) == Error("Divide by zero")
  ```
*/
let andThen: (t<'a, 'error>, ~f: 'a => t<'b, 'error>) => t<'b, 'error>

/** Run a function against an `Ok(value)`, ignores `Error`s.

  ## Examples

  ```rescript
  Result.tap(Ok("Dog"), ~f=Js.log)
  (* logs "Dog" *)
  ```
 */
let tap: (t<'ok, _>, ~f: 'ok => unit) => unit

/** Convert a [Result](Result.mdx#) to an [Option](Option.mdx#).

  An `Ok(x)` becomes `Some(x)`

  An `Error(_)` becomes `None`

  ## Examples

  ```rescript
  Result.toOption(Ok(42)) == Some(42)
  Result.toOption(Error("Missing number!")) == None
  ```
*/
let toOption: t<'ok, _> => option<'ok>

/** Test two results for equality using the provided functions.

  ## Examples

  ```rescript
  Result.equal(Ok(3), Ok(3), Int.equal, String.equal) == true
  Result.equal(Ok(3), Ok(4), Int.equal, String.equal) == false
  Result.equal(Error("Fail"), Error("Fail"), Int.equal, String.equal) == true
  Result.equal(Error("Expected error"), Error("Unexpected error"), Int.equal, String.equal) == false
  Result.equal(Error("Fail"), Ok(4), Int.equal, String.equal) == false
  ```
*/
let equal: (t<'ok, 'error>, t<'ok, 'error>, ('ok, 'ok) => bool, ('error, 'error) => bool) => bool

/** Compare results for using the provided functions.
  `f` will be used to compare `Ok`'s and `g` will be used on `Error`s.

  In the case when one of the results is an `Error` and one is `Ok`, `Error`s  are considered 'less' then `Ok`s.

  ## Examples

  ```rescript
  Result.compare(Ok(3), Ok(3), ~f=Int.compare, ~g=String.compare) == 0
  Result.compare(Ok(3), Ok(4), ~f=Int.compare, ~g=String.compare) == -1
  Result.compare(Error("Fail"), Error("Fail"), ~f=Int.compare, ~g=String.compare) == 0
  Result.compare(Error("Fail"), Ok(4), ~f=Int.compare, ~g=String.compare) == -1
  Result.compare(Ok(4), Error("Fail"), ~f=Int.compare, ~g=String.compare) == 1

  Result.compare(
  Error("Expected error"),
  Error("Unexpected error"),
  ~f=Int.compare,
  ~g=String.compare
  ) == -1
  ```
*/
let compare: (
  t<'ok, 'error>,
  t<'ok, 'error>,
  ~f: ('ok, 'ok) => int,
  ~g: ('error, 'error) => int,
) => int
