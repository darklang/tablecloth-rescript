/** A mutable vector of elements which must have the same type.

  Has constant time (O(1)) [get](#get), [set](#set) and [length](#length) operations.

  Arrays have a fixed length, if you want to be able to add an arbitrary number of elements maybe you want a [List](List.mdx#)}.
*/
type t<'a> = array<'a>

/** # Create

  You can create an `array` in Rescript with the `[1, 2, 3]` syntax.
*/
/** Create an array with only one element.

  ## Examples

  ```rescript
  Array.singleton(1234) == [1234]
  Array.singleton("hi") == ["hi"]
  ```
*/
let singleton: 'a => t<'a>

/** Creates an array of length `length` with the value `x` populated at each index.

  ## Examples

  ```rescript
  Array.repeat('a', ~length=5) == ['a', 'a', 'a', 'a', 'a']
  Array.repeat(7, ~length=0) == []
  Array.repeat("Why?", ~length=-1) == []
  ```
*/
let repeat: ('a, ~length: int) => t<'a>

/** Creates an array containing all of the integers from `from` if it is provided or `0` if not, up to but not including `to`.

  ## Examples

  ```rescript
  Array.range(5) == [0, 1, 2, 3, 4]
  Array.range(5, ~from=2) == [2, 3, 4]
  Array.range(3, ~from=-2) == [-2, -1, 0, 1, 2]
  ```
*/
let range: (~from: int=?, int) => t<int>

/** Initialize an array. `Array.initialize(n, ~f)` creates an array of length `n` with
  the element at index `i` initialized to the result of `f(i)`.

  ## Examples

  ```rescript
  Array.initialize(4, ~f=identity) == [0, 1, 2, 3]
  Array.initialize(4, ~f=n => n * n) == [0, 1, 4, 9]
  ```
*/
let initialize: (int, ~f: int => 'a) => t<'a>

/** Create an array from a [List](List.mdx#).

  ## Examples

  ```rescript
  Array.fromList(list{1, 2, 3}) == [1, 2, 3]
  ```
*/
let fromList: list<'a> => t<'a>

/** Create a shallow copy of an array.

  ## Examples

  ```rescript
  let numbers = [1, 2, 3]
  let otherNumbers = Array.copy(numbers)
  numbers[1] == 9
  numbers == [1, 9, 3]
  otherNumbers == [1, 2, 3]

  let numberGrid = [
  [1, 2, 3],
  [4, 5, 6],
  [7, 8, 9]
  ]

  let numberGridCopy = Array.copy(numberGrid)

  numberGrid[1][1] == 0
  numberGridCopy[1][1] == 9
  ```
*/
let clone: t<'a> => t<'a>

/** # Basic operations */
/** Get the element at the specified index.

  The first element has index number 0.

  The last element has index number `Array.length(a) - 1`.

  You should prefer using the dedicated literal syntax:

  ```rescript
  array[n]
  ```

  Or using the safer [Array.getAt](#get-at) function.

 ### Exceptions

  Raises `Invalid_argument("index out of bounds")` for indexes outside of the range `0` to `(Array.length(a) - 1)`.

  ## Examples

  ```rescript
  [1, 2, 3, 2, 1][3] == 2

  let animals = ["cat", "dog", "eel"]
  animals[2] == "eel"
  ```
*/
let get: (t<'a>, int) => 'a

/** Returns, as an [Option](Option.mdx#), the element at index number `n` of array `a`.

  Returns `None` if `n` is outside the range `0` to [(Array.length(a) - 1)].

  ## Examples

  ```rescript
  Array.getAt([0, 1, 2], ~index=5) == None
  Array.getAt([], ~index=0) == None
  ```
*/
let getAt: (t<'a>, ~index: int) => option<'a>

/** Modifies an array in place, replacing the element at `index` with `value`.

  You should prefer either to write

  ```rescript
  array[index] = value
  ```

  Or use the [Array.setAt](#set-at) function instead.

 ### Exceptions

  Raises `Invalid_argument("index out of bounds")` if `n` is outside the range `0` to `Array.length(a) - 1`.

  ## Examples

  ```rescript
  let numbers = [1, 2, 3]
  Array.set(numbers, 1, 1)
  numbers[2] = 0

  numbers  == [1, 1, 0]
  ```
*/
let set: (t<'a>, int, 'a) => unit

/** Like [Array.set](#set) but with labelled arguments. */
let setAt: (t<'a>, ~index: int, ~value: 'a) => unit

/** Get the first element of an array.

  Returns `None` if the array is empty.

  ## Examples

  ```rescript
  Array.first([1, 2, 3]) == Some(1)
  Array.first([1]) == Some(1)
  Array.first([]) == None
  ```
*/
let first: t<'a> => option<'a>

/** Get the last element of an array.

  Returns `None` if the array is empty.

  ## Examples

  ```rescript
  Array.last([1, 2, 3]) == Some(3)
  Array.last([1]) == Some(1)
  Array.last([]) == None
  ```
*/
let last: t<'a> => option<'a>

/** Get a sub-section of an array. `from` is a zero-based index where we will start our slice.

  The `to_` is a zero-based index that indicates the end of the slice.

  The slice extracts up to but not including `to_`.

  Both the `from` and `to_` indexes can be negative, indicating an offset from the end of the array.

  ## Examples

  ```rescript
  Array.slice([0, 1, 2, 3, 4], ~from=0, ~to_=3) == [0, 1, 2]
  Array.slice([0, 1, 2, 3, 4], ~from=1, ~to_=4) == [1, 2, 3]
  Array.slice([0, 1, 2, 3, 4], ~from=5, ~to_=3) == []
  Array.slice([0, 1, 2, 3, 4], ~from=1, ~to_=-1) == [1, 2, 3]
  Array.slice([0, 1, 2, 3, 4], ~from=-2, ~to_=5) == [3, 4]
  Array.slice([0, 1, 2, 3, 4], ~from=-2, ~to_=-1) == [3]
  ```
*/
let slice: (~to_: int=?, t<'a>, ~from: int) => t<'a>

/** Swaps the values at the provided indicies.

 ### Exceptions

  Raises an `Invalid_argument` exception of either index is out of bounds for the array.

  ## Examples

  ```rescript
  Array.swap([1, 2, 3], 1, 2) == [1, 3, 2]
  ```
*/
let swap: (t<'a>, int, int) => unit

/** Reverses an array *in place*, mutating the existing array.

  ## Examples

  ```rescript
  let numbers = [1, 2, 3]
  Array.reverse(numbers)
  numbers == [3, 2, 1]
  ```
*/
let reverse: t<'a> => unit

/** Sort in place, modifying the existing array, using the provided `compare` function to determine order.

  The time and space complexity of the sort cannot be guaranteed as it depends on the implementation.

  ## Examples

  ```rescript
  Array.sort([5, 6, 8, 3, 6], ~compare) == [3, 5, 6, 6, 8]
  ```
*/
let sort: (t<'a>, ~compare: ('a, 'a) => int) => unit

/** # Query */
/** Check if an array is empty.

  ## Examples

  ```rescript
  Array.isEmpty([1, 2, 3]) == false
  Array.isEmpty([]) == true
  ```
*/
let isEmpty: t<'a> => bool

/** Return the length of an array.

  ## Examples

  ```rescript
  Array.length([1, 2, 3]) == 3
  Array.length([]) == 0
  ```
*/
let length: t<'a> => int

/** Determine if `f` returns true for `any` values in an array.

  Iteration is stopped as soon as `f` returns `true`.

  ## Examples

  ```rescript
  Array.any([1, 2, 3, 5], ~f=Int.isEven) == true
  Array.any([1, 3, 5, 7], ~f=Int.isEven) == false
  Array.any([], ~f=Int.isEven) == false
  ```
*/
let any: (t<'a>, ~f: 'a => bool) => bool

/** Determine if `f` returns true for `all` values in an array.

  Iteration is stopped as soon as `f` returns `false`.

  ## Examples

  ```rescript
  Array.all([2, 4], ~f=Int.isEven) == true
  Array.all([2, 3], ~f=Int.isEven) == false
  Array.all([], ~f=Int.isEven) == true
  ```
*/
let all: (t<'a>, ~f: 'a => bool) => bool

/** Count the number of elements which function `f` will return `true`.

  ## Examples

  ```rescript
  Array.count([7, 5, 8, 6], ~f=Int.isEven) == 2
  ```
*/
let count: (t<'a>, ~f: 'a => bool) => int

/** Returns, as an [Option](Option.mdx#), the first element for which `f` evaluates to `true`.

  If `f` doesn't return `true` for any of the elements `find` will return `None`

  ## Examples

  ```rescript
  Array.find([1, 3, 4, 8], ~f=Int.isEven) == Some(4)
  Array.find([0, 2, 4, 8], ~f=Int.isOdd) == None
  Array.find([], ~f=Int.isEven) == None
  ```
*/
let find: (t<'a>, ~f: 'a => bool) => option<'a>

/** Similar to [Array.find](#find) but `f` is also called with the current index, and the return value will be a tuple of the index the passing value was found at and the passing value.

  ## Examples

  ```rescript
  Array.findIndex([1, 3, 4, 8], ~f=(index, number) => index > 2 && Int.isEven(number)) == Some(3, 8)
  ```
*/
let findIndex: (t<'a>, ~f: (int, 'a) => bool) => option<(int, 'a)>

/** Test if an array contains the specified element using the provided [equal] to test for equality.

  ## Examples

  ```rescript
  Array.includes([1, 2, 3], 2, ~equal=Int.equal) == true
  ```
*/
let includes: (t<'a>, 'a, ~equal: ('a, 'a) => bool) => bool

/** Find the smallest element using the provided `compare` function.

  Returns `None` if called on an empty array.

  ## Examples

  ```rescript
  Array.minimum([7, 5, 8, 6], ~compare=Int.compare) == Some(5)
  Array.minimum([], ~compare=Int.compare) == None
  ```
*/
let minimum: (t<'a>, ~compare: ('a, 'a) => int) => option<'a>

/** Find the largest element using the provided `compare` function.

  Returns `None` if called on an empty array.

  ## Examples

  ```rescript
  Array.maximum([7, 5, 8, 6], ~compare=Int.compare) == Some(8)
  Array.maximum([], ~compare=Int.compare) == None
  ```
*/
let maximum: (t<'a>, ~compare: ('a, 'a) => int) => option<'a>

/** Find a [Tuple2](#Tuple2.mdx) of the [Array.minimum](#minimum) and [Array.maximum](#maximum) in a single pass.

  Returns `None` if called on an empty array.

  ## Examples

  ```rescript
  Array.extent([7, 5, 8, 6], ~compare=Int.compare) == Some(5, 8)
  Array.extent([7], ~compare=Int.compare) == Some(7, 7)
  Array.extent([], ~compare=Int.compare) == None
  ```
*/
let extent: (t<'a>, ~compare: ('a, 'a) => int) => option<('a, 'a)>

/** Calculate the sum of an array using the provided modules `zero` value and `add` function.

  ## Examples

  ```rescript
  Array.sum([1, 2, 3], module(Int)) == 6
  Array.sum([4.0, 4.5, 5.0], module(Float)) == 13.5

  Array.sum(
  ["a", "b", "c"],
  module(
  {
    type rec t = string
    let zero = ""
    let add = (x, y) => x ++ y
  }
  ),
  ) == "abc"
  ```
*/
let sum: (t<'a>, module(TableclothContainer.Sum with type t = 'a)) => 'a

/** # Transform */
/** Create a new array which is the result of applying a function `f` to every element.

  ## Examples

  ```rescript
  Array.map([1.0, 4.0, 9.0], ~f=Float.squareRoot) == [1.0, 2.0, 3.0]
  ```
*/
let map: (t<'a>, ~f: 'a => 'b) => t<'b>

/** Apply a function `f` to every element with its index as the first argument.

  ## Examples

  ```rescript
  Array.mapWithIndex([5, 5, 5], ~f=Int.multiply) == [0, 5, 10]
  ```
*/
let mapWithIndex: (t<'a>, ~f: (int, 'a) => 'b) => t<'b>

/** Keep elements where function `f` will return `true`.

  ## Examples

  ```rescript
  Array.filter([1, 2, 3, 4, 5, 6], ~f=Int.isEven) == [2, 4, 6]
  ```
*/
let filter: (t<'a>, ~f: 'a => bool) => t<'a>

/** Allows you to combine [Array.map](#map) and [Array.filter](#filter) into a single pass.

  The output array only contains elements for which `f` returns `Some`.

  Why `Array.filterMap` and not just [Array.filter](#filter) then [Array.map](#map)?

  `filterMap` removes the [Option](Option.mdx#) layer automatically.

  If your mapping is already returning an [Option](Option.mdx#) and you want to skip over `None`s, then `filterMap` is much nicer to use.

  ## Examples

  ```rescript
  let characters = ['a', '9', '6', ' ', '2', 'z']
  Array.filterMap(characters, ~f=Char.toDigit) == [9, 6, 2]

  Array.filterMap([3, 4, 5, 6], ~f=number =>
  Int.isEven(number) ? Some(number * number) : None
  ) == [16, 36]
  ```
*/
let filterMap: (t<'a>, ~f: 'a => option<'b>) => t<'b>

/** [Array.map](#map) `f` onto an array and [Array.flatten](#flatten) the resulting arrays.

  ## Examples

  ```rescript
  Array.flatMap([1, 2, 3], ~f=n => [n, n]) == [1, 1, 2, 2, 3, 3]
  ```
*/
let flatMap: (t<'a>, ~f: 'a => t<'b>) => t<'b>

/** Just as [Array.prototype.reduce()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/Reduce) from the JS,
  `fold` will produce a new value from an array.

  `fold` takes two arguments, an `initial` 'accumulator' value and a function `f`.

  For each element of the array `f` will be called with two arguments: the current accumulator and an element.

  `f` returns the value that the accumulator should have for the next iteration.

  The `initial` value is the value the accumulator will have on the first call to `f`.

  After applying `f` to every element of the array, `fold` returns the accumulator.

  `fold` iterates over the elements of the array from first to last.

  Folding is useful whenever you have a collection of something, and want to produce a single value from it.

  For example, if we have:

  ```rescript
  let numbers = [(1, 2, 3)]
  let sum = Array.fold(numbers, ~initial=0, ~f=(accumulator, element) => accumulator + element)

  sum == 6
  ```

  Walking though each iteration step by step:

  + `accumulator: 0, element: 1, result: 1`
  + `accumulator: 1, element: 2, result: 3`
  + `accumulator: 3, element: 3, result: 6`

  And so the final result is `6`. (Note that in reality you probably want to use [Array.Array.sum](#Array.sum))

  ## Examples

  ```rescript
  Array.fold([3, 4, 5], ~f=Int.multiply, ~initial=2) == 120

  Array.fold([1, 1, 2, 2, 3], ~initial=Set.Int.empty, ~f=Set.add) -> Set.toArray == [1, 2, 3]

  let lastEven = integers =>
  Array.fold(integers, ~initial=None, ~f=(last, int) =>
    int->Int.isEven ? Some(int) : last
  )

  lastEven([1, 2, 3, 4, 5]) == Some(4)
  ```
*/
let fold: (t<'a>, ~initial: 'b, ~f: ('b, 'a) => 'b) => 'b

/** This method is like [Array.fold](#fold) except that it iterates over the elements of the array from last to first.

  ## Examples

  ```rescript
  Array.repeat(~length=3, 5) -> Array.foldRight(~f=Int.add, ~initial=0) == 15
  Array.foldRight([3, 4, 5], ~f=Int.multiply, ~initial=2) == 120
  ```
*/
let foldRight: (t<'a>, ~initial: 'b, ~f: ('b, 'a) => 'b) => 'b

/** Creates a new array which is the result of appending the second array onto the end of the first.

  ## Examples

  ```rescript
  let fortyTwos = Array.repeat(42, ~length=2)
  let eightyOnes = Array.repeat(81, ~length=3)
  Array.append(fourtyTwos, eightyOnes) == [42, 42, 81, 81, 81]
  ```
*/
let append: (t<'a>, t<'a>) => t<'a>

/** Flatten an array of arrays into a single array:

  ## Examples

  ```rescript
  Array.flatten([[1, 2], [3], [4, 5]]) == [1, 2, 3, 4, 5]
  ```
*/
let flatten: t<t<'a>> => t<'a>

/** Combine two arrays by merging each pair of elements into a [Tuple2](Tuple2.mdx#).

  If one array is longer, the extra elements are dropped.

  The same as [Array.map2(~f=Tuple2.make)]

  ## Examples

  ```rescript
  Array.zip([1, 2, 3, 4, 5], ["Dog", "Eagle", "Ferret"]) == [(1, "Dog"), (2, "Eagle"), (3, "Ferret")]
  ```
*/
let zip: (t<'a>, t<'b>) => t<('a, 'b)>

/** Combine two arrays, using `f` to combine each pair of elements.

  If one array is longer, the extra elements are dropped.

  ## Examples

  ```rescript
  let totals = (xs, ys) => Array.map2(~f=Int.add, xs, ys)

  totals([1, 2, 3], [4, 5, 6]) == [5, 7, 9]

  Array.map2(~f=Tuple2.make, ["alice", "bob", "chuck"], [2, 5, 7, 8])
  == [("alice", 2), ("bob", 5), ("chuck", 7)];
  ```
*/
let map2: (t<'a>, t<'b>, ~f: ('a, 'b) => 'c) => t<'c>

/** Combine three arrays, using `f` to combine each trio of elements.

  If one array is longer, the extra elements are dropped.

  ## Examples

  ```rescript
  Array.map3(
  ~f=Tuple3.make,
  ["alice", "bob", "chuck"],
  [2, 5, 7, 8],
  [true, false, true, false],
  )
  == [("alice", 2, true), ("bob", 5, false), ("chuck", 7, true)];
  ```
*/
let map3: (t<'a>, t<'b>, t<'c>, ~f: ('a, 'b, 'c) => 'd) => t<'d>

/** # Deconstruct */
/** Split an array into a [Tuple2](Tuple2.mdx#) of arrays. Values which `f` returns true for will end up in [Tuple2.first](Tuple2.mdx#first).

  ## Examples

  ```rescript
  Array.partition([1, 2, 3, 4, 5, 6], ~f=Int.isOdd) == ([1, 3, 5], [2, 4, 6])
  ```
*/
let partition: (t<'a>, ~f: 'a => bool) => (t<'a>, t<'a>)

/** Divides an array into a [Tuple2](Tuple2.mdx#) of arrays.

  Elements which have index upto (but not including) `index` will be in the first component of the tuple.

  Elements with an index greater than or equal to `index` will be in the second.

  ### Exceptions

  Raises an `Invalid_argument` exception if `index` is less than zero.

  ## Examples

  ```rescript
  Array.splitAt([1, 2, 3, 4, 5], ~index=2) == ([1, 2], [3, 4, 5])
  Array.splitAt([1, 2, 3, 4, 5], ~index=10) == ([1, 2, 3, 4, 5], [])
  Array.splitAt([1, 2, 3, 4, 5], ~index=0) == ([], [1, 2, 3, 4, 5])
  ```
*/
let splitAt: (t<'a>, ~index: int) => (t<'a>, t<'a>)

/** Divides an array at the first element `f` returns `true` for.

  Returns a [Tuple2](Tuple2.mdx#), the first component contains the elements `f` returned false for,
  the second component includes the element that `f` retutned `true` for an all the remaining elements.

  ## Examples

  ```rescript
  Array.splitWhen([5, 7, 8, 6, 4], ~f=Int.isEven) == ([5, 7], [8, 6, 4])

  Array.splitWhen(["Ant", "Bat", "Cat"], ~f=animal => String.length(animal) > 3) ==
  (["Ant", "Bat", "Cat"], [])

  Array.splitWhen([2., Float.pi, 1.111], ~f=Float.isInteger) == ([], [2., Float.pi, 1.111])
  ```
*/
let splitWhen: (t<'a>, ~f: 'a => bool) => (t<'a>, t<'a>)

/** Decompose an array of [Tuple2](Tuple2.mdx#)s into a [Tuple2](Tuple2.mdx#) of arrays.

  ## Examples

  ```rescript
  Array.unzip([(0, true), (17, false), (1337, true)]) == ([0, 17, 1337], [true, false, true])
  ```
*/
let unzip: t<('a, 'b)> => (t<'a>, t<'b>)

/** # Iterate */
/** Iterates over the elements of invokes `f` for each element.

  ## Examples

  ```rescript
  Array.forEach([1, 2, 3], ~f=int => Js.log(int))
  ```
*/
let forEach: (t<'a>, ~f: 'a => unit) => unit

/** Iterates over the elements of invokes `f` for each element.

  ## Examples

  ```rescript
  Array.forEachWithIndex([1, 2, 3], ~f=(index, int) => Js.log2(index, int))
  (*
  0 1
  1 2
  2 3
  *)
  ```
*/
let forEachWithIndex: (t<'a>, ~f: (int, 'a) => unit) => unit

/** Return all of the `Some` values from an array of options.

  ## Examples

  ```rescript
  Array.values([Some("Ant"), None, Some("Cat")]) == ["Ant", "Cat"]
  Array.values([None, None, None]) == []
  ```
*/
let values: t<option<'a>> => t<'a>

/** Places `sep` between all the elements of the given array.

  ## Examples

  ```rescript
  Array.intersperse(~sep="on", ["turtles", "turtles", "turtles"])
  == ["turtles", "on", "turtles", "on", "turtles"];

  Array.intersperse(~sep=0, []) == []
  ```
*/
let intersperse: (t<'a>, ~sep: 'a) => t<'a>

/** Split an array into equally sized chunks.

  If there aren't enough elements to make the last 'chunk', those elements are ignored.

  ## Examples

  ```rescript
  Array.chunksOf(["#FFBA49", "#9984D4", "#20A39E", "#EF5B5B", "#23001E"], ~size=2) == [
  ["#FFBA49", "#9984D4"],
  ["#20A39E", "#EF5B5B"],
  ]
  ```
 */
let chunksOf: (t<'a>, ~size: int) => t<t<'a>>

/** Provides a sliding 'window' of sub-arrays over an array.

  The first sub-array starts at index `0` of the array and takes the first `size` elements.

  The sub-array then advances the index `step` (which defaults to 1) positions before taking the next `size` elements.

  The sub-arrays are guaranteed to always be of length `size` and iteration stops once a sub-array would extend beyond the end of the array.

  ## Examples

  ```rescript
  Array.sliding([1, 2, 3, 4, 5], ~size=1) == [[1], [2], [3], [4], [5]]
  Array.sliding([1, 2, 3, 4, 5], ~size=2) == [[1, 2], [2, 3], [3, 4], [4, 5]]
  Array.sliding([1, 2, 3, 4, 5], ~size=3) == [[1, 2, 3], [2, 3, 4], [3, 4, 5]]
  Array.sliding([1, 2, 3, 4, 5], ~size=2, ~step=2) == [[1, 2], [3, 4]]
  Array.sliding([1, 2, 3, 4, 5], ~size=1, ~step=3) == [[1], [4]]
  ```
*/
let sliding: (~step: int=?, t<'a>, ~size: int) => t<t<'a>>

/** # Convert */
/** Converts an array of strings into a [String](String.mdx#), placing `sep` between each string in the result.

  ## Examples

  ```rescript
  Array.join(["Ant", "Bat", "Cat"], ~sep=", ") == "Ant, Bat, Cat"
  ```
 */
let join: (t<string>, ~sep: string) => string

/** Collect elements where function `f` will produce the same key.

  Produces a map from `'key` to a [List](List.mdx#) of all elements which produce the same `'key`.

  ## Examples

  ```rescript
  let animals = ["Ant", "Bear", "Cat", "Dewgong"]
  Array.groupBy(animals, module(Int), ~f=String.length) ==
  Map.Int.fromArray([
  (3, list{"Cat", "Ant"}),
  (4, list{"Bear"}),
  (7, list{"Dewgong"})
  ])
  ```
*/
let groupBy: (
  t<'value>,
  TableclothComparator.s<'key, 'id>,
  ~f: 'value => 'key,
) => TableclothMap.t<'key, list<'value>, 'id>

/** Create a [List](List.mdx#) of elements from an array.

  ## Examples

  ```rescript
  Array.toList([1, 2, 3]) == list{1, 2, 3}
  Array.toList(Array.fromList(list{3, 5, 8})) == list{3, 5, 8}
  ```
*/
let toList: t<'a> => list<'a>

/** Create an indexed [List](List.mdx#) from an array. Each element of the array will be paired with its index as a [Tuple2](Tuple2.mdx#).

  ## Examples

  ```rescript
  Array.toIndexedList(["cat", "dog"]) == list{(0, "cat"), (1, "dog")}
  ```
*/
let toIndexedList: t<'a> => list<(int, 'a)>

/** Test two arrays for equality using the provided function to test pairs of elements. */
let equal: (t<'a>, t<'a>, ('a, 'a) => bool) => bool

/** Compare two arrays using the provided `f` function to compare pairs of elements.

  A shorter array is 'less' than a longer one.

  ## Examples

  ```rescript
  Array.compare([1, 2, 3], [1, 2, 3, 4], Int.compare) == -1
  Array.compare([1, 2, 3], [1, 2, 3], Int.compare) == 0
  Array.compare([1, 2, 5], [1, 2, 3], Int.compare) == 1
  ```
*/
let compare: (t<'a>, t<'a>, ('a, 'a) => int) => int
