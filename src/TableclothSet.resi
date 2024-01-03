/** A [Set](Set.mdx#) represents a collection of unique values.

  `Set` is an immutable data structure which means operations like [Set.add](Set.mdx#add) and [Set.remove](Set.mdx#remove) do not modify the data structure, but return a new set with the desired changes.

  Since sets of `int`s and `string`s are so common the specialised [Set.Int](Set.mdx#Int) and [Set.String](Set.mdx#String) modules are available which offer a convenient way to construct new sets.

  You can create a Set by providing a module conform to the [Comparator.S](Comparator.mdx#S) signature by using [empty](#empty), [singleton](#singleton), [fromList](#fromList) or [fromArray](#fromArray).

  Specialised versions of the [empty](#empty), [singleton](#singleton), [fromList](#fromList) and [fromArray](#fromArray) functions available in the [Set.Int](Set.mdx#Int) and [Set.String](Set.mdx#String) sub-modules.

  Custom data types can be used with sets as long as the module satisfies the [Comparator.S](Comparator.mdx#S) interface.

  ```rescript
  module Point = {
  type rec t = (int, int)
  let compare = Tuple2.compare(~f=Int.compare, ~g=Int.compare)
  include Comparator.Make({
    type t = t
    let compare = compare
  })
  }

  let points = Set.fromArray(module(Point), [(0, 0), (3, 4), (6, 7)])
  ```

  See the [Comparator](Comparator.mdx#) module for a more details.
*/
type t<'a, 'id> = Belt.Set.t<'a, 'id>

/** A set with nothing in it. 

  Often used as an initial value for functions like [Array.fold](Array.mdx#fold)

  ## Examples

  ```rescript
  Array.fold(
  ['m', 'i', 's', 's', 'i', 's', 's', 'i', 'p', 'p', 'i'],
  ~initial=Set.empty(module(Char)),
  ~f=Set.add,
  )->Set.toArray
  == ['i', 'm', 'p', 's']
  ```
*/
let empty: TableclothComparator.s<'a, 'identity> => t<'a, 'identity>

/** Create a set from a single [Int](#Int).

  ## Examples

  ```rescript
  Set.singleton(7, module(Int)) |> Set.toArray == [7]
  ```
*/
let singleton: ('a, TableclothComparator.s<'a, 'identity>) => t<'a, 'identity>

/** Create a set from an [Array](#Array).

  ## Examples

  ```rescript
  Set.fromArray(["Ant", "Bat", "Bat", "Goldfish"], module(String))->Set.toArray
   == ["Ant", "Bat", "Goldfish"]
  ```
*/
let fromArray: (array<'a>, TableclothComparator.s<'a, 'identity>) => t<'a, 'identity>

/** Create a set from a [List](List.mdx#).

  ## Examples

  ```rescript
  Set.fromList(list{'A', 'B', 'B', 'G'}, module(Char))->Set.toArray == ['A', 'B', 'G']
  ```
*/
let fromList: (list<'a>, TableclothComparator.s<'a, 'identity>) => t<'a, 'identity>

/** Insert a value into a set.

  ## Examples

  ```rescript
  Set.add(Set.Int.fromArray([1, 2]), 3) -> Set.toArray == [1, 2, 3]
  Set.add(Set.Int.fromArray([1, 2]), 2) -> Set.toArray == [1, 2]
  ```
*/
let add: (t<'a, 'id>, 'a) => t<'a, 'id>

/** Remove a value from a set, if the set doesn't contain the value anyway, returns the original set.

  ## Examples

  ```rescript
  Set.remove(Set.Int.fromArray([1, 2]), 2)->Set.toArray == [1]
  
   let originalSet = Set.Int.fromArray([1, 2])
  let newSet = Set.remove(originalSet, 3)
  originalSet == newSet
  ```
*/
let remove: (t<'a, 'id>, 'a) => t<'a, 'id>

/** Determine if a value is in a set.

  ## Examples

   ```rescript
  Set.includes(Set.String.fromArray(["Ant", "Bat", "Cat"]), "Bat") == true
  ```
*/
let includes: (t<'a, _>, 'a) => bool

/** Determine the number of elements in a set.

  ## Examples

  ```rescript
  Set.length(Set.Int.fromArray([1, 2, 3])) == 3
  ```
*/
let length: t<_, _> => int

/** Returns, as an [Option](Option.mdx#), the first element for which `f` evaluates to `true`.
  If `f` doesn't return `true` for any of the elements `find` will return `None`.

  ## Examples

  ```rescript
  Set.find(Set.Int.fromArray([1, 3, 4, 8]), ~f=Int.isEven) == Some(4)
  Set.find(Set.Int.fromArray([0, 2, 4, 8]), ~f=Int.isOdd) == None
  Set.find(Set.Int.empty, ~f=Int.isEven) == None
  ```
*/
let find: (t<'value, _>, ~f: 'value => bool) => option<'value>

/** Check if a set is empty.

  ## Examples

  ```rescript
  Set.isEmpty(Set.Int.empty) == true
  Set.isEmpty(Set.Int.singleton(4)) == false
  ```
*/
let isEmpty: t<_, _> => bool

/** Determine if `f` returns true for `any` values in a set.

  ## Examples

  ```rescript
  Set.any(Set.Int.fromArray([2, 3]), ~f=Int.isEven) == true
  Set.any(Set.Int.fromArray([1, 3]), ~f=Int.isEven) == false
  Set.any(Set.Int.fromArray([]), ~f=Int.isEven) == false
  ```
*/
let any: (t<'value, _>, ~f: 'value => bool) => bool

/** Determine if `f` returns true for `all` values in a set.

  ## Examples

  ```rescript
  Set.all(Set.Int.fromArray([2, 4]), ~f=Int.isEven) == true
  Set.all(Set.Int.fromArray([2, 3]), ~f=Int.isEven) == false
  Set.all(Set.Int.empty, ~f=Int.isEven) == true
  ```
*/
let all: (t<'value, _>, ~f: 'value => bool) => bool

/** Returns a new set with the values from the first set which are not in the second set.

  ## Examples

  ```rescript
  Set.difference(
  Set.Int.fromArray([1, 2, 5]),
  Set.Int.fromArray([2, 3, 4])
  )->Set.toArray == [1, 5]

  Set.difference(
  Set.Int.fromArray([2, 3, 4]), 
  Set.Int.fromArray([1, 2, 5])
  )->Set.toArray == [3, 4]
  ```
*/
let difference: (t<'a, 'id>, t<'a, 'id>) => t<'a, 'id>

/** Get the intersection of two sets. Keeps values that appear in both sets.

  ## Examples

  ```rescript
  Set.intersection(
  Set.Int.fromArray([1, 2, 5]),
  Set.Int.fromArray([2, 3, 4])
  )->Set.toArray == [2]
  ```
*/
let intersection: (t<'a, 'id>, t<'a, 'id>) => t<'a, 'id>

/** Get the union of two sets. Keep all values.

  ## Examples

  ```rescript
  Set.union(
  Set.Int.fromArray([1, 2, 5]), 
  Set.Int.fromArray([2, 3, 4])
  )->Set.toArray == [1, 2, 3, 4, 5]
  ```
*/
let union: (t<'a, 'id>, t<'a, 'id>) => t<'a, 'id>

/** Keep elements that `f` returns `true` for.

  ## Examples

  ```rescript
  Set.filter(Set.Int.fromArray([1, 2, 3]), ~f=Int.isEven)->Set.toArray == [2]
  ```
*/
let filter: (t<'a, 'id>, ~f: 'a => bool) => t<'a, 'id>

/** Divide a set into two according to `f`. The first set will contain the values
  that `f` returns `true` for, values that `f` returns `false` for will end up in the second.

  ## Examples

  ```rescript
  let numbers = Set.Int.fromArray([1, 1, 5, 6, 5, 7, 9, 8])
  let (evens, odds) = Set.partition(numbers, ~f=Int.isEven)
  Set.toArray(evens) == [6, 8]
  Set.toArray(odds) == [1, 5, 7, 9]
  ```
*/
let partition: (t<'a, 'id>, ~f: 'a => bool) => (t<'a, 'id>, t<'a, 'id>)

/** Transform a set into a value which is result of running each element in the set through `f`,
  where each successive invocation is supplied the return value of the previous.

  See [Array.fold](Array.mdx#fold) for a more in-depth explanation.

  ## Examples

  ```rescript
  Set.fold(Set.Int.fromArray([1, 2, 3, 4], ~initial=1, ~f=Int.multiply)) == 24
  ```
*/
let fold: (t<'a, _>, ~initial: 'b, ~f: ('b, 'a) => 'b) => 'b

/** Runs a function `f` against each element of the set. */
let forEach: (t<'a, _>, ~f: 'a => unit) => unit

/** Converts a set into an [Array](#Array) */
let toArray: t<'a, _> => array<'a>

/** Converts a set into a [List](List.mdx#). */
let toList: t<'a, _> => list<'a>

/** Construct sets which can hold any data type using the polymorphic `compare` function. */
module Poly: {
  type identity

  type t<'a> = t<'a, identity>

  /** The empty set.

  A great starting point.
   */
  let empty: unit => t<'a>

  /** Create a set of a single value

  ## Examples

  ```rescript
  Set.Poly.singleton((5, "Emu"))->Set.toArray == [(5, "Emu")]
  ```
  */
  let singleton: 'a => t<'a>

  /** Create a set from an [Array](#Array)

  ## Examples

  ```rescript
  Set.Poly.fromArray([(1, "Ant"), (2, "Bat"), (2, "Bat")])->Set.toArray
  == [(1, "Ant"), (2, "Bat")]
  ```
  */
  let fromArray: array<'a> => t<'a>

  /** Create a set from a [List](List.mdx#)

  ## Examples

  ```rescript
  Set.Poly.fromList(list{(1, "Ant"), (2, "Bat"), (2, "Bat")})->Set.toArray
  == [(1, "Ant"), (2, "Bat")]
  ```
  */
  let fromList: list<'a> => t<'a>
}

/** Construct sets of [Int](#Int)s */
module Int: {
  type identity

  type t = t<TableclothInt.t, identity>

  /** A set with nothing in it. */
  let empty: t

  /** Create a set from a single [Int](#Int)

  ## Examples

  ```rescript
  Set.Int.singleton(5)->Set.toArray == [5]
  ```
  */
  let singleton: int => t

  /** Create a set from an [Array](Array.mdx#)

  ## Examples

  ```rescript
  Set.Int.fromArray([1, 2, 3, 3, 2, 1, 7])->Set.toArray == [1, 2, 3, 7]
  ```
  */
  let fromArray: array<int> => t

  /** Create a set from a [List](List.mdx#)

  ## Examples

  ```rescript
  Set.Int.fromList(list{1, 2, 3, 3, 2, 1, 7})->Set.toArray == [1, 2, 3, 7]
  ```
  */
  let fromList: list<int> => t
}

/** Construct sets of [String](String.mdx#)s */
module String: {
  type identity

  type t = t<TableclothString.t, identity>

  /** A set with nothing in it. */
  let empty: t

  /** Create a set of a single [String](String.mdx#).

  ## Examples

  ```rescript
  Set.String.singleton("Bat")->Set.toArray == ["Bat"]
  ```
  */
  let singleton: string => t

  /** Create a set from an [Array](Array.mdx#).

  ## Examples

  ```rescript
  Set.String.fromArray(["a", "b", "g", "b", "g", "a", "a"])->Set.toArray == ["a", "b", "g"]
  ```
  */
  let fromArray: array<string> => t

  /** Create a set from a [List](List.mdx#).

  ## Examples

  ```rescript
  Set.String.fromList(["a", "b", "g", "b", "g", "a", "a"])->Set.toArray == ["a", "b", "g"]
  ```
  */
  let fromList: list<string> => t
}
