@@ocaml.text(" ")

@@ocaml.text(" A {!Set} represents a collection of unique values.

    [Set] is an immutable data structure which means operations like {!Set.add} and {!Set.remove} do not modify the data structure, but return a new set with the desired changes.

    Since sets of [int]s and [string]s are so common the specialised {!Set.Int} and {!Set.String} modules are available which offer a convenient way to construct new sets.

    Custom data types can be used with sets as long as the module satisfies the {!Comparator.S} interface.

    {[
      module Point = {
        type rec t = (int, int)
        let compare = Tuple2.compare(~f=Int.compare, ~g=Int.compare)
        include Comparator.Make({
          type t = t
          let compare = compare
        })
      }

      let points = Set.fromArray(module(Point), [(0, 0), (3, 4), (6, 7)])
    ]}

    See the {!Comparator} module for a more details.
")

type t<'a, 'id> = Belt.Set.t<'a, 'id>

@@ocaml.text(" {1 Create}  
  
  You can create a Set by providing a module conform to the {!Comparator.S} signature by using {!empty}, {!singleton}, {!fromList} or {!fromArray}.

  Specialised versions of the {!empty}, {!singleton}, {!fromList} and {!fromArray} functions available in the {!Set.Int} and {!Set.String} sub-modules.
")

@ocaml.doc(" A set with nothing in it. 

    Often used as an initial value for functions like {!Array.fold}

    {2 Examples}

    {[
      Array.fold(
        ['m', 'i', 's', 's', 'i', 's', 's', 'i', 'p', 'p', 'i'],
        ~initial=Set.empty(module(Char)),
        ~f=Set.add,
      )->Set.toArray
      == ['i', 'm', 'p', 's']
    ]}
")
let empty: TableclothComparator.s<'a, 'identity> => t<'a, 'identity>

@ocaml.doc(" Create a set from a single {!Int}.

  {2 Examples}

  {[
      Set.singleton(7, module(Int)) |> Set.toArray == [7]
    ]}
")
let singleton: ('a, TableclothComparator.s<'a, 'identity>) => t<'a, 'identity>

@ocaml.doc(" Create a set from an {!Array}.

    {2 Examples}

    {[
      Set.fromArray([\"Ant\", \"Bat\", \"Bat\", \"Goldfish\"], module(String))->Set.toArray
       == [\"Ant\", \"Bat\", \"Goldfish\"]
    ]}
")
let fromArray: (array<'a>, TableclothComparator.s<'a, 'identity>) => t<'a, 'identity>

@ocaml.doc(" Create a set from a {!List}.

    {2 Examples}

    {[
      Set.fromList(list{'A', 'B', 'B', 'G'}, module(Char))->Set.toArray == ['A', 'B', 'G']
    ]}
")
let fromList: (list<'a>, TableclothComparator.s<'a, 'identity>) => t<'a, 'identity>

@@ocaml.text(" {1 Basic operations} ")

@ocaml.doc(" Insert a value into a set.

    {2 Examples}

    {[
      Set.add(Set.Int.fromArray([1, 2]), 3) -> Set.toArray == [1, 2, 3]
      Set.add(Set.Int.fromArray([1, 2]), 2) -> Set.toArray == [1, 2]
    ]}
")
let add: (t<'a, 'id>, 'a) => t<'a, 'id>

@ocaml.doc(" Remove a value from a set, if the set doesn't contain the value anyway, returns the original set.

    {2 Examples}

    {[
      Set.remove(Set.Int.fromArray([1, 2]), 2)->Set.toArray == [1]
      
     let originalSet = Set.Int.fromArray([1, 2])
      let newSet = Set.remove(originalSet, 3)
      originalSet == newSet
    ]}
")
let remove: (t<'a, 'id>, 'a) => t<'a, 'id>

@ocaml.doc(" Determine if a value is in a set.

    {2 Examples}

   {[
      Set.includes(Set.String.fromArray([\"Ant\", \"Bat\", \"Cat\"]), \"Bat\") == true
    ]}
")
let includes: (t<'a, _>, 'a) => bool

@ocaml.doc(" Determine the number of elements in a set.

    {2 Examples}

    {[
      Set.length(Set.Int.fromArray([1, 2, 3])) == 3
    ]}
")
let length: t<_, _> => int

@ocaml.doc(" Returns, as an {!Option}, the first element for which [f] evaluates to [true].
    If [f] doesn't return [true] for any of the elements [find] will return [None].

    {2 Examples}

    {[
      Set.find(Set.Int.fromArray([1, 3, 4, 8]), ~f=Int.isEven) == Some(4)
      Set.find(Set.Int.fromArray([0, 2, 4, 8]), ~f=Int.isOdd) == None
      Set.find(Set.Int.empty, ~f=Int.isEven) == None
    ]}
")
let find: (t<'value, _>, ~f: 'value => bool) => option<'value>

@@ocaml.text(" {1 Query} ")

@ocaml.doc(" Check if a set is empty.

    {2 Examples}

    {[
      Set.isEmpty(Set.Int.empty) == true
      Set.isEmpty(Set.Int.singleton(4)) == false
    ]}
")
let isEmpty: t<_, _> => bool

@ocaml.doc(" Determine if [f] returns true for [any] values in a set.

    {2 Examples}

    {[
      Set.any(Set.Int.fromArray([2, 3]), ~f=Int.isEven) == true
      Set.any(Set.Int.fromArray([1, 3]), ~f=Int.isEven) == false
      Set.any(Set.Int.fromArray([]), ~f=Int.isEven) == false
    ]}
")
let any: (t<'value, _>, ~f: 'value => bool) => bool

@ocaml.doc(" Determine if [f] returns true for [all] values in a set.

    {2 Examples}

    {[
      Set.all(Set.Int.fromArray([2, 4]), ~f=Int.isEven) == true
      Set.all(Set.Int.fromArray([2, 3]), ~f=Int.isEven) == false
      Set.all(Set.Int.empty, ~f=Int.isEven) == true
    ]}
")
let all: (t<'value, _>, ~f: 'value => bool) => bool

@@ocaml.text(" {1 Combine} ")

@ocaml.doc(" Returns a new set with the values from the first set which are not in the second set.

    {2 Examples}

    {[
      Set.difference(
        Set.Int.fromArray([1, 2, 5]),
        Set.Int.fromArray([2, 3, 4])
      )->Set.toArray == [1, 5]

      Set.difference(
        Set.Int.fromArray([2, 3, 4]), 
        Set.Int.fromArray([1, 2, 5])
      )->Set.toArray == [3, 4]
    ]}
")
let difference: (t<'a, 'id>, t<'a, 'id>) => t<'a, 'id>

@ocaml.doc(" Get the intersection of two sets. Keeps values that appear in both sets.

    {2 Examples}

    {[
      Set.intersection(
        Set.Int.fromArray([1, 2, 5]),
        Set.Int.fromArray([2, 3, 4])
      )->Set.toArray == [2]
    ]}
")
let intersection: (t<'a, 'id>, t<'a, 'id>) => t<'a, 'id>

@ocaml.doc(" Get the union of two sets. Keep all values.

    {2 Examples}

    {[
      Set.union(
        Set.Int.fromArray([1, 2, 5]), 
        Set.Int.fromArray([2, 3, 4])
      )->Set.toArray == [1, 2, 3, 4, 5]
    ]}
")
let union: (t<'a, 'id>, t<'a, 'id>) => t<'a, 'id>

@@ocaml.text(" {1 Transform} ")

@ocaml.doc(" Keep elements that [f] returns [true] for.

    {2 Examples}

    {[
      Set.filter(Set.Int.fromArray([1, 2, 3]), ~f=Int.isEven)->Set.toArray == [2]
    ]}
")
let filter: (t<'a, 'id>, ~f: 'a => bool) => t<'a, 'id>

@ocaml.doc(" Divide a set into two according to [f]. The first set will contain the values
    that [f] returns [true] for, values that [f] returns [false] for will end up in the second.

    {2 Examples}

    {[
      let numbers = Set.Int.fromArray([1, 1, 5, 6, 5, 7, 9, 8])
      let (evens, odds) = Set.partition(numbers, ~f=Int.isEven)
      Set.toArray(evens) == [6, 8]
      Set.toArray(odds) == [1, 5, 7, 9]
    ]}
")
let partition: (t<'a, 'id>, ~f: 'a => bool) => (t<'a, 'id>, t<'a, 'id>)

@ocaml.doc(" Transform a set into a value which is result of running each element in the set through [f],
    where each successive invocation is supplied the return value of the previous.

  See {!Array.fold} for a more in-depth explanation.

  {2 Examples}

  {[
      Set.fold(Set.Int.fromArray([1, 2, 3, 4], ~initial=1, ~f=Int.multiply)) == 24
    ]}
")
let fold: (t<'a, _>, ~initial: 'b, ~f: ('b, 'a) => 'b) => 'b

@ocaml.doc(" Runs a function [f] against each element of the set. ")
let forEach: (t<'a, _>, ~f: 'a => unit) => unit

@@ocaml.text(" {1 Convert} ")

@ocaml.doc(" Converts a set into an {!Array} ")
let toArray: t<'a, _> => array<'a>

@ocaml.doc(" Converts a set into a {!List}. ")
let toList: t<'a, _> => list<'a>

@ocaml.doc(
  " Construct sets which can hold any data type using the polymorphic [compare] function. "
)
module Poly: {
  type identity

  type t<'a> = t<'a, identity>

  @ocaml.doc(" The empty set.

      A great starting point.
   ")
  let empty: unit => t<'a>

  @ocaml.doc(" Create a set of a single value

      {2 Examples}

      {[
      Set.Poly.singleton((5, \"Emu\"))->Set.toArray == [(5, \"Emu\")]
    ]}
  ")
  let singleton: 'a => t<'a>

  @ocaml.doc(" Create a set from an {!Array}

      {2 Examples}

      {[
        Set.Poly.fromArray([(1, \"Ant\"), (2, \"Bat\"), (2, \"Bat\")])->Set.toArray
        == [(1, \"Ant\"), (2, \"Bat\")]
      ]}
  ")
  let fromArray: array<'a> => t<'a>

  @ocaml.doc(" Create a set from a {!List}

    {2 Examples}

    {[
      Set.Poly.fromList(list{(1, \"Ant\"), (2, \"Bat\"), (2, \"Bat\")})->Set.toArray
      == [(1, \"Ant\"), (2, \"Bat\")]
    ]}
  ")
  let fromList: list<'a> => t<'a>
}

@ocaml.doc(" Construct sets of {!Int}s ")
module Int: {
  type identity

  type t = t<TableclothInt.t, identity>

  @ocaml.doc(" A set with nothing in it. ")
  let empty: t

  @ocaml.doc(" Create a set from a single {!Int}

    {2 Examples}

    {[
      Set.Int.singleton(5)->Set.toArray == [5]
    ]}
  ")
  let singleton: int => t

  @ocaml.doc(" Create a set from an {!Array}

      {2 Examples}

      {[
      Set.Int.fromArray([1, 2, 3, 3, 2, 1, 7])->Set.toArray == [1, 2, 3, 7]
    ]}
  ")
  let fromArray: array<int> => t

  @ocaml.doc(" Create a set from a {!List}

      {2 Examples}

      {[
      Set.Int.fromList(list{1, 2, 3, 3, 2, 1, 7})->Set.toArray == [1, 2, 3, 7]
    ]}
  ")
  let fromList: list<int> => t
}

@ocaml.doc(" Construct sets of {!String}s ")
module String: {
  type identity

  type t = t<TableclothString.t, identity>

  @ocaml.doc(" A set with nothing in it. ")
  let empty: t

  @ocaml.doc(" Create a set of a single {!String}.

      {2 Examples}

      {[
      Set.String.singleton(\"Bat\")->Set.toArray == [\"Bat\"]
    ]}
  ")
  let singleton: string => t

  @ocaml.doc(" Create a set from an {!Array}.

      {2 Examples}

      {[
      Set.String.fromArray([\"a\", \"b\", \"g\", \"b\", \"g\", \"a\", \"a\"])->Set.toArray == [\"a\", \"b\", \"g\"]
    ]}
  ")
  let fromArray: array<string> => t

  @ocaml.doc(" Create a set from a {!List}.

      {2 Examples}

      {[
      Set.String.fromList([\"a\", \"b\", \"g\", \"b\", \"g\", \"a\", \"a\"])->Set.toArray == [\"a\", \"b\", \"g\"]
    ]}
  ")
  let fromList: list<string> => t
}

