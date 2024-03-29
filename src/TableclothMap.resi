@@ocaml.text(" ")

@@ocaml.text(" A [Map] represents a unique mapping from keys to values.

    [Map] is an immutable data structure which means operations like {!Map.add} and {!Map.remove} do not modify the data structure, but return a new map with the desired changes.

    Since maps of [int]s and [string]s are so common the specialized {!Map.Int} and {!Map.String} modules are available, which offer a convenient way to construct new maps.

    Custom data types can be used with maps as long as the module satisfies the {!Comparator.S} interface.

    {[
      module Point = {
        type t = (int, int)
        let compare = Tuple2.compare(~f=Int.compare, ~g=Int.compare)
        include Comparator.Make({
          type t = t
          let compare = compare
        })
      }

      type animal =
        | Cow
        | Pig
        | Alpacca

      let pointToAnimal = Map.fromArray(
        module(Point),
        [((0, 0), Alpacca), ((3, 4), Cow), ((6, 7), Pig)],
      )
     ]}

    See the {!Comparator} module for a more details.
")

type t<'key, 'value, 'cmp> = Belt.Map.t<'key, 'value, 'cmp>

@@ocaml.text(" {1 Create}

    You can create sets of modules types which conform to the {!Comparator.S} signature by using {!empty}, {!singleton}, {!fromList} or {!fromArray}.

    Specialised versions of the {!empty}, {!singleton}, {!fromList} and {!fromArray} functions available in the {!Set.Int} and {!Set.String} sub-modules.
")

@ocaml.doc(" A map with nothing in it.

    Often used as an intial value for functions like {!Array.fold}.

    {2 Examples}

    {[
      Array.fold([\"Pear\", \"Orange\", \"Grapefruit\"], ~initial=Map.empty(module(Int)), ~f=(
        lengthToFruit,
        fruit,
      ) => Map.add(lengthToFruit, ~key=String.length(fruit), ~value=fruit))->Map.toArray ==
      [(4, \"Pear\"), (6, \"Orange\"), (10, \"Grapefruit\")]
     ]}

    In this particular case you might want to use {!Array.groupBy}
")
let empty: TableclothComparator.s<'key, 'identity> => t<'key, 'value, 'identity>

@ocaml.doc(" Create a map from a key and value.

    {2 Examples}

    {[
      Map.singleton(module(Int), ~key=1, ~value=\"Ant\")->Map.toArray == [(1, \"Ant\")]
    ]}
")
let singleton: (
  TableclothComparator.s<'key, 'identity>,
  ~key: 'key,
  ~value: 'value,
) => t<'key, 'value, 'identity>

@ocaml.doc(" Create a map from an {!Array} of key-value tuples. ")
let fromArray: (
  TableclothComparator.s<'key, 'identity>,
  array<('key, 'value)>,
) => t<'key, 'value, 'identity>

@ocaml.doc(" Create a map of a {!List} of key-value tuples. ")
let fromList: (
  TableclothComparator.s<'key, 'identity>,
  list<('key, 'value)>,
) => t<'key, 'value, 'identity>

@@ocaml.text(" {1 Basic operations} ")

@ocaml.doc(" Adds a new entry to a map. If [key] is allready present, its previous value is replaced with [value].

    {2 Examples}

    {[
      Map.add(
        Map.Int.fromArray([(1, \"Ant\"), (2, \"Bat\")]),
        ~key=3,
        ~value=\"Cat\",
      )->Map.toArray == [(1, \"Ant\"), (2, \"Bat\"), (3, \"Cat\")]

      Map.add(
        Map.Int.fromArray([(1, \"Ant\"), (2, \"Bat\")]),
        ~key=2,
        ~value=\"Bug\",
      )->Map.toArray == [(1, \"Ant\"), (2, \"Bug\")]
     ]}
")
let add: (t<'key, 'value, 'id>, ~key: 'key, ~value: 'value) => t<'key, 'value, 'id>

@ocaml.doc(" Removes a key-value pair from a map based on they provided key.

    {2 Examples}

    {[
      let animalPopulations = Map.String.fromArray([
        (\"Elephant\", 3_156),
        (\"Mosquito\", 56_123_156),
        (\"Rhino\", 3),
        (\"Shrew\", 56_423),
      ])
      Map.remove(animalPopulations, \"Mosquito\")->Map.toArray
      == [(\"Elephant\", 3_156), (\"Rhino\", 3), (\"Shrew\", 56_423)]
     ]}
")
let remove: (t<'key, 'value, 'id>, 'key) => t<'key, 'value, 'id>

@ocaml.doc(" Get the value associated with a key. If the key is not present in the map, returns [None].

    {2 Examples}

    {[
      let animalPopulations = Map.String.fromArray([
        (\"Elephant\", 3_156),
        (\"Mosquito\", 56_123_156),
        (\"Rhino\", 3),
        (\"Shrew\", 56_423),
      ])
      Map.get(animalPopulations, \"Shrew\") == Some(56_423)
     ]}
")
let get: (t<'key, 'value, 'id>, 'key) => option<'value>

@ocaml.doc(" Update the value for a specific key using [f]. If [key] is not present in the map [f] will be called with [None].

    {2 Examples}

    {[
      let animalPopulations = Map.String.fromArray([
        (\"Elephant\", 3_156),
        (\"Mosquito\", 56_123_156),
        (\"Rhino\", 3),
        (\"Shrew\", 56_423),
      ])

      Map.update(animalPopulations, ~key=\"Hedgehog\", ~f=population =>
        switch population {
        | None => Some(1)
        | Some(count) => Some(count + 1)
        }
      )->Map.toArray ==
        [
          (\"Elephant\", 3_156),
          (\"Hedgehog\", 1),
          (\"Mosquito\", 56_123_156),
          (\"Rhino\", 3),
          (\"Shrew\", 56_423),
        ]
     ]}
")
let update: (
  t<'key, 'value, 'id>,
  ~key: 'key,
  ~f: option<'value> => option<'value>,
) => t<'key, 'value, 'id>

@@ocaml.text(" {1 Query} ")

@ocaml.doc(" Determine if a map is empty. ")
let isEmpty: t<_, _, _> => bool

@ocaml.doc(" Returns the number of key-value pairs present in the map.

    {2 Examples}

    {[
      Map.Int.fromArray([(1, \"Hornet\"), (3, \"Marmot\")])->Map.length == 2
     ]}
")
let length: t<_, _, _> => int

@ocaml.doc(" Determine if [f] returns [true] for [any] values in a map. ")
let any: (t<_, 'value, _>, ~f: 'value => bool) => bool

@ocaml.doc(" Determine if [f] returns [true] for [all] values in a map. ")
let all: (t<_, 'value, _>, ~f: 'value => bool) => bool

@ocaml.doc(" Returns, as an {!Option} the first key-value pair for which [f] evaluates to [true].

    If [f] doesn't return [true] for any of the elements [find] will return [None].

    Searches starting from the smallest {b key}

    {2 Examples}

    {[
      Map.String.fromArray([
        (\"Elephant\", 3_156),
        (\"Mosquito\", 56_123_156),
        (\"Rhino\", 3),
        (\"Shrew\", 56_423),
      ])->Map.find(~f=(~key, ~value) => value > 10_000)
      == Some(\"Mosquito\", 56_123_156)
     ]}
")
let find: (t<'key, 'value, _>, ~f: (~key: 'key, ~value: 'value) => bool) => option<('key, 'value)>

@ocaml.doc(" Determine if a map includes [key].  ")
let includes: (t<'key, _, _>, 'key) => bool

@ocaml.doc(" Returns, as an {!Option}, the smallest {b key } in the map.

    Returns [None] if the map is empty.

    {2 Examples}

    {[
      Map.Int.fromArray([(8, \"Pigeon\"), (1, \"Hornet\"), (3, \"Marmot\")])
      ->Map.minimum == Some(1)
     ]}
")
let minimum: t<'key, _, _> => option<'key>

@ocaml.doc(" Returns the largest {b key } in the map.

    Returns [None] if the map is empty.

    {2 Examples}

    {[
      Map.Int.fromArray([(8, \"Pigeon\"), (1, \"Hornet\"), (3, \"Marmot\")])
      ->Map.maximum == Some(8)
     ]}
")
let maximum: t<'key, _, _> => option<'key>

@ocaml.doc(" Returns, as an {!Option}, a {!Tuple2} of the [(minimum, maximum)] {b key}s in the map.

    Returns [None] if the map is empty.

    {2 Examples}

    {[
      Map.Int.fromArray([(8, \"Pigeon\"), (1, \"Hornet\"), (3, \"Marmot\")])
      ->Map.extent == Some(1, 8)
     ]}
")
let extent: t<'key, _, _> => option<('key, 'key)>

@@ocaml.text(" {1 Combine} ")

@ocaml.doc(" Combine two maps.

    You provide a function [f] which is provided the key and the optional
    value from each map and needs to account for the three possibilities:

    - Only the 'left' map includes a value for the key.
    - Both maps contain a value for the key.
    - Only the 'right' map includes a value for the key.

    You then traverse all the keys, building up whatever you want.

    {2 Examples}

    {[
      let animalToPopulation = Map.String.fromArray([(\"Elephant\", 3_156), (\"Shrew\", 56_423)])

      let animalToPopulationGrowthRate = Map.String.fromArray([
        (\"Elephant\", 0.88),
        (\"Squirrel\", 1.2),
        (\"Python\", 4.0),
      ])

      Map.merge(animalToPopulation, animalToPopulationGrowthRate, ~f=(_animal, population, growth) =>
        switch Option.both(population, growth) {
        | Some(population, growth) => Some(Float.fromInt(population) *. growth)
        | None => None
        }
      )->Map.toArray
      == [(\"Elephant\", 2777.28)]
     ]}
")
let merge: (
  t<'key, 'v1, 'id>,
  t<'key, 'v2, 'id>,
  ~f: ('key, option<'v1>, option<'v2>) => option<'v3>,
) => t<'key, 'v3, 'id>

@@ocaml.text(" {1 Transform} ")

@ocaml.doc(" Apply a function to all values in a dictionary.

    {2 Examples}

    {[
      Map.String.fromArray([(\"Elephant\", 3_156), (\"Shrew\", 56_423)])
      ->Map.map(~f=Int.toString)
      ->Map.toArray == [(\"Elephant\", \"3156\"), (\"Shrew\", \"56423\")]
     ]}
")
let map: (t<'key, 'value, 'id>, ~f: 'value => 'b) => t<'key, 'b, 'id>

@ocaml.doc(" Like {!map} but [f] is also called with each values corresponding key. ")
let mapWithIndex: (t<'key, 'value, 'id>, ~f: ('key, 'value) => 'b) => t<'key, 'b, 'id>

@ocaml.doc(" Keep elements that [f] returns [true] for.

    {2 Examples}

    {[
      Map.String.fromArray([(\"Elephant\", 3_156), (\"Shrew\", 56_423)])
      ->Map.filter(~f=population => population > 10_000)
      ->Map.toArray
      == [(\"Shrew\", 56423)]
     ]}
")
let filter: (t<'key, 'value, 'id>, ~f: 'value => bool) => t<'key, 'value, 'id>

@ocaml.doc(" Mombine {!map} and {!filter} into a single pass.

    The output list only contains elements for which [f] returns [Some].
")
let filterMap: (
  t<'key, 'value, 'id>,
  ~f: (~key: 'key, ~value: 'value) => option<'b>,
) => t<'key, 'b, 'id>

@ocaml.doc(" Divide a map into two, the first map will contain the key-value pairs that [f] returns [true] for, pairs that [f] returns [false] for will end up in the second.

    {2 Examples}

    {[
      let (endangered, notEndangered) =
        Map.String.fromArray([
          (\"Elephant\", 3_156),
          (\"Mosquito\", 56_123_156),
          (\"Rhino\", 3),
          (\"Shrew\", 56_423),
        ])->Map.partition(~f=(~key as _, ~value as population) => population < 10_000)

      endangered->Map.toArray == [(\"Elephant\", 3_156), (\"Rhino\", 3)]

      notEndangered->Map.toArray == [(\"Mosquito\", 56_123_156), (\"Shrew\", 56_423)]
     ]}
")
let partition: (
  t<'key, 'value, 'id>,
  ~f: (~key: 'key, ~value: 'value) => bool,
) => (t<'key, 'value, 'id>, t<'key, 'value, 'id>)

@ocaml.doc(" Like {!Array.fold} but [f] is also called with both the [key] and [value]. ")
let fold: (t<'key, 'value, _>, ~initial: 'a, ~f: ('a, ~key: 'key, ~value: 'value) => 'a) => 'a

@@ocaml.text(" {1 Iterate} ")

@ocaml.doc(" Runs a function [f] against each {b value} in the map. ")
let forEach: (t<_, 'value, _>, ~f: 'value => unit) => unit

@ocaml.doc(" Like {!Map.forEach} except [~f] is also called with the corresponding key. ")
let forEachWithIndex: (t<'key, 'value, _>, ~f: (~key: 'key, ~value: 'value) => unit) => unit

@@ocaml.text(" {1 Convert} ")

@ocaml.doc(" Get a {!List} of all of the keys in a map.

    {2 Examples}

    {[
      Map.String.fromArray([
        (\"Elephant\", 3_156),
        (\"Mosquito\", 56_123_156),
        (\"Rhino\", 3),
        (\"Shrew\", 56_423),
      ])->Map.keys
      == list{\"Elephant\", \"Mosquito\", \"Rhino\", \"Shrew\"}
     ]}
")
let keys: t<'key, _, _> => list<'key>

@ocaml.doc(" Get a {!List} of all of the values in a map.

    {2 Examples}

    {[
      Map.String.fromArray([
        (\"Elephant\", 3_156),
        (\"Mosquito\", 56_123_156),
        (\"Rhino\", 3),
        (\"Shrew\", 56_423),
      ])->Map.values
      == list{3_156, 56_123_156, 3, 56_423}
     ]}
")
let values: t<_, 'value, _> => list<'value>

@ocaml.doc(" Get an {!Array} of all of the key-value pairs in a map. ")
let toArray: t<'key, 'value, _> => array<('key, 'value)>

@ocaml.doc(" Get a {!List} of all of the key-value pairs in a map. ")
let toList: t<'key, 'value, _> => list<('key, 'value)>

@ocaml.doc(
  " Construct a Map which can be keyed by any data type using the polymorphic [compare] function. "
)
module Poly: {
  type identity

  type t<'key, 'value> = t<'key, 'value, identity>

  @ocaml.doc(" A map with nothing in it. ")
  let empty: unit => t<'key, 'value>

  @ocaml.doc(" Create a map from a key and value.

      {2 Examples}

      {[
      Map.Poly.singleton(~key=false, ~value=1)->Map.toArray == [(false, 1)]
    ]}
  ")
  let singleton: (~key: 'key, ~value: 'value) => t<'key, 'value>

  @ocaml.doc(" Create a map from an {!Array} of key-value tuples. ")
  let fromArray: array<('key, 'value)> => t<'key, 'value>

  @ocaml.doc(" Create a map from a {!List} of key-value tuples. ")
  let fromList: list<('key, 'value)> => t<'key, 'value>
}

@ocaml.doc(" Construct a Map with {!Int}s for keys. ")
module Int: {
  type identity

  type t<'value> = t<TableclothInt.t, 'value, identity>

  @ocaml.doc(" A map with nothing in it. ")
  let empty: t<'value>

  @ocaml.doc(" Create a map from a key and value.

      {2 Examples}

      {[
      Map.Int.singleton(~key=1, ~value=\"Ant\")->Map.toArray == [(1, \"Ant\")]
    ]}
  ")
  let singleton: (~key: int, ~value: 'value) => t<'value>

  @ocaml.doc(" Create a map from an {!Array} of key-value tuples. ")
  let fromArray: array<(int, 'value)> => t<'value>

  @ocaml.doc(" Create a map of a {!List} of key-value tuples. ")
  let fromList: list<(int, 'value)> => t<'value>
}

@ocaml.doc(" Construct a Map with {!String}s for keys. ")
module String: {
  type identity

  type t<'value> = t<TableclothString.t, 'value, identity>

  @ocaml.doc(" A map with nothing in it. ")
  let empty: t<'value>

  @ocaml.doc(" Create a map from a key and value.

      {2 Examples}

      {[
      Map.String.singleton(~key=\"Ant\", ~value=1)->Map.toArray == [(\"Ant\", 1)]
    ]}
  ")
  let singleton: (~key: string, ~value: 'value) => t<'value>

  @ocaml.doc(" Create a map from an {!Array} of key-value tuples. ")
  let fromArray: array<(string, 'value)> => t<'value>

  @ocaml.doc(" Create a map from a {!List} of key-value tuples. ")
  let fromList: list<(string, 'value)> => t<'value>
}
