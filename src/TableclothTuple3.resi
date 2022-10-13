@@ocaml.text(" ")

@@ocaml.text(" Functions for manipulating trios of values ")

type t<'a, 'b, 'c> = ('a, 'b, 'c)

@@ocaml.text(" {1 Create} ")

@ocaml.doc(" Create a {!Tuple3}.

    {2 Examples}

    {[
      Tuple3.make(3, \"cat\", false) == (3, \"cat\", false)
      
      Array.map3(~f=Tuple3.make, [1, 2, 3], ['a', 'b', 'c'], [4., 5., 6.])
      == [
          (1, 'a', 4.),
          (2, 'b', 5.),
          (3, 'c', 6.),
      ]
   ]}
")
let make: ('a, 'b, 'c) => ('a, 'b, 'c)

@ocaml.doc(" Create a tuple from the first two elements of an {!Array}.

    If the array is longer than three elements, the extra elements are ignored.

    If the array is less than three elements, returns [None]

    {2 Examples}

    {[
      Tuple3.fromArray([1, 2, 3]) == Some(1, 2, 3)
      Tuple3.fromArray([1, 2]) == None
      Tuple3.fromArray([4, 5, 6, 7]) == Some(4, 5, 6)
    ]}
")
let fromArray: array<'a> => option<('a, 'a, 'a)>

@ocaml.doc(" Create a tuple from the first two elements of a {!List}.

    If the list is longer than two elements, the extra elements are ignored.

    If the list is less than two elements, returns [None]

    {2 Examples}

    {[
      Tuple3.fromList(list{1, 2, 3}) == Some(1, 2, 3)
      Tuple3.fromList(list{1, 2}) == None
      Tuple3.fromList(list{4, 5, 6, 7}) == Some(4, 5, 6)
    ]}
")
let fromList: list<'a> => option<('a, 'a, 'a)>

@ocaml.doc(" Extract the first value from a tuple.

    {2 Examples}

    {[
      Tuple3.first((3, 4, 5)) == 3
      Tuple3.first((\"john\", \"danger\", \"doe\")) == \"john\"
    ]}
")
let first: (('a, 'b, 'c)) => 'a

@ocaml.doc(" Extract the second value from a tuple.

    {2 Examples}

    {[
      Tuple3.second((3, 4, 5)) == 4
      Tuple3.second((\"john\", \"danger\", \"doe\")) == \"danger\"
    ]}
")
let second: (('a, 'b, 'c)) => 'b

@ocaml.doc(" Extract the third value from a tuple.

    {2 Examples}

    {[
      Tuple3.third((3, 4, 5)) == 5
      Tuple3.third((\"john\", \"danger\", \"doe\")) == \"doe\"
    ]}
")
let third: (('a, 'b, 'c)) => 'c

@ocaml.doc(" Extract the first and second values of a {!Tuple3} as a {!Tuple2}.

    {2 Examples}

    {[
      Tuple3.initial((3, \"stressed\", false)) == (3, \"stressed\")
      Tuple3.initial((\"john\", 16, true)) == (\"john\", 16)
    ]}
")
let initial: (('a, 'b, 'c)) => ('a, 'b)

@ocaml.doc(" Extract the second and third values of a {!Tuple3} as a {!Tuple2}.

    {2 Examples}

    {[
      Tuple3.tail((3, \"stressed\", false)) == (\"stressed\", false)
      Tuple3.tail((\"john\", 16, true)) == (16, true)
    ]}
")
let tail: (('a, 'b, 'c)) => ('b, 'c)

@@ocaml.text(" {1 Modify} ")

@ocaml.doc(" Move each value in the tuple one position to the left, moving the value in the first position into the last position.

    {2 Examples}

    {[
      Tuple3.rotateLeft((3, 4, 5)) == (4, 5, 3)
      Tuple3.rotateLeft((\"was\", \"stressed\", \"then\")) == (\"stressed\", \"then\", \"was\")
    ]}
")
let rotateLeft: (('a, 'b, 'c)) => ('b, 'c, 'a)

@ocaml.doc(" Move each value in the tuple one position to the right, moving the value in the last position into the first position.

    {2 Examples}

    {[
      Tuple3.rotateRight((3, 4, 5)) == (5, 3, 4)
      Tuple3.rotateRight((\"was\", \"stressed\", \"then\")) == (\"then\", \"was\", \"stressed\")
    ]}
")
let rotateRight: (('a, 'b, 'c)) => ('c, 'a, 'b)

@ocaml.doc(" Transform the first value in a tuple.

    {2 Examples}

    {[
      Tuple3.mapFirst((\"stressed\", 16, false), ~f=String.reverse) == (\"desserts\", 16, false)
      Tuple3.mapFirst((\"stressed\", 16, false), ~f=String.length) == (8, 16, false)
    ]}
")
let mapFirst: (('a, 'b, 'c), ~f: 'a => 'x) => ('x, 'b, 'c)

@ocaml.doc(" Transform the second value in a tuple.

    {2 Examples}

    {[
      Tuple3.mapSecond((\"stressed\", 16., false), ~f=Float.squareRoot) == (\"stressed\", 4., false)
      Tuple3.mapSecond(~f=Int.negate, (\"stressed\", 16, false)) == (\"stressed\", -16, false)
    ]}
")
let mapSecond: (('a, 'b, 'c), ~f: 'b => 'y) => ('a, 'y, 'c)

@ocaml.doc(" Transform the third value in a tuple.

    {2 Examples}

    {[
      Tuple3.mapThird((\"stressed\", 16, false), ~f=Bool.not) == (\"stressed\", 16, true)
    ]}
")
let mapThird: (('a, 'b, 'c), ~f: 'c => 'z) => ('a, 'b, 'z)

@ocaml.doc(" Transform each value in a tuple by applying [f] to the {!first} value, [g] to the {!second} value and [h] to the {!third} value.

    {2 Examples}

    {[
      Tuple3.mapEach(
          (\"stressed\", 16., false)
          ~f=String.reverse, 
          ~g=Float.squareRoot, 
          ~h=Bool.not)
      == (\"desserts\", 4., true)
   ]}
")
let mapEach: (('a, 'b, 'c), ~f: 'a => 'x, ~g: 'b => 'y, ~h: 'c => 'z) => ('x, 'y, 'z)

@ocaml.doc(" Transform all the values of a tuple using the same function.

    [mapAll] can only be used on tuples which have the same type for each value.

    {2 Examples}

    {[
      Tuple3.mapAll((9., 16., 25.), ~f=Float.squareRoot) == (3., 4., 5.)
      Tuple3.mapAll((\"was\", \"stressed\", \"then\"), ~f=String.length) == (3, 8, 4)
    ]}
")
let mapAll: (('a, 'a, 'a), ~f: 'a => 'b) => ('b, 'b, 'b)

@ocaml.doc(" Turns a tuple into a {!List} of length three.

    This function can only be used on tuples which have the same type for each value.

    {2 Examples}

    {[
      Tuple3.toArray((3, 4, 5)) == [3, 4, 5]
      Tuple3.toArray((\"was\", \"stressed\", \"then\")) == [\"was\", \"stressed\", \"then\"]
    ]}
")
let toArray: (('a, 'a, 'a)) => array<'a>

@ocaml.doc(" Turns a tuple into a {!List} of length three.

    This function can only be used on tuples which have the same type for each value.

    {2 Examples}

    {[
      Tuple3.toList((3, 4, 5)) == list{3, 4, 5}
      Tuple3.toList((\"was\", \"stressed\", \"then\")) == list{\"was\", \"stressed\", \"then\"}
    ]}
")
let toList: (('a, 'a, 'a)) => list<'a>

@ocaml.doc(" Test two {!Tuple3}s for equality, using the provided functions to test the
    first, second and third components.

    {2 Examples}

    {[
      Tuple3.equal((1, \"Fox\", 'j'), (1, \"Fox\", 'k'), Int.equal, String.equal, Char.equal) == false
      Tuple3.equal((1, \"Fox\", 'j'), (2, \"Hen\", 'j'), Int.equal, String.equal, Char.equal) == false
    ]}
 ")
let equal: (
  t<'a, 'b, 'c>,
  t<'a, 'b, 'c>,
  ('a, 'a) => bool,
  ('b, 'b) => bool,
  ('c, 'c) => bool,
) => bool

@ocaml.doc(" Compare two {!Tuple3}s, using [f] to compare the first
    components then, if the first components are equal, the second components are compared with [g],
    then the third components are compared with [h].

    {2 Examples}

    {[
      Tuple3.compare((1, \"Fox\", 'j'), (1, \"Fox\", 'j'), ~f=Int.compare, ~g=String.compare, ~h=Char.compare) == 0
      Tuple3.compare((1, \"Fox\", 'j'), (1, \"Eel\", 'j'), ~f=Int.compare, ~g=String.compare, ~h=Char.compare) == 1
      Tuple3.compare((1, \"Fox\", 'j'), (2, \"Fox\", 'm'), ~f=Int.compare, ~g=String.compare, ~h=Char.compare) == -1
    ]}
 ")
let compare: (
  t<'a, 'b, 'c>,
  t<'a, 'b, 'c>,
  ~f: ('a, 'a) => int,
  ~g: ('b, 'b) => int,
  ~h: ('c, 'c) => int,
) => int

