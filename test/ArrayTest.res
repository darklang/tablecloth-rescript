open Tablecloth
open Jest
open Expect

open Array
describe("singleton", () => {
  test("equals an array literal of the same value", () => expect(singleton(1234))->toEqual([1234]))
  test("has length one", () => expect(length(singleton(1)))->toEqual(1))
})
describe("length", () => {
  test("equals an array literal of the same value", () => expect(length([]))->toEqual(0))
  test("has length one", () => expect(length(['a']))->toEqual(1))
  test("has length two", () => expect(length(["a", "b"]))->toEqual(2))
})
describe("isEmpty", () => {
  test("returns true for empty array literals", () => expect(isEmpty([]))->toEqual(true))
  test("returns false for literals a non-zero number of elements", () =>
    expect(isEmpty([1234]))->toEqual(false)
  )
})
describe("initialize", () => {
  test("create empty array", () => expect(initialize(0, ~f=Fun.identity))->toEqual([]))
  test("negative length gives an empty array", () =>
    expect(initialize(-1, ~f=Fun.identity))->toEqual([])
  )
  test("create array initialize", () => expect(initialize(3, ~f=Fun.identity))->toEqual([0, 1, 2]))
})
describe("repeat", () => {
  test("length zero creates an empty array", () => expect(repeat(0, ~length=0))->toEqual([]))
  test("negative length gives an empty array", () => expect(repeat(~length=-1, 0))->toEqual([]))
  test("create array of ints", () => expect(repeat(0, ~length=3))->toEqual([0, 0, 0]))
  test("create array strings", () =>
    expect(repeat("cat", ~length=3))->toEqual(["cat", "cat", "cat"])
  )
})
describe("range", () => {
  test("returns an array of the integers from zero and upto but not including [to]", () =>
    expect(range(5))->toEqual([0, 1, 2, 3, 4])
  )
  test("returns an empty array [to] is zero", () => expect(range(0))->toEqual([]))
  test("takes an optional [from] argument to start create empty array", () =>
    expect(range(~from=2, 5))->toEqual([2, 3, 4])
  )
  test("can start from negative values", () =>
    expect(range(~from=-2, 3))->toEqual([-2, -1, 0, 1, 2])
  )
  test("returns an empty array [from] > [to_]", () => expect(range(~from=5, 0))->toEqual([]))
})
describe("fromList", () =>
  test("transforms a list into an array of the same elements", () =>
    expect(fromList(list{1, 2, 3}))->toEqual([1, 2, 3])
  )
)
describe("toList", () =>
  test("transform an array into a list of the same elements", () =>
    expect(toList([1, 2, 3]))->toEqual(list{1, 2, 3})
  )
)
describe("toIndexedList", () => {
  test("returns an empty list for an empty array", () => expect(toIndexedList([]))->toEqual(list{}))
  test("transforms an array into a list of tuples", () =>
    expect(toIndexedList(["cat", "dog"]))->toEqual(list{(0, "cat"), (1, "dog")})
  )
})

describe("clone", () => {
  test("returns an int array", () => {
    let numbers = [1, 2, 3]
    let otherNumbers = clone(numbers)
    numbers[1] = 9
    expect(otherNumbers)->toEqual([1, 2, 3])
  })
  test("returns an array of int arrays", () => {
    let numberGrid = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]

    let numberGridCopy = clone(numberGrid)

    numberGrid[1][1] = 0

    numberGridCopy[1][1] = 9
    expect(numberGridCopy)->toEqual([[1, 2, 3], [4, 9, 6], [7, 8, 9]])
  })
})

describe("get", () => {
  test("returns Some for an in-bounds index", () =>
    expect(["cat", "dog", "eel"][2])->toEqual("eel")
  )
  testAll("throws for an out of bounds index", list{-1, 3, 5}, index =>
    toThrow(expect(() => [0, 1, 2][index]))
  )
  test("throws for an empty array", () => toThrow(expect(() => [][0])))
})
describe("getAt", () => {
  test("returns Some for an in-bounds index", () =>
    expect(getAt(~index=2, ["cat", "dog", "eel"]))->toEqual(Some("eel"))
  )
  test("returns None for an out of bounds index", () =>
    expect(getAt(~index=5, [0, 1, 2]))->toEqual(None)
  )
  test("returns None for an empty array", () => expect(getAt(~index=0, []))->toEqual(None))
})
describe("set", () =>
  test("can set a value at an index", () => {
    let numbers = [1, 2, 3]
    numbers[0] = 0
    expect(numbers)->toEqual([0, 2, 3])
  })
)
describe("setAt", () => {
  test("can be partially applied to set an element", () => {
    let numbers = [1, 2, 3]
    setAt(~value=0, numbers, ~index=2)
    setAt(~value=0, numbers, ~index=1)
    expect(numbers)->toEqual([1, 0, 0])
  })
  test("can be partially applied to set an index", () => {
    let animals = ["ant", "bat", "cat"]
    setAt(~index=0, animals, ~value="antelope")
    expect(animals)->toEqual(["antelope", "bat", "cat"])
  })
})

describe("first", () => {
  test("return first element", () => expect(first([1, 2, 3]))->toEqual(Some(1)))
  test("return none from empty array", () => expect(first([]))->toEqual(None))
})

describe("last", () => {
  test("return last element", () => expect(last([1, 2, 3]))->toEqual(Some(3)))
  test("return none from empty array", () => expect(last([]))->toEqual(None))
})

describe("sum", () => {
  test("equals zero for an empty array", () => expect(sum([], module(Int)))->toEqual(0))
  test("adds up the elements on an integer array", () =>
    expect(sum([1, 2, 3], module(Int)))->toEqual(6)
  )
})
describe("filter", () =>
  test("keep elements that [f] returns [true] for", () =>
    expect(filter(~f=Int.isEven, [1, 2, 3, 4, 5, 6]))->toEqual([2, 4, 6])
  )
)

describe("filterMap", () =>
  test("keep elements that [f] returns [true] for", () =>
    expect(
      filterMap(
        [3, 4, 5, 6],
        ~f=number =>
          if Int.isEven(number) {
            Some(number * number)
          } else {
            None
          },
      ),
    )->toEqual([16, 36])
  )
)

describe("flatMap", () =>
  test("{!map} [f] onto an array and {!flatten} the resulting arrays", () =>
    expect(flatMap(~f=n => [n, n], [1, 2, 3]))->toEqual([1, 1, 2, 2, 3, 3])
  )
)

describe("swap", () =>
  test("switches values at the given indicies", () => {
    let numbers = [1, 2, 3]
    swap(numbers, 1, 2)
    expect(numbers)->toEqual([1, 3, 2])
  })
)
describe("map", () =>
  test("Apply a function [f] to every element in an array", () =>
    expect(map(~f=Float.squareRoot, [1.0, 4.0, 9.0]))->toEqual([1.0, 2.0, 3.0])
  )
)
describe("mapWithIndex", () =>
  test("equals an array literal of the same value", () =>
    expect(mapWithIndex(~f=\"*", [5, 5, 5]))->toEqual([0, 5, 10])
  )
)
describe("map2", () => {
  test("works the order of arguments to `f` is not important", () =>
    expect(map2(~f=\"+", [1, 2, 3], [4, 5, 6]))->toEqual([5, 7, 9])
  )
  test("works the order of `f` is important", () =>
    expect(map2(~f=Tuple2.make, ["alice", "bob", "chuck"], [2, 5, 7, 8]))->toEqual([
      ("alice", 2),
      ("bob", 5),
      ("chuck", 7),
    ])
  )
})

describe("map3", () =>
  test("maps elements of 3 arrays", () =>
    expect(
      map3(~f=Tuple3.make, ["alice", "bob", "chuck"], [2, 5, 7, 8], [true, false, true, false]),
    )->toEqual([("alice", 2, true), ("bob", 5, false), ("chuck", 7, true)])
  )
)

describe("partition", () =>
  test("Split an array into a Tuple of arrays", () =>
    expect(partition([1, 2, 3, 4, 5, 6], ~f=Int.isOdd))->toEqual(([1, 3, 5], [2, 4, 6]))
  )
)

describe("splitAt", () => {
  test("Divides an array into a Tuple of arrays", () =>
    expect(splitAt([1, 2, 3, 4, 5], ~index=2))->toEqual(([1, 2], [3, 4, 5]))
  )
  test("Split array at array[0]", () =>
    expect(splitAt([1, 2, 3, 4, 5], ~index=0))->toEqual(([], [1, 2, 3, 4, 5]))
  )
})

describe("splitWhen", () => {
  test("Divides an array at the first element f returns true for", () =>
    expect(splitWhen([5, 7, 8, 6, 4], ~f=Int.isEven))->toEqual(([5, 7], [8, 6, 4]))
  )
  test("Divides an array at the first element f returns true for", () =>
    expect(splitWhen([5, 7, 8, 7, 4], ~f=Int.isEven))->toEqual(([5, 7], [8, 7, 4]))
  )
  test("Divides an array at the first element f returns true for", () =>
    expect(splitWhen(["Ant", "Bat", "Cat"], ~f=animal => String.length(animal) > 3))->toEqual((
      ["Ant", "Bat", "Cat"],
      [],
    ))
  )
  test("Divides an array at the first element f returns true for", () =>
    expect(splitWhen([2., Float.pi, 1.111], ~f=Float.isInteger))->toEqual((
      [],
      [2., Float.pi, 1.111],
    ))
  )
})
describe("flatmap", () =>
  test("flatMap", () => {
    let duplicate = n => [n, n]
    expect(flatMap(~f=duplicate, [1, 2, 3]))->toEqual([1, 1, 2, 2, 3, 3])
  })
)
describe("sliding", () => {
  test("size 1", () =>
    expect(sliding([1, 2, 3, 4, 5], ~size=1))->toEqual([[1], [2], [3], [4], [5]])
  )
  test("size 2", () =>
    expect(sliding([1, 2, 3, 4, 5], ~size=2))->toEqual([[1, 2], [2, 3], [3, 4], [4, 5]])
  )
  test("step 3 ", () =>
    expect(sliding([1, 2, 3, 4, 5], ~size=3))->toEqual([[1, 2, 3], [2, 3, 4], [3, 4, 5]])
  )
  test("size 2, step 2", () =>
    expect(sliding([1, 2, 3, 4, 5], ~size=2, ~step=2))->toEqual([[1, 2], [3, 4]])
  )
  test("size 1, step 3", () =>
    expect(sliding([1, 2, 3, 4, 5], ~size=1, ~step=3))->toEqual([[1], [4]])
  )
  test("size 2, step 3", () =>
    expect(sliding([1, 2, 3, 4, 5], ~size=2, ~step=3))->toEqual([[1, 2], [4, 5]])
  )
  test("step 7", () => expect(sliding([1, 2, 3, 4, 5], ~size=7))->toEqual([]))
})

describe("join", () => {
  test(
    "Convert an array of strings into a single String, placing [sep] comma between each string in the result",
    () => expect(join(["Ant", "Bat", "Cat"], ~sep=", "))->toEqual("Ant, Bat, Cat"),
  )
  test("Convert an empty array of strings into a String, returns an empty string", () =>
    expect(join([], ~sep=", "))->toEqual("")
  )
})

describe("count", () => {
  test(
    "returns the number of elements in array of odd and even numbers that isEven returns true for, returns int 2",
    () => expect(count(~f=Int.isEven, [1, 3, 4, 8]))->toEqual(2),
  )
  test(
    "returns the number of elements in array of odd numbers that isEven returns true for, returns int 0",
    () => expect(count(~f=Int.isEven, [1, 3]))->toEqual(0),
  )
  test(
    "returns the number of elements in an empty array that isEven returns true for, returns int 0",
    () => expect(count(~f=Int.isEven, []))->toEqual(0),
  )
})

describe("find", () => {
  test("returns the first element which `f` returns true for", () =>
    expect(find(~f=Int.isEven, [1, 3, 4, 8]))->toEqual(Some(4))
  )
  test("returns `None` if `f` returns false for all elements ", () =>
    expect(find(~f=Int.isOdd, [0, 2, 4, 8]))->toEqual(None)
  )
  test("returns `None` for an empty array", () => expect(find(~f=Int.isEven, []))->toEqual(None))
})
describe("findIndex", () => {
  test("returns the first (index,element) tuple which `f` returns true for", () =>
    expect(findIndex(~f=(index, number) => index > 2 && Int.isEven(number), [1, 3, 4, 8]))->toEqual(
      Some(3, 8),
    )
  )
  test("returns `None` if `f` returns false for all elements ", () =>
    expect(findIndex(~f=(_, _) => false, [0, 2, 4, 8]))->toEqual(None)
  )
  test("returns `None` for an empty array", () =>
    expect(findIndex(~f=(index, number) => index > 2 && Int.isEven(number), []))->toEqual(None)
  )
})

describe("includes", () => {
  test("returns true if equal", () => expect(includes([1, 2, 3], 2, ~equal=\"="))->toEqual(true))
  test("returns false if not equal", () =>
    expect(includes([1, 5, 3], 2, ~equal=\"="))->toEqual(false)
  )
  test("returns false if empty", () => expect(includes([], 2, ~equal=\"="))->toEqual(false))
})

describe("minimum", () => {
  test("returns smallest element", () =>
    expect(minimum([1, -2, 3], ~compare=Int.compare))->toEqual(Some(-2))
  )
  test("returns none is empty", () => expect(minimum([], ~compare=Int.compare))->toEqual(None))
})

describe("maximum", () => {
  test("returns largest element", () =>
    expect(maximum([1, -2, 3], ~compare=Int.compare))->toEqual(Some(3))
  )
  test("returns none is empty", () => expect(maximum([], ~compare=Int.compare))->toEqual(None))
})

describe("extent", () => {
  test("returns range", () =>
    expect(extent([1, -2, 3], ~compare=Int.compare))->toEqual(Some(-2, 3))
  )
  test("returns range on single", () =>
    expect(extent([1], ~compare=Int.compare))->toEqual(Some(1, 1))
  )
  test("returns none is empty", () => expect(extent([], ~compare=Int.compare))->toEqual(None))
})

describe("any", () => {
  test("returns false for empty arrays", () => expect(any([], ~f=Int.isEven))->toEqual(false))
  test("returns true if at least one of the elements of an array return true for [f]", () =>
    expect(any([1, 3, 4, 5, 7], ~f=Int.isEven))->toEqual(true)
  )
  test("returns false if all of the elements of an array return false for [f]", () =>
    expect(any([1, 3, 5, 7], ~f=Int.isEven))->toEqual(false)
  )
})
describe("all", () => {
  test("returns true for empty arrays", () => expect(all(~f=Int.isEven, []))->toEqual(true))
  test("returns true if [f] returns true for all elements", () =>
    expect(all(~f=Int.isEven, [2, 4]))->toEqual(true)
  )
  test("returns false if a single element fails returns false for [f]", () =>
    expect(all(~f=Int.isEven, [2, 3]))->toEqual(false)
  )
})

describe("append", () =>
  test("append", () =>
    expect(append(repeat(~length=2, 42), repeat(~length=3, 81)))->toEqual([42, 42, 81, 81, 81])
  )
)
describe("flatten", () =>
  test("flatten", () => expect(flatten([[1, 2], [3], [4, 5]]))->toEqual([1, 2, 3, 4, 5]))
)

describe("zip", () => {
  test("Combine two arrays by merging each pair of elements into a tuple", () =>
    expect(zip([1, 2, 3, 4, 5], ["Dog", "Eagle", "Ferret"]))->toEqual([
      (1, "Dog"),
      (2, "Eagle"),
      (3, "Ferret"),
    ])
  )
  test("Combine an empty array and another array", () =>
    expect(zip([], ["Dog", "Eagle", "Ferret"]))->toEqual([])
  )
})

describe("unzip", () =>
  test("Decompose an array of tuples into a tuple of arrays", () =>
    expect(unzip([(0, true), (17, false), (1337, true)]))->toEqual((
      [0, 17, 1337],
      [true, false, true],
    ))
  )
)

describe("values", () => {
  test("Return all of the [Some] values from an array of options", () =>
    expect(values([Some("Ant"), None, Some("Cat")]))->toEqual(["Ant", "Cat"])
  )
  test("Return all of the [Some] values from an empty array of options", () =>
    expect(values([]))->toEqual([])
  )
})

describe("compare", () => {
  test(
    "Compare two arrays of unequal length using provided function Int.compare to compare pairs of elements and returns -1",
    () => expect(compare([1, 2, 3], [1, 2, 3, 4], Int.compare))->toEqual(-1),
  )
  test(
    "Compare two identical arrays using provided function Int.compare to compare pairs of elements and returns 0",
    () => expect(compare([1, 2, 3], [1, 2, 3], Int.compare))->toEqual(0),
  )
  test(
    "Compare two arrays with of the same length and differing elements using provided function Int.compare to compare pairs of elements and returns 1",
    () => expect(compare([1, 2, 5], [1, 2, 3], Int.compare))->toEqual(1),
  )
})

describe("intersperse", () => {
  test("equals an array literal of the same value", () =>
    expect(intersperse(~sep="on", ["turtles", "turtles", "turtles"]))->toEqual([
      "turtles",
      "on",
      "turtles",
      "on",
      "turtles",
    ])
  )
  test("equals an array literal of the same value", () =>
    expect(intersperse(~sep=0, []))->toEqual([])
  )
})

describe("chunksOf", () => {
  test("Split an array into equally sized chunks", () =>
    expect(chunksOf(~size=2, ["#FFBA49", "#9984D4", "#20A39E", "#EF5B5B"]))->toEqual([
      ["#FFBA49", "#9984D4"],
      ["#20A39E", "#EF5B5B"],
    ])
  )
  test("Split an array into equally sized chunks ignoring partial chunks", () =>
    expect(chunksOf(~size=2, ["#FFBA49", "#9984D4", "#20A39E", "#EF5B5B", "#23001E"]))->toEqual([
      ["#FFBA49", "#9984D4"],
      ["#20A39E", "#EF5B5B"],
    ])
  )
  test("Split an empty array into equally sized chunks", () =>
    expect(chunksOf(~size=2, []))->toEqual([])
  )
})

describe("slice", () => {
  let numbers = [0, 1, 2, 3, 4]
  let positiveArrayLengths = list{length(numbers), length(numbers) + 1, 1000}

  let negativeArrayLengths = List.map(~f=Int.negate, positiveArrayLengths)

  test("a positive `from`", () => expect(slice(~from=1, numbers))->toEqual([1, 2, 3, 4]))
  test("a negative `from`", () => expect(slice(~from=-1, numbers))->toEqual([4]))
  testAll("`from` >= `length`", positiveArrayLengths, from =>
    expect(slice(~from, numbers))->toEqual([])
  )
  testAll("`from` <= negative `length`", negativeArrayLengths, from =>
    expect(slice(~from, numbers))->toEqual(numbers)
  )
  test("a positive `to_`", () => expect(slice(~from=0, ~to_=3, numbers))->toEqual([0, 1, 2]))
  test("a negative `to_`", () => expect(slice(~from=1, ~to_=-1, numbers))->toEqual([1, 2, 3]))
  testAll("`to_` >= length", positiveArrayLengths, to_ =>
    expect(slice(~from=0, ~to_, numbers))->toEqual(numbers)
  )
  testAll("`to_` <= negative `length`", negativeArrayLengths, to_ =>
    expect(slice(~from=0, ~to_, numbers))->toEqual([])
  )
  test("both `from` and `to_` are negative and `from` < `to_`", () =>
    expect(slice(~from=-2, ~to_=-1, numbers))->toEqual([3])
  )
  test("works `from` >= `to_`", () => expect(slice(~from=4, ~to_=3, numbers))->toEqual([]))
})
describe("fold", () => {
  test("works for an empty array", () => expect(fold([], ~f=\"^", ~initial=""))->toEqual(""))
  test("works for an ascociative operator", () =>
    expect(fold(~f=\"*", ~initial=1, repeat(~length=4, 7)))->toEqual(2401)
  )
  test("works the order of arguments to `f` is important", () =>
    expect(fold(["a", "b", "c"], ~f=\"^", ~initial=""))->toEqual("abc")
  )
  test("works the order of arguments to `f` is important", () =>
    expect(
      fold(~f=(list, element) => list{element, ...list}, ~initial=list{}, [1, 2, 3]),
    )->toEqual(list{3, 2, 1})
  )
})
describe("foldRight", () => {
  test("works for empty arrays", () => expect(foldRight([], ~f=\"^", ~initial=""))->toEqual(""))
  test("foldRight", () => expect(foldRight(~f=\"+", ~initial=0, repeat(~length=3, 5)))->toEqual(15))
  test("works the order of arguments to `f` is important", () =>
    expect(foldRight(["a", "b", "c"], ~f=\"^", ~initial=""))->toEqual("cba")
  )
  test("works the order of arguments to `f` is important", () =>
    expect(
      foldRight(~f=(list, element) => list{element, ...list}, ~initial=list{}, [1, 2, 3]),
    )->toEqual(list{1, 2, 3})
  )
})
describe("reverse", () =>
  test("alters an array in-place", () => {
    let numbers = [1, 2, 3]
    reverse(numbers)
    expect(numbers)->toEqual([3, 2, 1])
  })
)

describe("sort", () => {
  test("empty list", () => {
    let numbers = []
    sort(numbers, ~compare=Int.compare)
    expect(numbers)->toEqual([])
  })
  test("one element", () => {
    let numbers = [5]
    sort(numbers, ~compare=Int.compare)
    expect(numbers)->toEqual([5])
  })
  test("multiple elements", () => {
    let numbers = [5, 6, 8, 3, 6]
    sort(numbers, ~compare=Int.compare)
    expect(numbers)->toEqual([3, 5, 6, 6, 8])
  })
})

describe("groupBy", () => {
  test("returns an empty map for an empty array", () =>
    expect(Map.length(Array.groupBy([], module(Int), ~f=String.length)))->toEqual(0)
  )
  test("example test case", () => {
    let animals = ["Ant", "Bear", "Cat", "Dewgong"]
    expect(Map.toList(Array.groupBy(animals, module(Int), ~f=String.length)))->toEqual(list{
      (3, list{"Cat", "Ant"}),
      (4, list{"Bear"}),
      (7, list{"Dewgong"}),
    })
  })
})
