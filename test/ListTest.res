open Tablecloth
open Jest
open Expect
open List
describe("empty", () =>
  test("returns int 0 for empty list arg", () => expect(List.length(List.empty))->toEqual(0))
)
describe("singleton", () => {
  test("returns array singleton of empty string from arg empty string", () =>
    List.singleton("")->expect->toEqual(list{""})
  )
  test("returns array singleton of int arg", () =>
    expect(List.singleton(1234))->toEqual(list{1234})
  )
  test("returns array singleton of string arg", () =>
    expect(List.singleton("hello"))->toEqual(list{"hello"})
  )
})
describe("repeat", () => {
  test("returns empty list from repeat arg 0", () =>
    expect(List.repeat(~times=0, 7))->toEqual(list{})
  )
  test("returns empty list from negative repeat arg -10", () =>
    expect(List.repeat(~times=-10, 7))->toEqual(list{})
  )
  test("returns list of char 'a' repeated 5 times", () =>
    expect(List.repeat(~times=5, 'a'))->toEqual(list{'a', 'a', 'a', 'a', 'a'})
  )
})
describe("range", () => {
  test("returns empty when is zero", () => expect(List.range(0))->toEqual(list{}))
  test("with single", () => expect(List.range(5))->toEqual(list{0, 1, 2, 3, 4}))
  test("with to and from", () => expect(List.range(~from=2, 5))->toEqual(list{2, 3, 4}))
  test("with negative", () => expect(List.range(~from=-2, 2))->toEqual(list{-2, -1, 0, 1}))

  test("returns empty with count down", () => expect(List.range(~from=5, 2))->toEqual(list{}))
})

describe("initalize", () => {
  test("with identity", () =>
    expect(List.initialize(4, ~f=index => index))->toEqual(list{0, 1, 2, 3})
  )
  test("with math", () =>
    expect(List.initialize(4, ~f=index => index * index))->toEqual(list{0, 1, 4, 9})
  )
})

describe("fromArray", () => {
  test("from empty", () => expect(List.fromArray([]))->toEqual(list{}))
  test("from string", () =>
    expect(List.fromArray(['h', 'e', 'l', 'l', 'o']))->toEqual(list{'h', 'e', 'l', 'l', 'o'})
  )
  test("with int", () => expect(List.fromArray([-1, 3, 2, 7]))->toEqual(list{-1, 3, 2, 7}))
})
describe("head", () => {
  test("from empty", () => expect(List.head(list{}))->toEqual(None))
  test("with int", () => expect(List.head(list{-3, 2, 3, 4, 6}))->toEqual(Some(-3)))
})
describe("cons", () => {
  test("from empty", () => expect(List.cons(list{}, 1))->toEqual(list{1}))
  test("with int", () => expect(List.cons(list{2, 3, 4}, 1))->toEqual(list{1, 2, 3, 4}))
})

describe("filterMap", () =>
  test("keeps elements which return Some", () =>
    expect(List.filterMap(list{-1, 80, 99}, ~f=Char.fromCode))->toEqual(list{'P', 'c'})
  )
)

describe("filterWithIndex", () => {
  test("returns elements if index is greater than 1", () =>
    expect(filterWithIndex(list{2, 99, 0, 3}, ~f=(i, _) => i > 1))->toEqual(list{0, 3})
  )
  test("empty list", () => expect(filterWithIndex(list{}, ~f=(i, _) => i > 1))->toEqual(list{}))
})

describe("drop", () => {
  test("from an empty list", () => expect(drop(list{}, ~count=1))->toEqual(list{}))
  test("zero elements", () => expect(drop(list{1, 2, 3}, ~count=0))->toEqual(list{1, 2, 3}))
  test("the first element", () => expect(drop(list{1, 2, 3}, ~count=1))->toEqual(list{2, 3}))
  test("all elements", () => expect(drop(list{1, 2, 3}, ~count=3))->toEqual(list{}))
  test("greater than the number of elements", () =>
    expect(drop(list{1, 2, 3}, ~count=4))->toEqual(list{})
  )
  test("negative count", () => expect(drop(list{1, 2, 3}, ~count=-1))->toEqual(list{1, 2, 3}))
  test("zero count", () => expect(drop(list{1, 2, 3}, ~count=-1))->toEqual(list{1, 2, 3}))
})

describe("filter", () => {
  test("normal", () =>
    expect(filter(~f=Int.isEven, list{2, 4, 6, 7, 8, 9}))->toEqual(list{2, 4, 6, 8})
  )
  test("filter none", () => expect(filter(~f=Int.isEven, list{5, 7, 9}))->toEqual(list{}))
  test("filter all", () => expect(filter(~f=Int.isEven, list{2, 4, 6}))->toEqual(list{2, 4, 6}))
})

describe("dropWhile", () => {
  test("drops item from list if isEven, returns list of only odd items", () =>
    expect(dropWhile(~f=Int.isEven, list{2, 4, 6, 7, 8, 9}))->toEqual(list{7, 8, 9})
  )
  test(
    "drops none despite items dispersed throughout list because first item returns false for isEven, returns list without changes",
    () => expect(dropWhile(~f=Int.isEven, list{3, 4, 7, 8, 9}))->toEqual(list{3, 4, 7, 8, 9}),
  )
  test("drops all because all items are even, returns empty list", () =>
    expect(dropWhile(~f=Int.isEven, list{2, 4, 6, 8}))->toEqual(list{})
  )
  test("drops none because all items are odd, returns list without changes", () =>
    expect(dropWhile(~f=Int.isEven, list{1, 3, 5, 7}))->toEqual(list{1, 3, 5, 7})
  )
})

describe("take", () => {
  test("normal", () => expect(take(list{1, 2, 3}, ~count=2))->toEqual(list{1, 2}))
  test("from an empty list", () => expect(take(list{}, ~count=2))->toEqual(list{}))
  test("overflow", () => expect(take(list{1, 2, 3, 4}, ~count=8))->toEqual(list{1, 2, 3, 4}))
  test("overflow", () => expect(take(list{1, 2, 3, 4}, ~count=-1))->toEqual(list{}))
})

describe("takeWhile", () => {
  test("takes items until isEven returns false, returns list of items until first odd item", () =>
    expect(takeWhile(~f=Int.isEven, list{2, 4, 6, 7, 8, 9}))->toEqual(list{2, 4, 6})
  )
  test(
    "takes items in list of dispersed items until isEven returns false, returns list of items until first odd item",
    () => expect(takeWhile(~f=Int.isEven, list{2, 6, 7, 15, 10, 9, 8}))->toEqual(list{2, 6}),
  )
  test("takes no items as first item returns false to isEven, returns empty list", () =>
    expect(takeWhile(~f=Int.isEven, list{1, 6, 7, 15, 10, 9, 8}))->toEqual(list{})
  )
  test("takes all items as all items return true to isEven, returns list without changes", () =>
    expect(takeWhile(~f=Int.isEven, list{2, 4, 6}))->toEqual(list{2, 4, 6})
  )
  test("returns empty list from arg empty list", () =>
    expect(takeWhile(~f=Int.isEven, list{}))->toEqual(list{})
  )
  test("takes no items as all items return false to isEven, returns empty list", () =>
    expect(takeWhile(~f=Int.isEven, list{1, 3, 5}))->toEqual(list{})
  )
})

describe("findIndex", () => {
  test("returns the first (index, element) tuple which f returns true for", () =>
    expect(
      findIndex(~f=(index, number) => index > 2 && Int.isEven(number), list{1, 3, 4, 8}),
    )->toEqual(Some(3, 8))
  )
  test("returns `None` if `f` returns false for all elements ", () =>
    expect(findIndex(~f=(_, _) => false, list{0, 2, 4, 8}))->toEqual(None)
  )
  test("returns `None` for an empty array", () =>
    expect(findIndex(~f=(index, number) => index > 2 && Int.isEven(number), list{}))->toEqual(None)
  )
})

describe("includes", () => {
  test("includes int", () => expect(includes(list{1, 3, 5, 7}, 3, ~equal=Int.equal))->toEqual(true))
  test("does not includes int", () =>
    expect(includes(list{1, 3, 5, 7}, 4, ~equal=Int.equal))->toEqual(false)
  )
  test("empty list", () => expect(includes(list{}, 5, ~equal=Int.equal))->toEqual(false))
})

describe("reverse", () => {
  test("empty list", () => expect(reverse(list{}))->toEqual(list{}))
  test("one element", () => expect(reverse(list{0}))->toEqual(list{0}))
  test("two elements", () => expect(reverse(list{0, 1}))->toEqual(list{1, 0}))
})
describe("map2", () => {
  test("map2 empty lists", () => expect(map2(~f=(a,b) =>(a+b), list{}, list{}))->toEqual(list{}))
  test("map2 one element", () => expect(map2(~f=(a,b) =>(a+b), list{1}, list{1}))->toEqual(list{2}))
  test("map2 two elements", () =>
    expect(map2(~f=(a,b) =>(a+b), list{1, 2}, list{1, 2}))->toEqual(list{2, 4})
  )
  test("map2 with lists of different lengths", () =>
    expect(map2(~f=(a,b) =>(a+b), list{1, 2, 3}, list{1, 2}))->toEqual(list{2, 4})
  )
})

describe("map3", () => {
  test("map3 empty lists", () =>
    expect(map3(~f=(x, y, z) => x + y + z, list{}, list{}, list{}))->toEqual(list{})
  )
  test("map3 one element", () =>
    expect(map3(~f=(x, y, z) => x + y + z, list{1}, list{1}, list{1}))->toEqual(list{3})
  )
  test("map3 two elements", () =>
    expect(map3(~f=(x, y, z) => x + y + z, list{1, 2}, list{1, 2}, list{1, 2}))->toEqual(list{3, 6})
  )

  test("map3 elements on lists of different lengths", () =>
    expect(map3(~f=(x, y, z) => x + y + z, list{1, 2, 3}, list{1, 2, 3}, list{1, 2}))->toEqual(list{
      3,
      6,
    })
  )
})

describe("mapWithIndex", () => {
  test("on an empty list", () => expect(mapWithIndex(~f=(i, _) => i, list{}))->toEqual(list{}))
  test("with a single element", () =>
    expect(mapWithIndex(~f=(i, _) => i, list{'a'}))->toEqual(list{0})
  )
  test("with two elements", () =>
    expect(mapWithIndex(~f=(i, _) => i, list{'a', 'b'}))->toEqual(list{0, 1})
  )
})

describe("flatMap", () => {
  test("normal", () =>
    expect(flatMap(~f=x => list{x, x}, list{1, 2, 3}))->toEqual(list{1, 1, 2, 2, 3, 3})
  )
  test("on an empty list", () => expect(flatMap(~f=x => list{x, x}, list{}))->toEqual(list{}))
  test("deeper list", () =>
    expect(flatMap(~f=x => append(x, list{1}), list{list{1, 1}, list{2}, list{3}}))->toEqual(list{
      1,
      1,
      1,
      2,
      1,
      3,
      1,
    })
  )
})

describe("sliding", () => {
  test("size 1", () =>
    expect(sliding(list{1, 2, 3, 4, 5}, ~size=1))->toEqual(list{
      list{1},
      list{2},
      list{3},
      list{4},
      list{5},
    })
  )
  test("size 2", () =>
    expect(sliding(list{1, 2, 3, 4, 5}, ~size=2))->toEqual(list{
      list{1, 2},
      list{2, 3},
      list{3, 4},
      list{4, 5},
    })
  )
  test("step 3 ", () =>
    expect(sliding(list{1, 2, 3, 4, 5}, ~size=3))->toEqual(list{
      list{1, 2, 3},
      list{2, 3, 4},
      list{3, 4, 5},
    })
  )
  test("size 2, step 2", () =>
    expect(sliding(list{1, 2, 3, 4, 5}, ~size=2, ~step=2))->toEqual(list{list{1, 2}, list{3, 4}})
  )
  test("size 1, step 3", () =>
    expect(sliding(list{1, 2, 3, 4, 5}, ~size=1, ~step=3))->toEqual(list{list{1}, list{4}})
  )
  test("size 2, step 3", () =>
    expect(sliding(list{1, 2, 3, 4, 5}, ~size=2, ~step=3))->toEqual(list{list{1, 2}, list{4, 5}})
  )
  test("step 7", () => expect(sliding(list{1, 2, 3, 4, 5}, ~size=7))->toEqual(list{}))
})

describe("groupWhile", () => {
  test("empty list", () => expect(groupWhile(~f=String.equal, list{}))->toEqual(list{}))

  test("normal char", () =>
    expect(groupWhile(~f=(a,b) => a !==b, list{"a", "b", "b", "a", "a", "a", "b", "a"}))->toEqual(list{
      list{"a"},
      list{"b", "b"},
      list{"a", "a", "a"},
      list{"b"},
      list{"a"},
    })
  )

  test("normal int", () =>
    expect(
      groupWhile(~f=(x, y) => mod(x, 2) !== mod(y, 2), list{2, 4, 6, 5, 3, 1, 8, 7, 9}),
    )->toEqual(list{list{2, 4, 6}, list{5, 3, 1}, list{8}, list{7, 9}})
  )
})

describe("join", () => {
  test("normal", () => expect(join(list{"Ant", "Bat", "Cat"}, ~sep=", "))->toEqual("Ant, Bat, Cat"))
  test("empty list", () => expect(join(list{}, ~sep=", "))->toEqual(""))
})

describe("partition", () => {
  test("empty list", () => expect(partition(~f=Int.isEven, list{}))->toEqual((list{}, list{})))
  test("one element", () => expect(partition(~f=Int.isEven, list{1}))->toEqual((list{}, list{1})))
  test("four elements", () =>
    expect(partition(~f=Int.isEven, list{1, 2, 3, 4}))->toEqual((list{2, 4}, list{1, 3}))
  )
})

describe("sort", () => {
  test("empty list", () => expect(sort(~compare=Int.compare, list{}))->toEqual(list{}))
  test("one element", () => expect(sort(~compare=Int.compare, list{5}))->toEqual(list{5}))
  test("multiple elements", () =>
    expect(sort(~compare=Int.compare, list{5, 6, 8, 3, 6}))->toEqual(list{3, 5, 6, 6, 8})
  )

  test("with negative", () =>
    expect(sort(~compare=Int.compare, list{5, 6, -8, 3, 6}))->toEqual(list{-8, 3, 5, 6, 6})
  )
})

describe("sortBy", () => {
  test("empty list", () => expect(sortBy(~f=x => x, list{}))->toEqual(list{}))
  test("one element", () => expect(sortBy(~f=x => x, list{5}))->toEqual(list{5}))
  test("sort records", () => {
    let s1 = {JestStubs.Student.id: 200, JestStubs.Student.name: "Syd"}

    let s2 = {JestStubs.Student.id: 53, JestStubs.Student.name: "Bill"}

    let s3 = {JestStubs.Student.id: 30, JestStubs.Student.name: "Carmen"}

    let studentList = list{s1, s2, s3}
    let sortedRecs = sortBy(~f=x => x.JestStubs.Student.id, studentList)

    expect(sortedRecs)->toEqual(list{s3, s2, s1})
  })
  test("empty list", () =>
    expect(sortBy(~f=x => x * x, list{3, 2, 5, -2, 4}))->toEqual(list{2, -2, 3, 4, 5})
  )
})
describe("uniqueBy", () => {
  test("int self", () =>
    expect(uniqueBy(~f=element => string_of_int(element), list{1, 3, 4, 3, 7, 7, 6}))->toEqual(list{
      1,
      3,
      4,
      7,
      6,
    })
  )
  test("math", () =>
    expect(
      uniqueBy(~f=element => string_of_int(mod(5, element)), list{1, 3, 4, 3, 7, 7, 6}),
    )->toEqual(list{1, 3, 4, 7})
  )
  test("string self", () =>
    expect(
      uniqueBy(~f=element => element, list{"hello", "h", "e", "hello", "l", "l", "o"}),
    )->toEqual(list{"hello", "h", "e", "l", "o"})
  )
})
describe("minimumBy", () => {
  test("minimumBy non-empty list", () =>
    expect(List.minimumBy(~f=x => mod(x, 12), list{7, 9, 15, 10, 3, 22}))->toEqual(Some(15))
  )
  test("minimumBy empty list", () =>
    expect(List.minimumBy(~f=x => mod(x, 12), list{}))->toEqual(None)
  )
})

describe("maximumBy", () => {
  test("maximumBy non-empty list", () =>
    expect(List.maximumBy(~f=x => mod(x, 12), list{7, 9, 15, 10, 3, 22}))->toEqual(Some(10))
  )
  test("maximumBy empty list", () =>
    expect(List.maximumBy(~f=x => mod(x, 12), list{}))->toEqual(None)
  )
})

describe("minimum", () => {
  test("minimum non-empty list", () =>
    expect(minimum(list{7, 9, 15, 10, 3}, ~compare=Int.compare))->toEqual(Some(3))
  )
  test("minimum empty list", () => expect(minimum(list{}, ~compare=Int.compare))->toEqual(None))
})
describe("maximum", () => {
  test("maximum non-empty list", () =>
    expect(maximum(list{7, 9, 15, 10, 3}, ~compare=Int.compare))->toEqual(Some(15))
  )
  test("maximum empty list", () => expect(maximum(list{}, ~compare=Int.compare))->toEqual(None))
})

describe("extent", () => {
  test("return tuple of min and max", () =>
    expect(extent(list{7, 5, 8, 6}, ~compare=Int.compare))->toEqual(Some(5, 8))
  )
  test("return tuple of min and max from single element list", () =>
    expect(extent(list{5}, ~compare=Int.compare))->toEqual(Some(5, 5))
  )
  test("return tuple of min and max from empty list", () =>
    expect(extent(list{}, ~compare=Int.compare))->toEqual(None)
  )
})

describe("sum", () => {
  test("sum of empty", () => expect(sum(list{}, module(Int)))->toEqual(0))
  test("sum of ints", () => expect(sum(list{1, 1, 2, 3}, module(Int)))->toEqual(7))
  test("sum with module struct", () =>
    expect(
      sum(
        list{"a", "b", "c"},
        module(
          {
            type t = string

            let zero = ""

            let add = (a,b) => a ++ b
          }
        ),
      ),
    )->toEqual("abc")
  )
})

describe("map", () => {
  test("map sqrt", () =>
    expect(map(~f=Float.squareRoot, list{1.0, 4.0, 9.0}))->toEqual(list{1.0, 2.0, 3.0})
  )
  test("list char", () =>
    expect(
      map(~f=element => cons(element, 'h'), list{list{'i'}, list{'a'}, list{'o', 'p'}}),
    )->toEqual(list{list{'h', 'i'}, list{'h', 'a'}, list{'h', 'o', 'p'}})
  )
  test("empty list", () => expect(map(~f=Float.squareRoot, list{}))->toEqual(list{}))
})

describe("find", () => {
  test("find first of 2 matches", () =>
    expect(List.find(~f=Int.isEven, list{1, 3, 4, 8}))->toEqual(Some(4))
  )
  test("find 0 of 0 matches", () =>
    expect(List.find(~f=Int.isOdd, list{0, 2, 4, 8}))->toEqual(None)
  )
  test("empty list", () => expect(List.find(~f=Int.isEven, list{}))->toEqual(None))
})

describe("isEmpty", () => {
  test("from empty list", () => expect(isEmpty(List.empty))->toEqual(true))
  test("from empty list", () => expect(isEmpty(list{}))->toEqual(true))
  test("from list with elements", () => expect(isEmpty(list{1, -4, 5, 6}))->toEqual(false))
})

describe("length", () => {
  test("from empty list", () => expect(length(list{}))->toEqual(0))
  test("from list with elements", () => expect(length(list{1, -4, 5, 6}))->toEqual(4))
})

describe("any", () => {
  test("from empty list", () => expect(any(list{}, ~f=Int.isEven))->toEqual(false))
  test("from even list", () => expect(any(list{2, 3}, ~f=Int.isEven))->toEqual(true))
  test("from odd list", () => expect(any(list{1, 3}, ~f=Int.isEven))->toEqual(false))
})
describe("all", () => {
  test("from empty list", () => expect(all(list{}, ~f=Int.isEven))->toEqual(true))
  test("from even list", () => expect(all(list{2, 3}, ~f=Int.isEven))->toEqual(false))
  test("from all even list", () => expect(all(list{2, 4}, ~f=Int.isEven))->toEqual(true))
  test("from odd list", () => expect(any(list{1, 3}, ~f=Int.isEven))->toEqual(false))
})

describe("count", () => {
  test("empty list", () => expect(count(list{}, ~f=Int.isEven))->toEqual(0))

  test("one even element", () => expect(count(list{2, 3}, ~f=Int.isEven))->toEqual(1))
  test("all even elements", () => expect(count(list{2, 4}, ~f=Int.isEven))->toEqual(2))
})

describe("splitAt", () => {
  test("empty list", () => expect(splitAt(list{}, ~index=1))->toEqual((list{}, list{})))
  test("at evens", () => expect(splitAt(~index=0, list{2, 4, 6}))->toEqual((list{}, list{2, 4, 6})))
  test("four elements", () =>
    expect(splitAt(~index=2, list{1, 3, 2, 4}))->toEqual((list{1, 3}, list{2, 4}))
  )
  test("at end", () => expect(splitAt(~index=3, list{1, 3, 5}))->toEqual((list{1, 3, 5}, list{})))
  test("past end", () => expect(splitAt(~index=6, list{1, 3, 5}))->toEqual((list{1, 3, 5}, list{})))
  test("negative", () =>
    expect(splitAt(~index=-1, list{1, 3, 5}))->toEqual((list{}, list{1, 3, 5}))
  )
})
describe("splitWhen", () => {
  test("empty list", () => expect(splitWhen(~f=Int.isEven, list{}))->toEqual((list{}, list{})))
  test("the first element satisfies f", () =>
    expect(splitWhen(~f=Int.isEven, list{2, 4, 6}))->toEqual((list{}, list{2, 4, 6}))
  )
  test("the last element satisfies f", () =>
    expect(splitWhen(~f=Int.isEven, list{1, 3, 2, 4}))->toEqual((list{1, 3}, list{2, 4}))
  )
  test("no element satisfies f", () =>
    expect(splitWhen(~f=Int.isEven, list{1, 3, 5}))->toEqual((list{1, 3, 5}, list{}))
  )
})
describe("intersperse", () => {
  test("intersperse empty list", () => expect(intersperse(list{}, ~sep="on"))->toEqual(list{}))
  test("intersperse one turtle", () =>
    expect(intersperse(~sep="on", list{"turtles"}))->toEqual(list{"turtles"})
  )
  test("intersperse three turtles", () =>
    expect(intersperse(~sep="on", list{"turtles", "turtles", "turtles"}))->toEqual(list{
      "turtles",
      "on",
      "turtles",
      "on",
      "turtles",
    })
  )
})
describe("initial", () => {
  test("empty list", () => expect(initial(list{}))->toEqual(None))
  test("one element", () => expect(initial(list{'a'}))->toEqual(Some(list{})))
  test("two elements", () => expect(initial(list{'a', 'b'}))->toEqual(Some(list{'a'})))
})

describe("last", () => {
  test("empty list", () => expect(last(list{}))->toEqual(None))
  test("one element", () => expect(last(list{'a'}))->toEqual(Some('a')))
  test("two elements", () => expect(last(list{'a', 'b'}))->toEqual(Some('b')))
})

describe("getAt", () => {
  test("empty list", () => expect(List.getAt(list{}, ~index=2))->toEqual(None))
  test("normal", () => expect(List.getAt(list{1, 2, 3}, ~index=1))->toEqual(Some(2)))
  test("overflow", () => expect(List.getAt(list{1, 2, 3}, ~index=100))->toEqual(None))
  test("negative index", () => expect(List.getAt(list{1, 2, 3}, ~index=-2))->toEqual(None))
})

describe("append", () => {
  test("append empty lists", () => expect(append(list{}, list{}))->toEqual(list{}))
  test("append empty list", () => expect(append(list{}, list{"turtles"}))->toEqual(list{"turtles"}))
  test("append empty list", () => expect(append(list{"turtles"}, list{}))->toEqual(list{"turtles"}))
  test("append two lists", () =>
    expect(append(list{"on"}, list{"turtles"}))->toEqual(list{"on", "turtles"})
  )
})
describe("fold", () => {
  test("empty list", () => expect(fold(~f=cons, ~initial=list{}, list{}))->toEqual(list{}))
  test("one element", () => expect(fold(~f=cons, ~initial=list{}, list{1}))->toEqual(list{1}))
  test("three elements", () =>
    expect(fold(~f=cons, ~initial=list{}, list{1, 2, 3}))->toEqual(list{3, 2, 1})
  )
  test("foldr empty list", () =>
    expect(foldRight(~f=cons, ~initial=list{}, list{}))->toEqual(list{})
  )
  test("foldr one element", () =>
    expect(foldRight(~f=cons, ~initial=list{}, list{1}))->toEqual(list{1})
  )
  test("foldr three elements", () =>
    expect(foldRight(~f=cons, ~initial=list{}, list{1, 2, 3}))->toEqual(list{1, 2, 3})
  )
  test("-", () => expect(fold(~f=(a,b)=> a-b, ~initial=0, list{1, 2, 3}))->toEqual(-6))
  test("- foldRight", () => expect(foldRight(~f=(a,b)=> a-b, ~initial=0, list{1, 2, 3}))->toEqual(-6))
})
describe("insertAt", () => {
  test("empty list", () => expect(insertAt(~index=0, ~value=1, list{}))->toEqual(list{1}))
  test("in the middle", () =>
    expect(insertAt(~index=1, ~value=2, list{1, 3}))->toEqual(list{1, 2, 3})
  )
  test("in the front", () =>
    expect(insertAt(~index=0, ~value=2, list{1, 3}))->toEqual(list{2, 1, 3})
  )
  test("after end of list", () =>
    expect(insertAt(~index=4, ~value=2, list{1, 3}))->toEqual(list{1, 3, 2})
  )
  test("#216", () =>
    expect(insertAt(~index=5, ~value=1, list{0, 2, 3, 4, 5, 6, 7, 8, 9}))->toEqual(list{
      0,
      2,
      3,
      4,
      5,
      1,
      6,
      7,
      8,
      9,
    })
  )
  test("doc 1", () =>
    expect(insertAt(~index=2, ~value=999, list{100, 101, 102, 103}))->toEqual(list{
      100,
      101,
      999,
      102,
      103,
    })
  )
  test("doc 2", () =>
    expect(insertAt(~index=0, ~value=999, list{100, 101, 102, 103}))->toEqual(list{
      999,
      100,
      101,
      102,
      103,
    })
  )
  test("doc 3", () =>
    expect(insertAt(~index=4, ~value=999, list{100, 101, 102, 103}))->toEqual(list{
      100,
      101,
      102,
      103,
      999,
    })
  )
  test("doc 4", () =>
    expect(insertAt(~index=-1, ~value=999, list{100, 101, 102, 103}))->toEqual(list{
      999,
      100,
      101,
      102,
      103,
    })
  )
  test("doc 5", () =>
    expect(insertAt(~index=5, ~value=999, list{100, 101, 102, 103}))->toEqual(list{
      100,
      101,
      102,
      103,
      999,
    })
  )
})
describe("updateAt", () => {
  test("updateAt index smaller 0", () =>
    expect(updateAt(~index=-1, ~f=x => x + 1, list{1, 3}))->toEqual(list{1, 3})
  )
  test("updateAt empty list", () =>
    expect(updateAt(~index=0, ~f=x => x + 1, list{}))->toEqual(list{})
  )
  test("updateAt empty list", () =>
    expect(updateAt(~index=2, ~f=x => x + 1, list{}))->toEqual(list{})
  )
  test("updateAt inside the list", () =>
    expect(updateAt(~index=1, ~f=x => x + 1, list{1, 3}))->toEqual(list{1, 4})
  )
  test("updateAt in the front", () =>
    expect(updateAt(~index=0, ~f=x => x + 1, list{1, 3}))->toEqual(list{2, 3})
  )
  test("updateAt after end of list", () =>
    expect(updateAt(~index=4, ~f=x => x + 1, list{1, 3}))->toEqual(list{1, 3})
  )
})
describe("flatten", () => {
  test("two empty lists", () => expect(flatten(list{list{}, list{}}))->toEqual(list{}))
  test("one empty list", () => expect(flatten(list{list{1}, list{}}))->toEqual(list{1}))
  test("one empty list", () => expect(flatten(list{list{}, list{1}}))->toEqual(list{1}))
  test("several lists", () =>
    expect(flatten(list{list{1}, list{2}, list{3}}))->toEqual(list{1, 2, 3})
  )
  test("several lists", () =>
    expect(flatten(list{list{1}, list{}, list{2}, list{}, list{3}}))->toEqual(list{1, 2, 3})
  )
})

describe("zip", () => {
  test("zip num and string", () =>
    expect(zip(list{1, 2, 3, 4, 5}, list{"Dog", "Eagle", "Ferret"}))->toEqual(list{
      (1, "Dog"),
      (2, "Eagle"),
      (3, "Ferret"),
    })
  )
  test("zip num and empty", () => expect(zip(list{1, 2, 3, 4, 5}, list{}))->toEqual(list{}))
})

describe("unzip", () =>
  test("unzip num and bool", () =>
    expect(unzip(list{(0, true), (17, false), (1337, true)}))->toEqual((
      list{0, 17, 1337},
      list{true, false, true},
    ))
  )
)

describe("chunksOf", () => {
  test("empty list", () => expect(chunksOf(~size=2, list{}))->toEqual(list{}))
  test("normal", () =>
    expect(
      chunksOf(~size=2, list{"#FFBA49", "#9984D4", "#20A39E", "#EF5B5B", "#23001E"}),
    )->toEqual(list{list{"#FFBA49", "#9984D4"}, list{"#20A39E", "#EF5B5B"}})
  )
})

describe("initialize", () => {
  test("initialize length 0", () => expect(initialize(0, ~f=i => i))->toEqual(list{}))
  test("initialize length 1", () => expect(initialize(1, ~f=i => i))->toEqual(list{0}))
  test("initialize length 2", () => expect(initialize(2, ~f=i => i))->toEqual(list{0, 1}))
})
describe("removeAt", () => {
  test("removeAt index smaller 0", () =>
    expect(removeAt(~index=-1, list{1, 3}))->toEqual(list{1, 3})
  )
  test("removeAt empty list", () => expect(removeAt(~index=0, list{}))->toEqual(list{}))
  test("removeAt empty list", () => expect(removeAt(~index=2, list{}))->toEqual(list{}))
  test("removeAt index 1", () => expect(removeAt(~index=1, list{1, 3}))->toEqual(list{1}))
  test("removeAt index 0", () => expect(removeAt(~index=0, list{1, 3}))->toEqual(list{3}))
  test("removeAt after end of list", () =>
    expect(removeAt(~index=4, list{1, 3}))->toEqual(list{1, 3})
  )
})
describe("groupBy", () => {
  test("returns an empty map for an empty array", () =>
    expect(Map.length(List.groupBy(list{}, module(Int), ~f=String.length)))->toEqual(0)
  )
  test("example test case", () => {
    let animals = list{"Ant", "Bear", "Cat", "Dewgong"}
    expect(Map.toList(List.groupBy(animals, module(Int), ~f=String.length)))->toEqual(list{
      (3, list{"Cat", "Ant"}),
      (4, list{"Bear"}),
      (7, list{"Dewgong"}),
    })
  })
})

describe("toArray", () => {
  test("returns an array of int", () => expect(toArray(list{2, 4, 6, 8}))->toEqual([2, 4, 6, 8]))
  test("empty", () => expect(toArray(list{}))->toEqual([]))
  test("returns an array of char", () => expect(toArray(list{'h', 'i'}))->toEqual(['h', 'i']))
})

describe("equal", () => {
  test("normal int", () =>
    expect(equal(list{2, 5, 6, 11}, list{2, 5, 6, 11}, Int.equal))->toEqual(true)
  )
  test("normal int", () =>
    expect(equal(list{2, 5, -6, 11}, list{2, 5, 6, 11}, Int.equal))->toEqual(false)
  )
  test("normal string", () =>
    expect(equal(list{"hi", "hello", "hey"}, list{"hi", "hello", "hey"}, String.equal))->toEqual(
      true,
    )
  )
})

describe("compare", () => {
  test("normal int", () =>
    expect(compare(list{2, 5, 6, 11}, list{2, 5, 6, 11}, Int.compare))->toEqual(0)
  )
  test("normal int with different element", () =>
    expect(compare(list{2, 5, -6, 11}, list{2, 5, 6, 11}, Int.compare))->toEqual(-1)
  )
  test("normal int with shorter and longer array", () =>
    expect(compare(list{2, 5, 6, 11}, list{2, 5, 6}, Int.compare))->toEqual(1)
  )
  test("normal string", () =>
    expect(
      compare(list{"hi", "hello", "hey"}, list{"hi", "hello", "hey"}, String.compare),
    )->toEqual(0)
  )
})
