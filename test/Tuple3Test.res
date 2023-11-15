open Tablecloth
open Jest
open Expect

open Tuple3
test("make", () => expect(make(3, 4, 5))->toEqual((3, 4, 5)))
test("first", () => expect(first((3, 4, 5)))->toEqual(3))
test("second", () => expect(second((3, 4, 5)))->toEqual(4))
test("third", () => expect(third((3, 4, 5)))->toEqual(5))
test("initial", () => expect(initial((3, 4, 5)))->toEqual((3, 4)))
test("tail", () => expect(tail((3, 4, 5)))->toEqual((4, 5)))
test("mapFirst", () =>
  expect(mapFirst(~f=String.reverse, ("stressed", 16, false)))->toEqual(("desserts", 16, false))
)
test("mapSecond", () =>
  expect(mapSecond(~f=Float.squareRoot, ("stressed", 16., false)))->toEqual(("stressed", 4., false))
)
test("mapThird", () =>
  expect(mapThird(~f=not, ("stressed", 16, false)))->toEqual(("stressed", 16, true))
)
test("mapEach", () =>
  expect(
    mapEach(~f=String.reverse, ~g=Float.squareRoot, ~h=not, ("stressed", 16., false)),
  )->toEqual(("desserts", 4., true))
)
test("mapAll", () =>
  expect(mapAll(~f=String.reverse, ("was", "stressed", "now")))->toEqual(("saw", "desserts", "won"))
)
test("rotateLeft", () => expect(rotateLeft((3, 4, 5)))->toEqual((4, 5, 3)))
test("rotateRight", () => expect(rotateRight((3, 4, 5)))->toEqual((5, 3, 4)))
test("toArray", () => expect(toArray((3, 4, 5)))->toEqual([3, 4, 5]))
test("toList", () => expect(toList((3, 4, 5)))->toEqual(list{3, 4, 5}))
