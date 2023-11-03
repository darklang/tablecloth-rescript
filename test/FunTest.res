open Tablecloth

open Jest
open Expect

test("identity", () => expect(Fun.identity(1))->toEqual(1))
test("ignore", () => expect(Fun.ignore(1))->toEqual())
test("constant", () => expect(Fun.constant(1, 2))->toEqual(1))
test("sequence", () => expect(Fun.sequence(1, 2))->toEqual(2))
test("flip", () => expect(Fun.flip(Int.subtract, 2, 4))->toEqual(2))
test("negate", () => {
  let num = 5
  let greaterThanFour = n => n > 4
  expect(Fun.negate(greaterThanFour, num))->toEqual(false)
})
test("apply", () => expect(Fun.apply(a => a + 1, 1))->toEqual(2))
let increment = x => x + 1
let double = x => x * 2
test("compose", () => expect(Fun.compose(1, increment, double))->toEqual(4))
test("composeRight", () => expect(Fun.composeRight(1, increment, double))->toEqual(3))
test("tap", () =>
  expect(
    Fun.tap(
      ~f=Array.reverse,
      Fun.tap(~f=numbers => ignore(numbers[1] = 0), Array.filter([1, 3, 2, 5, 4], ~f=Int.isEven)),
    ),
  )->toEqual([0, 2])
)

test("curry", () => expect(Fun.curry(((a, b)) => a / b, 8, 4))->toEqual(2))
test("uncurry", () => expect(Fun.uncurry((a, b) => a / b, (8, 4)))->toEqual(2))
test("curry3", () => {
  let tupleAdder = ((a, b, c)) => a + b + c
  expect(Fun.curry3(tupleAdder, 3, 4, 5))->toEqual(12)
})
test("uncurry3", () => {
  let curriedAdder = (a, b, c) => a + b + c
  expect(Fun.uncurry3(curriedAdder, (3, 4, 5)))->toEqual(12)
})
