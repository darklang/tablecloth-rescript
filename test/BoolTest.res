open Tablecloth
open Jest
open Expect

open Bool
describe("fromInt", () => {
  test("converts zero to Some(false)", () => expect(fromInt(0))->toEqual(Some(false)))
  test("converts one to Some(true)", () => expect(fromInt(1))->toEqual(Some(true)))
  testAll(
    "converts everything else to None",
    list{Int.minimumValue, -2, -1, 2, Int.maximumValue},
    int => expect(fromInt(int))->toEqual(None),
  )
})
describe("fromString", () => {
  test("converts string to Some(true)", () => expect(fromString("true"))->toEqual(Some(true)))
  test("converts string to Some(false)", () => expect(fromString("false"))->toEqual(Some(false)))
  test("capital True returns None", () => expect(fromString("True"))->toEqual(None))
  test("non-string returns None", () => expect(fromString("1"))->toEqual(None))
})

describe("xor", () => {
  test("Returns [true] for xor of args true true", () => expect(xor(true, true))->toEqual(false))
  test("Returns [true] for xor of args true false]", () => expect(xor(true, false))->toEqual(true))
  test("Returns [true] for xor of args false true", () => expect(xor(false, true))->toEqual(true))
  test("Returns [false] for xor of args false false", () =>
    expect(xor(false, false))->toEqual(false)
  )
})

describe("not", () => {
  test("Returns negation of true, returns false", () => expect(!true)->toEqual(false))
  test("Returns negation of false, returns true", () => expect(!false)->toEqual(true))
})

describe("toString", () => {
  test("Returns string of bool, returns true as string", () =>
    expect(toString(true))->toEqual("true")
  )
  test("Returns string of bool, returns false as string", () =>
    expect(toString(false))->toEqual("false")
  )
})

describe("toInt", () => {
  test("Returns 1 for arg true", () => expect(toInt(true))->toEqual(1))
  test("Returns 0 for arg false", () => expect(toInt(false))->toEqual(0))
})
describe("equal", () => {
  test("Returns true for equal args true true", () => expect(equal(true, true))->toEqual(true))
  test("Returns true equal for args false false", () => expect(equal(false, false))->toEqual(true))
  test("Returns false for inqueal args true false", () =>
    expect(equal(true, false))->toEqual(false)
  )
})

describe("compare", () => {
  test("Returns int 0 to describe comparison of args true true", () =>
    expect(compare(true, true))->toEqual(0)
  )
  test("Returns int 1 to describe comparison of args true false", () =>
    expect(compare(true, false))->toEqual(1)
  )
  test("Returns int -1 to describe comparison of args false true", () =>
    expect(compare(false, true))->toEqual(-1)
  )
  test("Returns int 0 to describe comparison of args false false", () =>
    expect(compare(false, false))->toEqual(0)
  )
})
