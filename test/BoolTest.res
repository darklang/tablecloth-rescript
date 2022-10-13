open Tablecloth
open AlcoJest

let suite = suite("Bool", () => {
  open Bool
  describe("fromInt", () => {
    test("converts zero to Some(false)", () =>
      expect(fromInt(0)) |> toEqual(
        {
          open Eq
          option(bool)
        },
        Some(false),
      )
    )
    test("converts one to Some(true)", () =>
      expect(fromInt(1)) |> toEqual(
        {
          open Eq
          option(bool)
        },
        Some(true),
      )
    )
    testAll(
      "converts everything else to None",
      list{Int.minimumValue, -2, -1, 2, Int.maximumValue},
      int =>
        expect(fromInt(int)) |> toEqual(
          {
            open Eq
            option(bool)
          },
          None,
        ),
    )
  })
  describe("fromString", () => {
    test("converts string to Some(true)", () =>
      expect(fromString("true")) |> toEqual(
        {
          open Eq
          option(bool)
        },
        Some(true),
      )
    )
    test("converts string to Some(false)", () =>
      expect(fromString("false")) |> toEqual(
        {
          open Eq
          option(bool)
        },
        Some(false),
      )
    )
    test("capital True returns None", () =>
      expect(fromString("True")) |> toEqual(
        {
          open Eq
          option(bool)
        },
        None,
      )
    )
    test("non-string returns None", () =>
      expect(fromString("1")) |> toEqual(
        {
          open Eq
          option(bool)
        },
        None,
      )
    )
  })

  describe("xor", () => {
    test("Returns [true] for xor of args true true", () =>
      expect(xor(true, true)) |> toEqual(Eq.bool, false)
    )
    test("Returns [true] for xor of args true false]", () =>
      expect(xor(true, false)) |> toEqual(Eq.bool, true)
    )
    test("Returns [true] for xor of args false true", () =>
      expect(xor(false, true)) |> toEqual(Eq.bool, true)
    )
    test("Returns [false] for xor of args false false", () =>
      expect(xor(false, false)) |> toEqual(Eq.bool, false)
    )
  })

  describe("not", () => {
    test("Returns negation of true, returns false", () => expect(!true) |> toEqual(Eq.bool, false))
    test("Returns negation of false, returns true", () => expect(!false) |> toEqual(Eq.bool, true))
  })

  describe("toString", () => {
    test("Returns string of bool, returns true as string", () =>
      expect(toString(true)) |> toEqual(Eq.string, "true")
    )
    test("Returns string of bool, returns false as string", () =>
      expect(toString(false)) |> toEqual(Eq.string, "false")
    )
  })

  describe("toInt", () => {
    test("Returns 1 for arg true", () => expect(toInt(true)) |> toEqual(Eq.int, 1))
    test("Returns 0 for arg false", () => expect(toInt(false)) |> toEqual(Eq.int, 0))
  })
  describe("equal", () => {
    test("Returns true for equal args true true", () =>
      expect(equal(true, true)) |> toEqual(Eq.bool, true)
    )
    test("Returns true equal for args false false", () =>
      expect(equal(false, false)) |> toEqual(Eq.bool, true)
    )
    test("Returns false for inqueal args true false", () =>
      expect(equal(true, false)) |> toEqual(Eq.bool, false)
    )
  })

  describe("compare", () => {
    test("Returns int 0 to describe comparison of args true true", () =>
      expect(compare(true, true)) |> toEqual(Eq.int, 0)
    )
    test("Returns int 1 to describe comparison of args true false", () =>
      expect(compare(true, false)) |> toEqual(Eq.int, 1)
    )
    test("Returns int -1 to describe comparison of args false true", () =>
      expect(compare(false, true)) |> toEqual(Eq.int, -1)
    )
    test("Returns int 0 to describe comparison of args false false", () =>
      expect(compare(false, false)) |> toEqual(Eq.int, 0)
    )
  })
})

