open Tablecloth
open AlcoJest

let suite = suite("Int", () => {
  test("zero", () => expect(Int.zero) |> toEqual(Eq.int, 0))
  test("one", () => expect(Int.one) |> toEqual(Eq.int, 1))
  test("minimumValue", () => expect(Int.minimumValue - 1) |> toEqual(Eq.int, Int.maximumValue))
  test("maximumValue", () => expect(Int.maximumValue + 1) |> toEqual(Eq.int, Int.minimumValue))
  describe("add", () => test("add", () => expect(Int.add(3002, 4004)) |> toEqual(Eq.int, 7006)))
  describe("subtract", () =>
    test("subtract", () => expect(Int.subtract(4, 3)) |> toEqual(Eq.int, 1))
  )
  describe("multiply", () =>
    test("multiply", () => expect(Int.multiply(2, 7)) |> toEqual(Eq.int, 14))
  )
  describe("divide", () => {
    test("divide", () => expect(Int.divide(3, ~by=2)) |> toEqual(Eq.int, 1))
    test("division by zero", () => expect(() => Int.divide(3, ~by=0)) |> toThrow)
    test("divide", () =>
      expect({
        open Int
        divide(27, ~by=5)
      }) |> toEqual(Eq.int, 5)
    )
    test("divideFloat", () =>
      expect({
        open Int
        divideFloat(3, ~by=2)
      }) |> toEqual(Eq.float, 1.5)
    )
    test("divideFloat", () =>
      expect({
        open Int
        divideFloat(27, ~by=5)
      }) |> toEqual(Eq.float, 5.4)
    )
    test("divideFloat", () =>
      expect({
        open Int
        divideFloat(8, ~by=4)
      }) |> toEqual(Eq.float, 2.0)
    )
    test("divideFloat by 0", () =>
      expect(
        {
          open Int
          divideFloat(8, ~by=0)
        } == Float.infinity,
      ) |> toEqual(Eq.bool, true)
    )
    test("divideFloat 0", () =>
      expect(
        {
          open Int
          divideFloat(-8, ~by=0)
        } == Float.negativeInfinity,
      ) |> toEqual(Eq.bool, true)
    )
  })
  describe("power", () => {
    test("power", () => expect(Int.power(~base=7, ~exponent=3)) |> toEqual(Eq.int, 343))
    test("0 base", () => expect(Int.power(~base=0, ~exponent=3)) |> toEqual(Eq.int, 0))
    test("0 exponent", () => expect(Int.power(~base=7, ~exponent=0)) |> toEqual(Eq.int, 1))
  })
  describe("negate", () => {
    test("positive number", () => expect(Int.negate(8)) |> toEqual(Eq.int, -8))
    test("negative number", () => expect(Int.negate(-7)) |> toEqual(Eq.int, 7))
    test("zero", () => expect(Int.negate(0)) |> toEqual(Eq.int, -0))
  })
  describe("modulo", () =>
    test("documentation examples", () =>
      expect(Array.map([-4, -3, -2, -1, 0, 1, 2, 3, 4], ~f=Int.modulo(~by=3))) |> toEqual(
        {
          open Eq
          array(int)
        },
        [2, 0, 1, 2, 0, 1, 2, 0, 1],
      )
    )
  )
  describe("remainder", () =>
    test("documentation examples", () =>
      expect(Array.map([-4, -2, -1, 0, 1, 2, 3, 4], ~f=Int.remainder(~by=3))) |> toEqual(
        {
          open Eq
          array(int)
        },
        [-1, -2, -1, 0, 1, 2, 0, 1],
      )
    )
  )
  describe("absolute", () => {
    test("positive number", () => expect(Int.absolute(8)) |> toEqual(Eq.int, 8))
    test("negative number", () => expect(Int.absolute(-7)) |> toEqual(Eq.int, 7))
    test("zero", () => expect(Int.absolute(0)) |> toEqual(Eq.int, 0))
  })

  describe("minimum", () => {
    test("positive numbers", () => expect(Int.minimum(8, 18)) |> toEqual(Eq.int, 8))
    test("with zero", () => expect(Int.minimum(5, 0)) |> toEqual(Eq.int, 0))
    test("negative numbers", () => expect(Int.minimum(-4, -1)) |> toEqual(Eq.int, -4))
  })

  describe("maximum", () => {
    test("positive numbers", () => expect(Int.maximum(8, 18)) |> toEqual(Eq.int, 18))
    test("with zero", () => expect(Int.maximum(5, 0)) |> toEqual(Eq.int, 5))
    test("negative numbers", () => expect(Int.maximum(-4, -1)) |> toEqual(Eq.int, -1))
  })

  describe("isEven", () => {
    test("even number", () => expect(Int.isEven(8)) |> toEqual(Eq.bool, true))
    test("odd number", () => expect(Int.isEven(9)) |> toEqual(Eq.bool, false))
    test("zero even", () => expect(Int.isEven(0)) |> toEqual(Eq.bool, true))
  })

  describe("isOdd", () => {
    test("even number", () => expect(Int.isOdd(8)) |> toEqual(Eq.bool, false))
    test("odd number", () => expect(Int.isOdd(9)) |> toEqual(Eq.bool, true))
    test("zero even", () => expect(Int.isOdd(0)) |> toEqual(Eq.bool, false))
  })

  describe("clamp", () => {
    test("in range", () => expect(Int.clamp(~lower=0, ~upper=8, 5)) |> toEqual(Eq.int, 5))
    test("above range", () => expect(Int.clamp(~lower=0, ~upper=8, 9)) |> toEqual(Eq.int, 8))
    test("below range", () => expect(Int.clamp(~lower=2, ~upper=8, 1)) |> toEqual(Eq.int, 2))
    test("above negative range", () =>
      expect(Int.clamp(~lower=-10, ~upper=-5, 5)) |> toEqual(Eq.int, -5)
    )
    test("below negative range", () =>
      expect(Int.clamp(~lower=-10, ~upper=-5, -15)) |> toEqual(Eq.int, -10)
    )
    test("invalid arguments", () => expect(() => Int.clamp(~lower=7, ~upper=1, 3)) |> toThrow)
  })
  describe("inRange", () => {
    test("in range", () => expect(Int.inRange(~lower=2, ~upper=4, 3)) |> toEqual(Eq.bool, true))
    test("above range", () => expect(Int.inRange(~lower=2, ~upper=4, 8)) |> toEqual(Eq.bool, false))
    test("below range", () => expect(Int.inRange(~lower=2, ~upper=4, 1)) |> toEqual(Eq.bool, false))
    test("equal to ~upper", () =>
      expect(Int.inRange(~lower=1, ~upper=2, 2)) |> toEqual(Eq.bool, false)
    )
    test("negative range", () =>
      expect(Int.inRange(~lower=-7, ~upper=-5, -6)) |> toEqual(Eq.bool, true)
    )
    test("invalid arguments", () => expect(() => Int.inRange(~lower=7, ~upper=1, 3)) |> toThrow)
  })
  describe("toFloat", () => {
    test("5", () => expect(Int.toFloat(5)) |> toEqual(Eq.float, 5.))
    test("0", () => expect(Int.toFloat(0)) |> toEqual(Eq.float, 0.))
    test("-7", () => expect(Int.toFloat(-7)) |> toEqual(Eq.float, -7.))
  })
  describe("fromString", () => {
    test("0", () =>
      expect(Int.fromString("0")) |> toEqual(
        {
          open Eq
          option(int)
        },
        Some(0),
      )
    )
    test("-0", () =>
      expect(Int.fromString("-0")) |> toEqual(
        {
          open Eq
          option(int)
        },
        Some(-0),
      )
    )
    test("42", () =>
      expect(Int.fromString("42")) |> toEqual(
        {
          open Eq
          option(int)
        },
        Some(42),
      )
    )
    test("123_456", () =>
      expect(Int.fromString("123_456")) |> toEqual(
        {
          open Eq
          option(int)
        },
        Some(123_456),
      )
    )
    test("-42", () =>
      expect(Int.fromString("-42")) |> toEqual(
        {
          open Eq
          option(int)
        },
        Some(-42),
      )
    )
    test("0XFF", () =>
      expect(Int.fromString("0XFF")) |> toEqual(
        {
          open Eq
          option(int)
        },
        Some(255),
      )
    )
    test("0X000A", () =>
      expect(Int.fromString("0X000A")) |> toEqual(
        {
          open Eq
          option(int)
        },
        Some(10),
      )
    )
    test("Infinity", () =>
      expect(Int.fromString("Infinity")) |> toEqual(
        {
          open Eq
          option(int)
        },
        None,
      )
    )
    test("-Infinity", () =>
      expect(Int.fromString("-Infinity")) |> toEqual(
        {
          open Eq
          option(int)
        },
        None,
      )
    )
    test("NaN", () =>
      expect(Int.fromString("NaN")) |> toEqual(
        {
          open Eq
          option(int)
        },
        None,
      )
    )
    test("abc", () =>
      expect(Int.fromString("abc")) |> toEqual(
        {
          open Eq
          option(int)
        },
        None,
      )
    )
    test("--4", () =>
      expect(Int.fromString("--4")) |> toEqual(
        {
          open Eq
          option(int)
        },
        None,
      )
    )
    test("empty string", () =>
      expect(Int.fromString(" ")) |> toEqual(
        {
          open Eq
          option(int)
        },
        None,
      )
    )
  })
  describe("toString", () => {
    test("positive number", () => expect(Int.toString(1)) |> toEqual(Eq.string, "1"))
    test("negative number", () => expect(Int.toString(-1)) |> toEqual(Eq.string, "-1"))
  })
})

