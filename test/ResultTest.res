open Tablecloth

open Jest
open Expect

open Result
describe("ok", () => {
  test("returns ok type", () =>
    expect(Result.ok(String.reverse("desserts")))->toEqual(Ok("stressed"))
  )
  test("returns ok type", () =>
    expect(List.map(list{1, 2, 3}, ~f=Result.ok))->toEqual(list{Ok(1), Ok(2), Ok(3)})
  )
})

describe("error", () => {
  test("returns error type", () => expect(Result.error(Int.negate(3)))->toEqual(Error(-3)))
  test("returns error type", () =>
    expect(List.map(list{1, 2, 3}, ~f=Result.error))->toEqual(list{Error(1), Error(2), Error(3)})
  )
})

describe("fromOption", () => {
  test("maps None into Error", () =>
    expect(fromOption(~error="error message", None))->toEqual(Error("error message"))
  )
  test("maps Some into Ok", () =>
    expect(fromOption(~error="error message", Some(10)))->toEqual(Ok(10))
  )
})

describe("isOk", () => {
  test("returns true if result is Ok", () => expect(Result.isOk(Ok(3)))->toEqual(true))
  test("returns false if result is Error", () => expect(Result.isOk(Error(3)))->toEqual(false))
})

describe("isError", () => {
  test("returns false if result is Ok", () => expect(Result.isError(Ok(3)))->toEqual(false))
  test("returns true if result is Error", () => expect(Result.isError(Error(3)))->toEqual(true))
})

describe("and_", () => {
  test("returns second arg if both are Ok", () =>
    expect(Result.and_(Ok("Antelope"), Ok("Salmon")))->toEqual(Ok("Salmon"))
  )
  test("returns first error arg", () =>
    expect(Result.and_(Error("Finch"), Ok("Salmon")))->toEqual(Error("Finch"))
  )

  test("returns first error arg", () =>
    expect(Result.and_(Ok("Antelope"), Error("Finch")))->toEqual(Error("Finch"))
  )

  test("returns first error arg", () =>
    expect(Result.and_(Error("Honey bee"), Error("Finch")))->toEqual(Error("Honey bee"))
  )
})

describe("or_", () => {
  test("returns first arg if both are Ok", () =>
    expect(Result.or_(Ok("Boar"), Ok("Gecko")))->toEqual(Ok("Boar"))
  )
  test("returns ok arg", () =>
    expect(Result.or_(Error("Periwinkle"), Ok("Gecko")))->toEqual(Ok("Gecko"))
  )

  test("returns ok arg", () =>
    expect(Result.or_(Ok("Boar"), Error("Periwinkle")))->toEqual(Ok("Boar"))
  )

  test("returns second arg", () =>
    expect(Result.or_(Error("Periwinkle"), Error("Robin")))->toEqual(Error("Robin"))
  )

  describe("unwrapLazy", () =>
    test(
      "returns forced default arg if error",
      () => expect(Result.unwrapLazy(Error("Periwinkle"), ~default=lazy "Gecko"))->toEqual("Gecko"),
    )
  )
})
