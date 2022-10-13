open Tablecloth
open AlcoJest

let suite = suite("Result", () => {
  open Result
  describe("ok", () => {
    test("returns ok type", () =>
      expect(String.reverse("desserts") |> Result.ok) |> toEqual(
        {
          open Eq
          result(string, string)
        },
        Ok("stressed"),
      )
    )
    test("returns ok type", () =>
      expect(List.map(list{1, 2, 3}, ~f=Result.ok)) |> toEqual(
        {
          open Eq
          list(result(int, string))
        },
        list{Ok(1), Ok(2), Ok(3)},
      )
    )
  })

  describe("error", () => {
    test("returns error type", () =>
      expect(Int.negate(3) |> Result.error) |> toEqual(
        {
          open Eq
          result(string, int)
        },
        Error(-3),
      )
    )
    test("returns error type", () =>
      expect(List.map(list{1, 2, 3}, ~f=Result.error)) |> toEqual(
        {
          open Eq
          list(result(string, int))
        },
        list{Error(1), Error(2), Error(3)},
      )
    )
  })

  describe("fromOption", () => {
    test("maps None into Error", () =>
      expect(fromOption(~error="error message", None)) |> toEqual(
        {
          open Eq
          result(int, string)
        },
        Error("error message"),
      )
    )
    test("maps Some into Ok", () =>
      expect(fromOption(~error="error message", Some(10))) |> toEqual(
        {
          open Eq
          result(int, string)
        },
        Ok(10),
      )
    )
  })

  describe("isOk", () => {
    test("returns true if result is Ok", () => expect(Result.isOk(Ok(3))) |> toEqual(Eq.bool, true))
    test("returns false if result is Error", () =>
      expect(Result.isOk(Error(3))) |> toEqual(Eq.bool, false)
    )
  })

  describe("isError", () => {
    test("returns false if result is Ok", () =>
      expect(Result.isError(Ok(3))) |> toEqual(Eq.bool, false)
    )
    test("returns true if result is Error", () =>
      expect(Result.isError(Error(3))) |> toEqual(Eq.bool, true)
    )
  })

  describe("and_", () => {
    test("returns second arg if both are Ok", () =>
      expect(Result.and_(Ok("Antelope"), Ok("Salmon"))) |> toEqual(
        {
          open Eq
          result(string, string)
        },
        Ok("Salmon"),
      )
    )
    test("returns first error arg", () =>
      expect(Result.and_(Error("Finch"), Ok("Salmon"))) |> toEqual(
        {
          open Eq
          result(string, string)
        },
        Error("Finch"),
      )
    )

    test("returns first error arg", () =>
      expect(Result.and_(Ok("Antelope"), Error("Finch"))) |> toEqual(
        {
          open Eq
          result(string, string)
        },
        Error("Finch"),
      )
    )

    test("returns first error arg", () =>
      expect(Result.and_(Error("Honey bee"), Error("Finch"))) |> toEqual(
        {
          open Eq
          result(string, string)
        },
        Error("Honey bee"),
      )
    )
  })

  describe("or_", () => {
    test("returns first arg if both are Ok", () =>
      expect(Result.or_(Ok("Boar"), Ok("Gecko"))) |> toEqual(
        {
          open Eq
          result(string, string)
        },
        Ok("Boar"),
      )
    )
    test("returns ok arg", () =>
      expect(Result.or_(Error("Periwinkle"), Ok("Gecko"))) |> toEqual(
        {
          open Eq
          result(string, string)
        },
        Ok("Gecko"),
      )
    )

    test("returns ok arg", () =>
      expect(Result.or_(Ok("Boar"), Error("Periwinkle"))) |> toEqual(
        {
          open Eq
          result(string, string)
        },
        Ok("Boar"),
      )
    )

    test("returns second arg", () =>
      expect(Result.or_(Error("Periwinkle"), Error("Robin"))) |> toEqual(
        {
          open Eq
          result(string, string)
        },
        Error("Robin"),
      )
    )

    describe("unwrapLazy", () =>
      test("returns forced default arg if error", () =>
        expect(Result.unwrapLazy(Error("Periwinkle"), ~default=lazy "Gecko")) |> toEqual(
          {
            open Eq
            string
          },
          "Gecko",
        )
      )
    )
  })
})

