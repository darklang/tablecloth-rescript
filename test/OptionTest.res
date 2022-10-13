open Tablecloth
open AlcoJest

let suite = suite("Option", () => {
  describe("unwrapUnsafe", () => {
    test("returns the wrapped value for a Some", () =>
      expect(Option.unwrapUnsafe(Some(1))) |> toEqual(Eq.int, 1)
    )
    test("raises for a None", () =>
      expect(() => ignore(Option.unwrapUnsafe(None))) |> toRaise(
        Invalid_argument("Option.unwrapUnsafe called with None"),
      )
    )
  })
  describe("and_", () => {
    test("returns second argument", () =>
      expect(Option.and_(Some(1), Some(15))) |> toEqual(
        {
          open Eq
          option(int)
        },
        Some(15),
      )
    )

    test("returns none", () =>
      expect(Option.and_(None, Some(15))) |> toEqual(
        {
          open Eq
          option(int)
        },
        None,
      )
    )

    test("returns none", () =>
      expect(Option.and_(Some(1), None)) |> toEqual(
        {
          open Eq
          option(int)
        },
        None,
      )
    )

    test("returns none", () =>
      expect(Option.and_(None, None)) |> toEqual(
        {
          open Eq
          option(int)
        },
        None,
      )
    )
  })

  describe("or_", () => {
    test("returns first argument", () =>
      expect(Option.or_(Some(1), Some(15))) |> toEqual(
        {
          open Eq
          option(int)
        },
        Some(1),
      )
    )

    test("returns second argument some", () =>
      expect(Option.or_(None, Some(15))) |> toEqual(
        {
          open Eq
          option(int)
        },
        Some(15),
      )
    )

    test("returns first argument some", () =>
      expect(Option.or_(Some(1), None)) |> toEqual(
        {
          open Eq
          option(int)
        },
        Some(1),
      )
    )

    test("returns none", () =>
      expect(Option.or_(None, None)) |> toEqual(
        {
          open Eq
          option(int)
        },
        None,
      )
    )
  })

  describe("orElse", () => {
    test("returns second argument", () =>
      expect(Option.orElse(Some(1), Some(15))) |> toEqual(
        {
          open Eq
          option(int)
        },
        Some(15),
      )
    )

    test("returns second argument", () =>
      expect(Option.orElse(None, Some(15))) |> toEqual(
        {
          open Eq
          option(int)
        },
        Some(15),
      )
    )

    test("returns first argument some", () =>
      expect(Option.orElse(Some(1), None)) |> toEqual(
        {
          open Eq
          option(int)
        },
        Some(1),
      )
    )

    test("returns none", () =>
      expect(Option.orElse(None, None)) |> toEqual(
        {
          open Eq
          option(int)
        },
        None,
      )
    )
  })

  describe("both", () => {
    test("returns both as pair", () =>
      expect(Option.both(Some(3004), Some("Ant"))) |> toEqual(
        {
          open Eq
          option(pair(int, string))
        },
        Some(3004, "Ant"),
      )
    )

    test("returns none", () =>
      expect(Option.both(None, Some("Ant"))) |> toEqual(
        {
          open Eq
          option(pair(int, string))
        },
        None,
      )
    )

    test("returns none", () =>
      expect(Option.both(Some(3004), None)) |> toEqual(
        {
          open Eq
          option(pair(int, string))
        },
        None,
      )
    )

    test("returns none", () =>
      expect(Option.both(None, None)) |> toEqual(
        {
          open Eq
          option(pair(int, string))
        },
        None,
      )
    )
  })

  describe("flatten", () => {
    test("returns option layers as single option layer", () =>
      expect(Option.flatten(Some(Some(4)))) |> toEqual(
        {
          open Eq
          option(int)
        },
        Some(4),
      )
    )

    test("returns none", () =>
      expect(Option.flatten(Some(None))) |> toEqual(
        {
          open Eq
          option(int)
        },
        None,
      )
    )

    test("returns none", () =>
      expect(Option.flatten(None)) |> toEqual(
        {
          open Eq
          option(int)
        },
        None,
      )
    )
  })

  describe("map", () => {
    test("returns transformed value from inside option arg", () =>
      expect(Option.map(~f=x => x * x, Some(9))) |> toEqual(
        {
          open Eq
          option(int)
        },
        Some(81),
      )
    )

    test("returns transformed value from inside option arg", () =>
      expect(Option.map(~f=Int.toString, Some(9))) |> toEqual(
        {
          open Eq
          option(string)
        },
        Some("9"),
      )
    )

    test("returns none", () =>
      expect(Option.map(~f=x => x * x, None)) |> toEqual(
        {
          open Eq
          option(int)
        },
        None,
      )
    )
  })

  describe("map2", () => {
    test("returns transformed value from two option arg", () =>
      expect(Option.map2(Some(3), Some(4), ~f=Int.add)) |> toEqual(
        {
          open Eq
          option(int)
        },
        Some(7),
      )
    )

    test("returns none", () =>
      expect(Option.map2(Some(3), None, ~f=Int.add)) |> toEqual(
        {
          open Eq
          option(int)
        },
        None,
      )
    )
    test("returns none", () =>
      expect(Option.map2(None, Some(4), ~f=Int.add)) |> toEqual(
        {
          open Eq
          option(int)
        },
        None,
      )
    )
  })

  describe("andThen", () => {
    test("returns result of callback", () =>
      expect(Option.andThen(Some(list{1, 2, 3}), ~f=List.head)) |> toEqual(
        {
          open Eq
          option(int)
        },
        Some(1),
      )
    )

    test("returns none", () =>
      expect(Option.andThen(Some(list{}), ~f=List.head)) |> toEqual(
        {
          open Eq
          option(int)
        },
        None,
      )
    )
  })

  describe("unwrap", () => {
    test("returns unwrapped [option('a)]", () =>
      expect(Option.unwrap(~default=99, Some(42))) |> toEqual(Eq.int, 42)
    )

    test("returns default", () => expect(Option.unwrap(~default=99, None)) |> toEqual(Eq.int, 99))
  })

  describe("isSome", () => {
    test("returns true if is a Some", () =>
      expect(Option.isSome(Some(3004))) |> toEqual(Eq.bool, true)
    )

    test("returns false if is a None", () => expect(Option.isSome(None)) |> toEqual(Eq.bool, false))
  })

  describe("isNone", () => {
    test("returns false if is a Some", () =>
      expect(Option.isNone(Some(3004))) |> toEqual(Eq.bool, false)
    )

    test("returns true if is a None", () => expect(Option.isNone(None)) |> toEqual(Eq.bool, true))
  })

  describe("toArray", () => {
    test("returns option as array", () =>
      expect(Option.toArray(Some(3004))) |> toEqual(
        {
          open Eq
          array(int)
        },
        [3004],
      )
    )

    test("returns empty array if None", () =>
      expect(Option.toArray(None)) |> toEqual(
        {
          open Eq
          array(int)
        },
        [],
      )
    )
  })

  describe("toList", () => {
    test("returns option as list", () =>
      expect(Option.toList(Some(3004))) |> toEqual(
        {
          open Eq
          list(int)
        },
        list{3004},
      )
    )

    test("returns empty list if None", () =>
      expect(Option.toList(None)) |> toEqual(
        {
          open Eq
          list(int)
        },
        list{},
      )
    )
  })

  describe("equal", () => {
    test("returns bool true if options are equal", () =>
      expect(Option.equal(Some(1), Some(1), Int.equal)) |> toEqual(Eq.bool, true)
    )

    test("returns bool true if options are equal", () =>
      expect(Option.equal(Some(1), Some(3), Int.equal)) |> toEqual(Eq.bool, false)
    )

    test("returns bool true if options are equal", () =>
      expect(Option.equal(Some(1), None, Int.equal)) |> toEqual(Eq.bool, false)
    )
    test("returns bool true if options are equal", () =>
      expect(Option.equal(None, None, Int.equal)) |> toEqual(Eq.bool, true)
    )
  })

  describe("compare", () => {
    test("returns comparative value -1, 0, or 1", () =>
      expect(Option.compare(Some(1), Some(3), ~f=Int.compare)) |> toEqual(Eq.int, -1)
    )

    test("returns comparative value -1, 0, or 1", () =>
      expect(Option.compare(Some(1), None, ~f=Int.compare)) |> toEqual(Eq.int, 1)
    )

    test("returns comparative value -1, 0, or 1", () =>
      expect(Option.compare(None, None, ~f=Int.compare)) |> toEqual(Eq.int, 0)
    )
  })
})

