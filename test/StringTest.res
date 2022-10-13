open Tablecloth
open AlcoJest

let suite = suite("String", () => {
  open String
  testAll("fromChar", list{('a', "a"), ('z', "z"), (' ', " "), ('\n', "\n")}, ((char, string)) =>
    expect(fromChar(char)) |> toEqual(Eq.string, string)
  )
  describe("fromArray", () => {
    test("creates an empty string from an empty array", () =>
      expect(fromArray([])) |> toEqual(Eq.string, "")
    )
    test("creates a string of characters", () =>
      expect(fromArray(['K', 'u', 'b', 'o'])) |> toEqual(Eq.string, "Kubo")
    )
    test("creates a string of characters", () =>
      expect(fromArray([' ', '\n', '\t'])) |> toEqual(Eq.string, " \n\t")
    )
  })
  describe("indexOf", () => {
    test("returns some index of the first matching substring", () =>
      expect(indexOf("hello", "h")) |> toEqual(
        {
          open Eq
          option(int)
        },
        Some(0),
      )
    )
    test("returns the first index even though multiple present", () =>
      expect(indexOf("hellh", "h")) |> toEqual(
        {
          open Eq
          option(int)
        },
        Some(0),
      )
    )
    test("returns first substring that matches with multiple characters", () =>
      expect(indexOf("hellh", "ell")) |> toEqual(
        {
          open Eq
          option(int)
        },
        Some(1),
      )
    )
    test("returns None when no substring matches", () =>
      expect(indexOf("hello", "xy")) |> toEqual(
        {
          open Eq
          option(int)
        },
        None,
      )
    )
  })
  describe("indexOfRight", () => {
    test("returns some index of the last matching string", () =>
      expect(indexOfRight("helloh", "oh")) |> toEqual(
        {
          open Eq
          option(int)
        },
        Some(4),
      )
    )
    test("returns the last index even though multiple present", () =>
      expect(indexOfRight("ohelloh", "oh")) |> toEqual(
        {
          open Eq
          option(int)
        },
        Some(5),
      )
    )
    test("returns None when no character matches", () =>
      expect(indexOfRight("hello", "x")) |> toEqual(
        {
          open Eq
          option(int)
        },
        None,
      )
    )
  })
  describe("fromList", () => {
    test("creates an empty string from an empty array", () =>
      expect(fromList(list{})) |> toEqual(Eq.string, "")
    )
    test("creates a string of characters", () =>
      expect(fromList(list{'K', 'u', 'b', 'o'})) |> toEqual(Eq.string, "Kubo")
    )
    test("creates a string of characters", () =>
      expect(fromList(list{' ', '\n', '\t'})) |> toEqual(Eq.string, " \n\t")
    )
  })
  describe("repeat", () => {
    test("returns an empty string for count zero", () =>
      expect(repeat("bun", ~count=0)) |> toEqual(Eq.string, "")
    )
    test("raises for negative count", () => expect(() => repeat("bun", ~count=-1)) |> toThrow)
    test("returns the input string repeated count times", () =>
      expect(repeat("bun", ~count=3)) |> toEqual(Eq.string, "bunbunbun")
    )
  })
  describe("initialize", () => {
    test("returns an empty string for count zero", () =>
      expect(initialize(0, ~f=Fun.constant('A'))) |> toEqual(Eq.string, "")
    )
    test("raises for negative count", () =>
      expect(() => initialize(-1, ~f=Fun.constant('A'))) |> toThrow
    )
    test("returns the input string repeated count times", () =>
      expect(initialize(3, ~f=Fun.constant('A'))) |> toEqual(Eq.string, "AAA")
    )
  })
  describe("isEmpty", () => {
    test("true for zero length string", () => expect(isEmpty("")) |> toEqual(Eq.bool, true))
    testAll("false for length > 0 strings", list{"abc", " ", "\n"}, string =>
      expect(isEmpty(string)) |> toEqual(Eq.bool, false)
    )
  })
  test("length empty string", () => expect(String.length("")) |> toEqual(Eq.int, 0))
  test("length", () => expect(String.length("123")) |> toEqual(Eq.int, 3))
  test("reverse empty string", () => expect(String.reverse("")) |> toEqual(Eq.string, ""))
  test("reverse", () => expect(String.reverse("stressed")) |> toEqual(Eq.string, "desserts"))
  describe("split", () => {
    test("middle", () =>
      expect(String.split("abc", ~on="b")) |> toEqual(
        {
          open Eq
          list(string)
        },
        list{"a", "c"},
      )
    )
    test("start", () =>
      expect(String.split("ab", ~on="a")) |> toEqual(
        {
          open Eq
          list(string)
        },
        list{"", "b"},
      )
    )
    test("end", () =>
      expect(String.split("ab", ~on="b")) |> toEqual(
        {
          open Eq
          list(string)
        },
        list{"a", ""},
      )
    )
  })
  describe("insertAt", () => {
    test("middle", () =>
      expect(String.insertAt("abcde", ~value="**", ~index=2)) |> toEqual(
        {
          open Eq
          string
        },
        "ab**cde",
      )
    )
    test("start", () =>
      expect(String.insertAt("abcde", ~value="**", ~index=0)) |> toEqual(
        {
          open Eq
          string
        },
        "**abcde",
      )
    )
    test("end", () =>
      expect(String.insertAt("abcde", ~value="**", ~index=5)) |> toEqual(
        {
          open Eq
          string
        },
        "abcde**",
      )
    )
    test("negative", () =>
      expect(String.insertAt("abcde", ~value="**", ~index=-2)) |> toEqual(
        {
          open Eq
          string
        },
        "abc**de",
      )
    )
    test("negative overflow", () =>
      expect(String.insertAt("abcde", ~value="**", ~index=-9)) |> toEqual(
        {
          open Eq
          string
        },
        "**abcde",
      )
    )
    test("overflow", () =>
      expect(String.insertAt("abcde", ~value="**", ~index=9)) |> toEqual(
        {
          open Eq
          string
        },
        "abcde**",
      )
    )
  })
  test("toArray", () =>
    expect(String.toArray("Standard")) |> toEqual(
      {
        open Eq
        array(char)
      },
      ['S', 't', 'a', 'n', 'd', 'a', 'r', 'd'],
    )
  )
  test("toList", () =>
    expect(String.toList("Standard")) |> toEqual(
      {
        open Eq
        list(char)
      },
      list{'S', 't', 'a', 'n', 'd', 'a', 'r', 'd'},
    )
  )
})

