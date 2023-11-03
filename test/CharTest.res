open Tablecloth

open Jest
open Expect

open Char
test("toCode", () => expect(toCode('a'))->toEqual(97))
describe("fromCode", () => {
  test("valid ASCII codes return the corresponding character", () =>
    expect(fromCode(97))->toEqual(Some('a'))
  )
  test("negative integers return None", () => expect(fromCode(-1))->toEqual(None))
  test("integers greater than 255 return None", () => expect(fromCode(256))->toEqual(None))
})
test("toString", () => expect(toString('a'))->toEqual("a"))
describe("fromString", () => {
  test("one-length string return Some", () => expect(fromString("a"))->toEqual(Some('a')))
  test("multi character strings return None", () => expect(fromString("abc"))->toEqual(None))
  test("zero length strings return None", () => expect(fromString(""))->toEqual(None))
})
describe("toLowercase", () => {
  test("converts uppercase ASCII characters to lowercase", () =>
    expect(toLowercase('A'))->toEqual('a')
  )
  test("perserves lowercase characters", () => expect(toLowercase('a'))->toEqual('a'))
  test("perserves non-alphabet characters", () => expect(toLowercase('7'))->toEqual('7'))
  test("perserves non-ASCII characters", () => expect(toLowercase('\233'))->toEqual('\233'))
})
describe("toUppercase", () => {
  test("converts lowercase ASCII characters to uppercase", () =>
    expect(toUppercase('a'))->toEqual('A')
  )
  test("perserves uppercase characters", () => expect(toUppercase('A'))->toEqual('A'))
  test("perserves non-alphabet characters", () => expect(toUppercase('7'))->toEqual('7'))
  test("perserves non-ASCII characters", () => expect(toUppercase('\233'))->toEqual('\233'))
})
describe("toDigit", () => {
  test("toDigit - converts ASCII characters representing digits into integers", () =>
    expect(toDigit('0'))->toEqual(Some(0))
  )
  test("toDigit - converts ASCII characters representing digits into integers", () =>
    expect(toDigit('8'))->toEqual(Some(8))
  )
  test("toDigit - converts ASCII characters representing digits into integers", () =>
    expect(toDigit('a'))->toEqual(None)
  )
})
describe("isLowercase", () => {
  test("returns true for any lowercase character", () => expect(isLowercase('a'))->toEqual(true))
  test("returns false for all other characters", () => expect(isLowercase('7'))->toEqual(false))
  test("returns false for non-ASCII characters", () => expect(isLowercase('\236'))->toEqual(false))
})
describe("isUppercase", () => {
  test("returns true for any uppercase character", () => expect(isUppercase('A'))->toEqual(true))
  test("returns false for all other characters", () => expect(isUppercase('7'))->toEqual(false))
  test("returns false for non-ASCII characters", () => expect(isLowercase('\237'))->toEqual(false))
})
describe("isLetter", () => {
  test("returns true for any ASCII alphabet character", () => expect(isLetter('A'))->toEqual(true))
  testAll("returns false for all other characters", list{'7', ' ', '\n', '\011', '\236'}, char =>
    expect(isLetter(char))->toEqual(false)
  )
})
describe("isDigit", () => {
  testAll(
    "returns true for digits 0-9",
    list{'0', '1', '2', '3', '4', '5', '6', '7', '8', '9'},
    digit => expect(isDigit(digit))->toEqual(true),
  )
  test("returns false for all other characters", () => expect(isDigit('a'))->toEqual(false))
})
describe("isAlphanumeric", () => {
  test("returns true for any alphabet or digit character", () =>
    expect(isAlphanumeric('A'))->toEqual(true)
  )
  test("returns false for all other characters", () => expect(isAlphanumeric('?'))->toEqual(false))
})
describe("isPrintable", () => {
  test("returns true for a printable character", () => expect(isPrintable('~'))->toEqual(true))
  test("returns false for non-printable character", () =>
    expect(fromCode(31)->Option.map(~f=isPrintable))->toEqual(Some(false))
  )
})
describe("isWhitespace", () => {
  test("returns true for any whitespace character", () => expect(isWhitespace(' '))->toEqual(true))
  test("returns false for a non-whitespace character", () =>
    expect(isWhitespace('a'))->toEqual(false)
  )
})
