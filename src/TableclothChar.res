/*
  This file has a `Tablecloth` prefix since it uses Stdlib.Char in its implementation.
  Without the prefix we would encounter circular reference compiler errors.
*/

type t = char

include TableclothComparator.Make({
  type t = t

  let compare = compare
})

let toCode = (c: char) => Char.code(c)

let fromCode = (i): option<char> =>
  if 0 <= i && i <= 255 {
    Some(Char.chr(i))
  } else {
    None
  }

let toString = c => String.make(1, c)

let fromString = (str): option<char> =>
  switch String.length(str) {
  | 1 => Some(String.get(str, 0))
  | _ => None
  }

let toDigit = char =>
  switch char {
  | '0' .. '9' => Some(toCode(char) - toCode('0'))
  | _ => None
  }

let toLowercase = char =>
  switch char {
  | 'A' .. 'Z' => Char.chr(toCode('a') + (toCode(char) - toCode('A')))
  | _ => char
  }

let toUppercase = char =>
  switch char {
  | 'a' .. 'z' => Char.chr(toCode('A') + (toCode(char) - toCode('a')))
  | _ => char
  }

let isLowercase = x =>
  switch x {
  | 'a' .. 'z' => true
  | _ => false
  }

let isUppercase = x =>
  switch x {
  | 'A' .. 'Z' => true
  | _ => false
  }

let isLetter = x =>
  switch x {
  | 'a' .. 'z' | 'A' .. 'Z' => true
  | _ => false
  }

let isDigit = x =>
  switch x {
  | '0' .. '9' => true
  | _ => false
  }

let isAlphanumeric = x =>
  switch x {
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' => true
  | _ => false
  }

let isPrintable = x =>
  switch x {
  | ' ' .. '~' => true
  | _ => false
  }

let isWhitespace = x =>
  switch x {
  | '\t' | '\n' | '\011' | '\012' | '\r' | ' ' => true
  | _ => false
  }

let equal = \"="

let compare = compare
