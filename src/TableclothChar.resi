/** Functions for working with single characters.

  Character literals are enclosed in ['a'] pair of single quotes.

  ```rescript
  let digit = '7'
  ```

  The functions in this module work on ASCII characters (range 0-255) only,
  *not Unicode*. 
  
  Note that in Rescript source code you can include only the characters from 0-127 range. 
  Full list of available characters is available {{: https://www.w3schools.com/charsets/ref_html_ascii.asp } here}.

  Characters from 128-255 range can still be handled, but only as codes.
*/
type t = char

/** # Create

  You can also create a {!Char} using single quotes:

  ```rescript
  let char = 'c'
  ```
*/
/** Convert an ASCII {{: https://en.wikipedia.org/wiki/Code_point } code point } to a character.

  The full range of extended ASCII is from `0` to [255].
  For numbers outside that range, you get `None`.

  ## Examples

  ```rescript
  Char.fromCode(65) == Some('A')
  Char.fromCode(66) == Some('B')
  Char.fromCode(3000) == None
  Char.fromCode(-1) == None
  ```
*/
let fromCode: int => option<char>

/** Converts a string to character. 

  Returns `None` when the [string] isn't of length one.

  ## Examples

  ```rescript
  Char.fromString("A") == Some('A')
  Char.fromString(" ") == Some(' ')
  Char.fromString("") == None
  Char.fromString("abc") == None
  Char.fromString(" a") == None
  ```
*/
let fromString: string => option<char>

/** Detect lower case ASCII characters.

  ## Examples

  ```rescript
  Char.isLowercase('a') == true
  Char.isLowercase('b') == true
  Char.isLowercase('z') == true
  Char.isLowercase('0') == false
  Char.isLowercase('A') == false
  Char.isLowercase('-') == false
  ```
*/
let isLowercase: char => bool

/** Detect upper case ASCII characters.

  ## Examples

  ```rescript
  Char.isUppercase('A') == true
  Char.isUppercase('B') == true
  Char.isUppercase('Z') == true
  Char.isUppercase('h') == false
  Char.isUppercase('0') == false
  Char.isUppercase('-') == false
  ```
*/
let isUppercase: char => bool

/** Detect upper and lower case ASCII alphabetic characters.

  ## Examples

  ```rescript
  Char.isLetter('a') == true
  Char.isLetter('b') == true
  Char.isLetter('E') == true
  Char.isLetter('Y') == true
  Char.isLetter('0') == false
  Char.isLetter('-') == false
  ```
*/
let isLetter: char => bool

/** Detect when a character is a number.

  ## Examples

  ```rescript
  Char.isDigit('0') == true
  Char.isDigit('1') == true
  Char.isDigit('9') == true
  Char.isDigit('a') == false
  Char.isDigit('b') == false
  ```
*/
let isDigit: char => bool

/** Detect upper case, lower case and digit ASCII characters.

  ## Examples

  ```rescript
  Char.isAlphanumeric('a') == true
  Char.isAlphanumeric('b') == true
  Char.isAlphanumeric('E') == true
  Char.isAlphanumeric('Y') == true
  Char.isAlphanumeric('0') == true
  Char.isAlphanumeric('7') == true
  Char.isAlphanumeric('-') == false
  ```
*/
let isAlphanumeric: char => bool

/** Detect if a character is a {{: https://en.wikipedia.org/wiki/ASCII#Printable_characters } printable } character

  A Printable character has a {!Char.toCode} in the range 32 to 127, inclusive ([' '] to ['~']).

  ## Examples

  ```rescript
  Char.isPrintable('G') == true
  Char.isPrintable('%') == true
  Char.isPrintable(' ') == true
  Char.isPrintable('\t') == false
  Char.isPrintable('\007') == false
  ```
*/
let isPrintable: char => bool

/** Detect one of the following characters:
  - ['\t'] (tab)
  - ['\n'] (newline)
  - ['\011'] (vertical tab)
  - ['\012'] (form feed)
  - ['\r'] (carriage return)
  - [' '] (space)

  ## Examples

  ```rescript
  Char.isWhitespace('\t') == true
  Char.isWhitespace(' ') == true
  Char.isWhitespace('?') == false
  Char.isWhitespace('G') == false
  ```
*/
let isWhitespace: char => bool

/** Converts an ASCII character to lower case, preserving non alphabetic ASCII characters.

  ## Examples

  ```rescript
  Char.toLowercase('A') == 'a'
  Char.toLowercase('B') == 'b'
  Char.toLowercase('7') == '7'
  ```
*/
let toLowercase: char => char

/** Convert an ASCII character to upper case, preserving non alphabetic ASCII characters.

  ## Examples

  ```rescript
  Char.toUppercase('a') == 'A'
  Char.toUppercase('b') == 'B'
  Char.toUppercase('7') == '7'
  ```
*/
let toUppercase: char => char

/** Convert [char] to the corresponding ASCII {{: https://en.wikipedia.org/wiki/Code_point } code point}.

  ## Examples

  ```rescript
  Char.toCode('A') == 65
  Char.toCode('B') == 66
  ```
*/
let toCode: char => int

/** Convert a character into a [string].

  ## Examples

  ```rescript
  Char.toString('A') == "A"
  Char.toString('{') == "{"
  Char.toString('7') == "7"
  ```
*/
let toString: char => string

/** Converts a digit character to its corresponding {!Int}.

  Returns `None` when the character isn't a digit.

  ## Examples

  ```rescript
  Char.toDigit("7") == Some(7)
  Char.toDigit("0") == Some(0)
  Char.toDigit("A") == None
  Char.toDigit("") == None
  ```
*/
let toDigit: char => option<int>

/** Test two {!Char}s for equality */
let equal: (t, t) => bool

/** Compare two {!Char}s */
let compare: (t, t) => int

/** The unique identity for {!Comparator} */
type identity

let comparator: TableclothComparator.t<t, identity>
