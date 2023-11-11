/** Functions for working with ["strings"] */
type t = string

/** # Create

  Strings literals are created with the ["double quotes"], [`backticks`] syntax.
  *Warning* If string contains non-ASCII characters, use [`backticks`]

*/
/** Converts the given character to an equivalent string of length one. */
let fromChar: char => string

/** Create a string from an {!Array} of characters.

  Note that these must be individual characters in single quotes, not strings of length one.

  ## Examples

  ```rescript
  String.fromArray([]) == ""
  String.fromArray(['a', 'b', 'c']) == "abc"
  ```
*/
let fromArray: array<char> => string

/** Create a string from a [List](List.mdx#) of characters.

  Note that these must be individual characters in single quotes, not strings of length one.

  ## Examples

  ```rescript
  String.fromList(list{}) == ""
  String.fromList(list{'a', 'b', 'c'}) == "abc"
  ```
*/
let fromList: list<char> => string

/** Create a string by repeating a string [count] time.

 ### Exceptions

  If [count] is negative, [String.repeat] throws a [RangeError] exception.

  ## Examples

  ```rescript
  String.repeat("ok", ~count=3) == "okokok"
  String.repeat("", ~count=3) == ""
  String.repeat("ok", ~count=0) == ""
  ```
*/
let repeat: (string, ~count: int) => string

/** Create a string by providing a length and a function to choose characters.

  Returns an empty string if the length is negative.

  ## Examples

  ```rescript
  String.initialize(8, ~f=Fun.constant('9')) == "99999999"
  ```
*/
let initialize: (int, ~f: int => char) => string

/** # Basic operations */
/** Get the character at the specified index 

 ### Exceptions

  If index out of range, throws a `Invalid_argument` exception.
  Concider using {!getAt}, it returns an [option<char>]

  ## Examples

  ```rescript
  String.get("stressed", 1) == 't'
  ```

*/
let get: (string, int) => char

/** Get the character at [~index] */
let getAt: (string, ~index: int) => option<char>

/** Reverse a string

  ## Examples

  ```rescript
  String.reverse("stressed") == "desserts"
  ```
*/
let reverse: string => string

/** Extract a substring from the specified indicies.

  See {!Array.slice}.
*/
let slice: (~to_: int=?, string, ~from: int) => string

/** # Query */
/** Check if a string is empty */
let isEmpty: string => bool

/** Returns the length of the given string.

  ## Examples

  ```rescript
  String.length("abc") == 3
  ```
*/
let length: string => int

/** See if the string starts with [prefix].

  ## Examples

  ```rescript
  String.startsWith("theory", ~prefix="the") == true
  String.startsWith("theory", ~prefix="ory") == false
  ```
*/
let startsWith: (string, ~prefix: string) => bool

/** See if the string ends with [suffix].

  ## Examples

  ```rescript
  String.endsWith("theory", ~suffix="the") == false
  String.endsWith("theory", ~suffix="ory") == true
  ```
*/
let endsWith: (string, ~suffix: string) => bool

/** Check if one string appears within another

  ## Examples

  ```rescript
  String.includes("team", ~substring="tea") == true
  String.includes("team", ~substring="i") == false
  String.includes("ABC", ~substring="") == true
  ```
*/
let includes: (string, ~substring: string) => bool

/** Test if the first letter of a string is upper case.

  ## Examples

  ```rescript
  String.isCapitalized("Anastasia") == true
  String.isCapitalized("") == false
  ```
*/
let isCapitalized: string => bool

/** Drop [count] characters from the left side of a string.

  ## Examples

  ```rescript
  String.dropLeft("abcdefg", ~count=3) == "defg"
  String.dropLeft("abcdefg", ~count=0) == "abcdefg"
  String.dropLeft("abcdefg", ~count=7) == ""
  String.dropLeft("abcdefg", ~count=-2) == "fg"
  String.dropLeft("abcdefg", ~count=8) == ""
  ```
*/
let dropLeft: (string, ~count: int) => string

/** Drop [count] characters from the right side of a string.

  ## Examples

  ```rescript
  String.dropRight("abcdefg", ~count=3) == "abcd"
  String.dropRight("abcdefg", ~count=0) == "abcdefg"
  String.dropRight("abcdefg", ~count=7) == ""
  String.dropRight("abcdefg", ~count=-2) == "abcdefg"
  String.dropRight("abcdefg", ~count=8) == ""
  ```
*/
let dropRight: (string, ~count: int) => string

/** Returns the index of the first occurrence of [string] or None if string has no occurences of [string]

  ## Examples

  ```rescript
  String.indexOf("Hello World World", "World") == Some(6)
  String.indexOf("Hello World World", "Bye") == None
  ```
*/
let indexOf: (string, string) => option<int>

/** Returns the index of the last occurrence of [string] or None if string has no occurences of [string]

  ## Examples

  ```rescript
  String.indexOfRight("Hello World World", "World") == Some(12)
  String.indexOfRight("Hello World World", "Bye") == None
  ```
*/
let indexOfRight: (string, string) => option<int>

/** Insert a string at `index`.

  The character previously at index will now follow the inserted string.

  ## Examples

  ```rescript
  String.insertAt("abcde", ~value="**", ~index=2) == "ab**cde"
  String.insertAt("abcde", ~value="**", ~index=0) == "**abcde"
  String.insertAt("abcde", ~value="**", ~index=5) == "abcde**"
  String.insertAt("abcde", ~value="**", ~index=-2) == "abc**de"
  String.insertAt("abcde", ~value="**", ~index=-9) == "**abcde"
  String.insertAt("abcde", ~value="**", ~index=9) == "abcde**"
  ```
*/
let insertAt: (string, ~index: int, ~value: t) => string

/** Converts all upper case letters to lower case.

  ## Examples

  ```rescript
  String.toLowercase("AaBbCc123") == "aabbcc123"
  ```
*/
let toLowercase: string => string

/** Converts all lower case letters to upper case.

  ## Examples

  ```rescript
  String.toUppercase("AaBbCc123") == "AABBCC123"
  ```
*/
let toUppercase: string => string

/** Converts the first letter to lower case if it is upper case.

  ## Examples

  ```rescript
  String.uncapitalize("Anastasia") == "anastasia"
  ```
*/
let uncapitalize: string => string

/** Converts the first letter of [s] to lowercase if it is upper case.

  ## Examples

  ```rescript
  String.capitalize("den") == "Den"
  ```
*/
let capitalize: string => string

/** Removes leading and trailing {{!Char.isWhitespace} whitespace} from a string

  ## Examples

  ```rescript
  String.trim("  abc  ") == "abc"
  String.trim("  abc def  ") == "abc def"
  String.trim("\r\n\t abc \n\n") == "abc"
  ```
*/
let trim: string => string

/** Like {!trim} but only drops characters from the beginning of the string. */
let trimLeft: string => string

/** Like {!trim} but only drops characters from the end of the string. */
let trimRight: string => string

/** Pad a string up to a minimum length.

  If the string is shorted than the proivded length, adds [with_]
  to the left of the string until the minimum length is met.

  ## Examples

  ```rescript
  String.padLeft("5", 3, ~with_="0") == "005"
  ```
*/
let padLeft: (string, int, ~with_: string) => string

/** Pad a string up to a minimum length.

  If the string is shorted than the proivded length, adds [with_]
  to the left of the string until the minimum length is met.

  ## Examples

  ```rescript
  String.padRight("Ahh", 7, ~with_="h") == "Ahhhhhh"
  ```
*/
let padRight: (string, int, ~with_: string) => string

/** Returns, as an [Option](Option.mdx#), a tuple containing the first {!Char} and the remaining String.

  If given an empty string, returns `None`.

  ## Examples

  ```rescript
  String.uncons("abcde") == Some('a', "bcde")
  String.uncons("a") == Some('a', "")
  String.uncons("") == None
  ```
*/
let uncons: string => option<(char, string)>

/** Divide a string into a list of strings, splitting whenever [on] is encountered.

  ## Examples

  ```rescript
  String.split("a/b/c", ~on="/") == list{"a", "b", "c"}
  String.split("a--b--c", ~on="--") == list{"a", "b", "c"}
  String.split("abc", ~on="/") == list{"abc"}
  String.split("", ~on="/") == list{""}
  String.split("abc", ~on="") == list{"a", "b", "c"}
  ```
*/
let split: (string, ~on: string) => list<string>

/** # Iterate */
/** Run `f` on each character in a string. */
let forEach: (string, ~f: char => unit) => unit

/** Like {!Array.fold} but the elements are {!Char}s  */
let fold: (string, ~initial: 'a, ~f: ('a, char) => 'a) => 'a

/** # Convert */
/** Returns an {!Array} of the individual characters in the given string.

  ## Examples

  ```rescript
  String.toArray("") == []
  String.toArray("abc") == ['a', 'b', 'c']
  ```
*/
let toArray: string => array<char>

/** Returns a [List](List.mdx#) of the individual characters in the given string.

  ## Examples

  ```rescript
  String.toList("") == list{}
  String.toList("abc") == list{'a', 'b', 'c'}
  ```
*/
let toList: string => list<char>

/** {1 Compare} */
/** Test two string for equality. */
let equal: (string, string) => bool

/** Compare two strings. Strings use 'dictionary' ordering.
1
  Also known as {{: https://en.wikipedia.org/wiki/Lexicographical_order } lexicographical ordering }.

  ## Examples

  ```rescript
  String.compare("Z", "A") == 1
  String.compare("Be", "Bee") == -1
  String.compare("Pear", "pear") == 1
  String.compare("Peach", "Peach") == 0
  ```
*/
let compare: (string, string) => int

/** The unique identity for {!Comparator} */
type identity

let comparator: TableclothComparator.t<t, identity>
