@@ocaml.text(" ")

@@ocaml.text(" Functions for working with [\"strings\"] ")

type t = string

@@ocaml.text(" {1 Create}

    Strings literals are created with the [\"double quotes\"], [`backticks`] syntax.
    {b Warning} If string contains non-ASCII characters, use [`backticks`]

")

@ocaml.doc(" Converts the given character to an equivalent string of length one. ")
let fromChar: char => string

@ocaml.doc(" Create a string from an {!Array} of characters.

    Note that these must be individual characters in single quotes, not strings of length one.

    {2 Examples}

    {[
      String.fromArray([]) == \"\"
      String.fromArray(['a', 'b', 'c']) == \"abc\"
    ]}
")
let fromArray: array<char> => string

@ocaml.doc(" Create a string from a {!List} of characters.

    Note that these must be individual characters in single quotes, not strings of length one.

    {2 Examples}

    {[
      String.fromList(list{}) == \"\"
      String.fromList(list{'a', 'b', 'c'}) == \"abc\"
    ]}
")
let fromList: list<char> => string

@ocaml.doc(" Create a string by repeating a string [count] time.

    {3 Exceptions}

    If [count] is negative, [String.repeat] throws a [RangeError] exception.

    {2 Examples}

    {[
      String.repeat(\"ok\", ~count=3) == \"okokok\"
      String.repeat(\"\", ~count=3) == \"\"
      String.repeat(\"ok\", ~count=0) == \"\"
    ]}
")
let repeat: (string, ~count: int) => string

@ocaml.doc(" Create a string by providing a length and a function to choose characters.

    Returns an empty string if the length is negative.

    {2 Examples}

    {[
      String.initialize(8, ~f=Fun.constant('9')) == \"99999999\"
    ]}
")
let initialize: (int, ~f: int => char) => string

@@ocaml.text(" {1 Basic operations} ")

@ocaml.doc(" Get the character at the specified index 

    {3 Exceptions}

    If index out of range, throws a [Invalid_argument] exception.
    Concider using {!getAt}, it returns an [option<char>]

    {2 Examples}

    {[
      String.get(\"stressed\", 1) == 't'
    ]}

")
let get: (string, int) => char

@ocaml.doc(" Get the character at [~index] ")
let getAt: (string, ~index: int) => option<char>

@ocaml.doc(" Reverse a string

    {2 Examples}

    {[
      String.reverse(\"stressed\") == \"desserts\"
    ]}
")
let reverse: string => string

@ocaml.doc(" Extract a substring from the specified indicies.

    See {!Array.slice}.
")
let slice: (~to_: int=?, string, ~from: int) => string

@@ocaml.text(" {1 Query} ")

@ocaml.doc(" Check if a string is empty ")
let isEmpty: string => bool

@ocaml.doc(" Returns the length of the given string.

    {2 Examples}

    {[
      String.length(\"abc\") == 3
    ]}
")
let length: string => int

@ocaml.doc(" See if the string starts with [prefix].

    {2 Examples}

    {[
      String.startsWith(\"theory\", ~prefix=\"the\") == true
      String.startsWith(\"theory\", ~prefix=\"ory\") == false
    ]}
")
let startsWith: (string, ~prefix: string) => bool

@ocaml.doc(" See if the string ends with [suffix].

    {2 Examples}

    {[
      String.endsWith(\"theory\", ~suffix=\"the\") == false
      String.endsWith(\"theory\", ~suffix=\"ory\") == true
    ]}
")
let endsWith: (string, ~suffix: string) => bool

@ocaml.doc(" Check if one string appears within another

    {2 Examples}

    {[
      String.includes(\"team\", ~substring=\"tea\") == true
      String.includes(\"team\", ~substring=\"i\") == false
      String.includes(\"ABC\", ~substring=\"\") == true
    ]}
")
let includes: (string, ~substring: string) => bool

@ocaml.doc(" Test if the first letter of a string is upper case.

    {2 Examples}

    {[
      String.isCapitalized(\"Anastasia\") == true
      String.isCapitalized(\"\") == false
    ]}
")
let isCapitalized: string => bool

@ocaml.doc(" Drop [count] characters from the left side of a string.

    {2 Examples}

    {[
      String.dropLeft(\"abcdefg\", ~count=3) == \"defg\"
      String.dropLeft(\"abcdefg\", ~count=0) == \"abcdefg\"
      String.dropLeft(\"abcdefg\", ~count=7) == \"\"
      String.dropLeft(\"abcdefg\", ~count=-2) == \"fg\"
      String.dropLeft(\"abcdefg\", ~count=8) == \"\"
    ]}
")
let dropLeft: (string, ~count: int) => string

@ocaml.doc(" Drop [count] characters from the right side of a string.

    {2 Examples}

    {[
      String.dropRight(\"abcdefg\", ~count=3) == \"abcd\"
      String.dropRight(\"abcdefg\", ~count=0) == \"abcdefg\"
      String.dropRight(\"abcdefg\", ~count=7) == \"\"
      String.dropRight(\"abcdefg\", ~count=-2) == \"abcdefg\"
      String.dropRight(\"abcdefg\", ~count=8) == \"\"
    ]}
")
let dropRight: (string, ~count: int) => string

@ocaml.doc(" Returns the index of the first occurrence of [string] or None if string has no occurences of [string]

    {2 Examples}

    {[
      String.indexOf(\"Hello World World\", \"World\") == Some(6)
      String.indexOf(\"Hello World World\", \"Bye\") == None
    ]}
")
let indexOf: (string, string) => option<int>

@ocaml.doc(" Returns the index of the last occurrence of [string] or None if string has no occurences of [string]

    {2 Examples}

    {[
      String.indexOfRight(\"Hello World World\", \"World\") == Some(12)
      String.indexOfRight(\"Hello World World\", \"Bye\") == None
    ]}
")
let indexOfRight: (string, string) => option<int>

@ocaml.doc(" Insert a string at [index].

    The character previously at index will now follow the inserted string.

    {2 Examples}

    {[
      String.insertAt(\"abcde\", ~value=\"**\", ~index=2) == \"ab**cde\"
      String.insertAt(\"abcde\", ~value=\"**\", ~index=0) == \"**abcde\"
      String.insertAt(\"abcde\", ~value=\"**\", ~index=5) == \"abcde**\"
      String.insertAt(\"abcde\", ~value=\"**\", ~index=-2) == \"abc**de\"
      String.insertAt(\"abcde\", ~value=\"**\", ~index=-9) == \"**abcde\"
      String.insertAt(\"abcde\", ~value=\"**\", ~index=9) == \"abcde**\"
    ]}
")
let insertAt: (string, ~index: int, ~value: t) => string

@ocaml.doc(" Converts all upper case letters to lower case.

    {2 Examples}

    {[
      String.toLowercase(\"AaBbCc123\") == \"aabbcc123\"
    ]}
")
let toLowercase: string => string

@ocaml.doc(" Converts all lower case letters to upper case.

    {2 Examples}

    {[
      String.toUppercase(\"AaBbCc123\") == \"AABBCC123\"
    ]}
")
let toUppercase: string => string

@ocaml.doc(" Converts the first letter to lower case if it is upper case.

    {2 Examples}

    {[
      String.uncapitalize(\"Anastasia\") == \"anastasia\"
    ]}
")
let uncapitalize: string => string

@ocaml.doc(" Converts the first letter of [s] to lowercase if it is upper case.

    {2 Examples}

    {[
      String.capitalize(\"den\") == \"Den\"
    ]}
")
let capitalize: string => string

@ocaml.doc(" Removes leading and trailing {{!Char.isWhitespace} whitespace} from a string

    {2 Examples}

    {[
      String.trim(\"  abc  \") == \"abc\"
      String.trim(\"  abc def  \") == \"abc def\"
      String.trim(\"\r\n\t abc \n\n\") == \"abc\"
    ]}
")
let trim: string => string

@ocaml.doc(" Like {!trim} but only drops characters from the beginning of the string. ")
let trimLeft: string => string

@ocaml.doc(" Like {!trim} but only drops characters from the end of the string. ")
let trimRight: string => string

@ocaml.doc(" Pad a string up to a minimum length.

    If the string is shorted than the proivded length, adds [with_]
    to the left of the string until the minimum length is met.

    {2 Examples}

    {[
      String.padLeft(\"5\", 3, ~with_=\"0\") == \"005\"
    ]}
")
let padLeft: (string, int, ~with_: string) => string

@ocaml.doc(" Pad a string up to a minimum length.

    If the string is shorted than the proivded length, adds [with_]
    to the left of the string until the minimum length is met.

    {2 Examples}

    {[
      String.padRight(\"Ahh\", 7, ~with_=\"h\") == \"Ahhhhhh\"
    ]}
")
let padRight: (string, int, ~with_: string) => string

@ocaml.doc(" Returns, as an {!Option}, a tuple containing the first {!Char} and the remaining String.

    If given an empty string, returns [None].

    {2 Examples}

    {[
      String.uncons(\"abcde\") == Some('a', \"bcde\")
      String.uncons(\"a\") == Some('a', \"\")
      String.uncons(\"\") == None
    ]}
")
let uncons: string => option<(char, string)>

@ocaml.doc(" Divide a string into a list of strings, splitting whenever [on] is encountered.

    {2 Examples}

    {[
      String.split(\"a/b/c\", ~on=\"/\") == list{\"a\", \"b\", \"c\"}
      String.split(\"a--b--c\", ~on=\"--\") == list{\"a\", \"b\", \"c\"}
      String.split(\"abc\", ~on=\"/\") == list{\"abc\"}
      String.split(\"\", ~on=\"/\") == list{\"\"}
      String.split(\"abc\", ~on=\"\") == list{\"a\", \"b\", \"c\"}
    ]}
")
let split: (string, ~on: string) => list<string>

@@ocaml.text(" {1 Iterate} ")

@ocaml.doc(" Run [f] on each character in a string. ")
let forEach: (string, ~f: char => unit) => unit

@ocaml.doc(" Like {!Array.fold} but the elements are {!Char}s  ")
let fold: (string, ~initial: 'a, ~f: ('a, char) => 'a) => 'a

@@ocaml.text(" {1 Convert} ")

@ocaml.doc(" Returns an {!Array} of the individual characters in the given string.

    {2 Examples}

    {[
      String.toArray(\"\") == []
      String.toArray(\"abc\") == ['a', 'b', 'c']
    ]}
")
let toArray: string => array<char>

@ocaml.doc(" Returns a {!List} of the individual characters in the given string.

    {2 Examples}

    {[
      String.toList(\"\") == list{}
      String.toList(\"abc\") == list{'a', 'b', 'c'}
    ]}
")
let toList: string => list<char>

@@ocaml.text(" {1 Compare} ")

@ocaml.doc(" Test two string for equality. ")
let equal: (string, string) => bool

@ocaml.doc(" Compare two strings. Strings use 'dictionary' ordering.
1
    Also known as {{: https://en.wikipedia.org/wiki/Lexicographical_order } lexicographical ordering }.

    {2 Examples}

    {[
      String.compare(\"Z\", \"A\") == 1
      String.compare(\"Be\", \"Bee\") == -1
      String.compare(\"Pear\", \"pear\") == 1
      String.compare(\"Peach\", \"Peach\") == 0
    ]}
")
let compare: (string, string) => int

@ocaml.doc(" The unique identity for {!Comparator} ")
type identity

let comparator: TableclothComparator.t<t, identity>

