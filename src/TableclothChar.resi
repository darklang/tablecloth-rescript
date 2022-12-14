@@ocaml.text(" ")

@@ocaml.text(" Functions for working with single characters.

    Character literals are enclosed in ['a'] pair of single quotes.

    {[
      let digit = '7'
    ]}

    The functions in this module work on ASCII characters (range 0-255) only,
    {b not Unicode}. 
    
    Note that in Rescript source code you can include only the characters from 0-127 range. 
    Full list of available characters is available {{: https://www.w3schools.com/charsets/ref_html_ascii.asp } here}.

    Characters from 128-255 range can still be handled, but only as codes.
")

type t = char

@@ocaml.text(" {1 Create}

    You can also create a {!Char} using single quotes:

    {[
      let char = 'c'
    ]}
")

@ocaml.doc(" Convert an ASCII {{: https://en.wikipedia.org/wiki/Code_point } code point } to a character.

    The full range of extended ASCII is from [0] to [255].
    For numbers outside that range, you get [None].

    {2 Examples}

    {[
      Char.fromCode(65) == Some('A')
      Char.fromCode(66) == Some('B')
      Char.fromCode(3000) == None
      Char.fromCode(-1) == None
    ]}
")
let fromCode: int => option<char>

@ocaml.doc(" Converts a string to character. 

    Returns [None] when the [string] isn't of length one.

    {2 Examples}

    {[
      Char.fromString(\"A\") == Some('A')
      Char.fromString(\" \") == Some(' ')
      Char.fromString(\"\") == None
      Char.fromString(\"abc\") == None
      Char.fromString(\" a\") == None
    ]}
")
let fromString: string => option<char>

@ocaml.doc(" Detect lower case ASCII characters.

    {2 Examples}

    {[
      Char.isLowercase('a') == true
      Char.isLowercase('b') == true
      Char.isLowercase('z') == true
      Char.isLowercase('0') == false
      Char.isLowercase('A') == false
      Char.isLowercase('-') == false
    ]}
")
let isLowercase: char => bool

@ocaml.doc(" Detect upper case ASCII characters.

    {2 Examples}

    {[
      Char.isUppercase('A') == true
      Char.isUppercase('B') == true
      Char.isUppercase('Z') == true
      Char.isUppercase('h') == false
      Char.isUppercase('0') == false
      Char.isUppercase('-') == false
    ]}
")
let isUppercase: char => bool

@ocaml.doc(" Detect upper and lower case ASCII alphabetic characters.

    {2 Examples}

    {[
      Char.isLetter('a') == true
      Char.isLetter('b') == true
      Char.isLetter('E') == true
      Char.isLetter('Y') == true
      Char.isLetter('0') == false
      Char.isLetter('-') == false
    ]}
")
let isLetter: char => bool

@ocaml.doc(" Detect when a character is a number.

    {2 Examples}

    {[
      Char.isDigit('0') == true
      Char.isDigit('1') == true
      Char.isDigit('9') == true
      Char.isDigit('a') == false
      Char.isDigit('b') == false
    ]}
")
let isDigit: char => bool

@ocaml.doc(" Detect upper case, lower case and digit ASCII characters.

    {2 Examples}

    {[
      Char.isAlphanumeric('a') == true
      Char.isAlphanumeric('b') == true
      Char.isAlphanumeric('E') == true
      Char.isAlphanumeric('Y') == true
      Char.isAlphanumeric('0') == true
      Char.isAlphanumeric('7') == true
      Char.isAlphanumeric('-') == false
    ]}
")
let isAlphanumeric: char => bool

@ocaml.doc(" Detect if a character is a {{: https://en.wikipedia.org/wiki/ASCII#Printable_characters } printable } character

    A Printable character has a {!Char.toCode} in the range 32 to 127, inclusive ([' '] to ['~']).

    {2 Examples}

    {[
      Char.isPrintable('G') == true
      Char.isPrintable('%') == true
      Char.isPrintable(' ') == true
      Char.isPrintable('\t') == false
      Char.isPrintable('\007') == false
    ]}
")
let isPrintable: char => bool

@ocaml.doc(" Detect one of the following characters:
    - ['\t'] (tab)
    - ['\n'] (newline)
    - ['\011'] (vertical tab)
    - ['\012'] (form feed)
    - ['\r'] (carriage return)
    - [' '] (space)

    {2 Examples}

    {[
      Char.isWhitespace('\t') == true
      Char.isWhitespace(' ') == true
      Char.isWhitespace('?') == false
      Char.isWhitespace('G') == false
    ]}
")
let isWhitespace: char => bool

@ocaml.doc(" Converts an ASCII character to lower case, preserving non alphabetic ASCII characters.

    {2 Examples}

    {[
      Char.toLowercase('A') == 'a'
      Char.toLowercase('B') == 'b'
      Char.toLowercase('7') == '7'
    ]}
")
let toLowercase: char => char

@ocaml.doc(" Convert an ASCII character to upper case, preserving non alphabetic ASCII characters.

    {2 Examples}

    {[
      Char.toUppercase('a') == 'A'
      Char.toUppercase('b') == 'B'
      Char.toUppercase('7') == '7'
    ]}
")
let toUppercase: char => char

@ocaml.doc(" Convert [char] to the corresponding ASCII {{: https://en.wikipedia.org/wiki/Code_point } code point}.

    {2 Examples}

    {[
      Char.toCode('A') == 65
      Char.toCode('B') == 66
    ]}
")
let toCode: char => int

@ocaml.doc(" Convert a character into a [string].

    {2 Examples}

    {[
      Char.toString('A') == \"A\"
      Char.toString('{') == \"{\"
      Char.toString('7') == \"7\"
    ]}
")
let toString: char => string

@ocaml.doc(" Converts a digit character to its corresponding {!Int}.

    Returns [None] when the character isn't a digit.

    {2 Examples}

    {[
      Char.toDigit(\"7\") == Some(7)
      Char.toDigit(\"0\") == Some(0)
      Char.toDigit(\"A\") == None
      Char.toDigit(\"\") == None
    ]}
")
let toDigit: char => option<int>

@ocaml.doc(" Test two {!Char}s for equality ")
let equal: (t, t) => bool

@ocaml.doc(" Compare two {!Char}s ")
let compare: (t, t) => int

@ocaml.doc(" The unique identity for {!Comparator} ")
type identity

let comparator: TableclothComparator.t<t, identity>

