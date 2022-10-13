open Tablecloth
open AlcoJest

module Book = {
  type t = {
    isbn: string,
    title: string,
  }
}

module BookByIsbn = {
  type t = Book.t

  include Comparator.Make({
    type t = t

    let compare = (bookA: Book.t, bookB: Book.t) => String.compare(bookA.isbn, bookB.isbn)
  })
}

module BookByTitle = {
  type t = Book.t

  include Comparator.Make({
    type t = t

    let compare = (bookA: Book.t, bookB: Book.t) => String.compare(bookA.title, bookB.title)
  })
}

module BookByIsbnThenTitle = {
  type t = Book.t

  include Comparator.Make({
    type t = t

    let compare = (bookA: Book.t, bookB: Book.t) => {
      let isbnComparison = String.compare(bookA.isbn, bookB.isbn)
      if isbnComparison == 0 {
        String.compare(bookA.title, bookB.title)
      } else {
        isbnComparison
      }
    }
  })
}

let book = {
  let eq = (a: Book.t, b: Book.t): bool => a == b
  Eq.make(eq)
}

let mobyDick: Book.t = {isbn: "9788460767923", title: "Moby Dick or The Whale"}

let mobyDickReissue: Book.t = {isbn: "9788460767924", title: "Moby Dick or The Whale"}

let frankenstein: Book.t = {isbn: "9781478198406", title: "Frankenstein"}

let frankensteinAlternateTitle: Book.t = {isbn: "9781478198406", title: "The Modern Prometheus"}

let suite = suite("Comparator", () => {
  describe("Make", () =>
    test("module documentation example", () => {
      let result: list<Book.t> =
        Set.fromList(
          list{frankenstein, frankensteinAlternateTitle},
          module(BookByIsbn),
        ) |> Set.toList

      expect(result) |> toEqual(Eq.list(book), list{frankenstein})
    })
  )
  describe("make", () =>
    test("module documentation example", () => {
      let result: list<Book.t> =
        Set.fromList(list{mobyDick, mobyDickReissue}, module(BookByTitle)) |> Set.toList

      expect(result) |> toEqual(Eq.list(book), list{mobyDick})
    })
  )
})

