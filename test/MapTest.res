open Tablecloth
open AlcoJest

module Coordinate: {
  type t = (int, int)

  let compare: (t, t) => int

  type identity

  let comparator: Tablecloth.Comparator.comparator<t, identity>
} = {
  module T = {
    type t = (int, int)

    let compare = Tuple2.compare(~f=Int.compare, ~g=Int.compare)
  }

  include T
  include Tablecloth.Comparator.Make(T)
}

let suite = suite("Map", () => {
  describe("fromArray", () => {
    test(
      "returns map of key value pairs from array of pairs with ordered type key String",
      () => {
        let fromArrayMap = Map.fromArray(
          module(String),
          [("Cat", 4), ("Owl", 2), ("Fox", 5), ("Frog", 12), ("Camel", 2)],
        )

        let ansList = Map.toList(fromArrayMap)
        expect(ansList) |> toEqual(
          {
            open Eq
            list(pair(string, int))
          },
          list{("Camel", 2), ("Cat", 4), ("Fox", 5), ("Frog", 12), ("Owl", 2)},
        )
      },
    )
    test(
      "returns empty map from empty array of key value pairs",
      () => {
        let fromArrayMap = Map.fromArray(module(String), [])
        let ansList = Map.toList(fromArrayMap)
        expect(ansList) |> toEqual(
          {
            open Eq
            list(pair(string, int))
          },
          list{},
        )
      },
    )
  })

  describe("singleton", () =>
    test(
      "key value pair",
      () => {
        let singletonMap = Map.singleton(module(Int), ~key=1, ~value="Ant")

        let ans = Map.toList(singletonMap)
        expect(ans) |> toEqual(
          {
            open Eq
            list(pair(int, string))
          },
          list{(1, "Ant")},
        )
      },
    )
  )

  describe("empty", () =>
    test(
      "has length zero",
      () => expect(Tablecloth.Map.empty(module(Char)) |> Map.length) |> toEqual(Eq.int, 0),
    )
  )
  describe("Poly.fromList", () =>
    test(
      "creates a map from a list",
      () => {
        let map = Map.Poly.fromList(list{(#Ant, "Ant"), (#Bat, "Bat")})
        expect(Map.get(map, #Ant)) |> toEqual(
          {
            open Eq
            option(string)
          },
          Some("Ant"),
        )
      },
    )
  )
  describe("Int.fromList", () =>
    test(
      "creates a map from a list",
      () => {
        let map = Map.Int.fromList(list{(1, "Ant"), (2, "Bat")})
        expect(Map.get(map, 1)) |> toEqual(
          {
            open Eq
            option(string)
          },
          Some("Ant"),
        )
      },
    )
  )
  describe("String.fromList", () =>
    test(
      "creates a map from a list",
      () => {
        let map = Map.String.fromList(list{("Ant", 1), ("Bat", 1)})
        expect(Map.get(map, "Ant")) |> toEqual(
          {
            open Eq
            option(int)
          },
          Some(1),
        )
      },
    )
  )

  describe("filterMap", () =>
    test(
      "maps values by their results and filters out items with no result",
      () => {
        let filterMapMap = Map.String.fromArray([
          ("Cat", 4),
          ("Owl", 2),
          ("Fox", 5),
          ("Frog", 12),
          ("Camel", 2),
        ])

        let ansList =
          filterMapMap
          ->Map.filterMap(
            ~f=(~key as _, ~value) =>
              if value->Int.isEven {
                Some(value / 2)
              } else {
                None
              },
          )
          ->Map.toList

        expect(ansList) |> toEqual(
          {
            open Eq
            list(pair(string, int))
          },
          list{("Camel", 1), ("Cat", 2), ("Frog", 6), ("Owl", 1)},
        )
      },
    )
  )
})
