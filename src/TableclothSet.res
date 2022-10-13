type t<'a, 'id> = Belt.Set.t<'a, 'id>

let empty = comparator => Belt.Set.make(~id=Internal.toBeltComparator(comparator))

let singleton = (element: 'a, comparator: TableclothComparator.s<'a, 'identity>): t<
  'a,
  'identity,
> => Belt.Set.fromArray(~id=Internal.toBeltComparator(comparator), [element])

let fromArray = (elements: array<'a>, comparator: TableclothComparator.s<'a, 'identity>): t<
  'a,
  'identity,
> => Belt.Set.fromArray(~id=Internal.toBeltComparator(comparator), elements)

let fromList = (elements: list<'a>, comparator: TableclothComparator.s<'a, 'identity>): t<
  'a,
  'identity,
> => Belt.Set.fromArray(~id=Internal.toBeltComparator(comparator), Array.of_list(elements))

let length = Belt.Set.size

let isEmpty = Belt.Set.isEmpty

let includes = Belt.Set.has

let add = Belt.Set.add

let remove = Belt.Set.remove

let difference = Belt.Set.diff

let intersection = Belt.Set.intersect

let union = Belt.Set.union

let filter = (s, ~f) => Belt.Set.keep(s, f)

let partition = (s, ~f) => Belt.Set.partition(s, f)

let find = (s, ~f) => (Belt.Set.toArray(s) |> Belt.Array.getBy)(f)

let all = (s, ~f) => Belt.Set.every(s, f)

let any = (s, ~f) => Belt.Set.some(s, f)

let forEach = (s, ~f) => Belt.Set.forEach(s, f)

let fold = (s, ~initial, ~f) => Belt.Set.reduce(s, initial, f)

let toArray = Belt.Set.toArray

let toList = Belt.Set.toList

module Poly = {
  type identity

  type t<'a> = t<'a, identity>

  let fromArray = (type a, a: array<a>): t<a> =>
    Belt.Set.fromArray(
      a,
      ~id=module(
        {
          type t = a

          type identity = identity

          let cmp = Pervasives.compare |> Obj.magic
        }
      ),
    )

  let fromList = l => Array.of_list(l) |> fromArray

  let empty = () => fromArray([])

  let singleton = a => fromArray([a])
}

module Int = {
  type identity

  type t = t<TableclothInt.t, identity>

  let fromArray = a => Poly.fromArray(a) |> Obj.magic

  let empty = fromArray([])

  let singleton = a => fromArray([a])

  let fromList = l => Array.of_list(l) |> fromArray
}

module String = {
  type identity

  type t = t<TableclothString.t, identity>

  let fromArray = a => Poly.fromArray(a) |> Obj.magic

  let empty = fromArray([])

  let singleton = a => fromArray([a])

  let fromList = l => Array.of_list(l) |> fromArray
}

