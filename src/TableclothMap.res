module Option = TableclothOption

type t<'key, 'value, 'cmp> = Belt.Map.t<'key, 'value, 'cmp>

let fromArray = (comparator: TableclothComparator.s<'key, 'id>, values: array<('key, 'v)>): t<
  'key,
  'value,
  'id,
> => Belt.Map.fromArray(values, ~id=Internal.toBeltComparator(comparator))

let empty = comparator => fromArray(comparator, [])

let fromList = (comparator, l) => fromArray(comparator, Array.of_list(l))

let singleton = (comparator, ~key, ~value) => fromArray(comparator, [(key, value)])

let isEmpty = t => Belt.Map.isEmpty(t)

let includes = (t, k) => Belt.Map.has(t, k)

let length = t => Belt.Map.size(t)

let add = (m, ~key, ~value) => Belt.Map.set(m, key, value)

let remove = (t, k) => Belt.Map.remove(t, k)

let get = (t, k) => Belt.Map.get(t, k)

let update = (m, ~key, ~f) => Belt.Map.updateU(m, key, f)

let merge = (m1, m2, ~f) => Belt.Map.mergeU(m1, m2, f)

let map = (m, ~f) => Belt.Map.map(m, value => f(value))

let mapWithIndex = (t, ~f) => Belt.Map.mapWithKeyU(t, f)

let filter = (m, ~f) => Belt.Map.keep(m, (_, value) => f(value))

let filterMap = (m, ~f) => {
  let f' = ((key, value)) => f(~key, ~value)->Belt.Option.map(value' => (key, value'))
  m->Belt.Map.toArray->Belt.Array.keepMap(f')->Belt.Map.fromArray(~id=Belt.Map.getId(m))
}

let partition = (m, ~f) => Belt.Map.partition(m, (key, value) => f(~key, ~value))

let find = (m, ~f) => Belt.Map.findFirstBy(m, (key, value) => f(~key, ~value))

let any = (m, ~f) => Belt.Map.some(m, (_, value) => f(value))

let all = (m, ~f) => Belt.Map.every(m, (_, value) => f(value))

let forEach = (m, ~f) => Belt.Map.forEach(m, (_, value) => f(value))

let forEachWithIndex = (m, ~f) => Belt.Map.forEach(m, (key, value) => f(~key, ~value))

let fold = (m, ~initial, ~f) =>
  Belt.Map.reduce(m, initial, (acc, key, data) => f(acc, ~key, ~value=data))

let keys = m => Array.to_list(Belt.Map.keysToArray(m))

let values = m => Array.to_list(Belt.Map.valuesToArray(m))

let maximum = t => Belt.Map.maxKey(t)

let minimum = t => Belt.Map.minKey(t)

let extent = t => Option.both(minimum(t), maximum(t))

let toArray = t => Belt.Map.toArray(t)

let toList = t => Belt.Map.toList(t)

module Poly = {
  type identity

  type t<'k, 'v> = t<'k, 'v, identity>

  let fromArray = (type k v, a: array<(k, v)>): t<k, v> =>
    Belt.Map.fromArray(
      a,
      ~id=module(
        {
          type t = k

          type identity = identity

          let cmp = Obj.magic(Pervasives.compare)
        }
      ),
    )

  let empty = () => fromArray([])

  let fromList = l => fromArray(Array.of_list(l))

  let singleton = (~key, ~value) => fromArray([(key, value)])
}

module Int = {
  type identity

  type t<'value> = t<TableclothInt.t, 'value, identity>

  let fromArray = a => Obj.magic(Poly.fromArray(a))

  let empty = fromArray([])

  let singleton = (~key, ~value) => fromArray([(key, value)])

  let fromList = l => fromArray(Array.of_list(l))
}

module String = {
  type identity

  type t<'value> = t<TableclothString.t, 'value, identity>

  let fromArray = a => Obj.magic(Poly.fromArray(a))

  let empty = fromArray([])

  let singleton = (~key, ~value) => fromArray([(key, value)])

  let fromList = l => fromArray(Array.of_list(l))
}
