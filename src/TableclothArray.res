type t<'a> = array<'a>

let singleton = a => [a]

let clone = t => Array.map(TableclothFun.identity, t)

let length = Belt.Array.length

let isEmpty = a => length(a) == 0

let initialize = (length, ~f) => Belt.Array.makeBy(length, f)

let range = (~from=0, to_) => Belt.Array.makeBy(to_ - from, i => i + from)

let fromList = Belt.List.toArray

let toList: array<'a> => list<'a> = Belt.List.fromArray

let toIndexedList = (array: array<'a>): list<(int, 'a)> =>
  Belt.Array.reduceReverse(array, (length(array) - 1, list{}), ((i, acc), x) => (
    i - 1,
    list{(i, x), ...acc},
  )) |> snd

let get = Belt.Array.getExn

let getAt = (t, ~index) => Belt.Array.get(t, index)

let first = t => getAt(t, ~index=0)

let last = t => getAt(t, ~index=Array.length(t) - 1)

let set = (t, index, value) => t[index] = value

let setAt = (t, ~index, ~value) => t[index] = value

let filter = (t, ~f) => Belt.Array.keep(t, f)

let swap = (t, i, j) => {
  let temp = t[i]
  t[i] = t[j]
  t[j] = temp
  ()
}

let fold = (t, ~initial, ~f) => Belt.Array.reduce(t, initial, f)

let foldRight = (t, ~initial, ~f) => Belt.Array.reduceReverse(t, initial, f)

let maximum = (t, ~compare) =>
  fold(t, ~initial=None, ~f=(max, element) =>
    switch max {
    | None => Some(element)
    | Some(current) => compare(element, current) > 0 ? Some(element) : max
    }
  )

let minimum = (t, ~compare) =>
  fold(t, ~initial=None, ~f=(min, element) =>
    switch min {
    | None => Some(element)
    | Some(current) => compare(element, current) < 0 ? Some(element) : min
    }
  )

let extent = (t, ~compare) =>
  fold(t, ~initial=None, ~f=(range, element) =>
    switch range {
    | None => Some(element, element)
    | Some(min, max) =>
      Some(compare(element, min) < 0 ? element : min, compare(element, max) > 0 ? element : max)
    }
  )

let sum = (type a, t, module(M: TableclothContainer.Sum with type t = a)): a =>
  Array.fold_left(M.add, M.zero, t)

let map = (t, ~f) => Belt.Array.map(t, f)

let mapWithIndex = (t, ~f) => Belt.Array.mapWithIndex(t, f)

let map2 = (a, b, ~f: ('a, 'b) => 'c): array<'c> => Belt.Array.zipBy(a, b, f)

let map3 = (as_, bs, cs: t<'c>, ~f) => {
  let minLength = Belt.Array.reduce([length(bs), length(cs)], length(as_), min)

  Belt.Array.makeBy(minLength, i => f(as_[i], bs[i], cs[i]))
}

let zip = map2(~f=(a, b) => (a, b))

let flatMap = (t, ~f) => Belt.Array.map(t, f) |> Belt.Array.concatMany

let sliding = (~step=1, a, ~size) => {
  let n = Array.length(a)
  if size > n {
    []
  } else {
    initialize(1 + (n - size) / step, ~f=i => initialize(size, ~f=j => a[i * step + j]))
  }
}

let find = (t, ~f) => {
  let rec find_loop = (t, ~f, ~length, i) =>
    if i >= length {
      None
    } else if f(t[i]) {
      Some(t[i])
    } else {
      find_loop(t, ~f, ~length, i + 1)
    }

  find_loop(t, ~f, ~length=length(t), 0)
}

let findIndex = (array, ~f) => {
  let rec loop = index =>
    if index >= length(array) {
      None
    } else if f(index, array[index]) {
      Some(index, array[index])
    } else {
      loop(index + 1)
    }

  loop(0)
}

let any = (t, ~f) => Belt.Array.some(t, f)

let all = (t, ~f) => Belt.Array.every(t, f)

let includes = (t, v, ~equal) => any(t, ~f=equal(v))

let append = (a, a') => Belt.Array.concat(a, a')

let flatten = (ars: array<array<'a>>) => Belt.Array.concatMany(ars)

let intersperse = (t, ~sep) =>
  Belt.Array.makeBy(max(0, length(t) * 2 - 1), i =>
    if mod(i, 2) != 0 {
      sep
    } else {
      t[i / 2]
    }
  )

let slice = (~to_=?, array, ~from) => {
  let defaultTo = switch to_ {
  | None => length(array)
  | Some(i) => i
  }
  let sliceFrom = if from >= 0 {
    min(length(array), from)
  } else {
    max(0, min(length(array), length(array) + from))
  }

  let sliceTo = if defaultTo >= 0 {
    min(length(array), defaultTo)
  } else {
    max(0, min(length(array), length(array) + defaultTo))
  }

  if sliceFrom >= sliceTo {
    []
  } else {
    Belt.Array.makeBy(sliceTo - sliceFrom, i => array[i + sliceFrom])
  }
}

let count = (t, ~f) => fold(t, ~initial=0, ~f=(total, element) => total + (f(element) ? 1 : 0))

let chunksOf = (t, ~size) => sliding(t, ~step=size, ~size)

let reverse = Belt.Array.reverseInPlace

let forEach = (t, ~f): unit => Belt.Array.forEach(t, f)

let forEachWithIndex = (t, ~f): unit =>
  for i in 0 to length(t) - 1 {
    f(i, t[i])
  }

let partition = (t, ~f) => {
  let (left, right) = foldRight(t, ~initial=(list{}, list{}), ~f=((lefts, rights), element) =>
    if f(element) {
      (list{element, ...lefts}, rights)
    } else {
      (lefts, list{element, ...rights})
    }
  )

  (fromList(left), fromList(right))
}

let splitAt = (t, ~index) => (slice(t, ~from=0, ~to_=index), slice(t, ~from=index, ~to_=length(t)))

let splitWhen = (t, ~f) =>
  switch findIndex(t, ~f=(_, e) => f(e)) {
  | None => (t, [])
  | Some(index, _) => splitAt(t, ~index)
  }

let unzip = t => (Array.init(length(t), i => fst(t[i])), Array.init(length(t), i => snd(t[i])))

let repeat = (element, ~length) => Array.init(max(length, 0), _ => element)

let filterMap = (t, ~f) => {
  let result = fold(t, ~initial=list{}, ~f=(results, element) =>
    switch f(element) {
    | None => results
    | Some(value) => list{value, ...results}
    }
  )->fromList

  reverse(result)
  result
}

let sort = (a, ~compare) => Array.sort(compare, a)

let values = t => {
  let result = fold(t, ~initial=list{}, ~f=(results, element) =>
    switch element {
    | None => results
    | Some(value) => list{value, ...results}
    }
  )->fromList

  reverse(result)
  result
}

let join = (t, ~sep) => Js.Array.joinWith(sep, t)

let groupBy = (t, comparator, ~f) =>
  fold(t, ~initial=TableclothMap.empty(comparator), ~f=(map, element) => {
    let key = f(element)
    TableclothMap.update(map, ~key, ~f=x =>
      switch x {
      | None => Some(list{element})
      | Some(elements) => Some(list{element, ...elements})
      }
    )
  })

let equal = (a, b, equal) =>
  if length(a) != length(b) {
    false
  } else if length(a) == 0 {
    true
  } else {
    let rec loop = index =>
      if index == length(a) {
        true
      } else {
        equal(a[index], b[index]) && loop(index + 1)
      }

    loop(0)
  }

let compare = (a, b, compare) =>
  switch TableclothInt.compare(length(a), length(b)) {
  | 0 =>
    if length(a) === 0 {
      0
    } else {
      let rec loop = index =>
        if index == length(a) {
          0
        } else {
          switch compare(a[index], b[index]) {
          | 0 => loop(index + 1)
          | result => result
          }
        }

      loop(0)
    }
  | result => result
  }

