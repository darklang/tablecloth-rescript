type t<'a> = list<'a>

let empty = list{}

let singleton = x => list{x}

let fromArray = array => List.init(Array.length(array), i => array[i])

let range = (~from=0, to_) =>
  if to_ < from {
    list{}
  } else {
    List.init(to_ - from, i => i + from)
  }

let rec repeat = (element, ~times) =>
  if times <= 0 {
    list{}
  } else {
    list{element, ...repeat(element, ~times=times - 1)}
  }

let flatten = t => Belt.List.flatten(t)

let reverse = t => Belt.List.reverse(t)

let append = (a, b) => Belt.List.concat(a, b)

let sum = (type a, t, module(M: TableclothContainer.Sum with type t = a)) =>
  List.fold_left(M.add, M.zero, t)

let map = (t, ~f) => Belt.List.map(t, a => f(a))

let flatMap = (t, ~f) => flatten(map(t, ~f))

let mapWithIndex = (list, ~f) => Belt.List.mapWithIndex(list, (a, b) => f(a, b))

let map2 = (a, b, ~f) => Belt.List.zipBy(a, b, (a, b) => f(a, b))

let zip = (a, b) => map2(a, b, ~f=(a, b) => (a, b))

let rec map3 = (a, b, c, ~f) =>
  switch (a, b, c) {
  | (list{x, ...xs}, list{y, ...ys}, list{z, ...zs}) => list{f(x, y, z), ...map3(xs, ys, zs, ~f)}
  | _ => list{}
  }

let rec last = l =>
  switch l {
  | list{} => None
  | list{x} => Some(x)
  | list{_, ...rest} => last(rest)
  }

let unzip = list => (List.map(((a, _)) => a, list), List.map(((_, b)) => b, list))

let includes = (t, value, ~equal) => Belt.List.has(t, value, (a, b) => equal(a, b))

let uniqueBy = (l: list<'a>, ~f: 'a => string): list<'a> => {
  let rec uniqueHelper = (
    f: 'a => string,
    existing: Belt.Set.String.t,
    remaining: list<'a>,
    accumulator: list<'a>,
  ) =>
    switch remaining {
    | list{} => reverse(accumulator)
    | list{first, ...rest} =>
      let computedFirst = f(first)
      if Belt.Set.String.has(existing, computedFirst) {
        uniqueHelper(f, existing, rest, accumulator)
      } else {
        uniqueHelper(
          f,
          Belt.Set.String.add(existing, computedFirst),
          rest,
          list{first, ...accumulator},
        )
      }
    }

  uniqueHelper(f, Belt.Set.String.empty, l, list{})
}

let find = (t, ~f) => Belt.List.getBy(t, a => f(a))

let getAt = (t, ~index) => Belt.List.get(t, index)

let any = (t, ~f) => List.exists(a => f(a), t)

let head = l => Belt.List.head(l)

let drop = (t, ~count) =>
  switch Belt.List.drop(t, count) {
  | None =>
    if count <= 0 {
      t
    } else {
      list{}
    }
  | Some(v) => v
  }

let take = (t, ~count) =>
  switch Belt.List.take(t, count) {
  | None =>
    if count <= 0 {
      list{}
    } else {
      t
    }
  | Some(v) => v
  }

let initial = l =>
  switch reverse(l) {
  | list{} => None
  | list{_, ...rest} => Some(reverse(rest))
  }

let filterMap = (t, ~f) => Belt.List.keepMap(t, a => f(a))

let filter = (t, ~f) => Belt.List.keep(t, a => f(a))

let filterWithIndex = (t, ~f) => Belt.List.keepWithIndex(t, (e, i) => f(i, e))

let partition = (t, ~f) => Belt.List.partition(t, a => f(a))

let fold = (t, ~initial, ~f) => Belt.List.reduce(t, initial, (a, b) => f(a, b))

let count = (t, ~f) => fold(t, ~initial=0, ~f=(total, element) => total + (f(element) ? 1 : 0))

let foldRight = (t, ~initial, ~f) => Belt.List.reduceReverse(t, initial, (a, b) => f(a, b))

let findIndex = (list, ~f) => {
  let rec loop = (i, l) =>
    switch l {
    | list{} => None
    | list{x, ...rest} =>
      if f(i, x) {
        Some(i, x)
      } else {
        loop(i + 1, rest)
      }
    }

  loop(0, list)
}

let splitAt = (t, ~index) => (take(~count=index, t), drop(~count=index, t))

let updateAt: (t<'a>, ~index: int, ~f: 'a => 'a) => t<'a> = (t, ~index, ~f) =>
  Belt.List.mapWithIndex(t, (i, element) =>
    if i == index {
      f(element)
    } else {
      element
    }
  )

let length = l => Belt.List.length(l)

let rec dropWhile = (t, ~f) =>
  switch t {
  | list{} => list{}
  | list{x, ...rest} =>
    if f(x) {
      dropWhile(rest, ~f)
    } else {
      t
    }
  }

let isEmpty = t => t == list{}

let sliding = (~step=1, t, ~size) => {
  let rec loop = t =>
    if isEmpty(t) {
      list{}
    } else {
      let sample = Belt.List.take(t, size)
      let rest = Belt.List.drop(t, step)
      switch (sample, rest) {
      | (None, _) => list{}
      | (Some(x), None) => list{x}
      | (Some(x), Some(xs)) => list{x, ...loop(xs)}
      }
    }

  loop(t)
}

let chunksOf = (t, ~size) => sliding(t, ~step=size, ~size)

let cons = (t, element) => list{element, ...t}

let takeWhile = (t, ~f) => {
  let rec takeWhileHelper = (acc, t) =>
    switch t {
    | list{} => reverse(acc)
    | list{x, ...rest} =>
      if f(x) {
        takeWhileHelper(list{x, ...acc}, rest)
      } else {
        reverse(acc)
      }
    }

  takeWhileHelper(list{}, t)
}

let all = (t, ~f) => Belt.List.every(t, a => f(a))

let tail = t =>
  switch t {
  | list{} => None
  | list{_, ...rest} => Some(rest)
  }

let removeAt = (t, ~index) =>
  if index < 0 {
    t
  } else {
    let (front, back): (t<'a>, t<'a>) = splitAt(t, ~index)
    switch tail(back) {
    | None => t
    | Some(t) => append(front, t)
    }
  }

let minimumBy = (~f: 'a => 'comparable, l: list<'a>): option<'a> => {
  let minBy = ((y, fy), x) => {
    let fx = f(x)
    if fx < fy {
      (x, fx)
    } else {
      (y, fy)
    }
  }

  switch l {
  | list{} => None
  | list{x} => Some(x)
  | list{x, ...rest} => Some(fst(fold(~f=minBy, ~initial=(x, f(x)), rest)))
  }
}

let maximumBy = (~f: 'a => 'comparable, l: list<'a>): option<'a> => {
  let maxBy = ((y, fy), x) => {
    let fx = f(x)
    if fx > fy {
      (x, fx)
    } else {
      (y, fy)
    }
  }

  switch l {
  | list{} => None
  | list{x} => Some(x)
  | list{x, ...rest} => Some(fst(fold(~f=maxBy, ~initial=(x, f(x)), rest)))
  }
}

let minimum = (t, ~compare) =>
  fold(t, ~initial=None, ~f=(min, element) =>
    switch min {
    | None => Some(element)
    | Some(value) => compare(element, value) < 0 ? Some(element) : min
    }
  )

let maximum = (t, ~compare) =>
  fold(t, ~initial=None, ~f=(max, element) =>
    switch max {
    | None => Some(element)
    | Some(value) => compare(element, value) > 0 ? Some(element) : max
    }
  )

let extent = (t, ~compare) =>
  fold(t, ~initial=None, ~f=(current, element) =>
    switch current {
    | None => Some(element, element)
    | Some(min, max) =>
      Some(compare(element, min) < 0 ? element : min, compare(element, max) > 0 ? element : max)
    }
  )

let sort = (t, ~compare) => Belt.List.sort(t, (a, b) => compare(a, b))

let sortBy = (l: t<'a>, ~f: 'a => 'b): t<'a> =>
  Belt.List.sort(l, (a, b) => {
    let a' = f(a)
    let b' = f(b)
    if a' == b' {
      0
    } else if a' < b' {
      -1
    } else {
      1
    }
  })

let groupi = (l, ~break) => {
  let groups = Belt.List.reduceWithIndex(l, list{}, (acc, x, i) =>
    switch acc {
    | list{} => list{list{x}}
    | list{current_group, ...tl} =>
      if break(i, Belt.List.headExn(current_group), x) {
        list{list{x}, current_group, ...tl} /* start new group */
      } else {
        list{list{x, ...current_group}, ...tl}
      }
    }
  )
  /* extend current group */

  switch groups {
  | list{} => list{}
  | l => Belt.List.mapReverse(l, reverse)
  }
}

let groupWhile = (l, ~f) => groupi(l, ~break=(_, x, y) => f(x, y))

let insertAt = (t, ~index, ~value) => {
  let (front, back) = splitAt(t, ~index)
  append(front, list{value, ...back})
}

let splitWhen = (t, ~f) => {
  let rec loop = (front, back) =>
    switch back {
    | list{} => (t, list{})
    | list{element, ...rest} =>
      if f(element) {
        (reverse(front), back)
      } else {
        loop(list{element, ...front}, rest)
      }
    }

  loop(list{}, t)
}

let intersperse = (t, ~sep) =>
  switch t {
  | list{} => list{}
  | list{x} => list{x}
  | list{x, ...rest} =>
    list{x, ...foldRight(rest, ~initial=list{}, ~f=(acc, x) => list{sep, x, ...acc})}
  }

let initialize = (length, ~f) => Belt.List.makeBy(length, a => f(a))

let forEach = (t, ~f): unit => Belt.List.forEach(t, a => f(a))

let forEachWithIndex = (t, ~f): unit => Belt.List.forEachWithIndex(t, (a, b) => f(a, b))

let toArray = t => Array.of_list(t)

let join = (strings, ~sep) => Js.Array.joinWith(sep, toArray(strings))

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

let rec equal = (a, b, equalElement) =>
  switch (a, b) {
  | (list{}, list{}) => true
  | (list{x, ...xs}, list{y, ...ys}) => equalElement(x, y) && equal(xs, ys, equalElement)
  | _ => false
  }

let rec compare = (a, b, compareElement) =>
  switch (a, b) {
  | (list{}, list{}) => 0
  | (list{}, _) => -1
  | (_, list{}) => 1
  | (list{x, ...xs}, list{y, ...ys}) =>
    switch compareElement(x, y) {
    | 0 => compare(xs, ys, compareElement)
    | result => result
    }
  }
