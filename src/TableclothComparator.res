type t<'a, 'identity> = Belt.Id.cmp<'a, 'identity>

type comparator<'a, 'identity> = t<'a, 'identity>

module type T = {
  type t

  let compare: (t, t) => int
}

module type S = {
  type t

  type identity

  let comparator: comparator<t, identity>
}

type s<'a, 'identity> = module(S with type identity = 'identity and type t = 'a)

module Make = (M: T): (S with type t = M.t) => {
  module BeltComparator = Belt.Id.MakeComparableU({
    type t = M.t

    let cmp = M.compare
  })

  type t = M.t

  type identity = BeltComparator.identity

  let comparator = BeltComparator.cmp
}
