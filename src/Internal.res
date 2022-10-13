let toBeltComparator = (
  type a id,
  module(M: TableclothComparator.S with type identity = id and type t = a),
): Belt.Id.comparable<a, id> =>
  module(
    {
      type t = M.t

      type identity = M.identity

      let cmp = Obj.magic(M.comparator)
    }
  )

