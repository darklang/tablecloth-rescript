/** This module contains module signatures which are used in functions which
    accept first class modules.
*/
module type Sum = {
  /** Modules which conform to this signature can be used with functions like
      {!Array.sum} or {!List.sum}.
  */
  type t

  let zero: t

  let add: (t, t) => t
}
