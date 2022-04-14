# strict-pair

Strict.Pair provides a strict equivalent to `(,)` and its functions in Data.Tuple.
`data Pair a b = !a :!: !b` is equivalent to `data (a, b) = (a, b)`, except it evaluates its contents (to weak head normal form).
`fst'`, `snd'`, `curry'`, and `uncurry'` are equivalent to `fst`, `snd`, `curry` and `uncurry`.
Strict.Pair.Infix provides the infix alias `type (:!:) = Pair`.

`Pair` is useful when recursively constructing tuples of tuples. For example `pwxyz :: Pair (a, b) (c, d)` has the same strictness as `t4wxyz :: (a, b, c, d)`, while `t2wxyz :: ((a, b), (c, d))` can hide errors like `(undefined, (w, z))` or space leaks like `((x, y), hugeThunk)`.
