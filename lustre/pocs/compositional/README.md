compositional
=============

This small example illustrates `kind2`'s compositional reasoning facilities.
To see this, run `kind2` like this:

    kind2 bar.lus --compositional true

`kind2` will then effectively ignore the node bodies for `foo1` and `foo2` and will instead rely on the contracts for proof.
`kind2` will also prove that the preconditions for `foo1` and `foo2` are satisfied at the point of call.
You can convince yourself of this by, e.g., breaking the implementation of foo1 by sbustracting `b` from `a`.
When you do this, `kind2` will report that "nonnegative r" holds for bar even though it is violated in `foo1`.

You can see compositional proof fail by breaking a the contract, e.g., to declare that `foo1` or `foo2` may return a negative number.

To see compositional proof alongside local proof of each node, call `kind2` like this:

    kind2 bar.lus --compositional true --modular true

