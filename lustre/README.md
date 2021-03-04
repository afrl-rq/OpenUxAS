OpenUxAS/lustre
===============

This directory contains a Lustre model of a subset of services and messages in OpenUxAS.
The purpose of the model is to explore specification at a fairly high level of abstraction and to promote proof of putative properties at this level of abstraction.
The secondary purpose of the model is to envision how we might link a SPARK implementation into this kind of abstract model.
The model is contained in `uxas.lus`.

For an introduction to Lustre, consult the tutorial here: http://www-verimag.imag.fr/~halbwach/lustre-tutorial.html.
The introductory paragram in this document is extremely important: Lustre nodes (subprograms) operate on streams that represent sequences of values over time.

Since the purpose of the model is analysis, you will want to install an appropriate tool for analysis.
There are two options: `jkind` and `kind2`.
`jkind` is prefered, as it is more stable and less prone to unexpected issues with unsoundness.
`kind2` is more cutting edge and has arguably prettier output.

# Installing and Running jkind #

You can install `jkind` by downloading the latest relese from github: https://github.com/loonwerks/jkind
You will need Java to run `jkind` (the J is for Java).

Once installed, you can analyze the model like this:

    OpenUxAS-Lustre$ /path/to/jkind uxas.lus

You should see that all properties are proved to be valid.
The warnings related to ignored assertions and unguarded pre expressions are expected.

# Installing and Running kind2 #

If you want, you can also analyze some of the models using `kind2`.
Note that some models may use `jkind`-specific features and thus will not be compatible with `kind2`.

To install `kind2`, there are several steps that must be followed.

## Installing kind2 ##

The easiest way to do this is via `opam`, the OCaml package manager.
These steps worked for me (the steps on the `kind2` github did not quite work):

    $ sh <(curl -sL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)
    $ opam init
    $ opam install opam-depext
    $ opam depext kind2
    $ opam install z3
    $ opam install kind2

## Installing z3 ##

The install of `kind2`, above, doesn't install the `z3` binary, only the `z3` bindings for OCaml.
So you need to install `z3`, e.g., by downloading and installing a release or building from source.

## Running kind2 ##

To run `kind2`, you first need to export the `opam`-provided environment:

    $ eval "$( opam env )"

Then, you can run `kind2`, like this:

    OpenUxAS-Lustre$ kind2 uxas.lus --modular true

You should see that all properties are proved.
Here, too, there will be warnings about unguarded pre expressions.

# Model #

Lustre is a syntax-poor language.
This makes Lustre very easy to learn (and support in tools) but relatively difficult to use effectively.

This model makes heavy use of a particular pattern common in k-inductive model checking to effect quantification over types (which is not directly supported except in specific, experimental extensions to the language offered by some, but not all, tools).
In this pattern, we effect quantification by introducing a freely-chosen constant input to the node being analyzed.
This constant is effectively the skolemization of a universal quantifer over its type.
That is, the model checker is free to choose the value of the constant without loss of generality — which is the skolemization of the universal quantifier.

Because Lustre doesn't have syntax to declare that a stream is constant but freely chosen, we effect this constant by constraining the allowed values of the stream like this:

    assert (skolem_input = pre(skolem_input));

That is, after a value for `skolem_input` has been chosen, the model checker is not allowed to change its value.
Because the pre expression is unguarded, there is no constraint on the initial choice of value for this constant.
Usually, we don't want unguarded pre expressions; in this case, however, since the purpose is to enable analysis, this makes our analysis maximally conservative.

By constraining the values of interest for this skolemized quantifier on both sides of an implication, we can effectively introduce an existential quantifier.
We ask the model model checker to prove, in essence, that there is an assignment to the freely chosen constant that satisfies our property.
So if we want to prove that the current value of a stream was taken on by another stream at some point in the past, we can write this:

    stream = skolem_input => once(other_stream = skolem_input)

This says, in essence, that there exists a value of `skolem_input` such that `stream` has that value now and `other_stream` had that same value at some point in the past.

We use this pattern multiple times in the model to search the history for previous messages whose identifier matches the reference field of a current message, for example.
