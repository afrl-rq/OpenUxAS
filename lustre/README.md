OpenUxAS/lustre
===============

This directory contains a Lustre model of a subset of services and messages in OpenUxAS.
The purpose of the model is to explore a multi-language, compositional approach to proofs of putative properties about multiple, interacting services at the OpenUxAS framework level.
The top-level node in the model is contained in `uxas.lus`.

Properties in the model can be proved using `kind2`.
The analysis may require several hours to complete, depending on the computer used and the mode in which `kind2` is run.


# Background #

## Lustre ##

Lustre is a functional programming language with formal semantics that was originally designed to specify the cyclic execution of synchronous computations.
For an introduction to Lustre, consult the tutorial here: http://www-verimag.imag.fr/~halbwach/lustre-tutorial.html.
We use Lustre for this model because Lustre is the input language for a class of interesting tools, including modern infinite-state model checkers like `kind2`.

Lustre is syntactically poor, compared to many other languages, and is therefore not difficult to learn.
Two key differences between Lustre and typical programming languages are worth highlighting:

1. Lustre programs operate on _streams_ that represent sequences of values over time.
   This feature of the language is presented in the introductory paragraph of the above tutorial.

2. The Lustre _arrow_ operator (`->`) is *not* implication.
   The operator allows the programmer to designate the value to be used when an expression would otherwise be `nil`.
   The arrow operator is used in conjunction with `pre` to designate the value to be used when there is no prior value for an expression.

### VS Code Extension ###

There is a [VS Code](#https://code.visualstudio.com/) extension for Lustre: https://github.com/MercierCorentin/vscode-lustre that offers syntax highlighting and comment/uncomment functionality.
The support for `kind2`'s Lustre is incomplete but generally adequate.

## kind2 ##

`kind2` is an open-source model checker developed at the University of Iowa: https://kind2-mc.github.io/kind2/

### Installing Kind2 ###

The easiest way to install `kind2` is via `opam`, the OCaml package manager.
Follow these steps:

    $ sh <(curl -sL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)
    $ opam init
    $ opam install opam-depext
    $ opam depext kind2
    $ opam install z3
    $ opam install kind2

### Installing z3 ###

`z3` is an open-source SMT solver from Microsoft: https://github.com/Z3Prover/z3

The install of `kind2`, above, doesn't install the `z3` binary, only the `z3` bindings for OCaml.
So you need to install `z3`, e.g., by downloading and installing a release or building from source.

## Running kind2 ##

To run `kind2`, you first need to export the `opam`-provided environment:

    $ eval "$( opam env )"

Then, you can run `kind2` to reprove all properties on all nodes of the model, like this:

    OpenUxAS/lustre$ kind2 uxas.lus --modular true

This will take multiple hours.
If you prefer to see output from the tool while it is working, you can add `-v` to the command, like this:

    OpenUxAS/lustre$ kind2 uxas.lus --modular true -v

When complete, you should see that:

  * all properties with single-line text descriptions are falsified.
    These properties are requests that the model checker find a _witness_ for a property or behavior of instance in the model.
    We use these witnesses to build confidence that the model is operating as expected and to guard against vacuity in properties that are stated using implication.

  * all properties with multi-line text descriptions are proved.

The formatting of the results has been tailored to make checking witnesses v properties easier.

Additional details on proving the model are presented below.


# Model #

The OpenUxAS model is contained in `uxas.lus` and references files in `common` and `services`.
This section provides a high-level summary of the model and its supporting files.

The overall intent of the model is to illustrate how Lustre can be used to reason about data dependencies amonst messages that are sent and received at different points in time.
This objective is challenging: first-order-logic frameworks are good at reasoning about data dependencies but generally bad at reasoning about the ordering of events over time; temporal-logic frameworks are good at reasoning about the ordering of events but generally bad at reasoning about data relationships.

This kind of reasoning supports properties that explore how services work together to achieve framework objectives.

In the Lustre model, we have made an effort to restrict our use of temporal reasoning to the properties.
In the description of the service behaviors, we use only the `pre` operator to reference prior states of variables.
Moreover, we have phrased the description of service behaviors as much like software specifications as possible: we focus on what the service must do and attempt to be vague about specifically how the service will do these things.
This is somewhat difficult as Lustre generally requires that we completely specify the value of variables.

Our vision is that each service would be implemented in SPARK.
Each equation in the Lustre would become an element of the contract for the service in SPARK.
The Lustre model is written in such a way that it should be easy to rephrase each equation in SPARK - the simplicity of this translation should boost confidence that the SPARK contracts are a correct representation of the Lustre behavior.

Once the SPARK services are implemented and the implementation is proven to conform to the SPARK contracts, we should have confidence that the high-level behaviors of OpenUxAS are implied by the implementation:

  * the SPARK conforms to its contracts;
  * the SPARK contracts are a correct rephrasing of the Lustre service behaviors (an informal argument supported by inspection);
  * the Lustre service behavior conforms to service requirements (phrased as putative properties of the service);
  * service requriements imply OpenUxAS framework properties (phrased as putative properties of the framework).

Currently, the SPARK implementations of the services, where they exist, *are not* tied to the OpenUxAS lustre model.
New / updated implementations are still pending.
Thus the above argument remains incomplete.

> Note: Lustre does not provide an explicit means to quantify over types: propositions in Lustre must be quantifier free.
> We nevertheless need to quantify over types, specifically over messages, to state many of our properties.
>
> We work around this limitation in Lustre by introducing a _free constant_ that represents the skolemization of a universally quantified variable.
> An an example, if we want to prove:
>
>     forall a, b : Integer; b > 0 => a < a + b
>
> We could introduce two free constant integers x and y and say, without loss of generality that a = x and b = y, then
>
>     check ( y > 0 ) => ( x < x + y )
>
> This works because the model checker will explore all possible assignments to the free constants for the evaluation of the property.
> Essentially, the model checker performs universal quantification for us.
>
> These free constants are clearly identified in the model.

## uxas ##

The OpenUxAS model is described in the node `uxas`.
The node takes no input and returns no output (there is a return value, `r`, that is required by Lustre syntax; it is unused).

The `uxas` node relies on a bus abstraction for loop closure of messages.
The bus takes the previous output message as its input and returns an input message, which may be empty.
The bus also returns the ID of the last non-empty message; we use this ID for enforcing monotonicity and uniqueness of message IDs.

The `uxas` node then invokes both services.
The model is currently configured so that only one of the two services may respond with a non-empty message; this is an opportunity for improvement: the bus could take in a vector of messages and select from the vector in a random order to sequence message outputs.
This enhancement would certainly increase the complexity of the model; it may allow the model to exhibit more and more interesting behaviors.

Once the services have executed, the non-empty message returned by the services, if any, becomes the output message to the bus for the next time step.

The remainder of the `uxas` node expresses properties of interest.
These are grouped by purpose:

  * initial conditions;
  * evironment consistency checks;
  * properties;
  * compositional properties, which represent OpenUxAS-level requirements;
  * witnesses of model behaviors; and
  * witnesses of antecedents.

## Common ##

The `uxas` node relies on four files in the `common` directory:

  1. `types.lus`
  2. `sequences_simple.lus`
  3. `bus.lus`
  4. `pltl.lus`

### types.lus ###

Type definitions shared across the services and the `uxas` note are grouped here.
There are no nodes and no properties described here.

There is one function of interest, which expresses a critical predicate over our messages: `ids_imply_equality`.
This predicate checks that if two messages have the same ID, the two messages are identical.
We prove that this predicate holds as a property expressed on all messages sent and received in the `uxas` node and rely on this predicate in the antecedent of both compositional properties for the `uxas` node.

### sequences_simple.lus ###

Lustre does not have any notion of a sequence or an unbounded array.
All arrays must be of fixed, constant size.
Routes in our model, however, are naturally described as a sequence of waypoints.
Moreover, there is an important data relationship that we wish to prove: that waypoints transmitted in Mission Command messages are contained in routes sent in prior messages.

To allow us to describe and prove this property, we have introduced a partial theory of sequences.
This version, `sequences_simple`, is a simplication of the more general (but still partial) theory described in `sequences.lus` - that theory makes use of uninterpreted functions, which are not supported by IC3.
Since we need IC3 to complete our proofs automatically, we use `sequences_simple`, instead.

The details of the theory are out of scope for this README.
The theory is documented and contains many properties and witnesses intended to demonstrate the internal consistency of theory, demonstrate its usage, and build confidence in its correctness.

### bus.lus ###

The `bus` node is very simply defined.
Because the `bus` node needs to be able to make a nondeterministic choice in its output, we describe the behavior using an assertion rather than explicitly defining the behavior useing equations.

### pltl.lus ###

PLTL operators are implemented as nodes that witness the condition under which the operator should be true.
This implementation is adapted from https://github.com/lgwagner/pattern-observers/blob/master/pltl/pltl.lus

## Services ##

Two services are implemented in the model and included in the `services` directory:

  1. `automation-request-validator.lus`
  2. `waypoint-manager.lus`

### automation_request_validator.lus ###

This simple service receives a Unique Automation Response message and sends an Automation Response message.
Data and temporal relationships amongst the messages are stated as requirements and proved.
A contact is provided for the service that is sufficient to allow successful contract-based proof of the properties in the `uxas` node.

### waypoint_manager.lus ###

This service receives an Automation Response message and sends Mission Command messages containing the waypoints from the route in the Automation Response message, in order.
Data and temporal relationships amongst the messages are stated as requirements and proved.
A contact is provided for the service that is sufficient to allow successful contract-based proof of the properties in the `uxas` node.

# Analysis #

## Top-Level Proof ##

You can use `kind2` to prove the properties at the top-level like this:

    OpenUxAS/lustre$ kind2 uxas.lus

This will take multiple hours.

When complete, you should see that:

  * all properties with single-line text descriptions are falsified.
    These properties are requests that the model checker find a _witness_ for a property or behavior of instance in the model.
    We use these witnesses to build confidence that the model is operating as expected and to guard against vacuity in properties that are stated using implication.

  * all properties with multi-line text descriptions are proved.

The formatting of the results has been tailored to make checking witnesses v properties easier.

## Recursive Proof ##

You can use `kind2` to prove the top-level node and all properties on all called nodes like this:

    OpenUxAS/lustre$ kind2 uxas.lus --modular true

This will take multiple hours.

## Contract-based Proof ##

`kind2` offers an option for contract-based proof, which labels calls "compositional" proof.
In this mode, all properties at the top level of the OpenUxAS model can be proved in a few minutes.
You run this mode like this:

    OpenUxAS/lustre$ kind2 uxas.lus --compositional true

## Proving the Services ##

You can prove the properties on the services like this:

    OpenUxAS/lustre$ kind2 services/automation-request-validator.lus
    OpenUxAS/lustre$ kind2 services/waypoint-manager.lus

## Proving the Sequences Theory ##

Properties on the sequences theory (see below) can be proved like this:

    OpenUxAS/lustre$ kind2 common/sequences_simple.lus

## Suppressing Witnesses ##

The output from the witnesses - which present as proof failures - can make reading the output of the tool more difficult.
Also, when new properties are introduced that may not immediately prove, they can make finding the useful counterexample for the new property more difficult.
To ease this, the witness properties can be easily disabled.

In the `uxas`, `automation_request_validator`, and `waypoint_manager` nodes, the witnesses are grouped at the bottom of the node.
This way, they can easily be commented out so that they will not be considered during analysis.
