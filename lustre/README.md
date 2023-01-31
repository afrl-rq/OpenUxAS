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

## kind2 ##

`kind2` is an open-source model checker developed at the University of Iowa: https://kind2-mc.github.io/kind2/

### kind2 VS Code Extension ###

There is a an extension for VS Code available for kind2: https://marketplace.visualstudio.com/items?itemName=kind2-mc.vscode-kind2

This extension offers excellent syntax highlighting and coding support for lustre, as well as integrated commands for checking and simulating lustre nodes.
The extension does not support compositional analysis, which is required to prove the OpenUxAS properties.
You should thus also install the command-line tool, `kind2`.

### Installing Kind2 ###

The easiest way to install `kind2` is via `opam`, the OCaml package manager.
Follow these steps:

    $ sh <(curl -sL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)
    $ opam init
    $ opam install --update-invariant kind2

If you do not already have z3 installed, you can install it with `opam`:

    $ opam install z3

This takes a while.

## Running kind2 ##

To run `kind2`, you first need to export the `opam`-provided environment:

    $ eval "$( opam env )"

Then, you can run `kind2` to reprove all properties on all nodes of the model, like this:

    kind2 uxas.lus --lus_main uxas_props --compositional true --modular true

This will take a few minutes, depending on your hardware, and will result in a final message like this:

    Analysis breakdown, total runtime xx.xxxs seconds:
      automation_request_validator:
        safe in xx.xxxs
        without refinement: 0 abstract systems
        5 properties
      waypoint_manager:
        safe in xx.xxxs
        without refinement: 0 abstract systems
        9 properties
      task:
        safe in xx.xxxs
        without refinement: 0 abstract systems
        11 properties
      uxas_props:
        safe in xx.xxxs
        without refinement: 5 abstract systems
        5 properties

Above this message, `kind2` provides the result of analyzing each of the lustre nodes mentioned.

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
The bus takes the previous message output from the services as its input and returns an input message, which may be empty.

The `uxas` node memorizes the ID of the last non-empty message; we use this ID for enforcing monotonicity and uniqueness of message IDs.

The `uxas` node then invokes all services.

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

The `bus` node is used to down select the output from the services and to provide loop closure.
The bus receives an input from each service, but assumes that at most one services provides a message to the bus at each time step.
The bus outputs whatever message was received, or no message, if no message was received.
The bus also outputs a freely-chosen token that designates which service is allowed to send a message on the next time step.
This allows arbitrary interleavings of messages and quiescence.

### pltl.lus ###

PLTL operators are implemented as nodes that witness the condition under which the operator should be true.
This implementation is adapted from https://github.com/lgwagner/pattern-observers/blob/master/pltl/pltl.lus

## Services ##

The following services are implemented in the model and included in the `services` directory:

  1. `plan-builder.lus`
  2. `automation-request-validator.lus`
  3. `waypoint-manager.lus`
  4. `task.lus`

### plan-builder.lus ###

This simple service acts as a source for initiating events in the model.
It nondeterministically sends either an empty message or a new Unique Automation Response message.

### automation-request-validator.lus ###

This simple service receives a Unique Automation Response message and sends an Automation Response message.
Data and temporal relationships amongst the messages are stated as requirements and proved.
A contact is provided for the service that is sufficient to allow successful contract-based proof of the properties in the `uxas` node.

### waypoint-manager.lus ###

This service receives an Automation Response message and sends Mission Command messages containing the waypoints from the route in the Automation Response message, in order.
Data and temporal relationships amongst the messages are stated as requirements and proved.
A contact is provided for the service that is sufficient to allow successful contract-based proof of the properties in the `uxas` node.

### task.lus ###

This service monitors waypoints and either sends a Task Complete message when all expected waypoints have been seen or raises an error if waypoints are not seen in order.
The properties for this service are partial and need additional work.

# Analysis #

## Top-Level Proof ##

you can use `kind2` to reprove all properties on all nodes of the model, like this:

    kind2 uxas.lus --lus_main uxas_all_props --compositional true --modular true

This will take a few minutes, depending on your hardware, and will result in a final message like this (your times will be different):

    Analysis breakdown, total runtime 325.967s seconds:
      automation_request_validator:
        safe in 0.384s
        without refinement: 0 abstract systems
        5 properties
      waypoint_manager:
        safe in 1.899s
        without refinement: 0 abstract systems
        9 properties
      task:
        safe in 2.040s
        without refinement: 0 abstract systems
        11 properties
      uxas_props:
        safe in 306.166s
        without refinement: 5 abstract systems
        5 properties
      uxas_env:
        safe in 14.213s
        without refinement: 5 abstract systems
        8 properties
      uxas_ics:
        safe in 0.711s
        without refinement: 5 abstract systems
        5 properties

Above this message, `kind2` provides the result of analyzing each of the lustre nodes mentioned.

The top-level model is organized so different sets of properties are grouped in different lustre nodes.
They are:

* uxas_ics - properties stating initial conditions for the OpenUxAS model
* uxas_env - properties about the OpenUxAS operating environment
* uxas_env_antecedents - witnesses to check antecedents in sequent-style properties for the environment
* uxas_env_witnesses - witness to demonstrate that certain desireable behaviors in the OpenUxAS environment are possible
* uxas_props - safety, lemma, and compositional properties for the OpenUxAS model
* uxas_props_antecedents - witnesses to check antecedents in sequent-style properties for OpenUxAS
* uxas_all - includes all of the above.
* uxas_all_props - does not include witnesses or antecedents (which complicate reading the output)

Currently, all of the _antecedent and _witness properties should fail; all other properties should pass.

## Proving the Services ##

The contracts on nodes called by the OpenUxAS model are proved with the `--modular true` flag shown above.
This does not prove the properties contained within the service nodes, which are used to help establish the contracts and to build confidence in contract correctness.

You can prove the properties of individual services like this:

    OpenUxAS/lustre$ kind2 services/automation-request-validator.lus
    OpenUxAS/lustre$ kind2 services/waypoint-manager.lus

You should see that:

  * all properties with single-line text descriptions are falsified.
    These properties are requests that the model checker find a _witness_ for a property or behavior of instance in the model.
    We use these witnesses to build confidence that the model is operating as expected and to guard against vacuity in properties that are stated using implication.

  * all properties with multi-line text descriptions are proved.

The formatting of the results has been tailored to make checking witnesses v properties easier.

Note: to avoid re-proving the properties on the sequence theory, you can specify the service node as the lustre main node, like this:

    OpenUxAS/lustre$ kind2 services/waypoint-manager.lus --lus_main waypoint_manager

`kind2` will then only prove the properties associated with this and any called nodes.


## Proving the Sequences Theory ##

Properties on the sequences theory (see below) can be proved like this:

    OpenUxAS/lustre$ kind2 common/sequences_simple.lus

Note that there is one witness that proves - this means that `kind2` was not able to find the requested witness to the (falsified) property.
This arises because of the details of the sequences theory; additional information is provided in comments in the `sequences_simple.lus` file.

## Suppressing Witnesses ##

The output from the witnesses - which present as proof failures - can make reading the output of the tool more difficult.
Also, when new properties are introduced that may not immediately prove, they can make finding the useful counterexample for the new property more difficult.
To ease this, the witness properties can be easily disabled.

In the service nodes, the witnesses are grouped at the bottom of the node.
This way, they can easily be commented out so that they will not be considered during analysis.
Currently, they are commented out so that the proof output is clean when run modularly from the top-level node.
