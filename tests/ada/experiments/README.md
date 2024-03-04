OpenUxAS/tests/ada/experiments
==============================

This directory illustrates how back-to-back unit testing could be used to verify that an Ada implementation of the Plan Builder service matches the functionality of the original C++ implementation.

To run the tests, you first must build the C++ and Ada versions of OpenUxAS, like this:

    OpenUxAS$ ./anod build uxas
    OpenUxAS$ ./anod build uxas-ada

Once those builds are completed successfully, you can build and run the tests like this:

    OpenUxAS$ tests/ada/experiments/plan-builder-e2e-tests

The script can be invoked from anywhere, so you could also invoke it as:

    OpenUxAS/tests/ada/experiments$ ./plan-build-e2e-tests

The script will report on any discrepancies found in the output of the two services.
