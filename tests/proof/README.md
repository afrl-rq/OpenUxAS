
# Table of Contents

1.  [Using the proof testsuite]
    1.  [Dependencies]
    2.  [Running the testsuite]
2.  [Maintaining the proof testsuite]
    1.  [Principles]
    2.  [Adding a new test]
    3.  [Updating the existing tests]

# Using the proof testsuite

## Dependencies

First of all, the SPARK toolset needs to be installed on your
machine. The `anod` script at the root of this repository can download
the latest GNAT Community Edition, or it can be found online on
[the AdaCore website](https://www.adacore.com/download).

The dependencies of `uxas-ada` need to be built in
order to use the proof testsuite. They can be built by running the
following anod command:

   $ ./anod build uxas-ada

Then, the build environment for `uxas-ada` can be put in the path by
running the command:

   $ eval "$( ./anod printenv uxas-ada --build-env)"

## Running the testsuite

The testsuite can be run with the command

    $ ./run-proofs

Depending on the machine it is run on, the user may have to increase
the timeout to run the testsuite with the suitable option:

   $ ./run-proofs --timeout=1200

# Maintaining the proof testsuite

## Principles

The testsuite relies on a mechanism implemented in GNATprove called
the **replay** mode. During a normal run of the tool on the code, the
information on which prover proved which check in which steps will be
saved in files called **session files**. They are stored under
`OpenUxAS/src/ada/proof/sessions`. When running gnatprove with the
`--replay` option, the tool will only call the provers that are stored
in the sessions files with the appropriate number of steps. This
allows to greatly decrease the proof time, because only one prover
will be called for each check. By default, the proof testsuite runs in
replay mode.

Each test is located in `proofs/test_name`:
-   the `test.yaml` file stores the names of the files on which
GNATprove is called, and the level and timeout given to GNATprove to
prove the given files.
-   the `test.out` file stores the base output.

During a testsuite run, the output obtained by calling GNATprove is
compared to the base output. If there is a difference, the testsuite
can display the diffs when the option `-E` or `--show-error-output` is
passed.

## Maintaining the existing tests

If the source code of SPARK services in `uxas-ada` is modified, it is
important to check that the changes do not impact proof. To check
this, the user can run the testsuite by following the directions in
part "Using the proof testsuite". The version used in the continuous
integration is the latest GNAT Community Edition, so the version
installed should match so that the same versions of the provers are
run on the files.

If one test fails, you can show the diff by running the testsuite with
the `-E` option. It is very likely that the diff is a proof
regression, i.e. a new line with the `medium` keyword appears in the
output. If this happens, generating new session files might fix the
diff. To do so, the testsuite can be run in non-replay mode with the
following command:

   $ ./run-proofs --no-replay

GNATprove will be called on the files with the corresponding level and
timeout found in the `test.yaml` file, and it will generate new
session files. The old output can be overwritten with the option
`--replay`. Ideally, no patch should make a prof regress, i.e. add new
`medium` lines.

If the issue is not solved after generating new session files, is it
possible that the changes in the source code require additional
assertions and/or lemmas in order to prove the service again. It is
necessary to go back to the source code and assess the changes.

## Adding a new test

The steps to add a new test are detailed below:
-   create a new folder under `proofs/` whose name will be the name of
the test
-   create the file `test.yaml` in the test folder, with the following
syntax:
   filenames: ["file_1.ads", "file_2.ads"] # The files given in entry
                                           # to GNATprove

   level: N                                # optional: if different
                                           # from 0, this is the level
                                           # option given to GNATprove
                                           # to prove the files

   timeout: N                              # optional: if an additional
                                           # timeout was given to
                                           # GNATprove to prove the
                                           # files, it should be
                                           # specified here.

The `test.yaml` files of existing tests can be taken as templates to
create a new test.

-   run the testsuite to generate the session files, using
   $ ./run-tests test_name --no-replay
If they exist, GNATprove will be called with the level and timeout
specified in the `test.yaml` file.
-   commit the newly generated session files (in
`OpenUxAS/src/ada/proof) and the files in the new test folder.
-   check that the proof passes in replay mode using the command
   $ ./run-tests test_name

Additionally, it is necessary to check that the new files do not
impact proof on existing tests. For this procedure, you can refer to
"Maintaining the existing tests".
