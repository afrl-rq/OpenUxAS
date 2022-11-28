# License

*OpenUxAS* is developed by the Air Force Research Laboratory, Aerospace System Directorate, Power and Control Division.
The LMCP specification and all source code for *OpenUxAS* is publicly released under the Air Force Open Source Agreement Version 1.0. See LICENSE.md for complete details.
The Air Force Open Source Agreement closely follows the NASA Open Source Agreement Verion 1.3.
**NOTE the terms of the license include registering use of the software by emailing <a href="mailto:afrl.rq.opensource@us.af.mil?subject=OpenUxAS Registration&body=Please register me for use of OpenUxAS. Name: ____________">afrl.rq.opensource@us.af.mil</a>.**

OpenUxAS
========

[![Build and Test OpenUxAS C++](https://github.com/afrl-rq/OpenUxAS/actions/workflows/uxas-cpp.yaml/badge.svg?branch=develop)](https://github.com/afrl-rq/OpenUxAS/actions/workflows/uxas-cpp.yaml) [![Build and Prove OpenUxAS Ada](https://github.com/afrl-rq/OpenUxAS/actions/workflows/uxas-ada.yaml/badge.svg?branch=develop)](https://github.com/afrl-rq/OpenUxAS/actions/workflows/uxas-ada.yaml)

> ***This branch is intended to facilitate in-progress efforts to integrate DAIDALUS into OpenUxAS.***
> Most users should instead use the main development branch [here](http://github.com/afrl-rq/OpenUxAS).

UxAS is a collection of modular services that interact via a common message-passing architecture.
Similar in design to Robot Operating System (ROS), each service subscribes to messages in the system and responds to queries.
UxAS uses the open-source library ZeroMQ to connect all services to each other.
The content of each message conforms to the Light-weight Message Control Protocol (LMCP) format.
Software classes providing LMCP message creation, access, and serialization/deserialization are automatically generated from simple XML description documents (see the *LmcpGen* project).
These same XML descriptions detail the exact data fields, units, and default values for each message.
Since all UxAS services communicate with LMCP formatted messages, a developer can quickly determine the input/output data for each service. In a very real sense, the message traffic in the system exposes the interaction of the services that are required to achieve autonomous behavior.

Consider a simple example: the automated construction of the flight pattern to conduct surveillance of geometric lines (e.g. perimeters, roads, coasts).
A “line search task” message describes the line to be imaged and the desired camera angle.
Using this input description, a line search service calculates the appropriate waypoints to achieve the proper view angle.
When the UAV arrives at the first waypoint corresponding to the line search task, the line search service continuously updates the desired camera pointing location to smoothly step the camera along the intended route.

In addition to surveillance pattern automation, UxAS contains services that automate route planning, coordinate behavior among multiple vehicles, connect with external software, validate mission requests, log and diagram message traffic, and optimize task ordering.
In all, UxAS has approximately 30 services.

A core functionality provided by UxAS is the mechanism to calculate near-optimal task allocation across teams of unmanned vehicles.
With a collection of tasks that require servicing and a pool of vehicles available to service those tasks, UxAS is able to determine which vehicle should do which task in the proper order.
This task assignment pipeline is carried out by a series of services working together in a complex sequence.

OpenUxAS is a complex project, so there's a lot to talk about here.
If you want to get started using OpenUxAS as quickly as possible, you can jump to the [quick-start guide](#quick-start).
We've organized this README into sections, to simplify navigation.

*Table of Contents*

1. [Quick Start](#quick-start)
2. [Using OpenUxAS](#using)
3. [Developing OpenUxAS](#developing)
4. [Building the Documentation](#docs)
5. [Troubleshooting](#troubleshooting)

Throughout the remainder of the README, we will write commands that you should enter at your Linux command line like this:

    OpenUxAS$ command argument

This means that you have changed to the directory `OpenUxAS` and you are going to execute the command `command` with arguments `argument`.
If you would like to copy-paste commands from this README, you should only copy the part that begins after the `$`.

# 1. Quick Start<a name="quick-start" />

We've tried to make getting started with OpenUxAS as simple as possible.
Before you begin, you will need:

1. Ubuntu 22.04 or 20.04
2. git

Use git to clone this repository:

    $ git clone https://github.com/afrl-rq/OpenUxAS

Then, check out the DAIDALUS branch:

    OpenUxAS$ git checkout daidalus_integration

Then, use the provided `anod` command to fetch and build the dependencies for OpenUxAS and finally to build OpenUxAS:

    OpenUxAS$ ./anod build uxas

Then, use anod to build OpenAMASE, which provides simulation capabilities for OpenUxAS:

    OpenUxAS$ ./anod build amase

Now you can run OpenUxAS examples:

    OpenUxAS$ ./run-example 02_Example_WaterwaySearch

You can run OpenUxAS DAIDALUS-specific examples like this:

    OpenUxAS$ ./run-example 09_Collision

***Note:** DAIDALUS examples currently freeze after several seconds due to an issue with the ZeroMQ-TCP bridge used to connect OpenUxAS with OpenAMASE.*


# 2. Using OpenUxAS<a name="using" />

If your goal is to experiment with OpenUxAS, the best way to do that is to explore the examples that are provided as part of this repository.

First, you will need to build OpenUxAS and OpenAMASE.
You can follow the instructions in [Quick Start](#quick-start), above.

The simplest example is the Hello World example.
You can run the Hello World example like this:

    OpenUxAS$ ./run-example 01_HelloWorld

Then, you can run other examples like this:

    OpenUxAS$ ./run-example 02_Example_WaterwaySearch

You can get a list of available examples by running:

    OpenUxAS$ ./run-example --list


# 3. Developing OpenUxAS<a name="developing" />

If you wish to develop OpenUxAS, the easiest way to get started is to follow the [Quick Start](#quick-start) instructions, above.
This step is essential because the provided `anod` command will get and install all third-party dependencies needed to build OpenUxAS.
These dependencies are installed locally in the repository.
The Makefile knows how to find them, so you will not have to do any additional configuration of your environment.

Once you have cloned this repository and built OpenUxAS using anod, you can use `make` for incremental builds, like this:

    OpenUxAS$ make -j all

The `run-example` and `tests/run-tests` scripts will then select the binary built by `make`, allowing you to see the effects of and test your changes.

## 3.1. IDE Support

If you use [Visual Studio Code](https://code.visualstudio.com/) (VS Code) for development, search paths for IntelliSense have been set so that references to third-party dependencies can be resolved.
Additionally, build and clean tasks have been set up that use the Makefile.

If you use another IDE for development, you will need to configure the IDE so that references to third-party dependencies can be resolved.
These details are out of scope for this README, but will be available on the OpenUxAS documentation site.

## 3.2. Running the Tests

Once you have built OpenUxAS, you can run the tests.
Tests are found under the `tests` directory and are language specific.

### 3.2.1. C++ Tests

The C++ tests are found under `tests/cpp`.
You can run these tests like this:

    OpenUxAS/tests/cpp$ ./run-tests

More information about the C++ tests, including how to develop new tests, is provided in `tests/cpp/README.md`.

### 3.2.2. SPARK Proofs

The SPARK proofs can be replayed and compared to prior results using the script found under `tests/proof`, like this:

    OpenUxAS/tests/proof$ ./run-proofs

## 3.3. Modifying LmcpGen or OpenAMASE

If you need to modify LmcpGen or OpenAMASE to support your OpenUxAS development, the provided `anod` command provides a command to help simplify project setup.
Simply run:

    OpenUxAS$ ./anod devel-setup lmcp

or:

    OpenUxAS$ ./anod devel-setup amase

Anod will clone the requested repository and place the clone under a new directory named `develop`.
Anod will then use the cloned LmcpGen and/or OpenAMASE repository when doing builds.
Additionally, the `run-example` script will use the cloned OpenAMASE when running examples.


# 4. Building the Documentation<a name="docs" />

There are two parts to the OpenUxAS documentation:
1. a user manual;
2. Doxygen-generated reference documentation for the C++ code.

The user manual is written in LaTeX and requires a full TeX distribution to build.

To simplify building the documentation, we have provided a script in `resources` that will attempt to install needed packages on Ubuntu and then build the documentation.
Run it like this:

    OpenUxAS$ resources/build_documentation.sh


# 5. Troubleshooting<a name="troubleshooting" />

If things seem to be going wrong, all of the scripts offer increased verbosity that might help diagnose problems.
For example, passing `-vv` to the provided `anod` command will cause each command executed by anod to be printed:

    OpenUxAS$ ./anod -vv build

These commands can be entered manually, hopefully allowing problems to be identified and worked around.
