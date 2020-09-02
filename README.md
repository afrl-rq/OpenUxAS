# License

*OpenUxAS* is developed by the Air Force Research Laboratory, Aerospace System Directorate, Power and Control Division.
The LMCP specification and all source code for *OpenUxAS* is publicaly released under the Air Force Open Source Agreement Version 1.0. See LICENSE.md for complete details.
The Air Force Open Source Agreement closely follows the NASA Open Source Agreement Verion 1.3.
**NOTE the terms of the license include registering use of the software by emailing <a href="mailto:afrl.rq.opensource@us.af.mil?subject=OpenUxAS Registration&body=Please register me for use of OpenUxAS. Name: ____________">afrl.rq.opensource@us.af.mil</a>.**

OpenUxAS
========

[![Build Status](https://github.com/afrl-rq/OpenUxAS/workflows/Build/badge.svg)](https://github.com/afrl-rq/OpenUxAS/actions)

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
2. [Getting Started](#getting-started)
3. [Building OpenUxAS](#build)
4. [Running the Examples](#examples)
5. [Running the Tests](#tests)
6. [Building the Documentation](#docs)


# 1. Quick Start<a name="quick-start" />

The simplest approach to getting up and running with OpenUxAS is to use [OpenUxAS-bootstrap](https://github.com/afrl-rq/OpenUxAS-bootstrap), a supporting repository that we developed to simplify building, using and developing for OpenUxAS.
A detailed description of what OpenUxAS-bootstrap is and what it provides is available at the [OpenUxAS-bootstrap](https://github.com/afrl-rq/OpenUxAS-bootstrap) repository.

If you prefer to *not* use OpenUxAS bootstrap, you should skip this section and refer to the [next section](#getting-started) instead.

Before you begin, you will need:

1. Ubuntu 20.04
2. curl
3. git
4. python 3.8

Bootstrap your install by running this command:

    ~$ curl -L https://github.com/afrl-rq/OpenUxAS-bootstrap/raw/develop/install/bootstrap | bash

Configure your environment to run the build tool:

    ~$ eval "$( cd ~/bootstrap && install/install-anod-venv --printenv )"

Build OpenUxAS and OpenAMASE:

    ~/bootstrap$ ./anod build uxas
    ~/bootstrap$ ./anod build amase

Now you can run the OpenUxAS examples:

    ~/bootstrap$ ./run-example 02_Example_WaterwaySearch


# 2. Getting Started<a name="getting-started" />

Before you begin, you will need:

1. Ubuntu 20.04 or 18.04
2. git
3. python 3.7 or 3.8

Newer versions of Ubuntu may work, but will not have been tested.
Python is used to provide testing support and to automate the running of examples.

It is possible to build OpenUxAS on Mac, however this is not an explicitly supported configuration.
Likewise, building OpenUxAS on Windows using WSL2 should work, but again, this is not an explicitly supported configuration.

## 2.1. Third-Party Dependencies<a name="dependencies" />

Before building OpenUxAS, you need to make sure you have properly installed the following third-party dependencies.
Please note the version numbers, where listed; more recent versions may work but will not have been tested and should be avoided.

Dependencies required to build OpenUxAS in C++:
* [boost](boost_1_68_0.tar.bz2) 1.68.0
* [czmq](https://github.com/zeromq/czmq.git) 4.0.2
* [cppzmq](https://github.com/zeromq/cppzmq.git) 4.2.2
* [libzmq](https://github.com/zeromq/libzmq.git) 4.3.1
* [pugixml](https://github.com/zeux/pugixml.git) 1.2
* [serial](https://github.com/wjwwood/serial.git) 1.2.1
* [sqlite](sqlite-autoconf-3290000.tar.gz) 3.29.0
* [sqlitecpp](https://github.com/SRombauts/SQLiteCpp.git) 1.3.1
* [zyre](https://github.com/zeromq/zyre.git) 2.0.0

Dependencies required to build OpenUxAS in Ada:
* [libzmq](https://github.com/zeromq/libzmq.git) 4.3.1
* [zeromq-Ada](https://github.com/persan/zeromq-Ada.git) (master branch)

Additionally, to build the Ada code, you need an Ada compiler.
The [GNAT Community Edition](https://www.adacore.com/download) provides a free, complete development environment for Ada, including a compiler.

## 2.2. LMCP and OpenAMASE<a name="lmcp" />

The message set used by OpenUxAS is defined using LMCP.
To work with these messages, you need to build [LmcpGen](https://github.com/afrl-rq/LmcpGen) and generate the C++ or Ada message sets.
As part of this process, you will clone [OpenAMASE](https://github.com/afrl-rq/OpenAMASE).

1. Clone [LmcpGen](https://github.com/afrl-rq/LmcpGen).
   Place LmcpGen next to OpenUxAS in your filesystem.

2. Clone [OpenAMASE](https://github.com/afrl-rq/OpenAMASE).
   Place OpenAMASE next to OpenUxAS in your filesystem.

3. Run the python script `resources/RunLmcpGen.py`:
   
        OpenUxAS$ python3 resources/RunLmcpGen.py
   
    This will build the OpenUxAS message sets for C++ and python, build the OpenAMASE message set, and build the documentation for the message sets.

## 3. Building OpenUxAS<a name="build" />

Once you have installed the required third-party dependencies and built the message set, you are ready to build OpenUxAS.

For C++:

    OpenUxAS$ make all

For Ada:

    OpenUxAS/src/ada$ gprbuild -p afrl_ada_dev.gpr


# 4. Running the Examples<a name="examples" />

Once you have successfully built OpenUxAS, you can run examples.

First, you need to put OpenUxAS on your path:

    OpenUxAS$ export PATH=obj/cpp/uxas

The simplest example is the Hello World example.
You can run the Hello World example like this:

    OpenUxAS$ python3 run-example 01_HelloWorld

Before you can run the more interesting examples, you need to make sure [OpenAMASE](https://github.com/afrl-rq/OpenAMASE) is available.
Then, you can run other examples like this:

    OpenUxAS$ python3 run-example 02_Example_WaterwaySearch

You can get a list of available examples by running:

    OpenUxAS$ python3 run-example --list


# 5. Running the Tests<a name="tests" />

Once you have build OpenUxAS, you can run the tests.
Tests are found under the `tests` directory and are language specific.

## 5.1. C++ Tests

The C++ tests are found under `tests/cpp`.
You can run these tests like this:

    OpenUxAS/tests/cpp$ python3 run-tests

If OpenUxAS is built with coverage information, the test suite will report coverage results.

More information about the C++ tests, including how to develop new tests, is provided in `tests/cpp/README.md`.

## 5.2. SPARK Proofs

The SPARK proofs can be replayed and compared to prior results using the script found under `tests/proof`, like this:

    OpenUxAS/tests/proof$ python3 run-proofs


# 6. Building the Documentation<a name="docs" />

There are two parts to the OpenUxAS documentation:
1. a user manual;
2. Doxygen-generated reference documentation for the C++ code.

The user manual is written in LaTeX and requires a full TeX distribution to build.

To simplify building the documentation, we have provided a script in `resources` that will attempt to install needed packages on Ubuntu and then build the documentation.
Run it like this:

    OpenUxAS$ resources/build_documentation.sh

## 6.1 Building the Documentation Manually

If you'd like to do this process manually, then:

1. The User Manual can be generated by running:
   `pdflatex UxAS_UserManual.tex` in the folder `doc/reference/UserManual/`
2. Create HTML Doxygen reference documenation:
   * Open terminal in directory `doc/doxygen`
   * `sh RunDoxygen.sh`
   * In newly created `html` folder, open index.html
