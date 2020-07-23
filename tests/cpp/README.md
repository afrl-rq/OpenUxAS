
# Table of Contents

1.  [Background](#org2ee62c4)
2.  [Integration of GCOV in OpenUxAS](#orgaca9272)
    1.  [Compilation Options](#org9e8e07e)
    2.  [Modifications to UxAS executable](#orgd43fff9)
    3.  [Usage during testsuite run](#org3b5fd4b)
3.  [How to write tests using the pylmcp module](#org21977c7)
    1.  [Setup UxAS](#org4a0f819)
    2.  [Launch server](#orge1dfed2)
    3.  [Execute the test](#orgc3ce8fd)



<a id="org2ee62c4"></a>

# Background

**Gcov** is a test coverage program. Use it in concert with **GCC** to
analyze your programs to help create more efficient, faster running
code and to discover untested parts of your program. You can use
**gcov** as a profiling tool to help discover where your optimization
efforts will best affect your code. You can also use gcov along with
the other profiling tool, gprof, to assess which parts of your code
use the greatest amount of computing time. (from gcov documentation at
[link](https://gcc.gnu.org/onlinedocs/gcc/Gcov-Intro.html#Gcov-Intro)).

The main advantage of **gcov** is that it is freely available as part of
the **GCC** toolchain. The tool can be used for statement coverage at
source level and decision coverage at object level. An important note
is that this tool cannot be used in the context of DO178 certification
because of some limitations.

As **gcov** uses instrumentation to gather coverage information the
program must be compiled with **GCC** using some special options. Also,
**gcov** requires a file system to dump coverage information (so it won’t
be suitable for purely embedded development).


<a id="orgaca9272"></a>

# Integration of GCOV in OpenUxAS


<a id="org9e8e07e"></a>

## Compilation Options

The following compilation options were used: **-fprofile-arcs
-ftestcoverage**.


<a id="orgd43fff9"></a>

## Modifications to UxAS executable

The UxAS executable is meant to run indefinitely. **Gcov** by default
dumps all the coverage information at program termination. To ensure
that the UxAS program dumps the coverage information during testing, we
had to add some code that ensures that when receiving a SIGINT signal
we do manually the dumps. The resulting patch was:


    diff --git a/src/UxAS_Main.cpp b/src/UxAS_Main.cpp
    index 9e3fe289..0b4951d9 100644
    --- a/src/UxAS_Main.cpp
    +++ b/src/UxAS_Main.cpp
    @@ -37,6 +37,10 @@
    #include <thread>
    #include <locale>

    +#ifdef GCOV_MODE
    +#include <csignal>
    +#endif
    +
    #define ARG_CFG_PATH "-cfgPath"
    #define ARG_VERSION "-version"
    #define ARG_RUN_UNTIL "-runUntil"
    @@ -47,6 +51,15 @@

    #define BEFORE_LOG_MANAGER_INITIALIZATION_LOG_MESSAGE(message) std::cout << message << std::endl; std::cout.flush();

    +#ifdef GCOV_MODE
    +extern "C" void __gcov_flush();
    +
    +void signalHandler( int signum ) {
    +    __gcov_flush();
    +    std::exit(signum);
    +}
    +#endif
    +
    int
    main(int argc, char** argv)
    {
    @@ -67,6 +80,10 @@ main(int argc, char** argv)
        // declare relative paths of configuration files with default values
        // example arguments: -cfgBasePath ./cfg/cfgbase2.xml
        //
    +
    +    #ifdef GCOV_MODE
    +    signal (SIGINT, signalHandler);
    +    #endif
        std::string cfgPath{"cfg.xml"};
           uint32_t runUntil_sec = 0;

In order not to include the code when not performing coverage
activities, the additional code is controlled by the C macro `GCOV_MODE`
(code is enabled by adding `-DGCOV_MODE=1` during compilation).


<a id="org3b5fd4b"></a>

## Usage during testsuite run

Gcov produces various files both at compilation time and execution
time:

1.  \*.gcda: this is the coverage information produce during the execution
2.  \*.gcno: contains information to reconstruct the basic block graphs and assign source line numbers to blocks. This file is produced during compilation and placed with the object files
3.  \*.gcov: file produced by gcov once the gcda and gcno files are available. These files are basically annotated source files

During test-suite runs, we use the options: `GCOV_PREFIX` and
`GCOV_PREFIX_STRIP` in order to produce the gcda files inside the
directory specific to the testsuite. We ensure that at the beginning
of each testsuite run, the gcda files are reset.

Once all tests have been executed, we gather gcno files along with the
produced gcda files and call the gcov tool to generate \*.gcov files
(one for each source). Some of the produced files are then ignored as
not part of the project (system includes, …, …)

For more details see the function `dump_gcov_summary` in `run-tests`. The
function is quite generic. The only part that might need adjustment is
the invocation of gcov itself. The directory from where it should be
invoked might depend on the build itself as some path information (to
locate sources) stored in the gcno files might be relative paths.


<a id="org21977c7"></a>

# How to write tests using the pylmcp module

In the testsuite, each test case will define a specific scenario. In
the following paragraphs, we will explain how the test
`arv/correct_automation_request/test.py` has been created. It can be
modified to create other tests based on the same pattern.


<a id="org4a0f819"></a>

## Setup UxAS

The first thing to do is to configure the bridge.


    bridge_cfg = UxASConfig()
    bridge_cfg += AutomationRequestValidator()

We initialize the `bridge_cfg` variable to the base Object that contains
the configuration, `UxASConfig()`. Then, the desired service is added to
the configuration. Here, it is the AutomationRequestValidator. The
services are defined in `pylmcp/uxas.py`. It is possible to add more
service just by creating a new class in this package. The script will
create the corresponding `config.xml` passed to UxAS.


<a id="orge1dfed2"></a>

## Launch server

After adding the configuration to `bridge_cfg`, the server can be
launched along with UxAS


    with Server(bridge_cfg=bridge_cfg) as server:
    	try:

After this line, the python script UxAS is running and it is possible
to send messages on the bridge with the python code.


<a id="orgc3ce8fd"></a>

## Execute the test

The first thing done is to send configuration messages to the ARV
service so it can respond to the Automation Request.


    for obj in (Object(class_name='AirVehicleConfiguration', ID=400,
    		   randomize=True),
    	    Object(class_name='AirVehicleConfiguration', ID=500,
    		   randomize=True),
    	    Object(class_name='AirVehicleState', ID=400,
    		   randomize=True),
    	    Object(class_name='AirVehicleState', ID=500,
    		   randomize=True),
    	    Object(class_name='KeepInZone', ZoneID=1,
    		   randomize=True),
    	    Object(class_name='KeepOutZone', ZoneID=2,
    		   randomize=True),
    	    Object(class_name='OperatingRegion', ID=3,
    		   KeepInAreas=[1], KeepOutAreas=[2]),
    	    Object(class_name='cmasi.LineSearchTask', TaskID=1000,
    		   randomize=True),
    	    Object(class_name='TaskInitialized', TaskID=1000,
    		   randomize=True)):
        server.send_msg(obj)
        time.sleep(0.1)

We send nine messages on the bridge. Each message corresponds to an
instance of an Object.  Let’s focus on the first one, and decompose
it:

    Object(class_name='AirVehicleConfiguration', ID=400,
    randomize=True)

The class name corresponds to the message name in the mdms files. When
it is the only message with this name, it is possible to only put the
name, and not the full path. However, when message names can clash, it
is needed to put the path that disambiguates it, e.g. for
LineSearchTask, we use `class_name='cmasi.LineSearchTask'`. Then, it is
possible to set the attributes that need to be re-used later, or that
we want to keep fixed. Here, it is done with the ID attribute that
will be used in the Automation Request (in the requested `EntityList`).
Finally, all attributes that are not set can be randomized. This is
enabled by adding randomize=True to the object initialization.

For each created `Object`, we send it on the bridge using the
`server.send_msg(Object)` method, and wait for 0.1 second to give some
time to UxAS to process the message.

After the setup of UxAS internal data (Vehicle states, configurations,
            regions, …) has been done, we send the Automation Request:

    obj = Object(class_name='cmasi.AutomationRequest',
    	     TaskList=[1000], EntityList=[400, 500],
    	     OperatingRegion=3, randomize=True)
    server.send_msg(obj)

As said before, we can reuse the fixed attributes in the message to
create a correct Automation Request to send to the ARV service. Since
it is well formed, the service is supposed to send a corresponding
Unique Automation Request. We can catch it with the
server.wait<sub>for</sub><sub>msg</sub> method:

    msg = server.wait_for_msg(
    		descriptor='uxas.messages.task.UniqueAutomationRequest',
    		timeout=10.0)

If the server doesn’t see an UniqueAutomationRequest message within 10
seconds, it will return an error and the test will end. When it is
sent, the message is stored in the msg variable.

It enables the user two things:

-   First, it is possible to assert properties on the received message. In the test, we assert the fact that the field OriginalRequest of the UniqueAutomationRequest is equal to the sent AutomationRequest:

        assert(msg.obj['OriginalRequest'] == obj)

-   Second, it can be used to store data to further send coherent messages. In the test, the ARV service generates a unique ID corresponding to the Automation Request. If we want to check the nominal behavior of the ARV, it is necessary for it to receive a UniqueAutomationResponse with a ResponseID that is equal to the RequestID of the UniqueAutomationRequest within 5 seconds.

        unique_id = msg.obj.data["RequestID"]
        obj = Object(
        	       class_name='uxas.messages.task.UniqueAutomationResponse',
        	       ResponseID=unique_id, randomize=True)
        server.send_msg(obj)

    By storing the received RequestID, we are able to create a correct
    UniqueAutomationResponse.

The only action left is waiting for the corresponding
 AutomationResponse, and test that it is equal to the OriginalResponse
 in the sent UniqueAutomationResponse.

    msg = server.wait_for_msg(descriptor="afrl.cmasi.AutomationResponse",
     timeout=10.0)
    assert (msg.descriptor == "afrl.cmasi.AutomationResponse")
    assert (msg.obj == obj['OriginalResponse'])

After executing the test, gcov and lcov can generate a coverage report
 that will point to the unread lines. It is possible to create as many
 tests as needed to cover the entire file.
