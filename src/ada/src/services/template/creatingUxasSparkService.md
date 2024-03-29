

# Prepare for SPARK/Ada OpenUxAS Development

1.  Create a fork of OpenUxAS.
2.  Install gnat, SPARK/gnatprove, and GNAT Studio.
3.  Assuming you're using GNAT Studio for development, do the following each
    time you start a fresh session or open a new terminal window:
    1.  Run the following command in the terminal from the main OpenUxAS
        directory so that GNAT Studio can locate all necessary dependencies
        (e.g. LMCP message libraries and ZeroMQ):
        `eval "$( ./anod printenv uxas-ada --build-env )"`
    2.  Change to the Ada source directory: `cd src/ada`
    3.  Open the project: `gnatstudio -P afrl_ada_dev.gpr &`
4.  As you develop, build and/or prove your code locally through GNAT Studio,
    or use `gnat` and `gnatprove` from the command line.


# Add Support for Any New SPARK-Compatible Messages

-   If you need SPARK-compatible LMCP messages that have not previously been
    used in any existing SPARK/Ada UxAS service, you will need to manually add
    support for them. To this, you must:
    1.  Add a type definition for the message to SPARK package `LMCP_Messages` in
        `/src/services/spark/lmcp_messages.ads`. Note that you will need to
        recursively add type definitions for any field of the message whose type
        is a also message (or message array) that is currently undefined.
    2.  Add a function to convert the corresponding Ada LMCP message to the newly
        defined SPARK-compatible one to the specification and body of package
        `LMCP_Message_Conversions` in
        `/src/services/spark/lmcp_message_conversions.{ads,adb}`. Note that you
        will need to recursively add functions for any fields of the message for
        which such conversion functions are not currently defined. By convention,
        the name of such functions should be `As_<MessageType>_Message`. See
        existing functions in this package for examples.
-   You can publish SPARK-compatible LMCP messages directly from SPARK portions
    of a service implemented in SPARK package `<Service_Name>` using the
    `sendBroadcastMessage` procedure from that same package. This procedure
    relies on the `As_Object_Any` function from package
    `LMCP_Message_Conversions`, which in turn relies on manually defined
    functions for converting SPARK-compatible LMCP messages back to Ada LMCP
    messages. If you want to be able to broadcast SPARK-compatible LMCP messages
    directly from SPARK portions of your service, you will therefore need to:
    1.  Add a function to the body of package `LMCP_Message_Conversions` to
        convert the SPARK-compatible LMCP message type back to an access type or
        classwide access type for the original Ada LMCP message type.
        -   By convention, such functions are named `As_<MessageType>_Any` or
            `As_<MessageType>_Acc` depending on whether they return a classwide
            access type or simply an access type.
        -   Unless it is needed external to the package (which is unlikely), you
            should put the function declaration in the package body, not the
            package specification. See package `LMCP_Message_Conversions` for
            examples.
        -   You will need to recursively add functions for any fields whose types
            are messages for which such conversion functions are not currently
            defined.
    2.  Using the previous function, add a case for the message type to
        `function As_Object_Any` in the body of `LMCP_Message_Conversions`.
        Recursively add cases for any fields whose types are messages that
        are not currently handled in this function.


# Copy and Rename the Service Template Files

The instructions in this section and all subsequent sections assume you're in
the `src/ada` directory and that you've chosen names to associate with your
service: `<ServiceName>`, `<Service_Name>`, and `<Service_Name_Variant>` (and
lowercase versions of the last two). For example, `WaypointPlanManager`,
`Waypoint_Plan_Manager`, `waypoint_plan_manager`, `Waypoint_Plan_Management`,
and `waypoint_plan_management`.

1.  Copy the six "template" files from directory `src/services/template` to a
    new directory. By convention, this directory would be named
    `src/services/<service_name>`, but you can name it something else.
2.  These files have been given extensions `template_adb` and `template_ads` to
    prevent them from being compiled as part of the project. To include them in
    the project, for files with extension `template_adb`, change the extension
    to `adb`, and for files with extension `template_ads`, change the
    extenstion to `ads`.
3.  Within each file's name, change the substring `service_name` to your chosen
    `<service_name>` or the substring `service_name_variant` to your chosen
    `<service_name_variant>`.


# Modify the Content of the Service Template Files

The following instructions walk through how to modify the contents of each of
the service template files. Note that each instruction (except simple
instructions related to replacing service name placeholders like
`<Service_Name>`) has a corresponding `__TODO__` comment in the template file
itself, along with examples in a corresponding `__Example__` comment.


## The Communication Portion of the Service

Package `<Service_Name>_Communication` declares a "mailbox" type for the
service and associated procedures for using it, including a procedure
`sendBroadcastMessage` for sending Ada LMCP messages over ZeroMQ. In general,
the only thing that needs to be done to create a new service is to update
placeholder strings with the service's name:

1.  Within `<service_name>_communication.ads`:
    1.  Replace all instances of `<Service_Name>` with your chosen name.
2.  Within `<service_name>_communication.adb`:
    1.  Replace all instances of `<Service_Name>` with your chosen name.


## The Ada Portion of the Service

Package `UxAS.Comms.LMCP_Net_Client.Service.<Service_Name_Variant>`
implements the Ada portion of the service. This portion of the service does
the following:

-   Declares and initializes the service's main tagged type
    `<Service_Name>_Service`, which inherits from `Service_Base` and includes
    the service's mailbox, along with SPARK-compatible state and configuration
    data.
-   Declares and initializes the service's mailbox, state, and configuration.
    The mailbox is initialized through the `Initialize` procedure, and the
    state and configuration is initialized mainly through default initial
    values and the `Configure` procedure.
-   Declares and defines the name by which the service is referred to in the
    OpenUxAS configuration file.
-   Declares and defines the service's main LMCP message processing loop,
    making use of a mix of local Ada message handlers and local helper
    subprograms, along with SPARK message handlers and SPARK helper subprograms
    from package `<Service_Name>`.

The steps to implement this portion of the service from the template files
are as follows:

1.  Within `uxas-comms-lmcp_net_client-service-<service_name_variant>.ads`:
    1.  Add `with` clauses for any additional packages needed in this package's
        specification to the top of the file.
    2.  Replace all instances of `<Service_Name_Variant>` with your chosen name.
    3.  Replace all instances of `<Service_Name>` with your chosen name.
    4.  Replace the instance of `<ServiceName>` with your chosen name on the
        line that reads
        `Type_Name : constant String := "<ServiceName>Service"`.
        This is the name the Service Manager will use
        to load the service, and it is therefore the name by which you will
        refer to the service in the SPARK/Ada OpenUxAS configuration file.
    5.  Add any additional service-specific fields to record
        `<Service_Name>_Service`, which already has fields `Config`, `Mailbox`,
        and `State`. Note that additional fields may not be needed.
2.  Within `uxas-comms-lmcp_net_client-service-<service_name_variant>.adb`:
    1.  Add `with` clauses for any additional packages needed only within this
        package's body to the top of the file. This is likely to include LMCP
        messages, e.g. from package AFRL.CMASI, and it may include other packages
        as well.
    2.  Replace all instances of `<Service_Name_Variant>` with your chosen name.
    3.  Replace all instances of `<Service_Name>` with your chosen name.
    4.  Declare any subprograms used locally within the package. This should
        include procedures for handling Ada LMCP messages, which by convention
        should be named `Handle_<MessageType>_Msg`. Also by convention, their
        definitions are deferred until closer to the end of the file.
    5.  In `procedure Configure`,
        1.  Add any necessary service-specific configuration logic. This is
            likely to include logic to set service-specific configuration
            parameters read from the OpenUxAS XML configuration file.
        2.  Subscribe to any messages this service should receive.
    6.  Define any Ada LMCP message handling procedures that were declared
        earlier in the package.
        -   In some cases, these may essentially be wrappers around SPARK LMCP
            message handling procedures from package `<Service_Name>`. In such
            cases, the Ada LMCP message handler should use the
            `As_<MessageType>_Message` function from package
            `LMCP_Message_Conversions` to convert the Ada LMCP message to a
            SPARK-compatible LMCP message before calling the SPARK LMCP message
            handler, which by convention should be named `Handle_<MessageType>`.
        -   In some cases, there may be no Ada LMCP message handler for a
            particular type of message that the service subscribes to. Instead,
            there is only a handler for the analogous SPARK LMCP message, which
            is called by the `Process_Received_LMCP_Message` procedure described
            in the next step.
    7.  Within procedure `Process_Received_LMCP_Message`:
        1.  Add an `if-elsif` block to handle every type of message this service
            subscribes to. For each type of message, call either a local Ada
            LMCP message handler or a SPARK LMCP message handler from package
            `<Service_Name>`.
        2.  Add any additional processing logic that should occur every time
            after a message is received.
    8.  Define any other local procedures that were declared earlier in the
        package.


## The SPARK Portion of the Service

SPARK package `<Service_Name>` contains SPARK subprograms and associated data
structures used to implement key behaviors of the service that can be
formally verified. Public subprograms that update the service's state tend to
be called by the Ada portion of the service in package
`UxAS.Comms.LMCP_Net_Client.Service.<Service_Name_Variant>` when a new
message is received or other events occur, e.g. a timer within the service
triggers. The `<Service_Name>` SPARK package likely includes a significant
amount of ghost code for specification and to help guide proof, and it may
also include helper subprograms that decompose the processing and make proof
more tractable. The steps to implement this portion of the service from the
template files are as follows:

1.  Within `<service_name>.ads`:
    1.  Add `with` clauses for any additional packages needed in this package's
        specification to the top of the file.
    2.  Replace all instances of `<Service_Name>` with your chosen name.
    3.  Define the types needed for this SPARK package.
    4.  Declare the fields of record `<Service_Name>_Configuration_Data`, which
        holds configuration information for the service. This configuration
        information is generally from the OpenUxAS XML configuration file and
        is initialized by the Ada portion of the service using the `Configure`
        procedure.
    5.  Declare and optionally provide default values for the fields of record
        `<Service_Name>_State`, which holds state information for the
        service. This information tends to change as messages are processed and
        computations are performed.
    6.  Declare public subprograms needed in this package. These tend to
        include (a) procedures to handle SPARK-compatible LMCP messages; (b)
        subprograms that implement major behaviors of the service that need to
        be accessible by Ada portions of the service; and (c) ghost code for
        specification, especially for pre- and postconditions of subprograms
        for (a) and (b).
2.  Within `<service_name>.adb`:
    1.  Add `with` clauses for any additional packages needed in this package's
        body to the top of the file.
    2.  Replace all instances of `<Service_Name>` with your chosen name.
    3.  Add any local types or `use` clauses you would like to have.
    4.  Declare and define bodes for any local subprograms that are only used
        within the body of this package. This may include helper subprograms or
        ghost code (e.g. lemmas) to help with proof.
    5.  Define bodies for any subprograms that were declared in the package
        specification. These are likely to include procedures to handle
        SPARK-compatible LMCP messages (by convention named
        `Handle_<MessageType>`), along with other SPARK subprograms needed by
        the service.
        -   Note that procedures that send SPARK-compatible LMCP messages
            directly should include the service's mailbox as a parameter.
        -   Also, as a general tip for proof, subprograms that have complex
            contracts and operate on the state should in their implementations
            rely on helper subprograms that operate over **only** the required
            fields of the state and have contracts that can be leveraged for
            proof of the original subprogram's contract. This modularizes proof
            and minimizes context for the provers, making proof more tractable.


## Update the Main UxAS Executable

1.  Include your service in `src/ada/src/main/uxas_ada.adb` by adding
    the following lines to the file after analogous lines for other
    services:
    
        with UxAS.Comms.LMCP_Net_Client.Service.<Service_Name_Variant>;
        pragma Unreferenced (UxAS.Comms.LMCP_Net_Client.Service.<Service_Name_Variant>);


# Build and Run SPARK/Ada OpenUxAS

1.  Once you're ready to start using your code in conjunction with other
    anod-managed code, change to the main OpenUxAS directory, and build it with
    command: `./anod build uxas-ada`
2.  You can demonstrate your code with OpenAMASE by setting up an example in
    the `examples` directory that can be run from the main OpenUxAS directory
    using the command: `./run-example`
    -   See `examples/02a_Ada_WaterwaySearch` for an example that uses SPARK/Ada
        OpenUxAS alongside C++ OpenUxAS and OpenAMASE to do a line search. Files
        that affect running of the example include the configuration files for
        both the SPARK/Ada and C++ versions of OpenUxAS (`cfg_ada.xml`,
        `cfg_cpp.xml`), the configuration file for OpenAMASE
        (`Scenario_WaterwaySearch.xml`), the configuration file for `run-example`
        (`config.yaml`), and messages in the `MessagesToSend` directory, which
        are injected based on the configuration of the `SendMessagesService` in
        the OpenUxAS configuration file(s).

