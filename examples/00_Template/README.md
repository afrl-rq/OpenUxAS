# Example Template

The purpose of this directory `{OPENUXAS_ROOT}/examples/00_Template` is to serve as both an introduction to the standard workflow of using UxAS with AMASE and a rough template for creating new examples that interface UxAS with AMASE.


## Files:

* `MessagesToSend` - Directory containing messages to send to UxAS
* `cfg_Template.xml` - UxAS configuration file
* `config.yaml` - Configuration file for run_examples script
* `runAMASE_Template.sh` - Shell script to run AMASE
* `runUxAS_Template.sh` - Shell script to run UxAS
* `Scenario_Template.xml` - AMASE scenario file


## Running the example manually:

1. Open a terminal window in this directory: `{OPENUXAS_ROOT}/examples/00_Template`
2. Enter the command: `./runUxAS_Template.sh`
3. Open another terminal window in this directory: `{OPENUXAS_ROOT}/examples/00_Template`
4. Enter the command: `./runAMASE_Template.sh`
5. Start the AMASE simulation (i.e. push the play button)
6. Close the AMASE window when finished
7. Kill the UxAS process (Ctrl+C)


## Running the example with run-examples:

1. Open a terminal window in the UxAS root directory `{OPENUXAS_ROOT}`
2. Enter the command: `./run-examples 00_Template`
3. Start the AMASE simulation (i.e. push the play button)
4. Close the AMASE simulation when finished, which will also kill the UxAS process


## What happens when you run the example manually?

* Script `runUxAS_Template.sh` makes a subdirectory `RUNDIR_Template`, changes to that directory, clears out standard data and log subdirectories `datawork` and `log`, and runs the UxAS executable with flag `-cfgPath ../cfg_Template.xml`. This causes UxAS to load services with configuration parameters specified in `cfg_Template.xml`. Here, those services include: a TCP bridge to connect to AMASE, a service to synchronize UxAS with AMASE, the necessary services for UxAS to function in one of its standard configurations, optional services to log UxAS data, and a service that injects specified messages into UxAS (simulating a control control station or other interface).
* Script `runAMASE_Template.sh` starts AMASE with scenario file `Scenario_Template.xml`. This scenario file contains entity/vehicle configuration and initial state messages matching those that will be injected into UxAS, allowing AMASE to initialize a simulation containing those same vehicles/entities.
* AMASE simulates the movement of vehicles/entities responding to commands sent to their autopilots/controllers. In this case, those commands are supplied by UxAS. When you press the play button in AMASE, UxAS starts receiving entity/vehicle state messages from AMASE, causing an internal UxAS timer to regularly update in sync with AMASE. This in turn causes the UxAS message injection service to start. The injected messages define entity/vehicle configurations and initial states, zones, operating regions, tasks, and finally automation requests that reference these. After receiving these messages, UxAS computes corresponding automation responses and begins to execute them. Execution consists of sending updated mission commands and vehicle action commands to AMASE as UxAS observes vehicles in AMASE completing tasks (usually when designated waypoints are reached).


## What happens when you run the example with run-examples?

The command `./run-examples 00_Template` causes the `run-examples` script to look for file `config.yaml` in this directory `00_Template`. The file `config.yaml` specifies that a single instance of AMASE should be run with scenario file `Scenario_Template.xml`, and a single instance of UxAS should be run with configuration file `cfg_Template.xml` from within subdirectory `RUNDIR_Template`. The aggregate effect is the same as running the example manually.


## How might you use this example template?

Suppose you wanted to include a new service in UxAS and run it on a scenario similar to this one but with different tasks and/or vehicles. You could then: 

1. Copy the contents of this directory `{OPENUXAS_ROOT}/examples/00_Template` to a new directory in `examples`

Then, from within that new directory:

2. Include a new service:
   2.1. Edit `cfg_Template.xml` to include an XML configuration element for the new service
    2.2. Add new external messages to be injected for this service (if any) to subdirectory `MessagesToSend` 
    2.3. Edit `cfg_Template.xml` to modify the XML configuration element for `SendMessagesService` to include the new messages from step 2.2 (if any)
3. Add or remove tasks and/or vehicles from the scenario:
    3.1. Add or remove corresponding task and/or vehicle state and configuration messages in directory `MessagesToSend`
    3.2. Edit `cfg_Template.xml` to modify the XML configuration element for `SendMessagesService` to add or remove task and/or vehicle state and configuration messages to be injected
    3.3. Edit `cfg_Template.xml` to add or remove `WaypointPlanManagerService` XML configuration elements for corresponding vehicles
    3.4. Edit `Scenario_Template.xml` to add or remove vehicle configuration and state messages
    3.5. Modify automation request messages in directory `MessagesToSend` to add or remove references to vehicles and/or tasks
4. Change the names of files and directories (if desired)
    4.1 If you rename `cfg_Template.xml`, remember to update references to this file in `runUxAS_Template.sh`, `runUxAS_Template.py`, and `config.yaml`
    4.2 If you rename `Scenario_Template.xml`, remember to update references to this file in `runAMASE_Template.sh`
    4.3 If you want to rename the UxAS results directory `RUNDIR_Template` to something else, do a string replacement in `runUxAS_Template.sh`
    4.4 You can rename `runAMASE_Template.sh` or `runUxAS_Template.py` with no other changes
