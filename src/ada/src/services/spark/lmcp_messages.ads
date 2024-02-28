with SPARK.Containers.Functional.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Common;                use Common;

package LMCP_Messages with SPARK_Mode is

   type Message_Root is abstract tagged null record;

   --  Messages only contain functional containers and SPARK compatible data

   type AltitudeTypeEnum is (AGL, MSL);
   type SpeedTypeEnum is (Airspeed, Groundspeed);
   type TurnTypeEnum is (TurnShort, FlyOver);

   type Location3D is tagged record
      -- Latitude
      Latitude : Real64 := 0.0;
      -- Longitude
      Longitude : Real64 := 0.0;
      -- Altitude for this waypoint
      Altitude : Real32 := 0.0;
      -- Altitude type for specified altitude
      AltitudeType : AltitudeTypeEnum := MSL;
   end record;

   type EntityState is new Message_Root with record
      Id : Int64 := 0;
      Location : Location3D;
      Heading : Real32 := 0.0;
   end record;

   type PlanningState is record
      -- Identifier of the entitiy
      EntityID : Int64 := 0;
      -- Position of this entity for the plan. A valid PlanningState must define PlanningPosition (null not allowed).
      PlanningPosition : Location3D;
      -- Heading of this entity for the plan
      PlanningHeading : Real32 := 0.0;
   end record;

   package PS_Sequences is new SPARK.Containers.Functional.Vectors
     (Index_Type   => Positive,
      Element_Type => PlanningState);
   type PlanningState_Seq is new PS_Sequences.Sequence;

   type AutomationRequest is new Message_Root with record
      EntityList         : Int64_Seq;
      OperatingRegion    : Int64 := 0;
      TaskList           : Int64_Seq;
      TaskRelationships  : Unbounded_String;
      RedoAllTasks       : Boolean := False;
   end record;

   type TaskAutomationRequest is new AutomationRequest with record
      RequestID          : Int64 := 0;
      PlanningStates     : PlanningState_Seq;
      SandboxRequest     : Boolean := False;
   end record;

   type ImpactAutomationRequest is new AutomationRequest with record
      RequestID          : Int64 := 0;
      PlayID             : Int64 := 0;
      SolutionID         : Int64 := 0;
   end record;

   type UniqueAutomationRequest is new AutomationRequest with record
      RequestID          : Int64 := 0;
      PlanningStates     : PlanningState_Seq;
      SandboxRequest     : Boolean := False;
   end record;

   type VehicleAction is record
      -- A list of tasks that are associated with this action. A length of zero denotes no associated tasks. This field is for analysis purposes. The automation service should associate a list of tasks with each action to enable analysis of the allocation of tasks to vehicles.
      --
      AssociatedTaskList : Int64_Seq;
   end record;

   package VA_Sequences is new SPARK.Containers.Functional.Vectors
     (Index_Type   => Positive,
      Element_Type => VehicleAction);
   type VA_Seq is new VA_Sequences.Sequence;

   type CommandStatusTypeEnum is (Pending, Approved, InProcess, Executed, Cancelled);

   type VehicleActionCommand is new Message_Root with record
      CommandId : Int64 := 0;
      VehicleId : Int64 := 0;
      VehicleActionList : VA_Seq;
      Status : CommandStatusTypeEnum := Pending;
   end record;

   package VAC_Sequences is new SPARK.Containers.Functional.Vectors
     (Index_Type   => Positive,
      Element_Type => VehicleActionCommand);
   type VehicleActionCommand_Seq is new VAC_Sequences.Sequence;

   type Waypoint is new Location3D with record
      -- A unique waypoint number
      Number : Int64 := 0;
      -- The index of the next waypoint in the list. Consecutively numbered waypoints are <b>not</b> considered linked, the link must be explicitly stated in this field.
      NextWaypoint : Int64 := 0;
      -- Commanded speed for this waypoint. See SpeedType for defintion of this field.
      Speed : Real32 := 0.0;
      -- Type of commanded speed
      SpeedType : SpeedTypeEnum := Airspeed;
      -- The commanded climb rate. Positive values upwards. For surface (ground and sea) entities, this value is ignored.
      ClimbRate : Real32 := 0.0;
      -- The type of turn to execute
      TurnType : TurnTypeEnum := TurnShort;
      -- A list of actions to perform at this waypoint
      VehicleActionList : VA_Seq;
      -- A waypoint for contingency (e.g. lost-comm, alternate mission) operations. A value of zero denotes that no contingency point is specified.
      ContingencyWaypointA : Int64 := 0;
      -- A waypoint for contingency (e.g. lost-comm, alternate mission) operations. A value of zero denotes that no contingency point is specified.
      ContingencyWaypointB : Int64 := 0;
      -- A list of tasks that are associated with this waypoint. A length of zero denotes no associated tasks. This field is for analysis purposes. The automation service should associate a list of tasks with each waypoint to enable analysis of the allocation of tasks to vehicles.
      AssociatedTasks : Int64_Seq;
   end record;

   package WP_Sequences is new SPARK.Containers.Functional.Vectors
     (Index_Type   => Positive,
      Element_Type => Waypoint);
   type WP_Seq is new WP_Sequences.Sequence;

   type MissionCommand is new VehicleActionCommand with record
      WaypointList : WP_Seq;
      FirstWaypoint : Int64 := 0;
   end record;

   package MC_Sequences is new SPARK.Containers.Functional.Vectors
     (Index_Type   => Positive,
      Element_Type => MissionCommand);
   type MissionCommand_Seq is new MC_Sequences.Sequence;

   type KeyValuePair is record
      -- A key (name) for the property
      Key : Unbounded_String := To_Unbounded_String ("");
      -- A value for the property
      Value : Unbounded_String := To_Unbounded_String ("");
   end record;

   package KVP_Sequences is new SPARK.Containers.Functional.Vectors
     (Index_Type   => Positive,
      Element_Type => KeyValuePair);
   type KVP_Seq is new KVP_Sequences.Sequence;

   type AutomationResponse is new Message_Root with record
      MissionCommandList : MissionCommand_Seq;
      VehicleCommandList : VehicleActionCommand_Seq;
      Info               : KVP_Seq;
   end record;

   type TaskAutomationResponse is new AutomationResponse with record
      ResponseID         : Int64 := 0;
      FinalStates        : PlanningState_Seq;
   end record;

   type ImpactAutomationResponse is new AutomationResponse with record
      ResponseID : Int64 := 0;
      PlayId : Int64 := 0;
      SolutionId : Int64 := 0;
      Sandbox : Boolean := False;
   end record;

   type UniqueAutomationResponse is new AutomationResponse with record
      ResponseID         : Int64 := 0;
      FinalStates        : PlanningState_Seq;
   end record;

   type ServiceStatusTypeEnum is (Information, Warning, Error);

   type ServiceStatus is new Message_Root with record
      PercentComplete : Real32 := 0.0;
      Info            : KVP_Seq;
      StatusType      : ServiceStatusTypeEnum := Information;
   end record;

   type RouteConstraints is record
      -- ID denoting this set of route constraints
      RouteID : Int64 := 0;
      -- Location from which the planned route will start. A valid RouteConstraints message must define StartLocation (null not allowed).
      StartLocation : Location3D;
      -- Heading of entity at the start of the route
      StartHeading : Real32 := 0.0;
      -- If "true" the heading value in StartHeading must be used to start the route. If not, any starting heading can be used.
      UseStartHeading : Boolean := True;
      -- Location to which the planned route will end. A valid RouteConstraints message must define EndLocation (null not allowed).
      EndLocation : Location3D;
      -- Heading of entity at the end of the route
      EndHeading : Real32 := 0.0;
      -- If "true" the heading value in EndHeading must be used to end the route. If not, any ending heading can be used.
      UseEndHeading : Boolean := True;
   end record;

   package RC_Sequences is new SPARK.Containers.Functional.Vectors
     (Index_Type   => Positive,
      Element_Type => RouteConstraints);
   type RC_Seq is new RC_Sequences.Sequence;

   type RoutePlan is record
      -- ID denoting this plan corresponding with requested route constraint pair
      RouteID : Int64 := 0;
      -- Waypoints that connect the start location with the end location. Empty if only costs were requested
      Waypoints : WP_Seq;
      -- Time cost of route. If less than zero, a planning error has occurred
      RouteCost : Int64 := -1;
      -- Error messages, if applicable
      RouteError : KVP_Seq;
   end record;

   package RP_Sequences is new SPARK.Containers.Functional.Vectors
     (Index_Type   => Positive,
      Element_Type => RoutePlan);
   type RP_Seq is new RP_Sequences.Sequence with
     Predicate =>
       (for all I in 1 .. RP_Sequences.Last (RP_Sequences.Sequence (RP_Seq)) =>
          (for all J in 1 .. RP_Sequences.Last (RP_Sequences.Sequence (RP_Seq)) =>
               I = J or else RP_Sequences.Get (RP_Sequences.Sequence (RP_Seq), I).RouteID /=
               RP_Sequences.Get (RP_Sequences.Sequence (RP_Seq), J).RouteID));
   --  Sequence of route plans have no duplicates

   function Contains (S : RP_Seq; Id : Int64) return Boolean is
     (for some RP of S => RP.RouteID = Id);

   type RouteRequest is new Message_Root with record
      RequestID         : Int64;
      AssociatedTaskID  : Int64;
      VehicleID         : Int64_Seq;
      OperatingRegion   : Int64;
      IsCostOnlyRequest : Boolean;
      RouteRequests     : RC_Seq;
   end record;

   type RoutePlanRequest is new Message_Root with record
      RequestID         : Int64 := 0;
      AssociatedTaskID  : Int64 := 0;
      VehicleID         : Int64 := 0;
      OperatingRegion   : Int64 := 0;
      IsCostOnlyRequest : Boolean := False;
      RouteRequests     : RC_Seq;
   end record;

   type RoutePlanResponse is new Message_Root with record
      ResponseID       : Int64 := 0;
      -- Associated Task ID (0 if no associated task) that this set of responses corresponds to
      AssociatedTaskID : Int64 := 0;
      -- Vehicle that was considered during planning
      VehicleID        : Int64 := 0;
      -- Operating region that was considered during planning
      OperatingRegion  : Int64 := 0;
      -- List of all responses for this vehicle + operating region situation
      RouteResponses   : RP_Seq;
   end record;

   package RPR_Sequences is new SPARK.Containers.Functional.Vectors
     (Index_Type   => Positive,
      Element_Type => RoutePlanResponse);
   type RPR_Seq is new RPR_Sequences.Sequence;

   type RouteResponse is new Message_Root with record
      -- Response ID matching ID from request ({@link RouteRequest})
      ResponseID : Int64 := 0;
      -- Corresponding route responses for all requested vehicles
      Routes     : RPR_Seq;
   end record;

   type TaskOptionCost is new Message_Root with record
      -- Corresponding Vehicle ID
      VehicleID : Int64 := 0;
      -- Initial task ID (if zero, corresponds to current vehicle location)
      InitialTaskID : Int64 := 0;
      -- Initial task option
      InitialTaskOption : Int64 := 0;
      -- Destination task ID
      DestinationTaskID : Int64 := 0;
      -- Destination task option
      DestinationTaskOption : Int64 := 0;
      -- Timing corresponding to travel between ('InitialTask' using 'InitialTaskOption') and ('DestinationTask' using 'DestinationTaskOption'). If time is less than zero, no feasible path exists between tasks.
      TimeToGo : Int64 := 0;
   end record;

   package TOC_Sequences is new SPARK.Containers.Functional.Vectors
     (Index_Type   => Positive,
      Element_Type => TaskOptionCost);
   type TOC_Seq is new TOC_Sequences.Sequence;

   type AssignmentCostMatrix is new Message_Root with record
      -- ID that matches this cost matrix with the appropriate unique automation request
      CorrespondingAutomationRequestID : Int64 := 0;
      -- Over-arching task relationship description (directly from automation request). A process algebra string with only task IDs.
      --  TaskLevelRelationship : Unbounded_String := To_Unbounded_String("");
      -- List of all tasks that this cost matrix includes
      TaskList : Int64_Seq;
      -- Operating region that was used during matrix calculation
      OperatingRegion : Int64 := 0;
      -- Set of task-to-task timings for each requested vehicle. Assume 'T' max tasks [16], 'O' max options per task [8], 'V' max vehicles [16]: then max number of elements in matrix is 'V*T*O + (T*O)^2' [18432]
      CostMatrix : TOC_Seq;
   end record;

   type TaskOption is record
      -- Task ID
      TaskID : Int64 := 0;
      -- ID for this option
      OptionID : Int64 := 0;
      -- Eligible entities for completing this option with identical cost to complete. If list is empty, then all vehicles are assumed to be eligible.
      EligibleEntities : Int64_Seq;
      -- Cost to complete option in terms of time (given in milliseconds)
      Cost : Int64 := 0;
      -- Start location entering the option. A valid TaskOption must define StartLocation (null not allowed).
      StartLocation : Location3D;
      -- Start heading entering the option
      StartHeading : Real32 := 0.0;
      -- Ending location for this option. A valid TaskOption must define EndLocation (null not allowed).
      EndLocation : Location3D;
      -- Ending heading for this option
      EndHeading : Real32 := 0.0;
   end record;

   package TO_Sequences is new SPARK.Containers.Functional.Vectors
     (Index_Type   => Positive,
      Element_Type => TaskOption);
   type TaskOption_Seq is new TO_Sequences.Sequence;

   type TaskPlanOptions is new Message_Root with record
      -- ID that matches this message with the appropriate unique automation request
      CorrespondingAutomationRequestID : Int64 := 0;
      -- Task ID
      TaskID : Int64 := 0;
      -- Process algebra string encoding all of the different options
      Composition : Unbounded_String := To_Unbounded_String ("");
      -- List of options.
      Options : TaskOption_Seq;
   end record;

   type TaskAssignment is record
      -- Task ID
      TaskID : Int64 := 0;
      -- Option ID that was selected for this task
      OptionID : Int64 := 0;
      -- Vehicle that is assigned to this task
      AssignedVehicle : Int64 := 0;
      -- Time before which this task cannot begin
      TimeThreshold : Int64 := 0;
      -- Time that this task is assigned to be completed.
      TimeTaskCompleted : Int64 := 0;
   end record;

   package TaskAssignment_Sequences is new SPARK.Containers.Functional.Vectors
     (Index_Type   => Positive,
      Element_Type => TaskAssignment);
   type TaskAssignment_Sequence is new TaskAssignment_Sequences.Sequence;

   type TaskAssignmentSummary is new Message_Root with record
      -- ID that matches this summary with the appropriate unique automation request
      CorrespondingAutomationRequestID : Int64 := 0;
      -- Operating region which was considered during this assignment
      OperatingRegion : Int64 := 0;
      -- Ordered list of tasks to be completed
      TaskList : TaskAssignment_Sequence;
   end record;

end LMCP_Messages;
