with Ada.Containers.Formal_Ordered_Maps;
with Ada.Containers.Formal_Vectors;
with Ada.Containers;                             use all type Ada.Containers.Count_Type;
with Waypoint_Plan_Manager_Communication;        use Waypoint_Plan_Manager_Communication;
with Common;                                     use Common;
with LMCP_Messages;                              use LMCP_Messages;

package Waypoint_Plan_Manager with SPARK_Mode is

   Max : constant Ada.Containers.Count_Type := 2000;

   subtype Pos64 is Int64 range 1 .. Int64'Last;

   subtype Nat64 is Int64 range 0 .. Int64'Last;

   package WP_Maps is new Ada.Containers.Formal_Ordered_Maps (Pos64, Waypoint);
   type WP_Map is new WP_Maps.Map (Max);

   package ID_Maps is new Ada.Containers.Formal_Ordered_Maps (Pos64, Nat64);
   type ID_Map is new ID_Maps.Map (Max);

   package ID_Vectors is new Ada.Containers.Formal_Vectors (Positive, Pos64);
   type ID_Vector is new ID_Vectors.Vector (Max + 5);

   type Waypoint_Plan_Manager_Configuration_Data is record
      -- Max number of waypoints to serve for each segment.
      -- Defaults to Max to serve them all. Minimum is 1.
      NumberWaypointsToServe : Common.UInt32 := Common.UInt32 (Max);
      -- Number of waypoints remaining before starting the next segment.
      -- Minimum is 2.
      NumberWaypointsOverlap : Common.UInt32 := 3;
      -- Radius to use for loiters added by the waypoint manager
      LoiterRadiusDefault : Common.Real64 := 200.0;
      -- Turn type to use for loiters added by the waypoint manager
      TurnType : LMCP_Messages.TurnTypeEnum := TurnShort;
      -- Gimbal payload ID to use for loiters added by the waypoint manager
      GimbalPayloadId : Common.Int64 := -1;
      -- Vehicle ID of managed vehicle
      VehicleID : Common.Int64 := -1;
   end record;

   type Waypoint_Plan_Manager_State is record
      MC : MissionCommand;
      WPs : WP_Map;
      Succ_IDs : ID_Map;
      New_Command : Boolean;
      Next_Segment_ID : Nat64 := 0;
      Next_FirstID : Nat64 := 0;
      Prefix : ID_Vector;
      Cycle : ID_Vector;
      Segment : ID_Vector;
      AV_WP_ID : Pos64;
   end record;

   procedure Handle_MissionCommand
     (State : in out Waypoint_Plan_Manager_State;
      MC : MissionCommand)
     with Pre =>
       Length (MC.WaypointList) <= Max and then
       MC.FirstWaypoint > 0;

   procedure Produce_Segment
     (State : in out Waypoint_Plan_Manager_State;
      Config : Waypoint_Plan_Manager_Configuration_Data;
      Mailbox : in out Waypoint_Plan_Manager_Mailbox)
     with Pre =>
       State.Next_FirstID > 0 and then
       State.Next_Segment_ID > 0 and then
       Config.NumberWaypointsToServe >= 1 and then
       Config.NumberWaypointsToServe <= UInt32 (Max) and then
       Config.NumberWaypointsOverlap >= 2 and then
       Config.NumberWaypointsOverlap <= UInt32 (Max) - 1;

   procedure Print (State : Waypoint_Plan_Manager_State);

private

end Waypoint_Plan_Manager;
