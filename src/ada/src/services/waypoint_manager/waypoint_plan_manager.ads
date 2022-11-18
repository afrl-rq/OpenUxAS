with Ada.Containers.Formal_Hashed_Maps;
with Ada.Containers.Formal_Vectors;
with Ada.Containers;                             use all type Ada.Containers.Count_Type;
with Waypoint_Plan_Manager_Communication;        use Waypoint_Plan_Manager_Communication;
with Common;                                     use Common;
with LMCP_Messages;                              use LMCP_Messages;

package Waypoint_Plan_Manager with SPARK_Mode is

   Max : constant Ada.Containers.Count_Type := 2000;

   subtype Pos64 is Int64 range 1 .. Int64'Last;

   subtype Nat64 is Int64 range 0 .. Int64'Last;

   function Pos64_Hash (X : Pos64) return Ada.Containers.Hash_Type is
     (Ada.Containers.Hash_Type'Mod (X));

   package Pos64_WP_Maps is new Ada.Containers.Formal_Hashed_Maps (Pos64, Waypoint, Pos64_Hash);
   type Pos64_WP_Map is new Pos64_WP_Maps.Map (Max, Pos64_WP_Maps.Default_Modulus (Max));

   package Pos64_Nat64_Maps is new Ada.Containers.Formal_Hashed_Maps (Pos64, Nat64, Pos64_Hash);
   type Pos64_Nat64_Map is new Pos64_Nat64_Maps.Map (Max, Pos64_Nat64_Maps.Default_Modulus (Max));

   package Pos64_Vectors is new Ada.Containers.Formal_Vectors (Positive, Pos64);
   type Pos64_Vector is new Pos64_Vectors.Vector (Max + 1);

   type Waypoint_Plan_Manager_Configuration_Data is record

      -- Number of waypoints remaining before starting the next segment.
      -- Value must be in [2, Max - 1]
      NumberWaypointsOverlap : Common.UInt32 := 2;
      -- Max number of waypoints to serve for each segment.
      -- Value must be in [NumberWaypointsOverlap + 1, Max].
      -- Default to Max to serve them all.
      NumberWaypointsToServe : Common.UInt32 := Common.UInt32 (Max);
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
      Id_To_Waypoint : Pos64_WP_Map;
      Id_To_Next_Id : Pos64_Nat64_Map;
      New_Command : Boolean;
      Next_Segment_Id : Nat64 := 0;
      Next_First_Id : Nat64 := 0;
      Prefix : Pos64_Vector;
      Cycle : Pos64_Vector;
      Segment : Pos64_Vector;
      Headed_To_First_Id : Boolean := False;
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
       State.Next_Segment_Id > 0 and then
       State.Next_First_Id > 0 and then
       Config.NumberWaypointsOverlap >= 2 and then
       Config.NumberWaypointsOverlap <= UInt32 (Max) - 1 and then
       Config.NumberWaypointsToServe > Config.NumberWaypointsOverlap and then
       Config.NumberWaypointsToServe <= UInt32 (Max);

private

end Waypoint_Plan_Manager;
