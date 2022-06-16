with Ada.Containers.Formal_Doubly_Linked_Lists;
with Ada.Containers.Formal_Hashed_Maps;
with Ada.Containers.Formal_Ordered_Maps;
with Ada.Containers.Functional_Maps;
with Ada.Containers.Formal_Vectors;
with Ada.Containers;                             use all type Ada.Containers.Count_Type;
with Ada.Strings.Unbounded;                      use Ada.Strings.Unbounded;
with Waypoint_Plan_Manager_Communication;        use Waypoint_Plan_Manager_Communication;
with Common;                                     use Common;
with LMCP_Messages;                              use LMCP_Messages;

-- with AFRL.CMASI.Waypoint; use AFRL.CMASI.Waypoint;
-- with AFRL.CMASI.MissionCommand; use AFRL.CMASI.MissionCommand;

package Waypoint_Plan_Manager with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);

   Max : constant Ada.Containers.Count_Type := 2000;

   subtype Int64_Positive is Int64 range 1 .. Int64'Last;

   subtype Int64_Natural is Int64 range 0 .. Int64'Last;

   --  package WP_Vectors is new Ada.Containers.Formal_Vectors (Positive, WP);
   --  type WP_Vector is new WP_Vectors.Vector (Max);

   package WP_ID_Maps is new Ada.Containers.Formal_Ordered_Maps (Int64_Positive, Int64_Natural);
   type WP_ID_Map is new WP_ID_Maps.Map (Max);

   package WP_ID_Vectors is new Ada.Containers.Formal_Vectors (Positive, Int64_Positive);
   type WP_ID_Vector is new WP_ID_Vectors.Vector (Max + 5);

   type Waypoint_Plan_Manager_Configuration_Data is record
      -- Max number of waypoints to serve for each segment.
      -- Defaults to a large number to serve them all
      NumberWaypointsToServe : Common.UInt32 := 100000;
      -- Number of waypoints remaining before starting the next segment
      NumberWaypointOverlap : Common.UInt32 := 3;
   end record;

   type Waypoint_Plan_Manager_State is record
      Original_MC : MissionCommand;
      Current_WP_ID : Int64_Positive;
      ID_Map : WP_ID_Map;
      New_Command : Boolean;
      Next_Seg_ID : Int64_Natural;
      Second_Element_Is_FirstID : Boolean;
      Prefix : WP_ID_Vector;
      Cycle : WP_ID_Vector;
   end record;

   procedure Process_Mission_Command
     (This : in out Waypoint_Plan_Manager_State)
     with Pre =>
       Length (This.Original_MC.WaypointList) <= Max and then
       This.Original_MC.FirstWaypoint > 0;

private

end Waypoint_Plan_Manager;
