-- __TODO__
-- Add packages needed by the SPARK portion of the service.
--
-- __Example__
-- 
-- with SPARK.Containers.Formal.Hashed_Maps;
-- with SPARK.Containers.Formal.Vectors;
-- with Ada.Containers;                             use all type Ada.Containers.Count_Type;

with Common;                                     use Common;
with definitions;                                use definitions;
with LMCP_Messages;                              use LMCP_Messages;
with Daidalus_Response_Mailboxes;               use Daidalus_Response_Mailboxes;

package Daidalus_Response with SPARK_Mode is
   
   type WCV_data is private;

   -- __TODO__
   -- Define SPARK-compatible types needed by the SPARK portion of the service.
   -- Since this package is included by
   -- uxas-comms-lmcp_net_client-service-<Service_Name_Variant>, these types
   -- will be available in that package as well.
   --
   -- __Example__
   --
   -- Max : constant Ada.Containers.Count_Type := 2000;
   -- subtype Pos64 is Int64 range 1 .. Int64'Last;
   -- subtype Nat64 is Int64 range 0 .. Int64'Last;
   --
   -- subtype Ext_Vector_Index is Natural range 0 .. Integer (Max);
   -- subtype Vector_Index is Natural range 1 .. Integer (Max);
   -- 
   -- function Pos64_Hash (X : Pos64) return Ada.Containers.Hash_Type is
   --   (Ada.Containers.Hash_Type'Mod (X));
   --    
   -- package Pos64_Nat64_Maps is new
   --   SPARK.Containers.Formal.Hashed_Maps (Pos64, Nat64, Pos64_Hash);
   -- use Pos64_Nat64_Maps;
   -- subtype Pos64_Nat64_Map is
   --   Pos64_Nat64_Maps.Map (Max, Pos64_Nat64_Maps.Default_Modulus (Max));
   -- 
   -- package Pos64_Vectors is new SPARK.Containers.Formal.Vectors (Positive, Pos64);
   -- subtype Pos64_Vector is Pos64_Vectors.Vector (Max);
   -- package Pos_Vec_M renames Pos64_Vectors.Formal_Model.M;
   -- use Pos64_Vectors;
   -- 
   -- use Pos64_Nat64_Maps.Formal_Model;
   -- use Pos64_Vectors.Formal_Model;
 
   type Daidalus_Response_Configuration_Data is record
      VehicleID : definitions.ID_Type := -1;
      ActionTimeThreshold : definitions.action_time_sec := 25.0;
      PriorityTimeThreshold : definitions.priority_time_sec := -1.0;
      
      -- __TODO__
      -- Define fields for this record. This record is intended to hold data
      -- that tend to be invariant over the life of the service. Values for
      -- fields are generally given default values that can be changed by
      -- the `Service_Base` procedure `Configure` based on the contents of the
      -- UxAS XML configuration file.
      -- 
      -- __Example__
      --
      -- LoiterRadiusDefault : Common.Real64 := 200.0;
      -- TurnType : LMCP_Messages.TurnTypeEnum := TurnShort;
      -- VehicleID : Common.Int64 := -1;
   end record;

   type Daidalus_Response_State is record 
      MissionCommand : definitions.MissionCommand;
      ReadyToAct : Boolean := True;
      Status : definitions.Status_Type := OnMission;
      NextWaypoint : Int64 := -1;
      IsTrackingNextWaypoint : Boolean := True;
      Altitude_Max_m : Altitude_Type_m := 10_000.0;
      Altitude_Min_m : Altitude_Type_m := 100.0;
      Altitude_Interval_Buffer_m : Altitude_Buffer_Type_m := 20.0;
      Heading_Max_deg : Heading_Type_deg := 360.0;
      Heading_Min_deg : Heading_Type_deg := 0.0;
      Heading_Interval_Buffer_deg : Heading_Buffer_Type_deg := 5.0;
      GroundSpeed_Max_mps : GroundSpeed_Type_mps := 360.111;
      GroundSpeed_Min_mps : GroundSpeed_Type_mps := 5.1444;
      GroundSpeed_Interval_Buffer_mps : GroundSpeed_Buffer_Type_mps := 10.0;
      PreconditionsMet : Boolean;
   end record;
   
      -- __TODO__
      -- Define fields for this record, which is intended to hold data that tend
      -- to vary over the life of the service. Values for fields tend to be set
      -- when messages are received or internal logic processing occurs.
      --
      -- __Example__
      --
      -- Id_To_Next_Id : Pos64_Nat64_Map;
      -- Path : Pos64_Vector;
      -- Cycle_Index : Ext_Vector_Index;
      -- Segment : Pos64_Vector;
      -- Headed_To_First_Id : Boolean := False;

   -- __TODO__
   -- Define subprograms needed for the SPARK portion of the service. These
   -- include ghost functions for specification and subprograms that implement
   -- key parts of the service's behavior. The latter may include message
   -- handlers for SPARK-compatible messages, which by convention are named
   -- `Handle_<MessageType>` and are called by Ada message handlers in the Ada
   -- portion of the service, i.e. package
   -- `uxas-comms-lmcp_net_client-service-Daidalus_Response`.
   --
   -- Subprograms can directly broadcast SPARK-compatible messages using the
   -- mailbox and procedures from package <service_name>_communication. Such
   -- subprograms must be procedures because they have side-effects on the
   -- mailbox, and they likely have side-effects on the state as well.
   --
   -- __Example Stubs__
   --
   -- procedure Handle_MissionCommand
   --   (State : in out <Service_Name>_State;
   --    MC : MissionCommand)
   --   with
   --     Pre =>
   --       ...
   --     Post =>
   --       ...
   --
   -- procedure Produce_Segment
   --   (State : in out <Service_Name>_State;
   --    Config : <Service_Name>_Configuration_Data;
   --    Mailbox : in out <Service_Name>_Mailbox)
   --   with
   --     Pre =>
   --       ...
   --     Post =>
   --      ...
   procedure Process_WellclearViolation_Message 
     (m_DAIDALUSResponseServiceState : in out Daidalus_Response_State;
      m_DAIDALUSResponseServiceConfig : Daidalus_Response_Configuration_Data;
      m_DAIDALUSResponseServiceMailbox : in out Daidalus_Response_Mailbox;
      WCV_Intervals : LMCP_Messages.WellClearViolationIntervals);
   
   procedure Process_DAIDALUSConfiguration_Message 
     (m_DAIDALUSResponseServiceState : in out Daidalus_Response_State;
      m_DAIDALUSResponseServiceConfig : Daidalus_Response_Configuration_Data;
      ConfigurationMessage : LMCP_Messages.DAIDALUSConfiguration);
   
   procedure Process_MissionCommand_Message 
     (m_DAIDALUSResponseServiceState : in out Daidalus_Response_State;
      m_DAIDALUSResponseServiceConfig : Daidalus_Response_Configuration_Data;
      MissionCommandMessage : LMCP_Messages.MissionCommand);

private
   type WCV_data is record
      CurrentState : definitions.state_parameters;
      AltitudeBands : definitions.OrderedIntervalVector32;
      HeadingBands : definitions.OrderedIntervalVector;
      GroundspeedBands : definitions.OrderedIntervalVector;
      RAltitudeBands : definitions.OrderedIntervalVector32;
      RHeadingBands : definitions.OrderedIntervalVector;
      RGroundspeedBands : definitions.OrderedIntervalVector;
      IntrudersInfo : definitions.Intruder_info_Vector;
      AltitudeZones : definitions.ZoneVector;
      HeadingZones : definitions.ZoneVector;
      GroundspeedZones : definitions.ZoneVector;      
   end record;         
   
   Inconsistent_Message : exception;
   Violated_precondition : exception;
   Improper_Configuration : exception;

end Daidalus_Response;
