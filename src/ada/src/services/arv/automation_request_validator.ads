with SPARK.Containers.Formal.Doubly_Linked_Lists;
with SPARK.Containers.Formal.Hashed_Maps;
with SPARK.Containers.Functional.Maps;
with SPARK.Containers.Types;                     use SPARK.Containers.Types;
with Ada.Strings.Unbounded;                      use Ada.Strings.Unbounded;
with Automation_Request_Validator_Communication; use Automation_Request_Validator_Communication;
with Common;                                     use Common;
with LMCP_Messages;                              use LMCP_Messages;

--  Package containing the fonctionality of the service. It uses its communication
--  counter-part to send and receive messages.

package Automation_Request_Validator with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);

   --  Configuration data is separated from the service state as it is not
   --  handled by the same primitives. We use functional containers, as it is
   --  not supposed to be modified often.

   type OperatingRegionAreas is record
      KeepInAreas  : Int64_Seq;
      KeepOutAreas : Int64_Seq;
   end record;

   package Int64_Operating_Region_Maps is new SPARK.Containers.Functional.Maps
     (Key_Type     => Int64,
      Element_Type => OperatingRegionAreas);
   type Operating_Region_Map is new Int64_Operating_Region_Maps.Map;

   type Task_Kind is (Angled_Area_Search_Task, Impact_Line_Search_Task, Impact_Point_Search_Task, Other_Task);

   type Task_Kind_And_Id (Kind : Task_Kind := Other_Task) is record
      case Kind is
      when Angled_Area_Search_Task =>
         SearchAreaID : Int64;
      when Impact_Line_Search_Task =>
         LineID : Int64;
      when Impact_Point_Search_Task =>
         SearchLocationID : Int64;
      when Other_Task =>
         null;
      end case;
   end record;

   package Int64_Task_Maps is new SPARK.Containers.Functional.Maps
     (Key_Type     => Int64,
      Element_Type => Task_Kind_And_Id);
   type Task_Map is new Int64_Task_Maps.Map;

   type Automation_Request_Validator_Configuration_Data is record
      Available_Configuration_Entity_Ids : Int64_Set;
      Available_State_Entity_Ids         : Int64_Set;
      Available_KeepIn_Zones_Ids         : Int64_Set;
      Available_KeepOut_Zones_Ids        : Int64_Set;
      Available_Area_of_Interest_Ids     : Int64_Set;
      Available_Line_of_Interest_Ids     : Int64_Set;
      Available_Point_of_Interest_Ids    : Int64_Set;
      Available_Operating_Regions        : Operating_Region_Map;
      Available_Tasks                    : Task_Map;
      Available_Initialized_Tasks        : Int64_Set;
   end record;

   --  The mutable state of the service is stored inside formal containers which
   --  can be modified.

   type Automation_Request_Type is
     (Automation_Request,
      Sandbox_Automation_Request,
      Task_Automation_Request);

   type Request_Details (Request_Type : Automation_Request_Type := Automation_Request) is record
      case Request_Type is
         when Sandbox_Automation_Request =>
            Play_Id         : Int64 := 0;
            Soln_Id         : Int64 := 0;
         when Task_Automation_Request =>
            Task_Request_Id : Int64 := 0;
         when others =>
            null;
      end case;
   end record;

   package Int64_Request_Details_Maps is new SPARK.Containers.Functional.Maps
     (Key_Type     => Int64,
      Element_Type => Request_Details);

   type Request_Details_Map is new Int64_Request_Details_Maps.Map;

   package UniqueAutomationRequest_Lists is new SPARK.Containers.Formal.Doubly_Linked_Lists
     (Element_Type => UniqueAutomationRequest);

   Max_UniqueAutomationRequest_Deque_Depth : constant := 200; -- arbitrary
   subtype UniqueAutomationRequest_Ref_Deque is UniqueAutomationRequest_Lists.List
     (Capacity => Max_UniqueAutomationRequest_Deque_Depth);

   use UniqueAutomationRequest_Lists;

   type Automation_Request_Validator_State is record
      Sandbox                    : Request_Details_Map;
      Pending_Requests           : UniqueAutomationRequest_Ref_Deque;
      Requests_Waiting_For_Tasks : UniqueAutomationRequest_Ref_Deque;
   end record;

   --------------------------------------
   -- Functions for annotation purpose --
   --------------------------------------

   function All_Elements_In (V : Int64_Seq; S : Int64_Set) return Boolean with Ghost;

   function Contains (V : Int64_Seq; E : Int64) return Boolean with Ghost;

   function Valid_Automation_Request
     (This    : Automation_Request_Validator_Configuration_Data;
      Request : UniqueAutomationRequest)
      return Boolean
   with Ghost, Global => null;

   -------------------------------------
   -- Regular Service Functionalities --
   -------------------------------------

   use Int64_Request_Details_Maps;

   --  In the specification, we can clearly separate the parts which are updated
   --  or not to simplify annotation.

   procedure Check_Automation_Request_Requirements
     (Config  : Automation_Request_Validator_Configuration_Data;
      Sandbox : in out Request_Details_Map;
      Mailbox : in out Automation_Request_Validator_Mailbox;
      Request : in out UniqueAutomationRequest;
      IsReady : out Boolean)
     with Pre => Has_Key (Sandbox, Request.RequestID),
     Post =>
     --  Request was removed from Sandbox iff IsReady is False

     Has_Key (Sandbox, Request.RequestID) = IsReady
     and then Sandbox <= Sandbox'Old
     and then Keys_Included_Except
       (Left    => Sandbox'Old,
        Right   => Sandbox,
        New_Key => Request.RequestID)

     --  IsReady is true if the automation request is valid

     and then IsReady = Valid_Automation_Request (Config, Request'Old)
     and then Request.OperatingRegion'Old = Request.OperatingRegion
     and then Request.TaskList'Old = Request.TaskList
     and then Request.RedoAllTasks'Old = Request.RedoAllTasks
     and then Request.RequestID'Old = Request.RequestID
     and then Request.PlanningStates'Old = Request.PlanningStates
     and then Request.SandboxRequest'Old = Request.SandboxRequest;

   procedure Handle_Automation_Request
     (Config  : Automation_Request_Validator_Configuration_Data;
      State   : in out Automation_Request_Validator_State;
      Mailbox : in out Automation_Request_Validator_Mailbox;
      Request : AutomationRequest);

   procedure Handle_Impact_Automation_Request
     (Config  : Automation_Request_Validator_Configuration_Data;
      State   : in out Automation_Request_Validator_State;
      Mailbox : in out Automation_Request_Validator_Mailbox;
      Request : ImpactAutomationRequest)
   with
     Pre => not Has_Key (State.Sandbox, Request.RequestID);

   procedure Handle_Task_Automation_Request
     (Config  : Automation_Request_Validator_Configuration_Data;
      State   : in out Automation_Request_Validator_State;
      Mailbox : in out Automation_Request_Validator_Mailbox;
      Request : TaskAutomationRequest)
   with
     Pre => not Has_Key (State.Sandbox, Request.RequestID);

   procedure Handle_Automation_Response
     (State    : in out Automation_Request_Validator_State;
      Mailbox  : in out Automation_Request_Validator_Mailbox;
       Response : UniqueAutomationResponse);

   procedure Check_Tasks_Initialized
     (Config  : Automation_Request_Validator_Configuration_Data;
      State   : in out Automation_Request_Validator_State;
      Mailbox : in out Automation_Request_Validator_Mailbox);

private

   function All_Elements_In (V : Int64_Seq; S : Int64_Set) return Boolean is
     (for all E of V => Contains (S, E));

   function Contains (V : Int64_Seq; E : Int64) return Boolean is
     (for some F of V => E = F);

   function Check_For_Required_Entity_Configurations
     (Entity_Ids      : Int64_Seq;
      Configurations  : Int64_Set;
      States          : Int64_Set;
      Planning_States : PlanningState_Seq)
      return Boolean
   is
     (All_Elements_In (Entity_Ids, Configurations)
       and then
        (for all E of Entity_Ids =>
           (Contains (States, E)
              or else
                (not Is_Empty (States)
                 and then (for some Planning_State of Planning_States =>
                             E = Planning_State.EntityID))))
      and then
        (if Length (Entity_Ids) = 0 then
             (for some Id of Configurations => Contains (States, Id))))
   with Ghost;

   function Check_For_Required_Operating_Region_And_Keepin_Keepout_Zones
     (Operating_Region  : Int64;
      Operating_Regions : Operating_Region_Map;
      KeepIn_Zones_Ids  : Int64_Set;
      KeepOut_Zones_Ids : Int64_Set)
      return Boolean
   is
     (if Operating_Region /= 0
      then Has_Key (Operating_Regions, Operating_Region)
      and then All_Elements_In
        (Get (Operating_Regions, Operating_Region).KeepInAreas,
         KeepIn_Zones_Ids)
      and then All_Elements_In
        (Get (Operating_Regions, Operating_Region).KeepOutAreas,
         KeepOut_Zones_Ids))
   with Ghost;

   function Check_For_Specific_Task_Requirements
     (Available_Area_of_Interest_Ids  : Int64_Set;
      Available_Line_of_Interest_Ids  : Int64_Set;
      Available_Point_of_Interest_Ids : Int64_Set;
      ItTask                          : Task_Kind_And_Id)
      return Boolean
   is
     (case ItTask.Kind is
         when Angled_Area_Search_Task  =>
            ItTask.SearchAreaID = 0
      or else Contains (Available_Area_of_Interest_Ids, ItTask.SearchAreaID),
         when Impact_Line_Search_Task  =>
            ItTask.LineID = 0
      or else Contains (Available_Line_of_Interest_Ids, ItTask.LineID),
         when Impact_Point_Search_Task =>
            ItTask.SearchLocationID = 0
      or else Contains (Available_Point_of_Interest_Ids, ItTask.SearchLocationID),
         when Other_Task            => True)
   with Ghost;

   function Check_For_Required_Tasks_And_Task_Requirements
     (Available_Tasks                 : Task_Map;
      Available_Area_of_Interest_Ids  : Int64_Set;
      Available_Line_of_Interest_Ids  : Int64_Set;
      Available_Point_of_Interest_Ids : Int64_Set;
      TaskIds                         : Int64_Seq)
      return Boolean
   is
     (for all T of TaskIds =>
         Has_Key (Available_Tasks, T)
      and then Check_For_Specific_Task_Requirements
        (Available_Area_of_Interest_Ids,
         Available_Line_of_Interest_Ids,
         Available_Point_of_Interest_Ids,
         Get (Available_Tasks, T)))
   with Ghost;

   function Valid_Automation_Request
     (This    : Automation_Request_Validator_Configuration_Data;
      Request : UniqueAutomationRequest)
      return Boolean
   is
     (Check_For_Required_Entity_Configurations
       (Entity_Ids      => Request.EntityList,
        Configurations  => This.Available_Configuration_Entity_Ids,
        States          => This.Available_State_Entity_Ids,
        Planning_States => Request.PlanningStates)

       and then Check_For_Required_Operating_Region_And_Keepin_Keepout_Zones
         (Operating_Region  => Request.OperatingRegion,
          Operating_Regions => This.Available_Operating_Regions,
          KeepIn_Zones_Ids  => This.Available_KeepIn_Zones_Ids,
          KeepOut_Zones_Ids => This.Available_KeepOut_Zones_Ids)

      and then Check_For_Required_Tasks_And_Task_Requirements
        (Available_Tasks                 => This.Available_Tasks,
         Available_Area_of_Interest_Ids  => This.Available_Area_of_Interest_Ids,
         Available_Line_of_Interest_Ids  => This.Available_Line_of_Interest_Ids,
         Available_Point_of_Interest_Ids => This.Available_Point_of_Interest_Ids,
         TaskIds                         => Request.TaskList));
end Automation_Request_Validator;
