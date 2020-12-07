with Ada.Containers.Formal_Hashed_Maps;
with Ada.Containers.Functional_Maps;
with Assignment_Tree_Branch_Bound_Communication; use Assignment_Tree_Branch_Bound_Communication;
with Common;                                     use Common;
with LMCP_Messages;                              use LMCP_Messages;

package Assignment_Tree_Branch_Bound with SPARK_Mode is

   type Cost_Function_Kind is (Minmax, Cumulative);

   package Int64_UAR_Maps is new Ada.Containers.Formal_Hashed_Maps
       (Key_Type     => Int64,
        Element_Type => UniqueAutomationRequest,
        Hash         => Int64_Hash);
   subtype Int64_UniqueAutomationRequest_Map is
     Int64_UAR_Maps.Map (200, Int64_UAR_Maps.Default_Modulus (200));
   use Int64_UAR_Maps;

   package Int64_TaskPlanOptions_Maps is new Ada.Containers.Functional_Maps
     (Key_Type     => Int64,
      Element_Type => TaskPlanOptions);
   type Int64_TPO_Map is new Int64_TaskPlanOptions_Maps.Map;

   package Int64_TPO_Map_Maps is new Ada.Containers.Formal_Hashed_Maps
     (Key_Type => Int64,
      Element_Type => Int64_TPO_Map,
      Hash  => Int64_Hash);
   subtype Int64_TaskPlanOptions_Map_Map is
     Int64_TPO_Map_Maps.Map (200, Int64_TPO_Map_Maps.Default_Modulus (200));
   use Int64_TPO_Map_Maps;

   package Int64_ACM_Maps is new Ada.Containers.Formal_Hashed_Maps
       (Key_Type     => Int64,
        Element_Type => AssignmentCostMatrix,
        Hash         => Int64_Hash);
   subtype Int64_AssignmentCostMatrix_Map is
     Int64_ACM_Maps.Map (200, Int64_ACM_Maps.Default_Modulus (200));
   use Int64_ACM_Maps;

   ----------------------------
   -- Annotation subprograms --
   ----------------------------

   function Valid_TaskPlanOptions
     (TaskPlanOptions_Map : Int64_TPO_Map)
      return Boolean;

   function Valid_AssignmentCostMatrix
     (Assignment_Cost_Matrix : AssignmentCostMatrix)
      return Boolean;

   ----------------------------------------
   -- Assignment Tree Branch Bound types --
   ----------------------------------------

   type Assignment_Tree_Branch_Bound_Configuration_Data is record
      Cost_Function        : Cost_Function_Kind := Minmax;
      Number_Nodes_Maximum : Int64 := 0;
   end record;

   type Assignment_Tree_Branch_Bound_State is record
      m_uniqueAutomationRequests : Int64_UniqueAutomationRequest_Map;
      m_taskPlanOptions          : Int64_TaskPlanOptions_Map_Map;
      m_assignmentCostMatrixes   : Int64_AssignmentCostMatrix_Map;
   end record with
     Predicate =>
       (for all ReqId of m_taskPlanOptions =>
          Valid_TaskPlanOptions (Element (m_taskPlanOptions, ReqId)))
          and then
       (for all ReqId of m_assignmentCostMatrixes =>
          Valid_AssignmentCostMatrix (Element (m_assignmentCostMatrixes, ReqId)));

   ---------------------------
   -- Service functionality --
   ---------------------------

   procedure Handle_Unique_Automation_Request
     (Mailbox : in out Assignment_Tree_Branch_Bound_Mailbox;
      Data    : Assignment_Tree_Branch_Bound_Configuration_Data;
      State   : in out Assignment_Tree_Branch_Bound_State;
      Areq    : UniqueAutomationRequest)
   with
     Pre => not Contains (State.m_uniqueAutomationRequests, Areq.RequestID);

   procedure Handle_Task_Plan_Options
     (Mailbox : in out Assignment_Tree_Branch_Bound_Mailbox;
      Data    : Assignment_Tree_Branch_Bound_Configuration_Data;
      State   : in out Assignment_Tree_Branch_Bound_State;
      Options : TaskPlanOptions)
   with
     Pre =>
       (for all TaskOption of Options.Options =>
          (TaskOption.Cost >= 0
             and then Options.TaskID = TaskOption.TaskID))
         and then
       (not Contains (State.m_taskPlanOptions, Options.CorrespondingAutomationRequestID)
          or else
        not Has_Key (Element (State.m_taskPlanOptions, Options.CorrespondingAutomationRequestID), Options.TaskID));

   procedure Handle_Assignment_Cost_Matrix
     (Mailbox : in out Assignment_Tree_Branch_Bound_Mailbox;
      Data    : Assignment_Tree_Branch_Bound_Configuration_Data;
      State   : in out Assignment_Tree_Branch_Bound_State;
      Matrix  : AssignmentCostMatrix)
   with Pre =>
     not Contains (State.m_assignmentCostMatrixes, Matrix.CorrespondingAutomationRequestID)
       and then Valid_AssignmentCostMatrix (Matrix);

   procedure Check_Assignment_Ready
     (Mailbox : in out Assignment_Tree_Branch_Bound_Mailbox;
      Data    : Assignment_Tree_Branch_Bound_Configuration_Data;
      State   : in out Assignment_Tree_Branch_Bound_State;
      ReqId   : Int64);

   procedure Send_TaskAssignmentSummary
     (Mailbox : in out Assignment_Tree_Branch_Bound_Mailbox;
      Data    : Assignment_Tree_Branch_Bound_Configuration_Data;
      State   : in out Assignment_Tree_Branch_Bound_State;
      ReqId   : Int64)
   with
     Pre =>
       Contains (State.m_uniqueAutomationRequests, ReqId)
         and then Contains (State.m_assignmentCostMatrixes, ReqId)
         and then Contains (State.m_taskPlanOptions, ReqId)
         and then
           (for all TaskId of Element (State.m_uniqueAutomationRequests, ReqId).TaskList =>
              Has_Key (Element (State.m_taskPlanOptions, ReqId), TaskId))
         and then
           Valid_TaskPlanOptions (Element (State.m_taskPlanOptions, ReqId));

   procedure Run_Calculate_Assignment
     (Data                   : Assignment_Tree_Branch_Bound_Configuration_Data;
      Automation_Request     : UniqueAutomationRequest;
      Assignment_Cost_Matrix : AssignmentCostMatrix;
      TaskPlanOptions_Map    : Int64_TPO_Map;
      Summary                : out TaskAssignmentSummary)
   with
     Pre =>
       Valid_AssignmentCostMatrix (Assignment_Cost_Matrix)
         and then
       Valid_TaskPlanOptions (TaskPlanOptions_Map)
         and then
       (for all TaskId of Automation_Request.TaskList =>
          Has_Key (TaskPlanOptions_Map, TaskId))
         and then
       (for all Id of TaskPlanOptions_Map =>
          (for all TaskOption of Get (TaskPlanOptions_Map, Id).Options => TaskOption.TaskID = Id));
   --  Returns the assignment that minimizes the cost.

private

   function Valid_TaskPlanOptions
     (TaskPlanOptions_Map : Int64_TPO_Map)
      return Boolean
   is
      (for all TaskId of TaskPlanOptions_Map =>
         (TaskId = Get (TaskPlanOptions_Map, TaskId).TaskID
            and then
          (for all TaskOption of Get (TaskPlanOptions_Map, TaskId).Options =>
                  (TaskId = TaskOption.TaskID
                     and then TaskOption.Cost >= 0))));

   function Valid_AssignmentCostMatrix
     (Assignment_Cost_Matrix : AssignmentCostMatrix)
      return Boolean
   is
      (for all TOC of Assignment_Cost_Matrix.CostMatrix => TOC.TimeToGo >= 0);
end Assignment_Tree_Branch_Bound;
