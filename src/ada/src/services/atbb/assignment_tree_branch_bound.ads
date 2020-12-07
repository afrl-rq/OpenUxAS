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

   type Assignment_Tree_Branch_Bound_Configuration_Data is record
      Cost_Function        : Cost_Function_Kind := Minmax;
      Number_Nodes_Maximum : Int64 := 0;
   end record;

   type Assignment_Tree_Branch_Bound_State is record
      m_uniqueAutomationRequests : Int64_UniqueAutomationRequest_Map;
      m_taskPlanOptions          : Int64_TaskPlanOptions_Map_Map;
      m_assignmentCostMatrixes   : Int64_AssignmentCostMatrix_Map;
   end record;

   procedure Handle_Unique_Automation_Request
     (Mailbox : in out Assignment_Tree_Branch_Bound_Mailbox;
      Data    : Assignment_Tree_Branch_Bound_Configuration_Data;
      State   : in out Assignment_Tree_Branch_Bound_State;
      Areq    : UniqueAutomationRequest);

   procedure Handle_Task_Plan_Options
     (Mailbox : in out Assignment_Tree_Branch_Bound_Mailbox;
      Data    : Assignment_Tree_Branch_Bound_Configuration_Data;
      State   : in out Assignment_Tree_Branch_Bound_State;
      Options : TaskPlanOptions);

   procedure Handle_Assignment_Cost_Matrix
     (Mailbox : in out Assignment_Tree_Branch_Bound_Mailbox;
      Data    : Assignment_Tree_Branch_Bound_Configuration_Data;
      State   : in out Assignment_Tree_Branch_Bound_State;
      Matrix  : AssignmentCostMatrix);

   procedure Check_Assignment_Ready
     (Mailbox : in out Assignment_Tree_Branch_Bound_Mailbox;
      Data    : Assignment_Tree_Branch_Bound_Configuration_Data;
      State   : in out Assignment_Tree_Branch_Bound_State;
      ReqId   : Int64);

   procedure Send_TaskAssignmentSummary
     (Mailbox : in out Assignment_Tree_Branch_Bound_Mailbox;
      Data    : Assignment_Tree_Branch_Bound_Configuration_Data;
      State   : in out Assignment_Tree_Branch_Bound_State;
      ReqId   : Int64);

   procedure Run_Calculate_Assignment
     (Data                   : Assignment_Tree_Branch_Bound_Configuration_Data;
      Automation_Request     : UniqueAutomationRequest;
      Assignment_Cost_Matrix : AssignmentCostMatrix;
      TaskPlanOptions_Map    : Int64_TPO_Map;
      Summary                : out TaskAssignmentSummary);
   --  Returns the assignment that minimizes the cost.

end Assignment_Tree_Branch_Bound;
