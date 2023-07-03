with SPARK.Containers.Formal.Unbounded_Hashed_Maps;
with SPARK.Containers.Formal.Unbounded_Hashed_Sets;
with SPARK.Containers.Formal.Unbounded_Ordered_Maps;
with SPARK.Containers.Functional.Maps;
with SPARK.Containers.Functional.Vectors;
with SPARK.Containers.Formal.Doubly_Linked_Lists;

with Common;                              use Common;
with LMCP_Messages;                       use LMCP_Messages;
with Plan_Builder_Communication;          use Plan_Builder_Communication;

package Plan_Builder with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);

   pragma Assertion_Policy (Ignore);

   package ES_Maps is new SPARK.Containers.Functional.Maps
     (Key_Type     => Int64,
      Element_Type => EntityState);
   use ES_Maps;
   subtype EntityState_Map is ES_Maps.Map;

   package Int64_TAS_Maps is new SPARK.Containers.Formal.Unbounded_Hashed_Maps
       (Key_Type     => Int64,
        Element_Type => LMCP_Messages.TaskAssignmentSummary,
        Hash         => Int64_Hash);
   subtype Int64_TaskAssignmentSummary_Map is Int64_TAS_Maps.Map (Int64_TAS_Maps.Default_Modulus (10));
   use Int64_TAS_Maps;
   package Int64_TaskAssignmentSummary_Map_P renames Int64_TAS_Maps.Formal_Model.P;
   package Int64_TaskAssignmentSummary_Map_K renames Int64_TAS_Maps.Formal_Model.K;

   package Int64_TA_Maps is new SPARK.Containers.Formal.Unbounded_Hashed_Maps
       (Key_Type     => Int64,
        Element_Type => TaskAssignment,
        Hash         => Int64_Hash);
   subtype Int64_TaskAssignment_Map is Int64_TA_Maps.Map;
   use Int64_TA_Maps;

   package Int64_UAReq_Maps is new SPARK.Containers.Formal.Unbounded_Hashed_Maps
       (Key_Type     => Int64,
        Element_Type => UniqueAutomationRequest,
        Hash         => Int64_Hash);
   subtype Int64_UniqueAutomationRequest_Map is Int64_UAReq_Maps.Map (Int64_UAReq_Maps.Default_Modulus (10));
   use Int64_UAReq_Maps;

   package Int64_UAResp_Maps is new SPARK.Containers.Formal.Unbounded_Hashed_Maps
       (Key_Type     => Int64,
        Element_Type => UniqueAutomationResponse,
        Hash         => Int64_Hash);
   subtype Int64_UniqueAutomationResponse_Map is Int64_UAResp_Maps.Map (Int64_UAResp_Maps.Default_Modulus (10));
   use Int64_UAResp_Maps;

   type ProjectedState is record
      State : PlanningState;
      FinalWaypointID : Int64 := 0;
      Time : Int64 := 0;
   end record;

   package ProjectedState_Sequences is new SPARK.Containers.Functional.Vectors
        (Index_Type   => Positive,
         Element_Type => ProjectedState);
   type ProjectedState_Seq is new ProjectedState_Sequences.Sequence;

   package Int64_PS_Maps is new SPARK.Containers.Functional.Maps
       (Key_Type     => Int64,
        Element_Type => ProjectedState_Seq);
   subtype Int64_ProjectedState_Map is Int64_PS_Maps.Map;
   use Int64_PS_Maps;

   package Int64_TAL_Maps is new SPARK.Containers.Formal.Unbounded_Hashed_Maps
       (Key_Type     => Int64,
        Element_Type => TaskAssignment_Sequence,
        Hash         => Int64_Hash);
   subtype Int64_RemainingTaskAssignement_Map is Int64_TAL_Maps.Map (Int64_TAL_Maps.Default_Modulus (10));
   use Int64_TAL_Maps;

   package Int64_ER_Maps is new SPARK.Containers.Formal.Unbounded_Hashed_Maps
       (Key_Type     => Int64,
        Element_Type => Int64,
        Hash         => Int64_Hash);
   subtype Int64_Int64_Map is Int64_ER_Maps.Map (Int64_ER_Maps.Default_Modulus (10));
   use Int64_ER_Maps;

   package SpeedAltPair_Sequences is new SPARK.Containers.Functional.Vectors
     (Index_Type   => Positive,
      Element_Type => SpeedAltPair);
   type SpeedAltPair_Sequence is new SpeedAltPair_Sequences.Sequence;

   package Int64_ROR_Maps is new SPARK.Containers.Formal.Unbounded_Hashed_Maps
       (Key_Type     => Int64,
        Element_Type => SpeedAltPair_Sequence,
        Hash         => Int64_Hash);
   subtype Int64_ReqeustIDVsOverrides_Map is Int64_ROR_Maps.Map (Int64_ROR_Maps.Default_Modulus (10));
   use Int64_ROR_Maps;

   type Plan_Builder_State is record
      m_uniqueAutomationRequests : Int64_UniqueAutomationRequest_Map;
      m_inProgressResponse : Int64_UniqueAutomationResponse_Map;
      m_assignmentSummaries : Int64_TaskAssignmentSummary_Map;
      m_projectedEntityStates : Int64_ProjectedState_Map;
      m_remainingAssignments : Int64_RemainingTaskAssignement_Map;
      m_expectedResponseID : Int64_Int64_Map;
      m_currentEntityStates : EntityState_Map;
      m_reqeustIDVsOverrides : Int64_ReqeustIDVsOverrides_Map;
   end record;

   type Plan_Builder_Configuration_Data is record
      m_assignmentStartPointLead_m     : Real64       := 50.0;
      m_addLoiterToEndOfMission        : Boolean      := False;
      m_deafultLoiterRadius            : Real32       := 300.0;
      m_overrideTurnType               : Boolean      := False;
      m_turnType                       : TurnTypeEnum := TurnShort;
      m_taskImplementationId           : Int64        := 1;
      m_commandId                      : Int64        := 1;
   end record;

   procedure Process_Task_Implementation_Response
     (State            : in out Plan_Builder_State;
      Config           : in out Plan_Builder_Configuration_Data;
      Mailbox          : in out Plan_Builder_Mailbox;
      Received_Message : LMCP_Messages.TaskImplementationResponse);

   procedure Send_Next_Task_Implementation_Request
     (uniqueRequestID : Int64;
      Mailbox          : in out Plan_Builder_Mailbox;
      State : in out Plan_Builder_State;
      Config : in out Plan_Builder_Configuration_Data);

   procedure Process_Task_Assignment_Summary
     (State            : in out Plan_Builder_State;
      Config           : in out Plan_Builder_Configuration_Data;
      Mailbox          : in out Plan_Builder_Mailbox;
      Received_Message : TaskAssignmentSummary);

   procedure Check_Next_Task_Implementation_Request
     (uniqueRequestID : Int64;
      Mailbox : in out Plan_Builder_Mailbox;
      State : in out Plan_Builder_State;
      Config : in out Plan_Builder_Configuration_Data);

   procedure Add_Loiters_To_Mission_Commands
     (State : in out Plan_Builder_State;
      Config : in out Plan_Builder_Configuration_Data;
      Mailbox : in out Plan_Builder_Mailbox;
      Response : UniqueAutomationResponse);

end Plan_Builder;
