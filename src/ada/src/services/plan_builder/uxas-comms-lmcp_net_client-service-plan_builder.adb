with Ada.Characters.Handling;
with Ada.Numerics.Generic_Elementary_Functions; 
with DOM.Core.Elements;
with LMCP_Messages;
with LMCP_Message_Conversions;                       use LMCP_Message_Conversions;
with AFRL.CMASI.AirVehicleConfiguration;             use AFRL.CMASI.AirVehicleConfiguration;
with AFRL.CMASI.AirVehicleState;                     use AFRL.CMASI.AirVehicleState;
with AFRL.CMASI.AutomationRequest;                   use AFRL.CMASI.AutomationRequest;
with AFRL.Impact.ImpactAutomationRequest;            use AFRL.Impact.ImpactAutomationRequest;
with AFRL.Vehicles.GroundVehicleConfiguration;       use AFRL.Vehicles.GroundVehicleConfiguration;
with AFRL.Vehicles.GroundVehicleState;               use AFRL.Vehicles.GroundVehicleState;
with AFRL.Vehicles.SurfaceVehicleConfiguration;      use AFRL.Vehicles.SurfaceVehicleConfiguration;
with AFRL.Vehicles.SurfaceVehicleState;              use AFRL.Vehicles.SurfaceVehicleState;
with UxAS.Messages.Route.RoutePlanResponse;          use UxAS.Messages.Route.RoutePlanResponse;
with UxAS.Messages.Route.RouteRequest;               use UxAS.Messages.Route.RouteRequest;
with AFRL.CMASI.EntityConfiguration;                 use AFRL.CMASI.EntityConfiguration;
with AFRL.CMASI.EntityState;                         use AFRL.CMASI.EntityState;
with AFRL.CMASI.ServiceStatus;                       use AFRL.CMASI.ServiceStatus;
with AFRL.CMASI.MissionCommand;                      use AFRL.CMASI.MissionCommand;
with AFRL.CMASI.Waypoint;                            use AFRL.CMASI.Waypoint;
with AFRL.CMASI.KeyValuePair;                        use AFRL.CMASI.KeyValuePair;
with UxAS.Messages.lmcptask.TaskAutomationResponse;  use UxAS.Messages.lmcptask.TaskAutomationResponse;
with AVTAS.LMCP.Types;
with UxAS.Messages.lmcptask.TaskPlanOptions;         use UxAS.Messages.lmcptask.TaskPlanOptions;
with UxAS.Messages.lmcptask.UniqueAutomationRequest; use UxAS.Messages.lmcptask.UniqueAutomationRequest;
with UxAS.Messages.lmcptask.PlanningState;           use UxAS.Messages.lmcptask.PlanningState;
with AFRL.CMASI.Location3D;                          use AFRL.CMASI.Location3D;
with UxAS.Messages.lmcptask.TaskAssignment;          use UxAS.Messages.lmcptask.TaskAssignment;
with UxAS.Messages.lmcptask.TaskAssignmentSummary;   use UxAS.Messages.lmcptask.TaskAssignmentSummary;
with Ada.Containers; use Ada.Containers;
with UxAS.Messages.lmcptask.TaskImplementationRequest; use UxAS.Messages.lmcptask.TaskImplementationRequest;
with UxAS.Messages.lmcptask.TaskImplementationResponse; use UxAS.Messages.lmcptask.TaskImplementationResponse;
with Plan_Builder;
with AFRL.Impact.ImpactAutomationResponse; use AFRL.Impact.ImpactAutomationResponse;
with Common; use Common;
with SPARK.Containers.Formal.Unbounded_Hashed_Maps;
with SPARK.Containers.Formal.Doubly_Linked_Lists;

package body UxAS.Comms.LMCP_Net_Client.Service.Plan_Builder is

   procedure Handle_EntityState_Msg
     (This : in out Plan_Builder_Service;
      Msg  : EntityState_Any);

   procedure Handle_ImpactAutomationRequest_Msg
     (This : in out Plan_Builder_Service;
      Msg  : ImpactAutomationRequest_Any);

   procedure Handle_ImpactAutomationResponse_Msg
     (This : in out Plan_Builder_Service;
      Msg  : ImpactAutomationResponse_Any);

   procedure Handle_TaskAssignmentSummary_Msg
     (This : in out Plan_Builder_Service;
      Msg  : TaskAssignmentSummary_Any);

   procedure Handle_TaskImplementationResponse_Msg
     (This : in out Plan_Builder_Service;
      Msg  : TaskImplementationResponse_Any);

   procedure Handle_UniqueAutomationRequest_Msg
     (This : in out Plan_Builder_Service;
      Msg  : UniqueAutomationRequest_Any);

   ---------------
   -- Configure --
   ---------------

   overriding
   procedure Configure
     (This     : in out Plan_Builder_Service;
      XML_Node : DOM.Core.Element;
      Result   : out Boolean)
   is
      Unused : Boolean;
      use LMCP_Messages;
   begin
      declare
         Attr_AssignmentStartPointLead : constant String := DOM.Core.Elements.Get_Attribute (XML_Node, Name => "AssignmentStartPointLead_m");
         Attr_AddLoiterToEndOfMission : constant String := DOM.Core.Elements.Get_Attribute (XML_Node, Name => "AddLoiterToEndOfMission");
         Attr_DefaultLoiterRadius : constant String := DOM.Core.Elements.Get_Attribute (XML_Node, Name => "DefaultLoiterRadius_m");
         Attr_TurnType : constant String := DOM.Core.Elements.Get_Attribute (XML_Node, Name => "TurnType");
         use Ada.Characters.Handling;
      begin

         if not (Attr_AssignmentStartPointLead = "") then
            This.Config.m_assignmentStartPointLead_m := Common.Real64 (Float'Value (Attr_AssignmentStartPointLead));
         else --malformed configuration parameter
            Result := False;
            return;
         end if;

         if Attr_AddLoiterToEndOfMission = "" then
            This.Config.m_addLoiterToEndOfMission := False;  -- FastPlan is an optional parameter
         elsif To_Lower (Attr_AddLoiterToEndOfMission) = "true" then
            This.Config.m_addLoiterToEndOfMission := True;
         elsif To_Lower (Attr_AddLoiterToEndOfMission) = "false" then
            This.Config.m_addLoiterToEndOfMission := False;
         else -- malformed boolean value
            Result := False;
            return;
         end if;

         if not (Attr_DefaultLoiterRadius = "") then
            This.Config.m_deafultLoiterRadius := Common.Real32 (Float'Value (Attr_DefaultLoiterRadius));
         else --malformed configuration parameter
            Result := False;
            return;
         end if;

         if not (Attr_TurnType = "") then
            This.Config.m_overrideTurnType := True;
            
            if Attr_TurnType = "FlyOver" then
               This.Config.m_turnType := FlyOver;
            elsif Attr_TurnType = "TurnShort" then
               This.Config.m_turnType := TurnShort;
            else -- malformed value
               Result := False;
               return;
            end if;
         end if;
      end;

   -- addSubscriptionAddress(uxas::messages::task::UniqueAutomationRequest::Subscription);
      This.Add_Subscription_Address (UxAS.Messages.lmcptask.UniqueAutomationRequest.Subscription, Unused);

   -- addSubscriptionAddress(uxas::messages::task::TaskAssignmentSummary::Subscription);
      This.Add_Subscription_Address (UxAS.Messages.lmcptask.TaskAssignmentSummary.Subscription, Unused);

   -- addSubscriptionAddress(uxas::messages::task::TaskImplementationResponse::Subscription);
      This.Add_Subscription_Address (UxAS.Messages.lmcptask.TaskImplementationResponse.Subscription, Unused);

   -- addSubscriptionAddress(afrl::impact::ImpactAutomationRequest::Subscription);
      This.Add_Subscription_Address (AFRL.Impact.ImpactAutomationRequest.Subscription, Unused);

   -- addSubscriptionAddress(afrl::impact::ImpactAutomationResponse::Subscription);
      This.Add_Subscription_Address (AFRL.Impact.ImpactAutomationResponse.Subscription, Unused);

   --  addSubscriptionAddress(afrl::CMASI::EntityState::Subscription);
      This.Add_Subscription_Address (AFRL.CMASI.EntityState.Subscription, Unused);
      for Descendant of AFRL.CMASI.EntityState.EntityState_Descendants loop
         This.Add_Subscription_Address (Descendant, Unused);
      end loop;

      --  return true; // may not have the proper fast plan value, but proceed anyway
      Result := True;
   end Configure;

   ----------------
   -- Initialize --
   ----------------

   overriding
   procedure Initialize
     (This   : in out Plan_Builder_Service;
      Result : out Boolean)
   is
   begin
      Result := True; --  per the C++ version

      Plan_Builder_Communication.Initialize
        (This.Mailbox,
         Source_Group => Value (This.Message_Source_Group),
         Unique_Id    => Common.Int64 (UxAS.Comms.LMCP_Net_Client.Unique_Entity_Send_Message_Id),
         Entity_Id    => Common.UInt32 (This.Entity_Id),
         Service_Id   => Common.UInt32 (This.Network_Id));
   end Initialize;
   ------------
   -- Create --
   ------------

   function Create return Any_Service is
      Result : Any_Service;
   begin
      Result := new Plan_Builder_Service;
      Result.Construct_Service
        (Service_Type        => Type_Name,
         Work_Directory_Name => Directory_Name);
      return Result;
   end Create;
   
   ---------------------------------
   -- Registry_Service_Type_Names --
   ---------------------------------

   function Registry_Service_Type_Names return Service_Type_Names_List is
      (Service_Type_Names_List'(1 => Instance (Service_Type_Name_Max_Length, Content => Type_Name)));

   ----------------------------------
   -- Handle_RoutePlanResponse_Msg --
   ----------------------------------

   procedure Handle_EntityState_Msg
     (This : in out Plan_Builder_Service;
      Msg  : EntityState_Any)
   is
      use LMCP_Message_Conversions;
      Entity_State : constant LMCP_Messages.EntityState := As_EntityState_Message (Msg);
      Id           : constant Common.Int64 := Common.Int64 (Msg.getID);
   begin
      This.State.m_currentEntityStates := ES_Maps.Add (This.State.m_currentEntityStates, Id, Entity_State);
   end Handle_EntityState_Msg;
   
   procedure Handle_ImpactAutomationRequest_Msg
     (This : in out Plan_Builder_Service;
      Msg  : ImpactAutomationRequest_Any)
   is
      ReqID : Common.Int64 := Common.Int64 (Msg.getRequestID);
      Overrides : Vect_SpeedAltPair_Acc_Acc := Msg.getOverridePlanningConditions;
   begin
      declare
         Overrides_List : SpeedAltPair_Sequence;
      begin
         for Override of Overrides.all loop
            Overrides_List := Add (Overrides_List, As_SpeedAltPair_Message (Override));
         end loop;
         
         Int64_ROR_Maps.Include (This.State.m_reqeustIDVsOverrides, ReqID, Overrides_List);
      end;
         
   end Handle_ImpactAutomationRequest_Msg;
   
   procedure Handle_ImpactAutomationResponse_Msg
     (This : in out Plan_Builder_Service;
      Msg  : ImpactAutomationResponse_Any)
   is
      ResponseID : Common.Int64 := Common.Int64 (Msg.getResponseID);
   begin
      Int64_ROR_Maps.Delete (This.State.m_reqeustIDVsOverrides, ResponseID);
   end Handle_ImpactAutomationResponse_Msg;
   
   procedure Handle_TaskAssignmentSummary_Msg
     (This : in out Plan_Builder_Service;
      Msg  : TaskAssignmentSummary_Any)
   is
      Should_Terminate : Boolean;
   begin
      Process_Task_Assignment_Summary (This.State, This.Config, This.Mailbox, As_TaskAssignmentSummary_Message (Msg), Should_Terminate);
   end Handle_TaskAssignmentSummary_Msg;
   
   procedure Handle_TaskImplementationResponse_Msg
     (This : in out Plan_Builder_Service;
      Msg  : TaskImplementationResponse_Any)
   is
   begin
      Process_Task_Implementation_Response (This.State, This.Config, This.Mailbox, As_TaskImplementationReponse_Message (Msg));
   end Handle_TaskImplementationResponse_Msg;
   
   procedure Handle_UniqueAutomationRequest_Msg
     (This : in out Plan_Builder_Service;
      Msg  : UniqueAutomationRequest_Any)
   is
      UniqueAutoReq : LMCP_Messages.UniqueAutomationRequest := As_UniqueAutomationRequest_Message (Msg);
      ReqID : Common.Int64 := Common.Int64 (Msg.getRequestID);
      Task_Summary : LMCP_Messages.TaskAssignmentSummary;
      Projected_States : ProjectedState_Seq;
      Tasks : LMCP_Messages.TaskAssignment_Sequence;
      Aut_Response : LMCP_Messages.UniqueAutomationResponse;
   begin
      Int64_UAReq_Maps.Insert
        (This.State.m_uniqueAutomationRequests,
         ReqID,
         UniqueAutoReq);

      Int64_TAS_Maps.Insert
        (This.State.m_assignmentSummaries,
         ReqID,
         Task_Summary);
      
      This.State.m_projectedEntityStates := Int64_PS_Maps.Add
        (This.State.m_projectedEntityStates,
         ReqID,
         Projected_States);
      
      Int64_TAL_Maps.Insert
        (This.State.m_remainingAssignments,
         ReqID,
         Tasks);
      
      Int64_UAResp_Maps.Insert
        (This.State.m_inProgressResponse,
         ReqID,
         Aut_Response);
   end Handle_UniqueAutomationRequest_Msg;
      
   -----------------------------------
   -- Process_Received_LMCP_Message --
   -----------------------------------
   overriding
   procedure Process_Received_LMCP_Message
     (This             : in out Plan_Builder_Service;
      Received_Message : not null Any_LMCP_Message;
      Should_Terminate : out Boolean)
   is
   begin

      -- if(entityState)
      if Received_Message.Payload.all in EntityState'Class then
         This.Handle_EntityState_Msg (EntityState_Any (Received_Message.Payload));
         
         -- else if (afrl::impact::isImpactAutomationRequest(receivedLmcpMessage->m_object))
      elsif Received_Message.Payload.all in ImpactAutomationRequest'Class then
         This.Handle_ImpactAutomationRequest_Msg (ImpactAutomationRequest_Any (Received_Message.Payload));
                  
         -- else if (afrl::impact::isImpactAutomationResponse(receivedLmcpMessage->m_object))
      elsif Received_Message.Payload.all in ImpactAutomationResponse'Class then
         This.Handle_ImpactAutomationResponse_Msg (ImpactAutomationResponse_Any (Received_Message.Payload));
         
         -- else if(uxas::messages::task::isTaskAssignmentSummary(receivedLmcpMessage->m_object))
      elsif Received_Message.Payload.all in TaskAssignmentSummary'Class then
         This.Handle_TaskAssignmentSummary_Msg (TaskAssignmentSummary_Any (Received_Message.Payload));
         
         -- else if(uxas::messages::task::isTaskImplementationResponse(receivedLmcpMessage->m_object))
      elsif Received_Message.Payload.all in TaskImplementationResponse'Class then
         This.Handle_TaskImplementationResponse_Msg (TaskImplementationResponse_Any (Received_Message.Payload));
            
         -- else if(uxas::messages::task::isUniqueAutomationRequest(receivedLmcpMessage->m_object))
      elsif Received_Message.Payload.all in UniqueAutomationRequest'Class then
         This.Handle_UniqueAutomationRequest_Msg (UniqueAutomationRequest_Any (Received_Message.Payload));
      end if;
      
      Should_Terminate := False;
   end Process_Received_LMCP_Message;
            
end UxAS.Comms.LMCP_Net_Client.Service.Plan_Builder;
