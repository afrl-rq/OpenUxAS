with Ada.Text_IO; use Ada.Text_IO; -- temporarily

with Ada.Containers.Vectors;
with DOM.Core.Elements;
with Ada.Characters.Handling;
with LMCP_Message_Conversions;

with UxAS.Messages.Route.RouteRequest;          use UxAS.Messages.Route.RouteRequest;
with UxAS.Messages.Route.RoutePlanRequest;      use UxAS.Messages.Route.RoutePlanRequest;
with UxAS.Messages.Route.RoutePlanResponse;     use UxAS.Messages.Route.RoutePlanResponse;
with UxAS.Messages.lmcptask.PlanningState;      use UxAS.Messages.lmcptask.PlanningState;
with UxAS.Messages.Route.RouteConstraints;      use UxAS.Messages.Route.RouteConstraints;
with AFRL.Vehicles.GroundVehicleState;          use AFRL.Vehicles.GroundVehicleState;
with AFRL.Vehicles.SurfaceVehicleState;         use AFRL.Vehicles.SurfaceVehicleState;
with AFRL.Vehicles.GroundVehicleConfiguration;  use AFRL.Vehicles.GroundVehicleConfiguration;
with AFRL.Vehicles.SurfaceVehicleConfiguration; use AFRL.Vehicles.SurfaceVehicleConfiguration;
with AFRL.Impact.ImpactAutomationRequest;       use AFRL.Impact.ImpactAutomationRequest;
with AFRL.CMASI.AirVehicleConfiguration;        use AFRL.CMASI.AirVehicleConfiguration;
with AFRL.CMASI.AutomationRequest;              use AFRL.CMASI.AutomationRequest;
with AFRL.CMASI.Location3D;                     use AFRL.CMASI.Location3D;
with AFRL.CMASI.AirVehicleState;                use AFRL.CMASI.AirVehicleState;

package body UxAS.Comms.LMCP_Net_Client.Service.Route_Aggregation is

   procedure Handle_RoutePlanResponse_Msg
     (This : in out Route_Aggregator_Service;
      Msg  : RoutePlanResponse_Any);

   procedure Handle_RouteRequest_Msg
     (This : in out Route_Aggregator_Service;
      Msg  : RouteRequest_Any);

   procedure Handle_AirVehicleState_Msg
     (This : in out Route_Aggregator_Service;
      Msg  : EntityState_Any);

   procedure Handle_GroundVehicleState_Msg
     (This : in out Route_Aggregator_Service;
      Msg  : EntityState_Any);

   procedure Handle_SurfaceVehicleState_Msg
     (This : in out Route_Aggregator_Service;
      Msg  : EntityState_Any);

   procedure Handle_AirVehicleConfig_Msg
     (This : in out Route_Aggregator_Service;
      Msg  : EntityConfiguration_Any);

   procedure Handle_GroundVehicleConfig_Msg
     (This : in out Route_Aggregator_Service;
      Msg  : EntityConfiguration_Any);

   procedure Handle_SurfaceVehicleConfig_Msg
     (This : in out Route_Aggregator_Service;
      Msg  : EntityConfiguration_Any);

   procedure Handle_UniqueAutomationRequest_Msg
     (This : in out Route_Aggregator_Service;
      Msg  : UniqueAutomationRequest_Any);

   procedure Handle_ImpactAutomationRequest_Msg
     (This : in out Route_Aggregator_Service;
      Msg  : ImpactAutomationRequest_Any);

   procedure Handle_TaskPlanOptions_Msg
     (This : in out Route_Aggregator_Service;
      Msg  : TaskPlanOptions_Any);

   procedure Check_All_Route_Plans
     (This : in out Route_Aggregator_Service);

   package RoutePlanRequest_Sequences is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => RoutePlanRequest_Any);

   procedure Euclidian_Plan
     (This : in out Route_Aggregator_Service;
      Plan : RoutePlanRequest_Any);

   ---------------------------------
   -- Registry_Service_Type_Names --
   ---------------------------------

   function Registry_Service_Type_Names return Service_Type_Names_List is
      (Service_Type_Names_List'(1 => Instance (Service_Type_Name_Max_Length, Content => Type_Name)));

   ------------
   -- Create --
   ------------

   function Create return Any_Service is
      Result : Any_Service;
   begin
      Result := new Route_Aggregator_Service;
      Result.Construct_Service
        (Service_Type        => Type_Name,
         Work_Directory_Name => Directory_Name);
      return Result;
   end Create;

   ---------------
   -- Configure --
   ---------------

   overriding
   procedure Configure
     (This     : in out Route_Aggregator_Service;
      XML_Node : DOM.Core.Element;
      Result   : out Boolean)
   is
      Unused : Boolean;
   begin
      declare
         Attr_Value : constant String := DOM.Core.Elements.Get_Attribute (XML_Node, Name => "FastPlan");
         use Ada.Characters.Handling;
      begin
         if Attr_Value = "" then
            This.Config.m_fastPlan := False;  -- FastPlan is an optional parameter
         elsif To_Lower (Attr_Value) = "true" then
            This.Config.m_fastPlan := True;
         elsif To_Lower (Attr_Value) = "false" then
            This.Config.m_fastPlan := False;
         else -- malformed boolean value
            Result := False;
            return;
         end if;
      end;

      --  track states and configurations for assignment cost matrix calculation
      --  [EntityStates] are used to calculate costs from current position to first task
      --  [EntityConfigurations] are used for nominal speed values (all costs are in terms of time to arrive)

      --  addSubscriptionAddress(AFRL::cmasi::EntityConfiguration::Subscription);
      This.Add_Subscription_Address (AFRL.CMASI.EntityConfiguration.Subscription, Unused);
      for Descendant of EntityConfiguration_Descendants loop
         This.Add_Subscription_Address (Descendant, Unused);
      end loop;

      --  addSubscriptionAddress(AFRL::cmasi::EntityState::Subscription);
      This.Add_Subscription_Address (AFRL.CMASI.EntityState.Subscription, Unused);
      for Descendant of EntityState_Descendants loop
         This.Add_Subscription_Address (Descendant, Unused);
      end loop;

      --  track requests to kickoff matrix calculation
      --  addSubscriptionAddress(uxas::messages::task::UniqueAutomationRequest::Subscription);
      This.Add_Subscription_Address (UxAS.Messages.lmcptask.UniqueAutomationRequest.Subscription, Unused);

      --  subscribe to task plan options to build matrix
      --  addSubscriptionAddress(uxas::messages::task::TaskPlanOptions::Subscription);
      This.Add_Subscription_Address (UxAS.Messages.lmcptask.TaskPlanOptions.Subscription, Unused);

      --  handle batch route requests
      --  addSubscriptionAddress(uxas::messages::route::RouteRequest::Subscription);
      This.Add_Subscription_Address (UxAS.Messages.Route.RouteRequest.Subscription, Unused);

      --  listen for responses to requests from route planner(s)
      --  addSubscriptionAddress(uxas::messages::route::RoutePlanResponse::Subscription);
      This.Add_Subscription_Address (UxAS.Messages.Route.RoutePlanResponse.Subscription, Unused);

      --  // Subscribe to group messages (whisper from local route planner)
      --  //TODO REVIEW DESIGN "RouteAggregator" "RoutePlanner" flip message addressing effecting session behavior

      --  return true; // may not have the proper fast plan value, but proceed anyway
      Result := True;
   end Configure;

   ----------------
   -- Initialize --
   ----------------

   overriding
   procedure Initialize
     (This   : in out Route_Aggregator_Service;
      Result : out Boolean)
   is
   begin
      Result := True; --  per the C++ version

      Route_Aggregator_Communication.Initialize
        (This.Mailbox,
         Source_Group => Value (This.Message_Source_Group),
         Unique_Id    => Common.Int64 (UxAS.Comms.LMCP_Net_Client.Unique_Entity_Send_Message_Id),
         Entity_Id    => Common.UInt32 (This.Entity_Id),
         Service_Id   => Common.UInt32 (This.Network_Id));
   end Initialize;

   -----------------------------------
   -- Process_Received_LMCP_Message --
   -----------------------------------

   overriding
   procedure Process_Received_LMCP_Message
     (This             : in out Route_Aggregator_Service;
      Received_Message :        not null Any_LMCP_Message;
      Should_Terminate :    out Boolean)
   is
   begin
      --  if (uxas::messages::route::isRoutePlanResponse(receivedLmcpMessage->m_object.get()))
      if Received_Message.Payload.all in RoutePlanResponse'Class then
         This.Handle_RoutePlanResponse_Msg (RoutePlanResponse_Any (Received_Message.Payload));

      --  else if (uxas::messages::route::isRouteRequest(receivedLmcpMessage->m_object.get()))
      elsif Received_Message.Payload.all in RouteRequest'Class then
         This.Handle_RouteRequest_Msg (RouteRequest_Any (Received_Message.Payload));

      --  else if (std::dynamic_pointer_cast<AFRL::cmasi::AirVehicleState>(receivedLmcpMessage->m_object))
      elsif Received_Message.Payload.all in AirVehicleState'Class then
         This.Handle_AirVehicleState_Msg (EntityState_Any (Received_Message.Payload));

      --  else if (AFRL::vehicles::isGroundVehicleState(receivedLmcpMessage->m_object.get()))
      elsif Received_Message.Payload.all in GroundVehicleState'Class then
         This.Handle_GroundVehicleState_Msg (EntityState_Any (Received_Message.Payload));

      --  else if (AFRL::vehicles::isSurfaceVehicleState(receivedLmcpMessage->m_object.get()))
      elsif Received_Message.Payload.all in SurfaceVehicleState'Class then
         This.Handle_SurfaceVehicleState_Msg (EntityState_Any (Received_Message.Payload));

      --  else if (std::dynamic_pointer_cast<AFRL::cmasi::AirVehicleConfiguration>(receivedLmcpMessage->m_object))
      elsif Received_Message.Payload.all in AirVehicleConfiguration'Class then
         This.Handle_AirVehicleConfig_Msg (EntityConfiguration_Any (Received_Message.Payload));

      --  else if (AFRL::vehicles::isGroundVehicleConfiguration(receivedLmcpMessage->m_object.get()))
      elsif Received_Message.Payload.all in GroundVehicleConfiguration'Class then
         This.Handle_GroundVehicleConfig_Msg (EntityConfiguration_Any (Received_Message.Payload));

      --  else if (AFRL::vehicles::isSurfaceVehicleConfiguration(receivedLmcpMessage->m_object.get()))
      elsif Received_Message.Payload.all in SurfaceVehicleConfiguration'Class then
         This.Handle_SurfaceVehicleConfig_Msg (EntityConfiguration_Any (Received_Message.Payload));

      --  else if (uxas::messages::task::isUniqueAutomationRequest(receivedLmcpMessage->m_object.get()))
      elsif Received_Message.Payload.all in UxAS.Messages.lmcptask.UniqueAutomationRequest.UniqueAutomationRequest'Class then
         This.Handle_UniqueAutomationRequest_Msg (UniqueAutomationRequest_Any (Received_Message.Payload));

      --  else if (AFRL::impact::isImpactAutomationRequest(receivedLmcpMessage->m_object.get()))
      elsif Received_Message.Payload.all in ImpactAutomationRequest'Class then
         This.Handle_ImpactAutomationRequest_Msg (ImpactAutomationRequest_Any (Received_Message.Payload));

      --  else if (uxas::messages::task::isTaskPlanOptions(receivedLmcpMessage->m_object.get()))
      elsif Received_Message.Payload.all in TaskPlanOptions'Class then
         This.Handle_TaskPlanOptions_Msg (TaskPlanOptions_Any (Received_Message.Payload));
      end if;

      Should_Terminate := False;
   end Process_Received_LMCP_Message;

   ----------------------------------
   -- Handle_RoutePlanResponse_Msg --
   ----------------------------------

   procedure Handle_RoutePlanResponse_Msg
     (This : in out Route_Aggregator_Service;
      Msg  : RoutePlanResponse_Any)
   is
      use LMCP_Message_Conversions;
   begin
      Route_Aggregator.Handle_Route_Plan_Response (This.Mailbox, This.State, As_RoutePlanResponse_Message (Msg));
   end Handle_RoutePlanResponse_Msg;

   -----------------------------
   -- Handle_RouteRequest_Msg --
   -----------------------------

   procedure Handle_RouteRequest_Msg
     (This : in out Route_Aggregator_Service;
      Msg  : RouteRequest_Any)
   is
      use LMCP_Message_Conversions;
   begin
      Route_Aggregator.Handle_Route_Request (This.Config, This.Mailbox, This.State, As_RouteRequest_Message (Msg));
   end Handle_RouteRequest_Msg;

   --------------------------------
   -- Handle_AirVehicleState_Msg --
   --------------------------------

   procedure Handle_AirVehicleState_Msg
     (This : in out Route_Aggregator_Service;
      Msg  : EntityState_Any)
   is
      use LMCP_Message_Conversions;
   begin
      --  {
      --      int64_t id = std::static_pointer_cast<AFRL::cmasi::EntityState>(receivedLmcpMessage->m_object)->getID();
      --      m_entityStates[id] = std::static_pointer_cast<AFRL::cmasi::EntityState>(receivedLmcpMessage->m_object);
      --      m_airVehicles.insert(id);
      --  }
      Route_Aggregator.Handle_Air_Vehicle_State
        (This.Config,
         As_EntityState_Message (Msg));
   end Handle_AirVehicleState_Msg;

   -----------------------------------
   -- Handle_GroundVehicleState_Msg --
   -----------------------------------

   procedure Handle_GroundVehicleState_Msg
     (This : in out Route_Aggregator_Service;
      Msg  : EntityState_Any)
   is
      use LMCP_Message_Conversions;
   begin
      --  {
      --      int64_t id = std::static_pointer_cast<AFRL::cmasi::EntityState>(receivedLmcpMessage->m_object)->getID();
      --      m_entityStates[id] = std::static_pointer_cast<AFRL::cmasi::EntityState>(receivedLmcpMessage->m_object);
      --      m_groundVehicles.insert(id);
      --  }
      Route_Aggregator.Handle_Ground_Vehicle_State
        (This.Config,
         As_EntityState_Message (Msg));
   end Handle_GroundVehicleState_Msg;

   ------------------------------------
   -- Handle_SurfaceVehicleState_Msg --
   ------------------------------------

   procedure Handle_SurfaceVehicleState_Msg
     (This : in out Route_Aggregator_Service;
      Msg  : EntityState_Any)
   is
      use LMCP_Message_Conversions;
   begin
      --  {
      --      int64_t id = std::static_pointer_cast<AFRL::cmasi::EntityState>(receivedLmcpMessage->m_object)->getID();
      --      m_entityStates[id] = std::static_pointer_cast<AFRL::cmasi::EntityState>(receivedLmcpMessage->m_object);
      --      m_surfaceVehicles.insert(id);
      --  }
      Route_Aggregator.Handle_Surface_Vehicle_State
        (This.Config,
         As_EntityState_Message (Msg));
   end Handle_SurfaceVehicleState_Msg;

   ---------------------------------
   -- Handle_AirVehicleConfig_Msg --
   ---------------------------------

   procedure Handle_AirVehicleConfig_Msg
     (This : in out Route_Aggregator_Service;
      Msg  : EntityConfiguration_Any)
   is
      Id : constant Common.Int64 := Common.Int64 (Msg.getID);
   begin
      --  {
      --      int64_t id = std::static_pointer_cast<AFRL::cmasi::EntityConfiguration>(receivedLmcpMessage->m_object)->getID();
      --      m_entityConfigurations[id] = std::static_pointer_cast<AFRL::cmasi::EntityConfiguration>(receivedLmcpMessage->m_object);
      --      m_airVehicles.insert(id);
      --  }
      Route_Aggregator.Handle_Air_Vehicle_Config
        (This.Config,
         Id);
   end Handle_AirVehicleConfig_Msg;

   ------------------------------------
   -- Handle_GroundVehicleConfig_Msg --
   ------------------------------------

   procedure Handle_GroundVehicleConfig_Msg
     (This : in out Route_Aggregator_Service;
      Msg  : EntityConfiguration_Any)
   is
      Id : constant Common.Int64 := Common.Int64 (Msg.getID);
   begin
      --  {
      --      int64_t id = std::static_pointer_cast<AFRL::cmasi::EntityConfiguration>(receivedLmcpMessage->m_object)->getID();
      --      m_entityConfigurations[id] = std::static_pointer_cast<AFRL::cmasi::EntityConfiguration>(receivedLmcpMessage->m_object);
      --      m_groundVehicles.insert(id);
      --  }
      Route_Aggregator.Handle_Ground_Vehicle_Config
        (This.Config,
         Id);
   end Handle_GroundVehicleConfig_Msg;

   -------------------------------------
   -- Handle_SurfaceVehicleConfig_Msg --
   -------------------------------------

   procedure Handle_SurfaceVehicleConfig_Msg
     (This : in out Route_Aggregator_Service;
      Msg  : EntityConfiguration_Any)
   is
      Id : constant Common.Int64 := Common.Int64 (Msg.getID);
   begin
      --  {
      --      int64_t id = std::static_pointer_cast<AFRL::cmasi::EntityConfiguration>(receivedLmcpMessage->m_object)->getID();
      --      m_entityConfigurations[id] = std::static_pointer_cast<AFRL::cmasi::EntityConfiguration>(receivedLmcpMessage->m_object);
      --      m_surfaceVehicles.insert(id);
      --  }
      Route_Aggregator.Handle_Surface_Vehicle_Config
        (This.Config,
         Id);
   end Handle_SurfaceVehicleConfig_Msg;

   ----------------------------------------
   -- Handle_UniqueAutomationRequest_Msg --
   ----------------------------------------

   procedure Handle_UniqueAutomationRequest_Msg
     (This : in out Route_Aggregator_Service;
      Msg  : UniqueAutomationRequest_Any)
   is
   begin
      --  {
      --      auto areq = std::static_pointer_cast<uxas::messages::task::UniqueAutomationRequest>(receivedLmcpMessage->m_object);
      --      m_uniqueAutomationRequests[m_autoRequestId++] = areq;
      --      //ResetTaskOptions(areq); // clear m_taskOptions and wait for refresh from tasks
      --      CheckAllTaskOptionsReceived();
      --  }
      Route_Aggregator.Handle_Unique_Automation_Request
        (This.Config,
         This.Mailbox,
         This.State,
         LMCP_Message_Conversions.As_UniqueAutomationRequest_Message (Msg));
   end Handle_UniqueAutomationRequest_Msg;

   ----------------------------------------
   -- Handle_ImpactAutomationRequest_Msg --
   ----------------------------------------

   procedure Handle_ImpactAutomationRequest_Msg
     (This : in out Route_Aggregator_Service;
      Msg  : ImpactAutomationRequest_Any)
   is
      --      auto areq = std::shared_ptr<uxas::messages::task::UniqueAutomationRequest>();
      AReq : constant UniqueAutomationRequest_Any := new UxAS.Messages.lmcptask.UniqueAutomationRequest.UniqueAutomationRequest;
   begin
      --      auto sreq = std::static_pointer_cast<AFRL::impact::ImpactAutomationRequest>(receivedLmcpMessage->m_object);
      --  Msg corresponds to sreq in this Ada version

      --      areq->setOriginalRequest(sreq->getTrialRequest()->clone());
      AReq.setOriginalRequest (new AutomationRequest'(Msg.getTrialRequest.all));
      --      m_uniqueAutomationRequests[m_autoRequestId++] = areq;
      --      areq->setRequestID(m_autoRequestId);
      AReq.setRequestID (AVTAS.LMCP.Types.Int64 (This.State.m_autoRequestId + 1));
      --      //ResetTaskOptions(areq); // clear m_taskOptions and wait for refresh from tasks
      --      CheckAllTaskOptionsReceived();
      Route_Aggregator.Handle_Unique_Automation_Request
        (This.Config,
         This.Mailbox,
         This.State,
         LMCP_Message_Conversions.As_UniqueAutomationRequest_Message (AReq));

   end Handle_ImpactAutomationRequest_Msg;

   --------------------------------
   -- Handle_TaskPlanOptions_Msg --
   --------------------------------

   procedure Handle_TaskPlanOptions_Msg
     (This : in out Route_Aggregator_Service;
      Msg  : TaskPlanOptions_Any)
   is
   begin
      Route_Aggregator.Handle_Task_Plan_Options
        (This.Mailbox,
         This.Config,
         This.State,
         LMCP_Message_Conversions.As_TaskPlanOption_Message (Msg));
   end Handle_TaskPlanOptions_Msg;

   --------------------
   -- Euclidian_Plan --
   --------------------

   procedure Euclidian_Plan
     (This : in out Route_Aggregator_Service;
      Plan : RoutePlanRequest_Any)
   is
      use LMCP_Message_Conversions;
   begin
      Route_Aggregator.Euclidean_Plan
        (Data               => This.Config,
         routePlanResponses => This.State.m_routePlanResponses,
         routePlans         => This.State.m_routePlans,
         Request            => As_RoutePlanRequest_Message (Plan));
   end Euclidian_Plan;

   ---------------------------
   -- Check_All_Route_Plans --
   ---------------------------

   procedure Check_All_Route_Plans
     (This : in out Route_Aggregator_Service)
   is
   begin
      Route_Aggregator.Check_All_Route_Plans (This.Mailbox, This.State);
   end Check_All_Route_Plans;

   -----------------------------
   -- Package Executable Part --
   -----------------------------

   --  This is the executable part for the package, invoked automatically and only once.
begin
   --  All concrete service subclasses must call this procedure in their
   --  own package like this, with their own params.
   Register_Service_Creation_Function_Pointers (Registry_Service_Type_Names, Create'Access);
end UxAS.Comms.LMCP_Net_Client.Service.Route_Aggregation;
