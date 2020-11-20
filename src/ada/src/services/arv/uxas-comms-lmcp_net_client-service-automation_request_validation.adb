with AFRL.CMASI.AutomationRequest;                  use AFRL.CMASI.AutomationRequest;
with AFRL.CMASI.EntityConfiguration;                use AFRL.CMASI.EntityConfiguration;
with AFRL.CMASI.EntityState;                        use AFRL.CMASI.EntityState;
with AFRL.CMASI.lmcpTask;                           use AFRL.CMASI.lmcpTask;
with AFRL.CMASI.KeepInZone;                         use AFRL.CMASI.KeepInZone;
with AFRL.CMASI.KeepOutZone;                        use AFRL.CMASI.KeepOutZone;
with AFRL.CMASI.RemoveTasks;                        use AFRL.CMASI.RemoveTasks;
with AFRL.CMASI.ServiceStatus;                      use AFRL.CMASI.ServiceStatus;
with AFRL.CMASI.OperatingRegion;                    use AFRL.CMASI.OperatingRegion;

with AFRL.Impact.ImpactAutomationRequest;           use AFRL.Impact.ImpactAutomationRequest;
with AFRL.Impact.PointOfInterest;                   use AFRL.Impact.PointOfInterest;
with AFRL.Impact.LineOfInterest;                    use AFRL.Impact.LineOfInterest;
with AFRL.Impact.AreaOfInterest;                    use AFRL.Impact.AreaOfInterest;

with UxAS.Messages.lmcptask.TaskAutomationRequest;    use UxAS.Messages.lmcptask.TaskAutomationRequest;
with UxAS.Messages.lmcptask.TaskInitialized;          use UxAS.Messages.lmcptask.TaskInitialized;
with UxAS.Messages.lmcptask.UniqueAutomationResponse; use UxAS.Messages.lmcptask.UniqueAutomationResponse;

with AFRL.Impact.AngledAreaSearchTask;
with AFRL.Impact.ImpactLineSearchTask;
with AFRL.Impact.ImpactPointSearchTask;

with DOM.Core.Elements;

with Common; use Common;
with LMCP_Message_Conversions; use LMCP_Message_Conversions;

package body UxAS.Comms.LMCP_Net_Client.Service.Automation_Request_Validation is

   function UInt32_Attribute
     (XML_Node : DOM.Core.Element;
      Name     : String;
      Default  : AVTAS.LMCP.Types.UInt32)
   return AVTAS.LMCP.Types.UInt32;
   --  convenience function

   --  Refactored out of Process_Received_LMCP_Message for readability, comprehension, etc.
   function Is_Any_Automation_Request (Msg : Object_Any) return Boolean;

   --  Refactored out of Process_Received_LMCP_Message for readability, comprehension, etc.
   procedure Handle_EntityConfig_Msg
     (This         : in out Automation_Request_Validator_Service;
      EntityConfig : EntityConfiguration_Any);

   --  Refactored out of Process_Received_LMCP_Message for readability, comprehension, etc.
   procedure Handle_StateEntity_Msg
     (This  : in out Automation_Request_Validator_Service;
      State : EntityState_Any);

   --  Refactored out of Process_Received_LMCP_Message for readability, comprehension, etc.
   procedure Handle_InitializedTasks_Msg
     (This : in out Automation_Request_Validator_Service;
      Job  : lmcpTask_Any);

   --  Refactored out of Process_Received_LMCP_Message for readability, comprehension, etc.
   procedure Handle_ServiceStatus_Msg
     (This : in out Automation_Request_Validator_Service;
      Msg  : Any_LMCP_Message);

   --  Refactored out of Process_Received_LMCP_Message for readability, comprehension, etc.
   procedure Handle_RemoveTasks_Msg
     (This : in out Automation_Request_Validator_Service;
      Msg  : Any_LMCP_Message);

   --  Refactored out of Process_Received_LMCP_Message for readability, comprehension, etc.
   procedure Handle_TaskInitialized_Msg
     (This : in out Automation_Request_Validator_Service;
      Msg  : Any_LMCP_Message);

   --  Refactored out of Process_Received_LMCP_Message for readability, comprehension, etc.
   procedure Handle_AreaOfInterest_Msg
     (This : in out Automation_Request_Validator_Service;
      Msg  : Any_LMCP_Message);

   --  Refactored out of Process_Received_LMCP_Message for readability, comprehension, etc.
   procedure Handle_LineOfInterest_Msg
     (This : in out Automation_Request_Validator_Service;
      Msg  : Any_LMCP_Message);

   --  Refactored out of Process_Received_LMCP_Message for readability, comprehension, etc.
   procedure Handle_PointofInterest_Msg
     (This : in out Automation_Request_Validator_Service;
      Msg  : Any_LMCP_Message);

   --  Refactored out of Process_Received_LMCP_Message for readability, comprehension, etc.
   procedure Handle_KeepInZone_Msg
     (This : in out Automation_Request_Validator_Service;
      Msg  : Any_LMCP_Message);

   --  Refactored out of Process_Received_LMCP_Message for readability, comprehension, etc.
   procedure Handle_KeepOutZone_Msg
     (This : in out Automation_Request_Validator_Service;
      Msg  : Any_LMCP_Message);

   --  Refactored out of Process_Received_LMCP_Message for readability, comprehension, etc.
   procedure Handle_OperatingRegion_Msg
     (This : in out Automation_Request_Validator_Service;
      Msg  : Any_LMCP_Message);

   --  Refactored out of Process_Received_LMCP_Message for readability, comprehension, etc.
   procedure Handle_Automation_Request
     (This : in out Automation_Request_Validator_Service;
      Msg  : Any_LMCP_Message);

   --  Refactored out of Process_Received_LMCP_Message for readability, comprehension, etc.
   procedure Handle_Automation_Response
     (This : in out Automation_Request_Validator_Service;
      Msg  : Any_LMCP_Message);

   ---------------------------------
   -- Registry_Service_Type_Names --
   ---------------------------------

   function Registry_Service_Type_Names return Service_Type_Names_List is
      (Service_Type_Names_List'(1 => Instance (Service_Type_Name_Max_Length, Content => Type_Name)));

   ------------
   -- Create --
   ------------

   function Create return Any_Service is
      Result : Automation_Request_Validator_Service_Ref;
   begin
      Result := new Automation_Request_Validator_Service;

      Result.Construct_Service
        (Service_Type        => Type_Name,
         Work_Directory_Name => Directory_Name);
      return Any_Service (Result);
   end Create;

   ---------------
   -- Configure --
   ---------------

   overriding
   procedure Configure
     (This     : in out Automation_Request_Validator_Service;
      XML_Node : DOM.Core.Element;
      Result   : out Boolean)
   is
      Unused : Boolean;
   begin
      --  // configure response time parameter, ensure response time is reasonable
      --  m_maxResponseTime_ms = ndComponent.attribute("MaxResponseTime_ms").as_uint(m_maxResponseTime_ms);
      --  if(m_maxResponseTime_ms < 10) m_maxResponseTime_ms = 10;
      This.Max_Response_Time := UInt32_Attribute (XML_Node, "MaxResponseTime_ms", Default => This.Max_Response_Time);
      This.Max_Response_Time := AVTAS.LMCP.Types.UInt32'Max (This.Max_Response_Time, 10);

      --  // translate regular, impact, and task automation requests to unique automation requests
      --  addSubscriptionAddress(afrl::cmasi::AutomationRequest::Subscription);
      This.Add_Subscription_Address (AFRL.CMASI.AutomationRequest.Subscription, Unused);
      --  addSubscriptionAddress(afrl::impact::ImpactAutomationRequest::Subscription);
      This.Add_Subscription_Address (AFRL.Impact.ImpactAutomationRequest.Subscription, Unused);
      --  addSubscriptionAddress(uxas::messages::task::TaskAutomationRequest::Subscription);
      This.Add_Subscription_Address (UxAS.Messages.lmcptask.TaskAutomationRequest.Subscription, Unused);

      --  // respond with appropriate automation response based on unique response
      --  addSubscriptionAddress(uxas::messages::task::UniqueAutomationResponse::Subscription);
      This.Add_Subscription_Address (UxAS.Messages.lmcptask.UniqueAutomationResponse.Subscription, Unused);

      --  // track all entity configurations
      --  addSubscriptionAddress(afrl::cmasi::EntityConfiguration::Subscription);
      This.Add_Subscription_Address (AFRL.CMASI.EntityConfiguration.Subscription, Unused);
      --  std::vector< std::string > childconfigs = afrl::cmasi::EntityConfigurationDescendants();
      --  for(auto child : childconfigs)
      --      addSubscriptionAddress(child);
      for Descendant of EntityConfiguration_Descendants loop
         This.Add_Subscription_Address (Descendant, Unused);
      end loop;

      --  // track all entity states
      --  addSubscriptionAddress(afrl::cmasi::EntityState::Subscription);
      This.Add_Subscription_Address (AFRL.CMASI.EntityState.Subscription, Unused);
      --  std::vector< std::string > childstates = afrl::cmasi::EntityStateDescendants();
      --  for(auto child : childstates)
      --      addSubscriptionAddress(child);
      for Descendant of EntityState_Descendants loop
         This.Add_Subscription_Address (Descendant, Unused);
      end loop;

      --  // track airspace constraints
      --  addSubscriptionAddress(afrl::cmasi::OperatingRegion::Subscription);
      This.Add_Subscription_Address (AFRL.CMASI.OperatingRegion.Subscription, Unused);
      --  addSubscriptionAddress(afrl::cmasi::KeepInZone::Subscription);
      This.Add_Subscription_Address (AFRL.CMASI.KeepInZone.Subscription, Unused);
      --  addSubscriptionAddress(afrl::cmasi::KeepOutZone::Subscription);
      This.Add_Subscription_Address (AFRL.CMASI.KeepOutZone.Subscription, Unused);

      --  // track indicated locations of interest
      --  addSubscriptionAddress(afrl::impact::AreaOfInterest::Subscription);
      This.Add_Subscription_Address (AFRL.Impact.AreaOfInterest.Subscription, Unused);
      --  addSubscriptionAddress(afrl::impact::LineOfInterest::Subscription);
      This.Add_Subscription_Address (AFRL.Impact.LineOfInterest.Subscription, Unused);
      --  addSubscriptionAddress(afrl::impact::PointOfInterest::Subscription);
      This.Add_Subscription_Address (AFRL.Impact.PointOfInterest.Subscription, Unused);

      --  // track all tasks
      --  addSubscriptionAddress(afrl::cmasi::Task::Subscription);
      This.Add_Subscription_Address (AFRL.CMASI.lmcpTask.Subscription, Unused);
      --  std::vector< std::string > childtasks = afrl::cmasi::TaskDescendants();
      --  for(auto child : childtasks)
      --      addSubscriptionAddress(child);
      for Descendant of lmcpTask_Descendants loop
         This.Add_Subscription_Address (Descendant, Unused);
      end loop;

      --  // task removal and initialization
      --  addSubscriptionAddress(afrl::cmasi::RemoveTasks::Subscription);
      This.Add_Subscription_Address (AFRL.CMASI.RemoveTasks.Subscription, Unused);
      --  addSubscriptionAddress(uxas::messages::task::TaskInitialized::Subscription);
      This.Add_Subscription_Address (UxAS.Messages.lmcptask.TaskInitialized.Subscription, Unused);

      --  // track errors during automation request pipeline
      --  addSubscriptionAddress(afrl::cmasi::ServiceStatus::Subscription);
      This.Add_Subscription_Address (AFRL.CMASI.ServiceStatus.Subscription, Unused);

      --  return true;
      Result := True;
   end Configure;

   ----------------
   -- Initialize --
   ----------------

   overriding
   procedure Initialize
     (This   : in out Automation_Request_Validator_Service;
      Result : out Boolean)
   is
      -- since not doing the Timers
   begin
      --  the C++ version creates the timers here (but we don't, unless we implement the timers).
      Automation_Request_Validator_Communication.Initialize
        (This.Mailbox,
         Source_Group => Value (This.Message_Source_Group),
         Unique_Id    => Common.Int64 (UxAS.Comms.LMCP_Net_Client.Unique_Entity_Send_Message_Id),
         Entity_Id    => Common.UInt32 (This.Entity_Id),
         Service_Id   => Common.UInt32 (This.Network_Id));

      Result := True;
   end Initialize;

   -------------------------------
   -- Is_Any_Automation_Request --
   -------------------------------

   function Is_Any_Automation_Request (Msg : Object_Any) return Boolean is
      --  else if (afrl::cmasi::isAutomationRequest(receivedLmcpMessage->m_object) ||
      --          afrl::impact::isImpactAutomationRequest(receivedLmcpMessage->m_object) ||
      --          uxas::messages::task::isTaskAutomationRequest(receivedLmcpMessage->m_object))
      (Msg.all in AutomationRequest'Class       or
       Msg.all in ImpactAutomationRequest'Class or
       Msg.all in TaskAutomationRequest'Class);

   -----------------------------------
   -- Process_Received_LMCP_Message --
   -----------------------------------

   overriding
   procedure Process_Received_LMCP_Message
     (This             : in out Automation_Request_Validator_Service;
      Received_Message : not null Any_LMCP_Message;
      Should_Terminate : out Boolean)
   is
   begin
      if Received_Message.Payload.all in EntityConfiguration'Class then
         This.Handle_EntityConfig_Msg (EntityConfiguration_Any (Received_Message.Payload));

      elsif Received_Message.Payload.all in EntityState'Class then
         This.Handle_StateEntity_Msg (EntityState_Any (Received_Message.Payload));

      elsif Received_Message.Payload.all in lmcpTask'Class then
         This.Handle_InitializedTasks_Msg (lmcpTask_Any (Received_Message.Payload));

      elsif Received_Message.Payload.all in ServiceStatus'Class then
         This.Handle_ServiceStatus_Msg (Received_Message);

      elsif Received_Message.Payload.all in RemoveTasks'Class then
         This.Handle_RemoveTasks_Msg (Received_Message);

      elsif Received_Message.Payload.all in TaskInitialized'Class then
         This.Handle_TaskInitialized_Msg (Received_Message);

      elsif Received_Message.Payload.all in AreaOfInterest'Class then
         This.Handle_AreaOfInterest_Msg (Received_Message);

      elsif Received_Message.Payload.all in LineOfInterest'Class then
         This.Handle_LineOfInterest_Msg (Received_Message);

      elsif Received_Message.Payload.all in PointOfInterest'Class then
         This.Handle_PointofInterest_Msg (Received_Message);

      elsif Received_Message.Payload.all in KeepInZone'Class then
         This.Handle_KeepInZone_Msg (Received_Message);

      elsif Received_Message.Payload.all in KeepOutZone'Class then
         This.Handle_KeepOutZone_Msg (Received_Message);

      elsif Received_Message.Payload.all in OperatingRegion'Class then
         This.Handle_OperatingRegion_Msg (Received_Message);

      elsif Is_Any_Automation_Request (Received_Message.Payload) then
         This.Handle_Automation_Request (Received_Message);

      elsif Received_Message.Payload.all in UniqueAutomationResponse'Class then
         This.Handle_Automation_Response (Received_Message);
      end if;

      --  Note the C++ code never returns anything other than False...
      Should_Terminate := False;
   end Process_Received_LMCP_Message;

   -----------------------------
   -- Handle_EntityConfig_Msg --
   -----------------------------

   procedure Handle_EntityConfig_Msg
     (This         : in out Automation_Request_Validator_Service;
      EntityConfig : EntityConfiguration_Any)
   is
      ID : constant Common.Int64 := Common.Int64 (EntityConfig.getID);
   begin
      --  m_availableConfigurationEntityIds.insert(entityConfig->getID());
      if not Contains (This.Config.Available_Configuration_Entity_Ids, ID) then
         This.Config.Available_Configuration_Entity_Ids :=
           Add (This.Config.Available_Configuration_Entity_Ids, ID);
      end if;
   end Handle_EntityConfig_Msg;

   ----------------------------
   -- Handle_StateEntity_Msg --
   ----------------------------

   procedure Handle_StateEntity_Msg
     (This  : in out Automation_Request_Validator_Service;
      State : EntityState_Any)
   is
      ID : constant Common.Int64 := Common.Int64 (State.getID);
   begin
      --  m_availableConfigurationStateIds.insert(state->getID());
      if not Contains (This.Config.Available_State_Entity_Ids, ID) then
         This.Config.Available_State_Entity_Ids :=
           Add (This.Config.Available_State_Entity_Ids, ID);
      end if;
   end Handle_StateEntity_Msg;

   ---------------------------------
   -- Handle_InitializedTasks_Msg --
   ---------------------------------

   procedure Handle_InitializedTasks_Msg
     (This : in out Automation_Request_Validator_Service;
      Job  : lmcpTask_Any)
   is
      ID          : constant Common.Int64 := Common.Int64 (Job.getTaskID);

      Wrapped_Job : constant Task_Kind_And_Id :=
        (if Job.getLmcpTypeName = AFRL.Impact.AngledAreaSearchTask.Subscription then
           (Kind         => Angled_Area_Search_Task,
            SearchAreaID => Common.Int64 (AFRL.Impact.AngledAreaSearchTask.AngledAreaSearchTask (Job.all).getSearchAreaID))
         elsif Job.getLmcpTypeName = AFRL.Impact.ImpactLineSearchTask.Subscription then
           (Kind   => Impact_Line_Search_Task,
            LineID => Common.Int64 (AFRL.Impact.ImpactLineSearchTask.ImpactLineSearchTask (Job.all).getLineID))
         elsif Job.getLmcpTypeName = AFRL.Impact.ImpactPointSearchTask.Subscription then
           (Kind             => Impact_Point_Search_Task,
            SearchLocationID => Common.Int64 (AFRL.Impact.ImpactPointSearchTask.ImpactPointSearchTask (Job.all).getSearchLocationID))
         else (Kind => Other_Task));

   begin
      if Has_Key (This.Config.Available_Tasks, ID) then
         This.Config.Available_Tasks := Set (This.Config.Available_Tasks, ID, Wrapped_Job);
      else
         This.Config.Available_Tasks := Add (This.Config.Available_Tasks, ID, Wrapped_Job);
      end if;
   end Handle_InitializedTasks_Msg;

   ------------------------------
   -- Handle_ServiceStatus_Msg --
   ------------------------------

   procedure Handle_ServiceStatus_Msg
     (This : in out Automation_Request_Validator_Service;
      Msg  : Any_LMCP_Message)
   is
   begin
      --  TODO This procedure is useless while Error Response is not sent
      --  on timeout.
      null;
   end Handle_ServiceStatus_Msg;

   ----------------------------
   -- Handle_RemoveTasks_Msg --
   ----------------------------

   procedure Handle_RemoveTasks_Msg
     (This : in out Automation_Request_Validator_Service;
      Msg  : Any_LMCP_Message)
   is
      --  auto removeTasks = std::static_pointer_cast<afrl::cmasi::RemoveTasks>(receivedLmcpMessage->m_object);
      Remove_Msg : constant RemoveTasks_Any := RemoveTasks_Any (Msg.Payload);
   begin
      --  for (auto& taskId : removeTasks->getTaskList())
      for TaskId of Remove_Msg.getTaskList.all loop
         declare
            Id : constant Common.Int64 := Common.Int64 (TaskId);
         begin

            if Has_Key (This.Config.Available_Tasks, Id) then
               This.Config.Available_Tasks := Remove (This.Config.Available_Tasks, Id);
            end if;

            if Contains (This.Config.Available_Initialized_Tasks, Id) then
               This.Config.Available_Initialized_Tasks :=
                 Remove (This.Config.Available_Initialized_Tasks, Id);
            end if;
         end;
      end loop;
   end Handle_RemoveTasks_Msg;

   --------------------------------
   -- Handle_TaskInitialized_Msg --
   --------------------------------

   procedure Handle_TaskInitialized_Msg
     (This : in out Automation_Request_Validator_Service;
      Msg  : Any_LMCP_Message)
   is
      --  auto taskInitialized = std::static_pointer_cast<uxas::messages::task::TaskInitialized>(receivedLmcpMessage->m_object);
      TaskInit : constant TaskInitialized_Any := TaskInitialized_Any (Msg.Payload);
      Id       : constant Common.Int64 := Common.Int64 (TaskInit.getTaskID);
   begin
      --  m_availableInitializedTasks.insert(taskInitialized->getTaskID());
      if not Contains (This.Config.Available_Initialized_Tasks, Id) then
         This.Config.Available_Initialized_Tasks :=
           Add (This.Config.Available_Initialized_Tasks, Id);
      end if;

      --  checkTasksInitialized();
      Check_Tasks_Initialized (This.Config, This.State, This.Mailbox);
   end Handle_TaskInitialized_Msg;

   -------------------------------
   -- Handle_AreaOfInterest_Msg --
   -------------------------------

   procedure Handle_AreaOfInterest_Msg
     (This : in out Automation_Request_Validator_Service;
      Msg  : Any_LMCP_Message)
   is
      --  auto areaOfInterest = std::static_pointer_cast<afrl::impact::AreaOfInterest>(receivedLmcpMessage->m_object);
      Area : constant AreaOfInterest_Any := AreaOfInterest_Any (Msg.Payload);
      Id   : constant Common.Int64 := Common.Int64 (Area.getAreaID);
   begin
      --  m_availableAreaOfInterestIds.insert(areaOfInterest->getAreaID());
      if not Contains (This.Config.Available_Area_of_Interest_Ids, Id) then
         This.Config.Available_Area_of_Interest_Ids :=
           Add (This.Config.Available_Area_of_Interest_Ids, Id);
      end if;
   end Handle_AreaOfInterest_Msg;

   -------------------------------
   -- Handle_LineOfInterest_Msg --
   -------------------------------

   procedure Handle_LineOfInterest_Msg
     (This : in out Automation_Request_Validator_Service;
      Msg  : Any_LMCP_Message)
   is
      --  auto lineOfInterest = std::static_pointer_cast<afrl::impact::LineOfInterest>(receivedLmcpMessage->m_object);
      Line : constant LineOfInterest_Any := LineOfInterest_Any (Msg.Payload);
      Id   : constant Common.Int64 := Common.Int64 (Line.getLineID);
   begin
      --  m_availableLineOfInterestIds.insert(lineOfInterest->getLineID());
      if not Contains (This.Config.Available_Line_of_Interest_Ids, Id) then
         This.Config.Available_Line_of_Interest_Ids :=
           Add (This.Config.Available_Line_of_Interest_Ids, Id);
      end if;
   end Handle_LineOfInterest_Msg;

   --------------------------------
   -- Handle_PointofInterest_Msg --
   --------------------------------

   procedure Handle_PointofInterest_Msg
     (This : in out Automation_Request_Validator_Service;
      Msg  : Any_LMCP_Message)
   is
      --  auto pointOfInterest = std::static_pointer_cast<afrl::impact::PointOfInterest>(receivedLmcpMessage->m_object);
      Point : constant PointOfInterest_Any := PointOfInterest_Any (Msg.Payload);
      Id   : constant Common.Int64 := Common.Int64 (Point.getPointID);
   begin
      --  m_availablePointOfInterestIds.insert(pointOfInterest->getPointID());
      if not Contains (This.Config.Available_Point_of_Interest_Ids, Id) then
         This.Config.Available_Point_of_Interest_Ids :=
           Add (This.Config.Available_Point_of_Interest_Ids, Id);
      end if;
   end Handle_PointofInterest_Msg;

   ---------------------------
   -- Handle_KeepInZone_Msg --
   ---------------------------

   procedure Handle_KeepInZone_Msg
     (This : in out Automation_Request_Validator_Service;
      Msg  : Any_LMCP_Message)
   is
      --  auto keepInZone = std::static_pointer_cast<afrl::cmasi::KeepInZone>(receivedLmcpMessage->m_object);
      Zone : constant KeepInZone_Any := KeepInZone_Any (Msg.Payload);
      Id   : constant Common.Int64 := Common.Int64 (Zone.getZoneID);
   begin
      --  m_availableKeepInZonesIds.insert(keepInZone->getZoneID());
      if not Contains (This.Config.Available_KeepIn_Zones_Ids, Id) then
         This.Config.Available_KeepIn_Zones_Ids :=
           Add (This.Config.Available_KeepIn_Zones_Ids, Id);
      end if;
   end Handle_KeepInZone_Msg;

   ----------------------------
   -- Handle_KeepOutZone_Msg --
   ----------------------------

   procedure Handle_KeepOutZone_Msg
     (This : in out Automation_Request_Validator_Service;
      Msg  : Any_LMCP_Message)
   is
      --  auto keepOutZone = std::static_pointer_cast<afrl::cmasi::KeepOutZone>(receivedLmcpMessage->m_object);
      Zone : constant KeepOutZone_Any := KeepOutZone_Any (Msg.Payload);
      Id   : constant Common.Int64 := Common.Int64 (Zone.getZoneID);
   begin
      --  m_availableKeepOutZonesIds.insert(keepOutZone->getZoneID());
      if not Contains (This.Config.Available_KeepOut_Zones_Ids, Id) then
         This.Config.Available_KeepOut_Zones_Ids :=
           Add (This.Config.Available_KeepOut_Zones_Ids, Id);
      end if;
   end Handle_KeepOutZone_Msg;

   --------------------------------
   -- Handle_OperatingRegion_Msg --
   --------------------------------

   procedure Handle_OperatingRegion_Msg
     (This : in out Automation_Request_Validator_Service;
      Msg  : Any_LMCP_Message)
   is

      function Get_Areas
        (Region : OperatingRegion_Any) return OperatingRegionAreas;

      function Get_Areas
        (Region : OperatingRegion_Any) return OperatingRegionAreas
      is
         InAreas  : constant AFRL.CMASI.OperatingRegion.Vect_Int64_Acc := Region.all.getKeepInAreas;
         OutAreas : constant AFRL.CMASI.OperatingRegion.Vect_Int64_Acc := Region.all.getKeepOutAreas;
      begin
         return R : OperatingRegionAreas do
            for E of InAreas.all loop
               R.KeepInAreas := Add (R.KeepInAreas, Common.Int64 (E));
            end loop;
            for E of OutAreas.all loop
               R.KeepOutAreas := Add (R.KeepOutAreas, Common.Int64 (E));
            end loop;
         end return;
      end Get_Areas;

      Operating_Region : constant OperatingRegion_Any := OperatingRegion_Any (Msg.Payload);
      Wrapped_Region   : constant OperatingRegionAreas := Get_Areas (Operating_Region);
      Id               : constant Common.Int64 := Common.Int64 (Operating_Region.getID);
   begin
      if Has_Key (This.Config.Available_Operating_Regions, Id) then
         This.Config.Available_Operating_Regions :=
           Set (This.Config.Available_Operating_Regions, Id, Wrapped_Region);
      else
         This.Config.Available_Operating_Regions :=
           Add (This.Config.Available_Operating_Regions, Id, Wrapped_Region);
      end if;
   end Handle_OperatingRegion_Msg;

   procedure Handle_Automation_Request
     (This : in out Automation_Request_Validator_Service;
      Msg  : Any_LMCP_Message)
   is
      Payload : constant Object_Any := Msg.Payload;
   begin
      if Payload.all in AutomationRequest'Class then
         Automation_Request_Validator.Handle_Automation_Request
           (This.Config,
            This.State,
            This.Mailbox,
            As_AutomationRequest_Message (AutomationRequest_Any (Payload)));
      elsif Payload.all in TaskAutomationRequest'Class then
         Automation_Request_Validator.Handle_Task_Automation_Request
           (This.Config,
            This.State,
            This.Mailbox,
            As_TaskAutomationRequest_Message (TaskAutomationRequest_Any (Payload)));
      elsif Payload.all in ImpactAutomationRequest'Class then
         Automation_Request_Validator.Handle_Impact_Automation_Request
           (This.Config,
            This.State,
            This.Mailbox,
            As_ImpactAutomationRequest_Message (ImpactAutomationRequest_Any (Payload)));
      else
         raise Program_Error with "Msg is not an AnyAutomationRequest";
      end if;
   end Handle_Automation_Request;

   procedure Handle_Automation_Response
     (This : in out Automation_Request_Validator_Service;
      Msg  : Any_LMCP_Message)
   is
   begin
      Automation_Request_Validator.Handle_Automation_Response
        (This.State,
         This.Mailbox,
         As_UniqueAutomationResponse_Message
           (UniqueAutomationResponse_Any (Msg.Payload)));
   end Handle_Automation_Response;

   ----------------------
   -- UInt32_Attribute --
   ----------------------

   function UInt32_Attribute
     (XML_Node : DOM.Core.Element;
      Name     : String;
      Default  : AVTAS.LMCP.Types.UInt32)
      return AVTAS.LMCP.Types.UInt32
   is
      use DOM.Core;
      Attr_Value : constant DOM_String := Elements.Get_Attribute (XML_Node, Name);
   begin
      if Attr_Value /= "" and then (for all C of Attr_Value => C in '0' .. '9')
      then
         return AVTAS.LMCP.Types.UInt32'Value (Attr_Value);
      else
         return Default;
      end if;
   end UInt32_Attribute;

   -----------------------------
   -- Package Executable Part --
   -----------------------------

   --  This is the executable part for the package, invoked automatically and only once.
begin
   --  All concrete service subclasses must call this procedure in their
   --  own package like this, with their own params. The effect is the same as the
   --  following:
   --
   --    AutomationRequestValidatorService::ServiceBase::CreationRegistrar<AutomationRequestValidatorService>
   --    AutomationRequestValidatorService::s_registrar(AutomationRequestValidatorService::s_registryServiceTypeNames());
   --
   --  located at the top of the cpp file

   Register_Service_Creation_Function_Pointers (Registry_Service_Type_Names, Create'Access);
end UxAS.Comms.LMCP_Net_Client.Service.Automation_Request_Validation;
