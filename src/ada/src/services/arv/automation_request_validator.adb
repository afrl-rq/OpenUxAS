with Ada.Containers;             use Ada.Containers;
with AVTAS.LMCP.Types;
with UxAS.Comms.LMCP_Net_Client; use UxAS.Comms.LMCP_Net_Client;

package body Automation_Request_Validator with SPARK_Mode is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Check_Required_Entity_Configurations
     (Entity_Ids       : Int64_Seq;
      Configurations   : Int64_Set;
      States           : Int64_Set;
      Planning_States  : PlanningState_Seq;
      ReasonForFailure : in out Unbounded_String;
      IsReady          : in out Boolean;
      EntityList       : in out Int64_Seq)
   with Post =>
     IsReady = (IsReady'Old and
                  Check_For_Required_Entity_Configurations
                    (Entity_Ids,
                     Configurations,
                     States,
                     Planning_States));

   procedure Check_Required_Operating_Region_And_Keepin_Keepout_Zones
     (Operating_Region  : Int64;
      Operating_Regions : Operating_Region_Map;
      KeepIn_Zones_Ids  : Int64_Set;
      KeepOut_Zones_Ids : Int64_Set;
      ReasonForFailure  : in out Unbounded_String;
      IsReady           : in out Boolean)
   with Post =>
       IsReady = (IsReady'Old and
                    Check_For_Required_Operating_Region_And_Keepin_Keepout_Zones
                      (Operating_Region,
                       Operating_Regions,
                       KeepIn_Zones_Ids,
                       KeepOut_Zones_Ids));

   procedure Check_Required_Tasks_And_Task_Requirements
     (Available_Tasks                 : Task_Map;
      Available_Area_of_Interest_Ids  : Int64_Set;
      Available_Line_of_Interest_Ids  : Int64_Set;
      Available_Point_of_Interest_Ids : Int64_Set;
      TaskIds                         : Int64_Seq;
      ReasonForFailure                : in out Unbounded_String;
      IsReady                         : in out Boolean)
   with Post =>
       IsReady = (IsReady'Old and
                    Check_For_Required_Tasks_And_Task_Requirements
                      (Available_Tasks,
                       Available_Area_of_Interest_Ids,
                       Available_Line_of_Interest_Ids,
                       Available_Point_of_Interest_Ids,
                       TaskIds));

   procedure Get_Unique_Request_Id (Val : out Int64);

   procedure Send_Next_Request
     (Mailbox          : in out Automation_Request_Validator_Mailbox;
      Pending_Requests : UniqueAutomationRequest_Ref_Deque);

   procedure Send_Response
     (Mailbox  : in out Automation_Request_Validator_Mailbox;
      Sandbox  : Request_Details_Map;
      Response : UniqueAutomationResponse);

   -------------------------------------------
   -- Check_Automation_Request_Requirements --
   -------------------------------------------

   procedure Check_Automation_Request_Requirements
     (Config    : Automation_Request_Validator_Configuration_Data;
      Sandbox : in out Request_Details_Map;
      Mailbox : in out Automation_Request_Validator_Mailbox;
      Request : in out UniqueAutomationRequest;
      IsReady : out Boolean)
   is
      ReasonForFailure : Unbounded_String :=
        To_Unbounded_String
          ("Automation Request ID["
           & Int64'Image (Request.RequestID) & "] Not Ready ::");
      EntityList : Int64_Seq;
   begin
      IsReady := True;

      Check_Required_Entity_Configurations
        (Entity_Ids       => Request.EntityList,
         Configurations   => Config.Available_Configuration_Entity_Ids,
         States           => Config.Available_State_Entity_Ids,
         Planning_States  => Request.PlanningStates,
         ReasonForFailure => ReasonForFailure,
         IsReady          => IsReady,
         EntityList       => EntityList);

      Check_Required_Operating_Region_And_Keepin_Keepout_Zones
        (Operating_Region  => Request.OperatingRegion,
         Operating_Regions => Config.Available_Operating_Regions,
         KeepIn_Zones_Ids  => Config.Available_KeepIn_Zones_Ids,
         KeepOut_Zones_Ids => Config.Available_KeepOut_Zones_Ids,
         ReasonForFailure  => ReasonForFailure,
         IsReady           => IsReady);

      Check_Required_Tasks_And_Task_Requirements
        (Available_Tasks                 => Config.Available_Tasks,
         Available_Area_of_Interest_Ids  => Config.Available_Area_of_Interest_Ids,
         Available_Line_of_Interest_Ids  => Config.Available_Line_of_Interest_Ids,
         Available_Point_of_Interest_Ids => Config.Available_Point_of_Interest_Ids,
         TaskIds                         => Request.TaskList,
         ReasonForFailure                => ReasonForFailure,
         IsReady                         => IsReady);

      if Length (Request.EntityList) = 0 then
         Request.EntityList := EntityList;
      end if;

      if not IsReady then
         declare
            KVP : constant KeyValuePair :=
              (Key   => To_Unbounded_String ("RequestValidator"),
               Value => ReasonForFailure);
            errResponse : UniqueAutomationResponse;
         begin
            errResponse.ResponseID := Request.RequestID;
            errResponse.Info := Add (errResponse.Info, KVP);
            Send_Response (Mailbox, Sandbox, errResponse);
            Sandbox := Remove (Sandbox, Request.RequestID);
         end;
      end if;
   end Check_Automation_Request_Requirements;

   ------------------------------------------
   -- Check_Required_Entity_Configurations --
   ------------------------------------------

   procedure Check_Required_Entity_Configurations
     (Entity_Ids       : Int64_Seq;
      Configurations   : Int64_Set;
      States           : Int64_Set;
      Planning_States  : PlanningState_Seq;
      ReasonForFailure : in out Unbounded_String;
      IsReady          : in out Boolean;
      EntityList       : in out Int64_Seq)
   is
   begin
      if Length (Entity_Ids) /= 0 then
         if not Is_Empty (Configurations) then
            if Length (Entity_Ids) /= 0 then
               for I in 1 .. Last (Entity_Ids) loop
                  declare
                     Id : constant Int64 := Get (Entity_Ids, I);
                  begin
                     if not Contains (Configurations, Id) then
                        Append_To_Msg (Msg  => ReasonForFailure,
                                       Tail => String'("- EntityConfiguration for Entity Id["
                                         & Int64'Image (Id) & "] not available."));
                        IsReady := False;
                     end if;

                     pragma Loop_Invariant
                       (IsReady
                        = (IsReady'Loop_Entry
                          and (for all K in 1 .. I =>
                                 Contains (Configurations, Get (Entity_Ids, K)))));
                  end;
               end loop;
            end if;
         else
            Append_To_Msg (Msg  => ReasonForFailure,
                           Tail => "- No EntityConfigurations available.");
            IsReady := False;
         end if;

         -- check for required entity states, if none are required, make sure there is at least one with matching configuration

         if not Is_Empty (States) then
            for I in 1 .. Last (Entity_Ids) loop
               pragma Loop_Invariant
                 (IsReady = (IsReady'Loop_Entry
                  and then ((for all K in 1 .. I - 1 =>
                               Contains (States, Get (Entity_Ids, K))
                             or (for some Planning_State of Planning_States =>
                                   Planning_State.EntityID = Get (Entity_Ids, K))))));

               declare
                  Id           : constant Int64 := Get (Entity_Ids, I);
                  IsReadyLocal : Boolean := False;
               begin

                  if Contains (States, Id) then
                     IsReadyLocal := True;
                  end if;

                  if not IsReadyLocal then
                     for I in 1 .. Last (Planning_States) loop
                        declare
                           planningStateId : constant Int64 := Get (Planning_States, I).EntityID;
                        begin
                           if planningStateId = Id then
                              pragma Assert
                                (for some Planning_State of Planning_States => Planning_State.EntityID = Id);
                              IsReadyLocal := True;
                              exit;
                           end if;
                        end;
                        pragma Loop_Invariant
                          (for all K in 1 .. I => Get (Planning_States, K).EntityID /= Id);
                     end loop;
                  end if;

                  if not IsReadyLocal then
                     IsReady := False;
                     Append_To_Msg (Msg  => ReasonForFailure,
                                    Tail => "- EntityState for Entity Id["
                                    & Int64'Image (Id) & "] not available.");
                  end if;
               end;
            end loop;
         else
            Append_To_Msg (Msg  => ReasonForFailure,
                           Tail => "- No EntityStates available.");
            IsReady := False;
            pragma Assert
              (not Check_For_Required_Entity_Configurations
                 (Entity_Ids      => Entity_Ids,
                  Configurations  => Configurations,
                  States          => States,
                  Planning_States => Planning_States));
         end if;
      else --  if(!uniqueAutomationRequest->getOriginalRequest()->getEntityList().empty())
         pragma Assert (Length (Entity_Ids) = 0);

         if not Is_Empty (Configurations)
           and then not Is_Empty (States)
         then
            declare
               IsFoundAMatch : Boolean := False;
               Id : Int64;
               use Int64_Sets;
            begin
               for S in Iterate (Configurations) loop
                  pragma Loop_Invariant
                    (IsFoundAMatch
                     = (for some I of Configurations =>
                            (not Contains (S, I) and then Contains (States, I))));
                  Id := Int64_Sets.Choose (S);
                  if Contains (States, Id) then
                     pragma Assume (Length (EntityList) < Count_Type'Last, "we have less than Count_Type'Last vehicles");
                     EntityList := Add (EntityList, Id);
                     IsFoundAMatch := True;
                  end if;
               end loop;

               pragma Assert (IsFoundAMatch = (for some I of Configurations => Contains (States, I)));

               if not IsFoundAMatch then
                  Append_To_Msg (Msg  => ReasonForFailure,
                                 Tail => "- No EntityStates that match EntityConfigurations"
                                 & " are available.");
                  IsReady := False;
               end if;
            end;
         else
            if Is_Empty (Configurations) then
               Append_To_Msg (Msg  => ReasonForFailure,
                              Tail => "- No EntityConfigurations available.");
            else
               Append_To_Msg (Msg  => ReasonForFailure,
                              Tail => "- No EntityStates available.");
            end if;
            IsReady := False;
         end if;
      end if;
   end Check_Required_Entity_Configurations;

   --------------------------------------------------------------
   -- Check_Required_Operating_Region_And_Keepin_Keepout_Zones --
   --------------------------------------------------------------

   procedure Check_Required_Operating_Region_And_Keepin_Keepout_Zones
     (Operating_Region  : Int64;
      Operating_Regions : Operating_Region_Map;
      KeepIn_Zones_Ids  : Int64_Set;
      KeepOut_Zones_Ids : Int64_Set;
      ReasonForFailure  : in out Unbounded_String;
      IsReady           : in out Boolean)
   is
   begin
      if Operating_Region /= 0 then
         if Has_Key (Operating_Regions, Operating_Region) then
            declare
               ItOperatingRegion : constant OperatingRegionAreas :=
                 Get (Operating_Regions, Operating_Region);
               KeepInAreas       : constant Int64_Seq := ItOperatingRegion.KeepInAreas;
               KeepOutAreas      : constant Int64_Seq := ItOperatingRegion.KeepOutAreas;
            begin
               for I in 1 .. Last (KeepInAreas) loop
                  declare
                     KeepInArea : constant Int64 := Get (KeepInAreas, I);
                  begin
                     if not Contains (KeepIn_Zones_Ids, KeepInArea) then
                        Append_To_Msg (Msg  => ReasonForFailure,
                                       Tail => "- KeepInArea Id["
                                       & Int64'Image (KeepInArea) & "] not available.");
                        IsReady := False;
                     end if;

                     pragma Loop_Invariant
                       (IsReady =
                          (IsReady'Loop_Entry and then
                               (for all K in 1 .. I =>
                                      Contains (KeepIn_Zones_Ids, Get (KeepInAreas, K)))));
                  end;
               end loop;
               for I in 1 .. Last (KeepOutAreas) loop
                  declare
                     KeepOutArea : constant Int64 := Get (KeepOutAreas, I);
                  begin
                     if not Contains (KeepOut_Zones_Ids, KeepOutArea) then
                        Append_To_Msg (Msg  => ReasonForFailure,
                                Tail => "- KeepOutArea Id["
                                & Int64'Image (KeepOutArea) & "] not available.");
                        IsReady := False;
                     end if;
                     pragma Loop_Invariant
                       (IsReady =
                          (IsReady'Loop_Entry and then
                               (for all K in 1 .. I =>
                                      Contains (KeepOut_Zones_Ids, Get (KeepOutAreas, K)))));
                  end;
               end loop;
            end;
         else
            Append_To_Msg (Msg  => ReasonForFailure,
                           Tail => "- OperatingRegion Id["
                           & Int64'Image (Operating_Region)
                           & "] not available.");
            IsReady := False;
         end if;
      end if;
   end Check_Required_Operating_Region_And_Keepin_Keepout_Zones;

   ------------------------------------------------
   -- Check_Required_Tasks_And_Task_Requirements --
   ------------------------------------------------

   procedure Check_Required_Tasks_And_Task_Requirements
     (Available_Tasks                 : Task_Map;
      Available_Area_of_Interest_Ids  : Int64_Set;
      Available_Line_of_Interest_Ids  : Int64_Set;
      Available_Point_of_Interest_Ids : Int64_Set;
      TaskIds                         : Int64_Seq;
      ReasonForFailure                : in out Unbounded_String;
      IsReady                         : in out Boolean)
   is
   begin
      for I in 1 .. Last (TaskIds) loop
         pragma Loop_Invariant
           (IsReady =
              (IsReady'Loop_Entry and then
                   (for all K in 1 .. I - 1 =>
                          Has_Key (Available_Tasks, Get (TaskIds, K))
                    and then Check_For_Specific_Task_Requirements
                      (Available_Area_of_Interest_Ids  => Available_Area_of_Interest_Ids,
                       Available_Line_of_Interest_Ids  => Available_Line_of_Interest_Ids,
                       Available_Point_of_Interest_Ids => Available_Point_of_Interest_Ids,
                       ItTask                          =>
                         Get (Available_Tasks, Get (TaskIds, K))))));
         declare
            TaskId      : constant Int64 := Get (TaskIds, I);
            IsReadyPrev : constant Boolean := IsReady with Ghost;
            pragma Unreferenced (IsReadyPrev);
         begin
            if Has_Key (Available_Tasks, TaskId) then
               declare
                  ItTask : constant Task_Kind_And_Id :=
                    Get (Available_Tasks, TaskId);
               begin
                  -- check for specific task requirements
                  if ItTask.Kind = Angled_Area_Search_Task then
                     if ItTask.SearchAreaID /= 0 then
                        if not Contains (Available_Area_of_Interest_Ids,
                                         ItTask.SearchAreaID)
                        then
                           Append_To_Msg (Msg  => ReasonForFailure,
                                          Tail => "- AreaOfInterest Id["
                                          & Int64'Image (ItTask.SearchAreaID)
                                          & "] not available.");
                           IsReady := False;
                        end if;
                     end if;
                  elsif ItTask.Kind = Impact_Line_Search_Task then
                     if ItTask.LineID /= 0 then
                        if not Contains (Available_Line_of_Interest_Ids,
                                         ItTask.LineID)
                        then
                           Append_To_Msg (Msg  => ReasonForFailure,
                                          Tail => "- LineOfInterest Id["
                                          & Int64'Image (ItTask.LineID)
                                          & "] not available.");
                           IsReady := False;
                        end if;
                     end if;
                  elsif ItTask.Kind = Impact_Point_Search_Task then
                     if ItTask.SearchLocationID /= 0 then
                        if not Contains (Available_Point_of_Interest_Ids,
                                         ItTask.SearchLocationID)
                        then
                           Append_To_Msg (Msg  => ReasonForFailure,
                                          --  Point of interest ??
                                          Tail => "- LineOfInterest Id["
                                          & Int64'Image (ItTask.SearchLocationID)
                                          & "] not available.");
                           IsReady := False;
                        end if;
                     end if;
                  end if;
               end;
            else
               pragma Assert (not Has_Key (Available_Tasks, TaskId));
               Append_To_Msg (Msg  => ReasonForFailure,
                              Tail => "- Task with the Id[" & Int64'Image (TaskId)
                              & "] is unknown. Ensure task description preceeds automation request.");
               IsReady := False;
            end if;
         end;
      end loop;
   end Check_Required_Tasks_And_Task_Requirements;

   -----------------------------
   -- Check_Tasks_Initialized --
   -----------------------------

   procedure Check_Tasks_Initialized
     (Config  : Automation_Request_Validator_Configuration_Data;
      State   : in out Automation_Request_Validator_State;
      Mailbox : in out Automation_Request_Validator_Mailbox)
   is
      areAllTasksReady    : Boolean := True;
      isNewPendingRequest : Boolean := False;
   begin
      while areAllTasksReady and then Length (State.Requests_Waiting_For_Tasks) > 0 loop
         declare
            Req : constant UniqueAutomationRequest := First_Element (State.Requests_Waiting_For_Tasks);
         begin
            areAllTasksReady :=
              (for all TaskId of Req.TaskList => Contains (Config.Available_Initialized_Tasks, TaskId));
            if areAllTasksReady then
               isNewPendingRequest := True;

               pragma Assume (Length (State.Pending_Requests) < State.Pending_Requests.Capacity, "we have enough space for another request");
               Append (State.Pending_Requests, Req);
               Delete_First (State.Requests_Waiting_For_Tasks);
            end if;
         end;
      end loop;
      if isNewPendingRequest then
         Send_Next_Request (Mailbox, State.Pending_Requests);
      end if;
   end Check_Tasks_Initialized;

   ---------------------------
   -- Get_Unique_Request_Id --
   ---------------------------

   procedure Get_Unique_Request_Id (Val : out Int64) is
      Id : AVTAS.LMCP.Types.Int64;
   begin
      Get_Unique_Entity_Send_Message_Id (Id);
      Val := Int64 (Id);
   end Get_Unique_Request_Id;

   -------------------------------
   -- Handle_Automation_Request --
   -------------------------------

   procedure Handle_Automation_Request
     (Config  : Automation_Request_Validator_Configuration_Data;
      State   : in out Automation_Request_Validator_State;
      Mailbox : in out Automation_Request_Validator_Mailbox;
      Request : AutomationRequest)
   is
      Unique_Automation_Request : UniqueAutomationRequest;
      ReqId                     : Int64;
      Details                   : Request_Details;
      isReady                   : Boolean;
   begin

      Get_Unique_Request_Id (ReqId);
      pragma Assume (not Has_Key (State.Sandbox, ReqId), "returned Id is actually unique");

      Unique_Automation_Request.RequestID := ReqId;
      Unique_Automation_Request.EntityList := Request.EntityList;
      Unique_Automation_Request.OperatingRegion := Request.OperatingRegion;
      Unique_Automation_Request.TaskList := Request.TaskList;
      Unique_Automation_Request.TaskRelationships := Request.TaskRelationships;

      State.Sandbox := Add (State.Sandbox, ReqId, Details);

      Check_Automation_Request_Requirements
        (Config,
         State.Sandbox,
         Mailbox,
         Unique_Automation_Request,
         isReady);

      if isReady then
         pragma Assume (Length (State.Requests_Waiting_For_Tasks) < State.Requests_Waiting_For_Tasks.Capacity, "we have enough space for another request");
         Append (State.Requests_Waiting_For_Tasks, Unique_Automation_Request);
         Check_Tasks_Initialized (Config, State, Mailbox);
      end if;
   end Handle_Automation_Request;

   --------------------------------
   -- Handle_Automation_Response --
   --------------------------------

   procedure Handle_Automation_Response
      (State    : in out Automation_Request_Validator_State;
       Mailbox  : in out Automation_Request_Validator_Mailbox;
       Response : UniqueAutomationResponse)
   is
   begin
      if Length (State.Pending_Requests) = 0 then
         return;
      end if;

      declare
         First : constant UniqueAutomationRequest := First_Element (State.Pending_Requests);
      begin
         if First.RequestID = Response.ResponseID
           and then Has_Key (State.Sandbox, Response.ResponseID)
         then
            Send_Response (Mailbox, State.Sandbox, Response);
            State.Sandbox := Remove (State.Sandbox, Response.ResponseID);
            Delete_First (State.Pending_Requests);
            Send_Next_Request (Mailbox, State.Pending_Requests);
         end if;
      end;
   end Handle_Automation_Response;

   --------------------------------------
   -- Handle_Impact_Automation_Request --
   --------------------------------------

   procedure Handle_Impact_Automation_Request
     (Config  : Automation_Request_Validator_Configuration_Data;
      State   : in out Automation_Request_Validator_State;
      Mailbox : in out Automation_Request_Validator_Mailbox;
      Request : ImpactAutomationRequest)
   is
      Unique_Automation_Request : UniqueAutomationRequest;
      ReqId                     : Int64;
      Details                   : Request_Details (Sandbox_Automation_Request);
      isReady                   : Boolean;
   begin

      ReqId := Request.RequestID;

      Unique_Automation_Request.RequestID := ReqId;
      Unique_Automation_Request.EntityList := Request.EntityList;
      Unique_Automation_Request.OperatingRegion := Request.OperatingRegion;
      Unique_Automation_Request.TaskList := Request.TaskList;
      Unique_Automation_Request.TaskRelationships := Request.TaskRelationships;
      Unique_Automation_Request.SandboxRequest := True;

      Details.Play_Id := Request.PlayID;
      Details.Soln_Id := Request.SolutionID;
      State.Sandbox := Add (State.Sandbox, ReqId, Details);

      Check_Automation_Request_Requirements
        (Config,
         State.Sandbox,
         Mailbox,
         Unique_Automation_Request,
         isReady);

      if isReady then
         pragma Assume (Length (State.Requests_Waiting_For_Tasks) < State.Requests_Waiting_For_Tasks.Capacity, "we have enough space for another request");
         Append (State.Requests_Waiting_For_Tasks, Unique_Automation_Request);
         Check_Tasks_Initialized (Config, State, Mailbox);
      end if;
   end Handle_Impact_Automation_Request;

   ------------------------------------
   -- Handle_Task_Automation_Request --
   ------------------------------------

   procedure Handle_Task_Automation_Request
     (Config  : Automation_Request_Validator_Configuration_Data;
      State   : in out Automation_Request_Validator_State;
      Mailbox : in out Automation_Request_Validator_Mailbox;
      Request : TaskAutomationRequest)
   is
      Unique_Automation_Request : UniqueAutomationRequest;
      ReqId                     : Int64;
      Details                   : Request_Details (Task_Automation_Request);
      isReady                   : Boolean;
   begin

      ReqId := Request.RequestID;

      Unique_Automation_Request.RequestID := ReqId;
      Unique_Automation_Request.EntityList := Request.EntityList;
      Unique_Automation_Request.OperatingRegion := Request.OperatingRegion;
      Unique_Automation_Request.TaskList := Request.TaskList;
      Unique_Automation_Request.TaskRelationships := Request.TaskRelationships;
      Unique_Automation_Request.SandboxRequest := Request.SandboxRequest;
      Unique_Automation_Request.PlanningStates := Request.PlanningStates;

      Details.Task_Request_Id := ReqId;
      State.Sandbox := Add (State.Sandbox, ReqId, Details);

      Check_Automation_Request_Requirements
        (Config,
         State.Sandbox,
         Mailbox,
         Unique_Automation_Request,
         isReady);

      if isReady then
         pragma Assume (Length (State.Requests_Waiting_For_Tasks) < State.Requests_Waiting_For_Tasks.Capacity, "we have enough space for another request");
         Append (State.Requests_Waiting_For_Tasks, Unique_Automation_Request);
         Check_Tasks_Initialized (Config, State, Mailbox);
      end if;
   end Handle_Task_Automation_Request;

   -----------------------
   -- Send_Next_Request --
   -----------------------

   procedure Send_Next_Request
     (Mailbox          : in out Automation_Request_Validator_Mailbox;
      Pending_Requests : UniqueAutomationRequest_Ref_Deque)
   is
   begin
      if Length (Pending_Requests) = 0 then
         return;
      end if;

      declare
         Req            : constant UniqueAutomationRequest := First_Element (Pending_Requests);
         Service_Status : ServiceStatus;
         KVP            : KeyValuePair;
      begin
         sendBroadcastMessage (Mailbox, Req);
         KVP.Key :=
           To_Unbounded_String
             ("UniqueAutomationRequest[" & Req.RequestID'Image & "] - sent");
         Service_Status.Info := Add (Service_Status.Info, KVP);
         sendBroadcastMessage (Mailbox, Service_Status);
      end;
   end Send_Next_Request;

   -------------------
   -- Send_Response --
   -------------------

   procedure Send_Response
     (Mailbox  : in out Automation_Request_Validator_Mailbox;
      Sandbox  : Request_Details_Map;
      Response : UniqueAutomationResponse)
   is
   begin
      if not Has_Key (Sandbox, Response.ResponseID)
        or else Get (Sandbox, Response.ResponseID).Request_Type = Automation_Request
      then
         declare
            Result : constant AutomationResponse :=
              (Response.MissionCommandList,
               Response.VehicleCommandList,
               Response.Info);
         begin
            sendBroadcastMessage (Mailbox, Result);
         end;
      elsif
        Get (Sandbox, Response.ResponseID).Request_Type = Task_Automation_Request
      then
         declare
            Result : constant TaskAutomationResponse :=
              (Response.MissionCommandList,
               Response.VehicleCommandList,
               Response.Info,
               Get (Sandbox, Response.ResponseID).Task_Request_Id,
               Response.FinalStates);
         begin
            sendBroadcastMessage (Mailbox, Result);
         end;
      elsif
        Get (Sandbox, Response.ResponseID).Request_Type = Sandbox_Automation_Request
      then
         declare
            Details : constant Request_Details := Get (Sandbox, Response.ResponseID);
            Result  : constant ImpactAutomationResponse :=
              (Response.MissionCommandList,
               Response.VehicleCommandList,
               Response.Info,
               Response.ResponseID,
               Details.Play_Id,
               Details.Soln_Id,
               True);
         begin
            sendBroadcastMessage (Mailbox, Result);
         end;
      end if;
   end Send_Response;
end Automation_Request_Validator;
