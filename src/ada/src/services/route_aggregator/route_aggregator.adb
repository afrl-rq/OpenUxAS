with Ada.Text_IO; use Ada.Text_IO;
with SPARK.Big_Integers; use SPARK.Big_Integers;
with SPARK.Big_Intervals; use SPARK.Big_Intervals;

package body Route_Aggregator with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);

   pragma Assertion_Policy (Ignore);

   -----------------------------
   -- Lift_From_Keys_To_Model --
   -----------------------------

   procedure Lift_From_Keys_To_Model (PendingRoute : Int64_Formal_Set_Map)
   --  Lemma: Lift quantification done on Keys of the pending route map to its
   --  model.

   with Ghost,
     Global => null,
     Post =>
       (for all Key of Model (PendingRoute) =>
          (Find (Keys (PendingRoute), Key) > 0
           and then Int_Set_Maps_K.Get (Keys (PendingRoute), Find (Keys (PendingRoute), Key)) =
             Key));

   procedure Lift_From_Keys_To_Model (PendingRoute : Int64_Formal_Set_Map)
   is
   begin
      null;
   end Lift_From_Keys_To_Model;

   --  Subprograms wraping insertion and deletion in m_pendingRoute. They
   --  restate part of the postcondition of the callee, but also reestablish
   --  the predicate of Route_Aggregator_State and compute the effect of the
   --  modification on Plan_To_Route.

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Check_All_Route_Plans_PendingAutoReq
     (Mailbox : in out Route_Aggregator_Mailbox;
      State   : in out Route_Aggregator_State)
     with
     --  General invariants

     Pre  => All_Plans_Registered (State.m_routePlanResponses, State.m_routePlans)
     and Only_Pending_Plans (State.m_routePlanResponses, State.m_routePlans)
     and Valid_Plan_Responses (State.m_pendingRoute, State.m_pendingAutoReq, State.m_routePlanResponses)
     and (for all K in 1 .. Length (State.m_pendingRoute) =>
            Is_Pending (Model (State.m_pendingRoute), Model (State.m_routePlanResponses), Int_Set_Maps_K.Get (Keys (State.m_pendingRoute), K)))

     --  History invariants

     and Valid_Events (State.m_routeRequestId)
     and No_RouteRequest_Lost (State.m_pendingRoute)
     and No_PlanResponse_Lost (State.m_pendingRoute, State.m_routePlanResponses)
     and All_Pending_Plans_Sent (State.m_pendingRoute, State.m_routePlanResponses),

     --  General invariants

     Post => All_Plans_Registered (State.m_routePlanResponses, State.m_routePlans)
     and Only_Pending_Plans (State.m_routePlanResponses, State.m_routePlans)
     and Valid_Plan_Responses (State.m_pendingRoute, State.m_pendingAutoReq, State.m_routePlanResponses)
     and No_Finished_Request (State.m_pendingRoute, State.m_pendingAutoReq, State.m_routePlanResponses)

     --  History invariants

     and History'Old <= History
     and Valid_Events (State.m_routeRequestId)
     and No_RouteRequest_Lost (State.m_pendingRoute)
     and No_PlanResponse_Lost (State.m_pendingRoute, State.m_routePlanResponses)
     and All_Pending_Plans_Sent (State.m_pendingRoute, State.m_routePlanResponses);

   procedure Check_All_Route_Plans_PendingRoute
     (Mailbox : in out Route_Aggregator_Mailbox;
      State   : in out Route_Aggregator_State)
     with
     --  General invariants

     Pre  => All_Plans_Registered (State.m_routePlanResponses, State.m_routePlans)
     and Only_Pending_Plans (State.m_routePlanResponses, State.m_routePlans)
     and Valid_Plan_Responses (State.m_pendingRoute, State.m_pendingAutoReq, State.m_routePlanResponses)

     --  History invariants

     and Valid_Events (State.m_routeRequestId)
     and No_RouteRequest_Lost (State.m_pendingRoute)
     and No_PlanResponse_Lost (State.m_pendingRoute, State.m_routePlanResponses)
     and All_Pending_Plans_Sent (State.m_pendingRoute, State.m_routePlanResponses),

     --  General invariants

     Post => All_Plans_Registered (State.m_routePlanResponses, State.m_routePlans)
     and Only_Pending_Plans (State.m_routePlanResponses, State.m_routePlans)
     and Valid_Plan_Responses (State.m_pendingRoute, State.m_pendingAutoReq, State.m_routePlanResponses)
     and (for all K in 1 .. Length (State.m_pendingRoute) =>
            Is_Pending (Model (State.m_pendingRoute), Model (State.m_routePlanResponses), Int_Set_Maps_K.Get (Keys (State.m_pendingRoute), K)))

     --  History invariants

     and History'Old <= History
     and Valid_Events (State.m_routeRequestId)
     and No_RouteRequest_Lost (State.m_pendingRoute)
     and No_PlanResponse_Lost (State.m_pendingRoute, State.m_routePlanResponses)
     and All_Pending_Plans_Sent (State.m_pendingRoute, State.m_routePlanResponses);

   procedure Delete_PendingRequest
     (m_pendingRequest : in out Int64_Formal_Set_Map;
      otherPending     : Int64_Formal_Set_Map;
      m_routeRequestId : Int64;
      Position         : in out Int64_Formal_Set_Maps.Cursor)
   with
     Pre => Has_Element (m_pendingRequest, Position)
     and All_Pending_Requests_Seen (Model (m_pendingRequest), m_routeRequestId)
     and No_Overlaps (Model (m_pendingRequest))
     and No_Overlaps (Model (m_pendingRequest), Model (otherPending)),

     Post =>

     --  Predicate of State

     All_Pending_Requests_Seen (Model (m_pendingRequest), m_routeRequestId)
     and No_Overlaps (Model (m_pendingRequest))
     and No_Overlaps (Model (m_pendingRequest), Model (otherPending))

     --  Postcondition copied from Int64_Formal_Set_Maps.Delete

     and Length (m_pendingRequest) = Length (m_pendingRequest)'Old - 1
     and not Int_Set_Maps_M.Has_Key (Model (m_pendingRequest), Key (m_pendingRequest, Position)'Old)
     and not Int_Set_Maps_P.Has_Key (Positions (m_pendingRequest), Position'Old)
     and Model (m_pendingRequest) <= Model (m_pendingRequest)'Old
     and Int_Set_Maps_M.Keys_Included_Except
       (Model (m_pendingRequest)'Old,
        Model (m_pendingRequest),
        Key (m_pendingRequest, Position)'Old)
     and Int_Set_Maps_K.Range_Equal
       (Left  => Keys (m_pendingRequest)'Old,
        Right => Keys (m_pendingRequest),
        Fst   => 1,
        Lst   => Int_Set_Maps_P.Get (Positions (m_pendingRequest)'Old, Position'Old) - 1)
     and Int_Set_Maps_K.Range_Shifted
       (Left   => Keys (m_pendingRequest),
        Right  => Keys (m_pendingRequest)'Old,
        Fst    => Int_Set_Maps_P.Get (Positions (m_pendingRequest)'Old, Position'Old),
        Lst    => Length (m_pendingRequest),
        Offset => 1)
     and P_Positions_Shifted
       (Positions (m_pendingRequest),
        Positions (m_pendingRequest)'Old,
        Cut   => Int_Set_Maps_P.Get (Positions (m_pendingRequest)'Old, Position'Old))

     --  Effect on Plan_To_Route

     and Plan_To_Route (m_pendingRequest) <= Plan_To_Route (m_pendingRequest)'Old
     and (for all I of Plan_To_Route (m_pendingRequest)'Old =>
              Has_Key (Plan_To_Route (m_pendingRequest), I)
          or else Get (Plan_To_Route (m_pendingRequest)'Old, I) = Key (m_pendingRequest, Position)'Old);

   procedure Insert_PendingRequest
     (m_pendingRequest : in out Int64_Formal_Set_Map;
      otherPending     : Int64_Formal_Set_Map;
      m_routeRequestId : Int64;
      RequestID        : Int64;
      PlanRequests     : Int64_Formal_Set)
     with Pre => not Int_Set_Maps_M.Has_Key (Model (m_pendingRequest), RequestID)
       and Length (m_pendingRequest) < Count_Type'Last
       and
         All_Pending_Requests_Seen (Model (m_pendingRequest), m_routeRequestId)
       and
         No_Overlaps (Model (m_pendingRequest))
       and
         No_Overlaps (Model (m_pendingRequest), Model (otherPending))
       and
         (for all Id of PlanRequests => Id <= m_routeRequestId)
       and
       (for all R_Id of Model (m_pendingRequest) =>
          (for all E of Int_Set_Maps_M.Get (Model (m_pendingRequest), R_Id) => not Contains (PlanRequests, E)))
       and
       (for all R_Id of Model (otherPending) =>
          (for all E of Int_Set_Maps_M.Get (Model (otherPending), R_Id) => not Contains (PlanRequests, E))),
     Post =>
     --  Predicate of State
       All_Pending_Requests_Seen (Model (m_pendingRequest), m_routeRequestId)
     and No_Overlaps (Model (m_pendingRequest))
     and No_Overlaps (Model (m_pendingRequest), Model (otherPending))

     --  Part of the postcondition copied from Int64_Formal_Set_Maps.Insert
     and Model (m_pendingRequest)'Old <= Model (m_pendingRequest)
     and Contains (Model (m_pendingRequest), RequestID)
     and (for all E of Int_Set_Maps_M.Get (Model (m_pendingRequest), RequestID) =>
            Contains (PlanRequests, E))
     and (for all E of PlanRequests => Contains (Int_Set_Maps_M.Get (Model (m_pendingRequest), RequestID), E))
     and (for all K of Model (m_pendingRequest) =>
              Int_Set_Maps_M.Has_Key (Model (m_pendingRequest)'Old, K)
          or K = RequestID)

     --  Effect on Plan_To_Route

     and Plan_To_Route (m_pendingRequest)'Old <= Plan_To_Route (m_pendingRequest)
     and (for all I of PlanRequests =>
            Has_Key (Plan_To_Route (m_pendingRequest), I)
          and then Get (Plan_To_Route (m_pendingRequest), I) = RequestID)
     and (for all I of Plan_To_Route (m_pendingRequest) =>
              Contains (PlanRequests, I) or Has_Key (Plan_To_Route (m_pendingRequest)'Old, I));

   ---------------------------
   -- Build_Matrix_Requests --
   ---------------------------

   procedure Build_Matrix_Requests
     (Mailbox : in out Route_Aggregator_Mailbox;
      Data  : Route_Aggregator_Configuration_Data;
      State : in out Route_Aggregator_State;
      ReqId : Int64)
     with
       SPARK_Mode => Off
   is
      sendAirPlanRequest    : RPReq_Seq;
      sendGroundPlanRequest : RPReq_Seq;
      Empty_Formal_Set      : Int64_Formal_Set;
   begin
      Insert (State.m_pendingAutoReq, ReqId, Empty_Formal_Set);

      if Length (Element (State.m_uniqueAutomationRequests, ReqId).EntityList) = 0 then
         declare
            AReq : UniqueAutomationRequest :=
              Element (State.m_uniqueAutomationRequests, ReqId);
         begin
            AReq.EntityList := Data.m_entityStates;
            Replace (State.m_uniqueAutomationRequests, ReqId, AReq);
         end;
      end if;

      declare
         AReq : constant UniqueAutomationRequest :=
           Element (State.m_uniqueAutomationRequests, ReqId);
      begin
         For_Each_Vehicle : for VehicleId of AReq.EntityList loop
            Make_Request : declare
               StartHeading_Deg   : Real32 := 0.0;
               StartLocation      : Location3D;
               FoundPlanningState : Boolean := False;
               Vehicle            : EntityState;
            begin
               for PlanningState of AReq.PlanningStates loop

                  if PlanningState.EntityID = VehicleId then
                     StartLocation := PlanningState.PlanningPosition;
                     StartHeading_Deg := PlanningState.PlanningHeading;
                     FoundPlanningState := True;
                     exit;
                  end if;

               end loop;

               if FoundPlanningState
                 or else (for some EntityId of Data.m_entityStates =>
                            (EntityId = VehicleId))
               then

                  Build_Eligible_Task_Options : declare
                     TaskOptionList : TaskOption_Seq;
                     Found_Elig     : Boolean := False;
                     PlanRequest    : RoutePlanRequest;
                     Set            : Int64_Formal_Set := Element (State.m_pendingAutoReq, ReqId);
                  begin
                     for TaskId of AReq.TaskList loop

                        if Contains (State.m_taskOptions, TaskId) then
                           for Option of Element (State.m_taskOptions, TaskId).Options loop

                              Found_Elig := False;
                              for V of Option.EligibleEntities loop
                                 if V = VehicleId then
                                    Found_Elig := True;
                                    exit;
                                 end if;
                              end loop;

                              if Length (Option.EligibleEntities) = 0
                                or else Found_Elig
                              then
                                 TaskOptionList := Add (TaskOptionList, Option);
                              end if;

                           end loop;
                        end if;
                     end loop;

                     PlanRequest.AssociatedTaskID := 0;
                     PlanRequest.IsCostOnlyRequest := False;
                     PlanRequest.OperatingRegion := AReq.OperatingRegion;
                     PlanRequest.VehicleID := VehicleId;
                     State.m_routeRequestId := State.m_routeRequestId + 1;
                     PlanRequest.RequestID := State.m_routeRequestId;
                     Insert (Set, State.m_routeRequestId);
                     Replace (State.m_pendingAutoReq,
                                    ReqId,
                                    Set);

                     if not FoundPlanningState then
                        Vehicle := ES_Maps.Get (Data.m_entityStatesInfo, VehicleId);
                        StartLocation := Vehicle.Location;
                        StartHeading_Deg := Vehicle.Heading;
                     end if;

                     for Option of TaskOptionList loop
                        declare
                           TOP : constant TaskOptionPair :=
                           (VehicleId, 0, 0, Option.TaskID, Option.OptionID);
                           R   : RouteConstraints;

                        begin
                           Insert (State.m_routeTaskPairing, State.m_routeId + 1, TOP);
                           R.StartLocation := StartLocation;
                           R.StartHeading := StartHeading_Deg;
                           R.EndLocation := Option.StartLocation;
                           R.EndHeading := Option.StartHeading;
                           R.RouteID := State.m_routeId + 1;
                           PlanRequest.RouteRequests :=
                             Add (PlanRequest.RouteRequests, R);
                           State.m_routeId := State.m_routeId + 1;
                        end;
                     end loop;

                     for T1 in TaskOptionList loop
                        for T2 in TaskOptionList loop

                           if T1 /= T2 then
                              declare
                                 O1  : constant TaskOption :=
                                   Get (TaskOptionList, T1);
                                 O2  : constant TaskOption :=
                                   Get (TaskOptionList, T2);
                                 TOP : constant TaskOptionPair :=
                                   (VehicleId,
                                    O1.TaskID,
                                    O1.OptionID,
                                    O2.TaskID,
                                    O2.OptionID);
                                 R   : RouteConstraints;
                              begin
                                 Insert (State.m_routeTaskPairing, State.m_routeId + 1, TOP);
                                 R.StartLocation := O1.EndLocation;
                                 R.StartHeading := O1.EndHeading;
                                 R.EndLocation := O2.StartLocation;
                                 R.EndHeading := O2.StartHeading;
                                 R.RouteID := State.m_routeId + 1;
                                 PlanRequest.RouteRequests :=
                                   Add (PlanRequest.RouteRequests, R);
                                 State.m_routeId := State.m_routeId + 1;
                              end;
                           end if;
                        end loop;
                     end loop;

                     if Contains (Data.m_groundVehicles, VehicleId) then
                        sendGroundPlanRequest :=
                          Add (sendGroundPlanRequest, PlanRequest);
                     else
                        sendAirPlanRequest :=
                          Add (sendAirPlanRequest, PlanRequest);
                     end if;
                  end Build_Eligible_Task_Options;
               end if;
            end Make_Request;
         end loop For_Each_Vehicle;

         for RPReq of sendAirPlanRequest loop
            sendLimitedCastMessage (Mailbox,
                                    AircraftPathPlanner,
                                    RPReq);
         end loop;

         for RPReq of sendGroundPlanRequest loop
            if Data.m_fastPlan then
               Euclidean_Plan (Data,
                               State.m_routePlanResponses,
                               State.m_routePlans,
                               RPReq);
            else
               sendLimitedCastMessage (Mailbox,
                                       GroundPathPlanner,
                                       RPReq);
            end if;
         end loop;

         if Data.m_fastPlan then
            Check_All_Route_Plans (Mailbox, State);
         end if;
      end;
   end Build_Matrix_Requests;

   ---------------------------
   -- Check_All_Route_Plans --
   ---------------------------

   procedure Check_All_Route_Plans
     (Mailbox : in out Route_Aggregator_Mailbox;
      State   : in out Route_Aggregator_State)
   is
   begin
      Check_All_Route_Plans_PendingRoute (Mailbox, State);
      Check_All_Route_Plans_PendingAutoReq (Mailbox, State);
   end Check_All_Route_Plans;

   ------------------------------------------
   -- Check_All_Route_Plans_PendingAutoReq --
   ------------------------------------------

   procedure Check_All_Route_Plans_PendingAutoReq
     (Mailbox : in out Route_Aggregator_Mailbox;
      State   : in out Route_Aggregator_State)
   is
      i : Int64_Formal_Set_Maps.Cursor := First (State.m_pendingAutoReq);
      D : Count_Type := 0 with Ghost;
      --  Number of removed elements
   begin
      --  Check pending automation requests

      while Has_Element (State.m_pendingAutoReq, i) loop
         pragma Loop_Invariant (Has_Element (State.m_pendingAutoReq, i));

         pragma Loop_Invariant
           (D = Length (State.m_pendingAutoReq)'Loop_Entry - Length (State.m_pendingAutoReq));
         pragma Loop_Invariant
           (Model (State.m_pendingAutoReq) <= Int_Set_Maps_M.Map'(Model (State.m_pendingAutoReq))'Loop_Entry);
         pragma Loop_Invariant
           (for all K in 1 .. Int_Set_Maps_P.Get (Positions (State.m_pendingAutoReq), i) - 1 =>
              (for some L in 1 .. Int_Set_Maps_P.Get (Positions (State.m_pendingAutoReq), i) - 1 + D =>
                    Int_Set_Maps_K.Get (Keys (State.m_pendingAutoReq), K) = Int_Set_Maps_K.Get (Keys (State.m_pendingAutoReq)'Loop_Entry, L)));
         pragma Loop_Invariant
           (for all K in 1 .. Int_Set_Maps_P.Get (Positions (State.m_pendingAutoReq), i) - 1 =>
              Is_Pending (Model (State.m_pendingAutoReq), Model (State.m_routePlanResponses), Int_Set_Maps_K.Get (Keys (State.m_pendingAutoReq), K)));
         pragma Loop_Invariant
           (for all K in Int_Set_Maps_P.Get (Positions (State.m_pendingAutoReq), i) .. Length (State.m_pendingAutoReq) =>
              Int_Set_Maps_K.Get (Keys (State.m_pendingAutoReq), K) = Int_Set_Maps_K.Get (Keys (State.m_pendingAutoReq)'Loop_Entry, K + D));

         --  General invariants

         pragma Loop_Invariant
           (All_Plans_Registered (State.m_routePlanResponses, State.m_routePlans));
         pragma Loop_Invariant
           (Only_Pending_Plans (State.m_routePlanResponses, State.m_routePlans));
         pragma Loop_Invariant
           (Valid_Plan_Responses (State.m_pendingRoute, State.m_pendingAutoReq, State.m_routePlanResponses));
         pragma Loop_Invariant
           (for all K in 1 .. Length (State.m_pendingRoute) =>
             Is_Pending (Model (State.m_pendingRoute), Model (State.m_routePlanResponses), Int_Set_Maps_K.Get (Keys (State.m_pendingRoute), K)));

         --  History invariants

         pragma Loop_Invariant (History'Loop_Entry <= History);
         pragma Loop_Invariant (Valid_Events (State.m_routeRequestId));
         pragma Loop_Invariant (No_RouteRequest_Lost (State.m_pendingRoute));
         pragma Loop_Invariant (No_PlanResponse_Lost (State.m_pendingRoute, State.m_routePlanResponses));
         pragma Loop_Invariant (All_Pending_Plans_Sent (State.m_pendingRoute, State.m_routePlanResponses));

         declare
            isFulfilled : Boolean := True;
            Formal_Set  : Int64_Formal_Set renames Element (State.m_pendingAutoReq, i);
            C : Int64_Formal_Sets.Cursor := First (Formal_Set);
         begin
            while Has_Element (Formal_Set, C) loop
               pragma Loop_Invariant
                 ((for all K in 1 .. Int_Set_P.Get (Positions (Formal_Set), C) - 1 =>
                      Contains (State.m_routePlanResponses, Int_Set_E.Get (Elements (Formal_Set), K)))
                  = isFulfilled);
               if not Contains (State.m_routePlanResponses, Element (Formal_Set, C)) then
                  isFulfilled := False;
               end if;
               Next (Formal_Set, C);
            end loop;

            pragma Assert
              (isFulfilled
               = (for all J of Element (State.m_pendingAutoReq, i) =>
                   Contains (State.m_routePlanResponses, J)));
            if isFulfilled then
               SendMatrix (Mailbox,
                           State.m_uniqueAutomationRequests,
                           State.m_pendingRoute,
                           State.m_pendingAutoReq,
                           State.m_routePlans,
                           State.m_routeTaskPairing,
                           State.m_routePlanResponses,
                           State.m_taskOptions,
                           Key (State.m_pendingAutoReq, i));

               declare
                  Dummy   : Int64_Formal_Set_Maps.Cursor := i;
                  UAR_Key : constant Int64 := Key (State.m_pendingAutoReq, i);
                  Pos     : constant Count_Type := Int_Set_Maps_P.Get (Positions (State.m_pendingAutoReq), i) with Ghost;
               begin
                  Next (State.m_pendingAutoReq, i);
                  Delete_PendingRequest (State.m_pendingAutoReq, State.m_pendingRoute, State.m_routeRequestId, Dummy);
                  Delete (State.m_uniqueAutomationRequests, UAR_Key);
                  D := D + 1;
                  pragma Assert
                    (for all K in 1 .. Pos - 1 =>
                       Is_Pending (Model (State.m_pendingAutoReq), Model (State.m_routePlanResponses), Int_Set_Maps_K.Get (Keys (State.m_pendingAutoReq), K)));
               end;
            else
               Next (State.m_pendingAutoReq, i);
            end if;
         end;
      end loop;

      --  Restablish No_Finished_Request

      pragma Assert
        (for all K in 1 .. Length (State.m_pendingRoute) =>
             Is_Pending (Model (State.m_pendingRoute), Model (State.m_routePlanResponses), Int_Set_Maps_K.Get (Keys (State.m_pendingRoute), K)));
      Lift_From_Keys_To_Model (State.m_pendingRoute);
      pragma Assert
        (for all K in 1 .. Length (State.m_pendingAutoReq) =>
             Is_Pending (Model (State.m_pendingAutoReq), Model (State.m_routePlanResponses), Int_Set_Maps_K.Get (Keys (State.m_pendingAutoReq), K)));
      Lift_From_Keys_To_Model (State.m_pendingAutoReq);
   end Check_All_Route_Plans_PendingAutoReq;

   ----------------------------------------
   -- Check_All_Route_Plans_PendingRoute --
   ----------------------------------------

   procedure Check_All_Route_Plans_PendingRoute
     (Mailbox : in out Route_Aggregator_Mailbox;
      State   : in out Route_Aggregator_State)
   is
      i : Int64_Formal_Set_Maps.Cursor := First (State.m_pendingRoute);
      D : Count_Type := 0 with Ghost;
      --  Number of removed elements
   begin
      -- check pending route requests
      while Has_Element (State.m_pendingRoute, i) loop
         pragma Loop_Invariant (Has_Element (State.m_pendingRoute, i));

         pragma Loop_Invariant
           (D = Length (State.m_pendingRoute)'Loop_Entry - Length (State.m_pendingRoute));
         pragma Loop_Invariant
           (Model (State.m_pendingRoute) <= Int_Set_Maps_M.Map'(Model (State.m_pendingRoute))'Loop_Entry);
         pragma Loop_Invariant
           (for all K in 1 .. Int_Set_Maps_P.Get (Positions (State.m_pendingRoute), i) - 1 =>
              (for some L in 1 .. Int_Set_Maps_P.Get (Positions (State.m_pendingRoute), i) - 1 + D =>
                    Int_Set_Maps_K.Get (Keys (State.m_pendingRoute), K) = Int_Set_Maps_K.Get (Keys (State.m_pendingRoute)'Loop_Entry, L)));
         pragma Loop_Invariant
           (for all K in 1 .. Int_Set_Maps_P.Get (Positions (State.m_pendingRoute), i) - 1 =>
              Is_Pending (Model (State.m_pendingRoute), Model (State.m_routePlanResponses), Int_Set_Maps_K.Get (Keys (State.m_pendingRoute), K)));
         pragma Loop_Invariant
           (for all K in Int_Set_Maps_P.Get (Positions (State.m_pendingRoute), i) .. Length (State.m_pendingRoute) =>
                Int_Set_Maps_K.Get (Keys (State.m_pendingRoute), K) = Int_Set_Maps_K.Get (Keys (State.m_pendingRoute)'Loop_Entry, K + D));

         --  General invariants

         pragma Loop_Invariant
           (All_Plans_Registered (State.m_routePlanResponses, State.m_routePlans));
         pragma Loop_Invariant
           (Only_Pending_Plans (State.m_routePlanResponses, State.m_routePlans));
         pragma Loop_Invariant
           (Valid_Plan_Responses (State.m_pendingRoute, State.m_pendingAutoReq, State.m_routePlanResponses));

         --  History invariants

         pragma Loop_Invariant (History'Loop_Entry <= History);
         pragma Loop_Invariant (Valid_Events (State.m_routeRequestId));
         pragma Loop_Invariant (No_RouteRequest_Lost (State.m_pendingRoute));
         pragma Loop_Invariant (No_PlanResponse_Lost (State.m_pendingRoute, State.m_routePlanResponses));
         pragma Loop_Invariant (All_Pending_Plans_Sent (State.m_pendingRoute, State.m_routePlanResponses));

         declare
            isFulfilled : Boolean := True;
            Formal_Set  : Int64_Formal_Set renames Element (State.m_pendingRoute, i);
            C : Int64_Formal_Sets.Cursor := First (Formal_Set);
         begin
            while Has_Element (Formal_Set, C) loop
               pragma Loop_Invariant
                 ((for all K in 1 .. Int_Set_P.Get (Positions (Formal_Set), C) - 1 =>
                      Contains (State.m_routePlanResponses, Int_Set_E.Get (Elements (Formal_Set), K)))
                  = isFulfilled);
               if not Contains (State.m_routePlanResponses, Element (Formal_Set, C)) then
                  isFulfilled := False;
               end if;
               Next (Formal_Set, C);
            end loop;

            pragma Assert
              (isFulfilled
               = (for all J of Element (State.m_pendingRoute, i) =>
                   Contains (State.m_routePlanResponses, J)));
            if isFulfilled then
               SendRouteResponse (Mailbox, State.m_pendingRoute, State.m_pendingAutoReq, State.m_routePlanResponses, State.m_routePlans, Key (State.m_pendingRoute, i));

               declare
                  Dummy    : Int64_Formal_Set_Maps.Cursor := i;
                  Pos      : constant Count_Type := Int_Set_Maps_P.Get (Positions (State.m_pendingRoute), i) with Ghost;
               begin
                  Next (State.m_pendingRoute, i);
                  Delete_PendingRequest (State.m_pendingRoute, State.m_pendingAutoReq, State.m_routeRequestId, Dummy);
                  D := D + 1;
                  pragma Assert
                    (for all K in 1 .. Pos - 1 =>
                       Is_Pending (Model (State.m_pendingRoute), Model (State.m_routePlanResponses), Int_Set_Maps_K.Get (Keys (State.m_pendingRoute), K)));
               end;
               pragma Assert (All_Pending_Plans_Sent (State.m_pendingRoute, State.m_routePlanResponses));

            else
               Next (State.m_pendingRoute, i);
            end if;
         end;
      end loop;

      pragma Assert
        (for all K in 1 .. Length (State.m_pendingRoute) =>
             Is_Pending (Model (State.m_pendingRoute), Model (State.m_routePlanResponses), Int_Set_Maps_K.Get (Keys (State.m_pendingRoute), K)));
   end Check_All_Route_Plans_PendingRoute;

   -------------------------------------
   -- Check_All_Task_Options_Received --
   -------------------------------------

   procedure Check_All_Task_Options_Received
     (Mailbox : in out Route_Aggregator_Mailbox;
      Data  : Route_Aggregator_Configuration_Data;
      State : in out Route_Aggregator_State)
     with
       SPARK_Mode => Off
   is
      C : UAR_Maps.Cursor :=
        First (State.m_uniqueAutomationRequests);
   begin
      while Has_Element (State.m_uniqueAutomationRequests, C) loop

         declare
            Areq        : constant UniqueAutomationRequest :=
              Element (State.m_uniqueAutomationRequests, C);
            AllReceived : constant Boolean :=
              (for all TaskId of Areq.TaskList =>
                 Contains (State.m_taskOptions, TaskId));
         begin

            if AllReceived then
               Build_Matrix_Requests
                 (Mailbox,
                  Data,
                  State,
                  Key (State.m_uniqueAutomationRequests, C));
            end if;

         end;

         Next (State.m_uniqueAutomationRequests, C);
      end loop;
   end Check_All_Task_Options_Received;

   ---------------------------
   -- Delete_PendingRequest --
   ---------------------------

   procedure Delete_PendingRequest
     (m_pendingRequest : in out Int64_Formal_Set_Map;
      otherPending     : Int64_Formal_Set_Map;
      m_routeRequestId : Int64;
      Position         : in out Int64_Formal_Set_Maps.Cursor)
   is
      pragma Unreferenced (otherPending, m_routeRequestId);
      Old_pendingRoute_M : constant Int64_Formal_Set_Maps.Formal_Model.M.Map :=
        Int64_Formal_Set_Maps.Formal_Model.Model (m_pendingRequest) with Ghost;
      Old_pendingRoute   : constant Int_Set_Maps_M.Map := Model (m_pendingRequest) with Ghost;
   begin
      Delete (m_pendingRequest, Position);

      --  Establish the effect on the redefined Model of maps of formal sets
      pragma Assert (Model (m_pendingRequest) <= Old_pendingRoute);
   end Delete_PendingRequest;

   --------------------
   -- Euclidean_Plan --
   --------------------

   procedure Euclidean_Plan
     (Data               : Route_Aggregator_Configuration_Data;
      routePlanResponses : in out Int64_RouteResponse_Map;
      routePlans         : in out Int64_IdPlanPair_Map;
      Request            : RoutePlanRequest)
   is
      pragma Unreferenced (Data, routePlans, routePlanResponses, Request);
      --  --  UxAS::common::utilities::CUnitConversions flatEarth;
      --  FlatEarth : Conversions.Unit_Converter;
      --  --  int64_t regionId = request->getOperatingRegion();
      --  RegionId  : Int64 := Request.OperatingRegion;
      --  --  int64_t vehicleId = request->getVehicleID();
      --  VehicleId : Int64 := Request.VehicleID;
      --  --  int64_t taskId = request->getAssociatedTaskID();
      --  TaskId    : Int64 := Request.AssociatedTaskID;
      --  --  double speed = 1.0; // default if no speed available
      --  Speed : Real64 := 1.0;
   begin
      raise Program_Error with "Euclidean_Plan is unimplemented";

      --  if (m_entityConfigurations.find(vehicleId) != m_entityConfigurations.end())
      --  {
      --      double speed = m_entityConfigurations[vehicleId]->getNominalSpeed();
      --      if (speed < 1e-2)
      --      {
      --          speed = 1.0; // default to 1 if too small for division
      --      }
      --  }

      --  auto response = std::shared_ptr<UxAS::messages::route::RoutePlanResponse>(new UxAS::messages::route::RoutePlanResponse);
      --  response->setAssociatedTaskID(taskId);
      --  response->setOperatingRegion(regionId);
      --  response->setVehicleID(vehicleId);
      --  response->setResponseID(request->getRequestID());
      --
      --  for (size_t k = 0; k < request->getRouteRequests().size(); k++)
      --  {
      --      UxAS::messages::route::RouteConstraints* routeRequest = request->getRouteRequests().at(k);
      --      int64_t routeId = routeRequest->getRouteID();
      --      VisiLibity::Point startPt, endPt;
      --      double north, east;
      --
      --      UxAS::messages::route::RoutePlan* plan = new UxAS::messages::route::RoutePlan;
      --      plan->setRouteID(routeId);
      --
      --      flatEarth.ConvertLatLong_degToNorthEast_m(routeRequest->getStartLocation()->getLatitude(), routeRequest->getStartLocation()->getLongitude(), north, east);
      --      startPt.set_x(east);
      --      startPt.set_y(north);
      --
      --      flatEarth.ConvertLatLong_degToNorthEast_m(routeRequest->getEndLocation()->getLatitude(), routeRequest->getEndLocation()->getLongitude(), north, east);
      --      endPt.set_x(east);
      --      endPt.set_y(north);
      --
      --      double linedist = VisiLibity::distance(startPt, endPt);
      --      plan->setRouteCost(linedist / speed * 1000); // milliseconds to arrive
      --      m_routePlans[routeId] = std::make_pair(request->getRequestID(), std::shared_ptr<UxAS::messages::route::RoutePlan>(plan));
      --  }
      --  m_routePlanResponses[response->getResponseID()] = response;
   end Euclidean_Plan;

   --------------------------------
   -- Handle_Route_Plan_Response --
   --------------------------------

   procedure Handle_Route_Plan_Response
     (Mailbox  : in out Route_Aggregator_Mailbox;
      State    : in out Route_Aggregator_State;
      Response : RoutePlanResponse)
   is
   begin
      --  pragma Assume (Length (History) < Count_Type'Last, "We still have room for a new event in History");
      History := Add (History, (Kind => Receive_PlanResponse, Id => Response.ResponseID));
      pragma Assert (No_RouteRequest_Lost (State.m_pendingRoute));

      pragma Assume (Length (State.m_routePlanResponses) < Count_Type'Last, "We have some room for a new plan response");
      Insert (State.m_routePlanResponses, Response.ResponseID, Response);
      pragma Assert (Valid_Plan_Responses (State.m_pendingRoute, State.m_pendingAutoReq, State.m_routePlanResponses));
      pragma Assert
        (Only_Pending_Plans (State.m_routePlanResponses, State.m_routePlans));

      for p in 1 .. Last (Response.RouteResponses) loop

         --  All plans are registered up to p

         pragma Loop_Invariant
           (for all RP of Model (State.m_routePlanResponses) =>
              (if RP /= Response.ResponseID then
                   (for all Pl of Element (State.m_routePlanResponses, RP).RouteResponses =>
                          Contains (State.m_routePlans, Pl.RouteID)
                    and then Element (State.m_routePlans, Pl.RouteID).Id = RP)));
         pragma Loop_Invariant
           (for all K in 1 .. p - 1 =>
              Contains (State.m_routePlans, Get (Response.RouteResponses, K).RouteID)
            and then Element (State.m_routePlans, Get (Response.RouteResponses, K).RouteID).Id = Response.ResponseID);
         pragma Loop_Invariant
           (for all K in p .. Last (Response.RouteResponses) =>
              not Contains (State.m_routePlans, Get (Response.RouteResponses, K).RouteID));

         --  Invariants

         pragma Loop_Invariant
           (Only_Pending_Plans (State.m_routePlanResponses, State.m_routePlans));

         pragma Assume (Length (State.m_routePlans) < Count_Type'Last, "We have enough room for all route plans");

         declare
            ID   : constant Int64 := Get (Response.RouteResponses, p).RouteID;
            Plan : constant RoutePlan := Get (Response.RouteResponses, p);
         begin
            pragma Assume (Length (State.m_routePlans) < Count_Type'Last, "We still have room for a response");
            Insert (State.m_routePlans, ID,
                    IdPlanPair'(Id   => Response.ResponseID,
                                Plan => Plan,
                                Cost => Get (Response.RouteResponses, p).RouteCost));
         end;
         pragma Assert (Contains (Element (State.m_routePlanResponses, Response.ResponseID).RouteResponses, Get (Response.RouteResponses, p).RouteID));
      end loop;

      pragma Assert (No_RouteRequest_Lost (State.m_pendingRoute));
      pragma Assert (for all Pl of Element (State.m_routePlanResponses, Response.ResponseID).RouteResponses =>
                       Contains (State.m_routePlans, Pl.RouteID)
                     and then Element (State.m_routePlans, Pl.RouteID).Id = Response.ResponseID);
      pragma Assert (for all RP of Model (State.m_routePlanResponses) =>
                       (for all Pl of Element (State.m_routePlanResponses, RP).RouteResponses =>
                            Contains (State.m_routePlans, Pl.RouteID)
                        and then Element (State.m_routePlans, Pl.RouteID).Id = RP));
      Check_All_Route_Plans (Mailbox, State);
   end Handle_Route_Plan_Response;

   --------------------------
   -- Handle_Route_Request --
   --------------------------

   procedure Handle_Route_Request
     (Data    : Route_Aggregator_Configuration_Data;
      Mailbox : in out Route_Aggregator_Mailbox;
      State   : in out Route_Aggregator_State;
      Request : RouteRequest)
   is
      use all type History_Type;
      Vehicle_Ids        : Int64_Seq := Request.VehicleID;
      PlanRequests       : Int64_Formal_Set;
   begin
      pragma Assert (No_PlanResponse_Lost (State.m_pendingRoute, State.m_routePlanResponses));
      --  pragma Assume (Length (History) < Count_Type'Last, "We still have room for a new event in History");
      History := Add (History, (Kind => Receive_RouteRequest, Id => Request.RequestID));
      pragma Assert
        (for all Pos in Interval'(Event_Sequences.First, Last (History) - 1) =>
           (if Get (History, Pos).Kind = Receive_PlanResponse
            and Has_Key (Plan_To_Route (State.m_pendingRoute), Get (History, Pos).Id)
            then Contains (State.m_routePlanResponses, Get (History, Pos).Id)));
      pragma Assert (No_PlanResponse_Lost (State.m_pendingRoute, State.m_routePlanResponses));

---------------------------------------------------------------------------------------------------------------
--  this doesn't match the C++ exactly, because it doesn't put the new vehicle
--  ids into the message parameter (which is "by reference" via pointer in C++)
--  but that seems to be OK because only EuclidianPlan would use it, apparently
      if Length (Vehicle_Ids) = 0 then
         Vehicle_Ids := Data.m_entityStates;
      end if;
---------------------------------------------------------------------------------------------------------------

      --  We only have route plan responses with Ids smaller than State.m_routeRequestId
      pragma Assert
        (for all K of Model (State.m_routePlanResponses) =>
             K <= State.m_routeRequestId);
      pragma Assert
        (for all E of History =>
           (if E.Kind = Receive_PlanResponse then
                 E.Id <= State.m_routeRequestId));

      for K in 1 .. Last (Vehicle_Ids) loop

         --  We are only adding to planrequests new request ids

         pragma Loop_Invariant
           (State.m_routeRequestId'Loop_Entry <= State.m_routeRequestId);
         pragma Loop_Invariant
           (for all Id of Model (PlanRequests) => Id > State.m_routeRequestId'Loop_Entry
            and Id <= State.m_routeRequestId);
         pragma Loop_Invariant
           (for all Id of Model (State.m_pendingRoute) =>
              (for all K of Int_Set_Maps_M.Get (Model (State.m_pendingRoute), Id) =>
                   K <= State.m_routeRequestId'Loop_Entry));
         pragma Loop_Invariant (Length (PlanRequests) <= Count_Type (K - 1));

         --  If fast planning is used, we may already have some responses for the
         --  new plan requests.

         pragma Loop_Invariant
           (for all K of Model (State.m_routePlanResponses) =>
                Contains (Model (State.m_routePlanResponses)'Loop_Entry, K)
            or else (Data.m_fastPlan and then Contains (PlanRequests, K)));

         --  General Invariants

         pragma Loop_Invariant
           (All_Plans_Registered (State.m_routePlanResponses, State.m_routePlans));
         pragma Loop_Invariant
           (Only_Pending_Plans (State.m_routePlanResponses, State.m_routePlans));
         pragma Loop_Invariant
           (No_Finished_Request (State.m_pendingRoute, State.m_pendingAutoReq, State.m_routePlanResponses));

         --  Update of the history, it may contain new plan requests

         pragma Loop_Invariant (History'Loop_Entry <= History);
         pragma Loop_Invariant
           (for all I in Interval'(1, Last (History)) =>
                (if I >  Last (History)'Loop_Entry then
                        Get (History, I).Kind = Send_PlanRequest));

         --  History Invariants

         pragma Loop_Invariant (Valid_Events (State.m_routeRequestId));
         pragma Loop_Invariant
           (No_PlanResponse_Lost (State.m_pendingRoute, State.m_routePlanResponses));
         pragma Loop_Invariant
           (All_Pending_Plans_Sent (State.m_pendingRoute, State.m_routePlanResponses));
         pragma Loop_Invariant
           (for all Id of Model (PlanRequests) =>
              PlanRequest_Processed (State.m_routePlanResponses, Id));

         declare
            Vehicle_Id  : Int64 renames Get (Vehicle_Ids, K);

            -- create a new route plan request

            pragma Assume (State.m_routeRequestId < Int64'Last, "The request ID does not overflow");
            planRequest : constant RoutePlanRequest :=
              (AssociatedTaskID  => Request.AssociatedTaskID,
               IsCostOnlyRequest => Request.IsCostOnlyRequest,
               OperatingRegion   => Request.OperatingRegion,
               VehicleID         => Vehicle_Id,
               RequestID         => State.m_routeRequestId + 1,
               RouteRequests     => Request.RouteRequests);
         begin

            State.m_routeRequestId := State.m_routeRequestId + 1;

            Insert (PlanRequests, planRequest.RequestID);

            if Contains (Data.m_groundVehicles, Vehicle_Id) then
               if Data.m_fastPlan then
                  -- short-circuit and just plan with straight line planner

                  Euclidean_Plan (Data,
                                  State.m_routePlanResponses,
                                  State.m_routePlans,
                                  planRequest);
               else

                  -- send externally

                  sendLimitedCastMessage
                    (Mailbox, GroundPathPlanner, planRequest);
                  --  pragma Assume (Length (History) < Count_Type'Last, "We still have room for a new event in History");
                  History := Add (History, (Kind => Send_PlanRequest, Id => planRequest.RequestID));
                  pragma Assert (PlanRequest_Sent (planRequest.RequestID));
               end if;
            else
               pragma Assert
                 (Contains (Data.m_airVehicles, Vehicle_Id)
                  or else Contains (Data.m_surfaceVehicles, Vehicle_Id));

               -- send to aircraft planner

               sendLimitedCastMessage
                 (Mailbox, AircraftPathPlanner, planRequest);
               --  pragma Assume (Length (History) < Count_Type'Last, "We still have room for a new event in History");
               History := Add (History, (Kind => Send_PlanRequest, Id => planRequest.RequestID));
               pragma Assert (PlanRequest_Sent (planRequest.RequestID));

            end if;
         end;
         pragma Assert
           (for all Id of Model (PlanRequests) =>
                PlanRequest_Processed (State.m_routePlanResponses, Id));
         pragma Assert (All_Pending_Plans_Sent (State.m_pendingRoute, State.m_routePlanResponses));
      end loop;

      --  Restate part of the loop invariants after the loop
      pragma Assert
        (for all E of History =>
           (if E.Kind = Receive_PlanResponse then not Contains (PlanRequests, E.Id)));
      pragma Assert (All_Pending_Plans_Sent (State.m_pendingRoute, State.m_routePlanResponses));
      pragma Assert
        (No_Finished_Request (State.m_pendingRoute, State.m_pendingAutoReq, State.m_routePlanResponses));

      pragma Assert
        (for all R_Id of Model (State.m_pendingRoute) =>
             (for all E of Int_Set_Maps_M.Get (Model (State.m_pendingRoute), R_Id) =>
                   not Contains (PlanRequests, E)));

      pragma Assume (Length (State.m_pendingRoute) < Count_Type'Last, "We have enough room for a new pending route request");
      Insert_PendingRequest
        (State.m_pendingRoute, State.m_pendingAutoReq, State.m_routeRequestId, Request.RequestID, PlanRequests);

      --  System invariants have been reestablished

      pragma Assert (All_Pending_Plans_Sent (State.m_pendingRoute, State.m_routePlanResponses));
      pragma Assert (Valid_Plan_Responses (State.m_pendingRoute, State.m_pendingAutoReq, State.m_routePlanResponses));
      pragma Assert
        (No_PlanResponse_Lost (State.m_pendingRoute, State.m_routePlanResponses));

      -- if fast planning, then all routes should be complete; kick off response

      if Data.m_fastPlan then
         Check_All_Route_Plans (Mailbox, State);
      else
         pragma Assert (Is_Pending (Model (State.m_pendingRoute), Model (State.m_routePlanResponses), Request.RequestID));
         pragma Assert (No_Finished_Request (State.m_pendingRoute, State.m_pendingAutoReq, State.m_routePlanResponses));
      end if;
   end Handle_Route_Request;

   ------------------------------
   -- Handle_Task_Plan_Options --
   ------------------------------

   procedure Handle_Task_Plan_Options
     (Mailbox : in out Route_Aggregator_Mailbox;
      Data  : Route_Aggregator_Configuration_Data;
      State : in out Route_Aggregator_State;
      Options : TaskPlanOptions)
     with
       SPARK_Mode => Off
   is
      Id : constant Int64 := Options.TaskID;
   begin
      Insert (State.m_taskOptions, Id, Options);
      Check_All_Task_Options_Received (Mailbox, Data, State);
   end Handle_Task_Plan_Options;

   --------------------------------------
   -- Handle_Unique_Automation_Request --
   --------------------------------------

   procedure Handle_Unique_Automation_Request
     (Data    : Route_Aggregator_Configuration_Data;
      Mailbox : in out Route_Aggregator_Mailbox;
      State   : in out Route_Aggregator_State;
      Areq    : UniqueAutomationRequest)
   is
   begin
      Insert (State.m_uniqueAutomationRequests,
              State.m_autoRequestId + 1,
              Areq);
      State.m_autoRequestId := State.m_autoRequestId + 1;
      Check_All_Task_Options_Received (Mailbox, Data, State);
   end Handle_Unique_Automation_Request;

   ---------------------------
   -- Insert_PendingRequest --
   ---------------------------

   procedure Insert_PendingRequest
     (m_pendingRequest : in out Int64_Formal_Set_Map;
      otherPending     : Int64_Formal_Set_Map;
      m_routeRequestId : Int64;
      RequestID        : Int64;
      PlanRequests     : Int64_Formal_Set)
   is
      pragma Unreferenced (otherPending);
      Old_pendingRequest   : constant Int_Set_Maps_M.Map := Model (m_pendingRequest) with Ghost;
      Old_pendingRequest_M : constant Int64_Formal_Set_Maps.Formal_Model.M.Map :=
        Int64_Formal_Set_Maps.Formal_Model.Model (m_pendingRequest) with Ghost;
   begin

      Insert (m_pendingRequest, RequestID, PlanRequests);
      --  Establish the effect on the redefined Model of maps of formal sets
      pragma Assert (Old_pendingRequest <= Model (m_pendingRequest));

   end Insert_PendingRequest;

   --  Model functions used in contracts

   -----------
   -- Model --
   -----------

   function Model (M : Int64_Formal_Set_Map) return Int_Set_Maps_M.Map is

      function Model (S : Int64_Formal_Set) return Int64_Set with
        Post =>
          (for all E of Model'Result => Contains (S, E))
          and
            (for all E of S => Contains (Model'Result, E));

      function Model (S : Int64_Formal_Set) return Int64_Set is
         Res : Int64_Set;
      begin
         for C in S loop
            pragma Loop_Variant (Increases => Int_Set_P.Get (Positions (S), C));
            pragma Loop_Invariant (Length (Res) = +(Int_Set_P.Get (Positions (S), C) - 1));
            pragma Loop_Invariant (for all E of Res => Contains (S, E));
            pragma Loop_Invariant
              (for all K in 1 .. Int_Set_P.Get (Positions (S), C) - 1 =>
                 Contains (Res, Int_Set_E.Get (Elements (S), K)));
            pragma Loop_Invariant
              (for all K in Int_Set_P.Get (Positions (S), C) .. Length (S) =>
                    not Contains (Res, Int_Set_E.Get (Elements (S), K)));
            Res := Add (Res, Element (S, C));
         end loop;
         return Res;
      end Model;

      Res : Int_Set_Maps_M.Map;
   begin
      for C in M loop
         pragma Loop_Variant (Increases => Int_Set_Maps_P.Get (Positions (M), C));
         pragma Loop_Invariant (Int_Set_Maps_M.Length (Res) = +(Int_Set_Maps_P.Get (Positions (M), C) - 1));
         pragma Loop_Invariant (for all I of Res => Contains (M, I));
         pragma Loop_Invariant
           (for all I of Res =>
              (for all E of Int_Set_Maps_M.Get (Res, I) =>
                   Contains (Element (M, I), E)));
         pragma Loop_Invariant
           (for all I of Res =>
              (for all E of Element (M, I) =>
                   Contains (Int_Set_Maps_M.Get (Res, I), E)));
         pragma Loop_Invariant
           (for all K in 1 .. Int_Set_Maps_P.Get (Positions (M), C) - 1 =>
                 Int_Set_Maps_M.Has_Key (Res, Int_Set_Maps_K.Get (Keys (M), K)));
         pragma Loop_Invariant
           (for all K in Int_Set_Maps_P.Get (Positions (M), C) .. Length (M) =>
                 not Int_Set_Maps_M.Has_Key (Res, Int_Set_Maps_K.Get (Keys (M), K)));
         Res := Int_Set_Maps_M.Add (Res, Key (M, C), Model (Element (M, C)));
      end loop;
      return Res;
   end Model;

   ----------------
   -- SendMatrix --
   ----------------

   procedure SendMatrix
     (Mailbox                    : in out Route_Aggregator_Mailbox;
      m_uniqueAutomationRequests : Int64_UniqueAutomationRequest_Map;
      m_pendingRoute             : Int64_Formal_Set_Map;
      m_pendingAutoReq           : Int64_Formal_Set_Map;
      m_routePlans               : in out Int64_IdPlanPair_Map;
      m_routeTaskPairing         : in out Int64_TaskOptionPair_Map;
      m_routePlanResponses       : in out Int64_RouteResponse_Map;
      m_taskOptions              : in out Int64_TaskPlanOptions_Map;
      autoKey                    : Int64)
   is
      areq            : constant UniqueAutomationRequest :=
        Element (m_uniqueAutomationRequests, autoKey);
      matrix          : AssignmentCostMatrix;
      pendingRequests : Int64_Formal_Set renames Element (m_pendingAutoReq, autoKey);
      Old_routePlanResponses : constant Int64_RouteResponse_Map := m_routePlanResponses with Ghost;
   begin
      matrix.CorrespondingAutomationRequestID := areq.RequestID;
      matrix.OperatingRegion := areq.OperatingRegion;
      matrix.TaskList := areq.TaskList;

      for Cu in pendingRequests loop

         pragma Loop_Invariant
           (for all I in 1 .. Int_Set_P.Get (Positions (pendingRequests), Cu) - 1 =>
               not Contains (m_routePlanResponses, Int_Set_E.Get (Elements (pendingRequests), I)));
         pragma Loop_Invariant
           (for all I in Int_Set_P.Get (Positions (pendingRequests), Cu) .. Length (pendingRequests) =>
                Contains (m_routePlanResponses, Int_Set_E.Get (Elements (pendingRequests), I)));
         pragma Loop_Invariant
           (for all Id of Old_routePlanResponses =>
              (if not Contains (pendingRequests, Id) then
                    Contains (m_routePlanResponses, Id)));
         pragma Loop_Invariant
           (for all Id of Model (m_routePlanResponses) =>
                Contains (Old_routePlanResponses, Id));

         --  Invariants

         pragma Loop_Invariant
           (Valid_Plan_Responses (m_pendingRoute, m_pendingAutoReq, m_routePlanResponses));
         pragma Loop_Invariant
           (All_Plans_Registered (m_routePlanResponses, m_routePlans));
         pragma Loop_Invariant
           (Only_Pending_Plans (m_routePlanResponses, m_routePlans));

         --  History invariants

         pragma Loop_Invariant (No_RouteRequest_Lost (m_pendingRoute));
         pragma Loop_Invariant (No_PlanResponse_Lost (m_pendingRoute, m_routePlanResponses));
         pragma Loop_Invariant (All_Pending_Plans_Sent (m_pendingRoute, m_routePlanResponses));

         declare
            rId : constant Int64 := Element (pendingRequests, Cu);
            pragma Assert (Contains (m_routePlanResponses, rId));
         begin

            declare
               plan  : Int64_RouteResponse_Maps.Cursor := Find (m_routePlanResponses, rId);

               --  NB. The if statement checking whether rId is in
               --  routePlanResponses was removed as SendRouteResponse is only
               --  called when all plan responses have been received.
               pragma Assert (Has_Element (m_routePlanResponses, plan));

               resps : RP_Seq renames Element (m_routePlanResponses, plan).RouteResponses;
            begin

               -- delete all individual routes from storage

               for i in 1 .. Last (resps) loop

                  --  We have removed all elements of resps from routePlans
                  --  up to i.

                  pragma Loop_Invariant
                    (for all RP of Model (m_routePlanResponses) =>
                         (if RP /= rId then
                              (for all Pl of Element (m_routePlanResponses, RP).RouteResponses =>
                                     Contains (m_routePlans, Pl.RouteID)
                               and then Element (m_routePlans, Pl.RouteID).Id = RP)));
                  pragma Loop_Invariant
                    (Only_Pending_Plans (m_routePlanResponses, m_routePlans));
                  pragma Loop_Invariant
                    (for all Pl of Model (m_routePlans) =>
                         (if Element (m_routePlans, Pl).Id = rId then
                            (for some K in i .. Last (resps) =>
                                 Get (resps, K).RouteID = Pl)));
                  pragma Loop_Invariant
                    (for all K in i .. Last (resps) =>
                         Contains (m_routePlans, Get (resps, K).RouteID)
                     and then Element (m_routePlans, Get (resps, K).RouteID).Id = rId);

                  if Contains (m_routeTaskPairing, Get (resps, i).RouteID) then

                     declare
                        routeplan : constant IdPlanPair :=
                          Element (m_routePlans, Get (resps, i).RouteID);
                        taskpair  : constant TaskOptionPair :=
                          Element (m_routeTaskPairing, Get (resps, i).RouteID);
                        toc       : TaskOptionCost;
                     begin
                        if routeplan.Cost < 0 then
                           Put_Line ("Route not found: V[" &
                                       taskpair.vehicleId'Image & "](" &
                                       taskpair.prevTaskId'Image & "," &
                                       taskpair.prevTaskOption'Image & ")-(" &
                                       taskpair.taskId'Image & "," &
                                       taskpair.taskOption'Image & ")");
                        end if;

                        toc.DestinationTaskID := taskpair.taskId;
                        toc.DestinationTaskOption := taskpair.taskOption;
                        toc.InitialTaskID := taskpair.prevTaskId;
                        toc.InitialTaskOption := taskpair.prevTaskOption;
                        toc.TimeToGo := routeplan.Cost;
                        toc.VehicleID := taskpair.vehicleId;
                        pragma Assume (Length (matrix.CostMatrix) < Count_Type'Last, "we still have room in the matrix");
                        matrix.CostMatrix := Add (matrix.CostMatrix, toc);
                     end;

                     Delete (m_routeTaskPairing, Get (resps, i).RouteID);
                  end if;

                  --  We only delete plans associated to rId
                  pragma Assert (Element (m_routePlans, Get (resps, i).RouteID).Id = rId);
                  Delete (m_routePlans, Get (resps, i).RouteID);
               end loop;

               pragma Assert
                 (for all Pl of Model (m_routePlans) => Element (m_routePlans, Pl).Id /= rId);
               pragma Assert (All_Pending_Plans_Sent (m_pendingRoute, m_routePlanResponses));
               pragma Assert (No_Overlaps (Model (m_pendingRoute), Model (m_pendingAutoReq)));
               pragma Assert (for all Id of Plan_To_Route (m_pendingRoute) => Id /= Key (m_routePlanResponses, plan));
               Delete (m_routePlanResponses, plan);
               pragma Assert (All_Pending_Plans_Sent (m_pendingRoute, m_routePlanResponses));
               pragma Assert
                 (Only_Pending_Plans (m_routePlanResponses, m_routePlans));
            end;
         end;
      end loop;

      sendBroadcastMessage (Mailbox, matrix);
      Clear (m_taskOptions);
   end SendMatrix;

   -----------------------
   -- SendRouteResponse --
   -----------------------

   procedure SendRouteResponse
     (Mailbox            : in out Route_Aggregator_Mailbox;
      pendingRoute       : Int64_Formal_Set_Map;
      pendingAutoReq     : Int64_Formal_Set_Map;
      routePlanResponses : in out Int64_RouteResponse_Map;
      routePlans         : in out Int64_IdPlanPair_Map;
      routeKey           : Int64)
   is
      Response               : RouteResponse;
      PlanResponses          : Int64_Formal_Set renames Element (pendingRoute, routeKey);
      Old_routePlanResponses : constant RR_Maps_M.Map := Model (routePlanResponses) with Ghost;
   begin
      Response.ResponseID := routeKey;
      for Cu in PlanResponses loop

         --  Number of elements added to response.Routes

         pragma Loop_Invariant (Length (Response.Routes) < Int_Set_P.Get (Positions (PlanResponses), Cu));

         --  We have removed all elements of PlanResponses from routePlanResponses
         --  up to Cu.

         pragma Loop_Invariant
           (for all I in 1 .. Int_Set_P.Get (Positions (PlanResponses), Cu) - 1 =>
              not Contains (routePlanResponses, Int_Set_E.Get (Elements (PlanResponses), I)));
         pragma Loop_Invariant
           (for all I in Int_Set_P.Get (Positions (PlanResponses), Cu) .. Length (PlanResponses) =>
              Contains (routePlanResponses, Int_Set_E.Get (Elements (PlanResponses), I)));
         pragma Loop_Invariant
           (for all Id of Old_routePlanResponses =>
              (if not Contains (PlanResponses, Id) then
                    Contains (routePlanResponses, Id)));
         pragma Loop_Invariant
           (for all Id of Model (routePlanResponses) =>
                Contains (Old_routePlanResponses, Id));

         --  Invariants

         pragma Loop_Invariant
           (Valid_Plan_Responses (pendingRoute, pendingAutoReq, routePlanResponses));
         pragma Loop_Invariant
           (All_Plans_Registered (routePlanResponses, routePlans));
         pragma Loop_Invariant
           (Only_Pending_Plans (routePlanResponses, routePlans));

         --  History invariants:
         --  We have only removed responses associated to routeKey

         pragma Loop_Invariant
           (for all Id of Plan_To_Route (pendingRoute) =>
                (if Get (Plan_To_Route (pendingRoute), Id) /= routeKey then
                      Contains (routePlanResponses, Id)
                 or else PlanRequest_Sent (Id)));
         pragma Loop_Invariant
           (for all E of History =>
              (if E.Kind = Receive_PlanResponse
               and then Has_Key (Plan_To_Route (pendingRoute), E.Id)
               and then Get (Plan_To_Route (pendingRoute), E.Id) /= routeKey
               then Contains (routePlanResponses, E.Id)));

         declare
            rId : Int64 renames Element (PlanResponses, Cu);
         begin

            declare
               plan  : Int64_RouteResponse_Maps.Cursor := Find (routePlanResponses, rId);

               --  NB. The if statement checking whether rId is in
               --  routePlanResponses was removed as SendRouteResponse is only
               --  called when all plan responses have been received.
               pragma Assert (Has_Element (routePlanResponses, plan));

               resps : RP_Seq renames Element (routePlanResponses, plan).RouteResponses;
            begin
               Response.Routes := Add (Response.Routes, Element (routePlanResponses, plan));

               -- delete all individual routes from storage

               for i in 1 .. Last (resps) loop

                  --  We have removed all elements of resps from routePlans
                  --  up to i.

                  pragma Loop_Invariant
                    (for all RP of Model (routePlanResponses) =>
                       (if RP /= rId then
                            (for all Pl of Element (routePlanResponses, RP).RouteResponses =>
                                   Contains (routePlans, Pl.RouteID)
                             and then Element (routePlans, Pl.RouteID).Id = RP)));
                  pragma Loop_Invariant
                    (Only_Pending_Plans (routePlanResponses, routePlans));
                  pragma Loop_Invariant
                    (for all Pl of Model (routePlans) =>
                       (if Element (routePlans, Pl).Id = rId then
                            (for some K in i .. Last (resps) =>
                                   Get (resps, K).RouteID = Pl)));
                  pragma Loop_Invariant
                    (for all K in i .. Last (resps) =>
                       Contains (routePlans, Get (resps, K).RouteID)
                     and then Element (routePlans, Get (resps, K).RouteID).Id = rId);

                  --  We only delete plans associated to rId
                  pragma Assert (Element (routePlans, Get (resps, i).RouteID).Id = rId);
                  Delete (routePlans, Get (resps, i).RouteID);
               end loop;

               pragma Assert
                 (Only_Pending_Plans (routePlanResponses, routePlans));
               pragma Assert (plan = Find (routePlanResponses, rId));
               Delete (routePlanResponses, plan);
            end;
         end;
      end loop;
      pragma Assert (All_Plans_Registered (routePlanResponses, routePlans));
      pragma Assert
        (for all Id of Plan_To_Route (pendingRoute) =>
             (if Get (Plan_To_Route (pendingRoute), Id) /= routeKey then
                   Contains (routePlanResponses, Id)
              or else PlanRequest_Sent (Id)));

      -- send the results of the query
      sendBroadcastMessage (Mailbox, Response);
      --  pragma Assume (Length (History) < Count_Type'Last, "We still have room for a new event in History");
      History := Add (History, (Kind => Send_RouteResponse, Id => Response.ResponseID));
   end SendRouteResponse;

   -----------------
   -- Plan_To_Route --
   -----------------

   function Plan_To_Route (pendingRoute : Int64_Formal_Set_Map) return Int64_Map is
      Res : Int64_Map;
   begin
      for C in pendingRoute loop
         pragma Loop_Variant (Increases => Int_Set_Maps_P.Get (Positions (pendingRoute), C));
         pragma Loop_Invariant
           (for all I of Res =>
              Int_Set_Maps_M.Has_Key (Model (pendingRoute), Get (Res, I))
            and then Contains (Int_Set_Maps_M.Get (Model (pendingRoute), Get (Res, I)), I));
         pragma Loop_Invariant
           (for all J in 1 .. Int_Set_Maps_P.Get (Positions (pendingRoute), C) - 1 =>
              (for all K of Int_Set_Maps_M.Get (Model (pendingRoute), Int_Set_Maps_K.Get (Keys (pendingRoute), J)) =>
                   Has_Key (Res, K)
               and then Get (Res, K) = Int_Set_Maps_K.Get (Keys (pendingRoute), J)));
         pragma Loop_Invariant
           (for all J in Int_Set_Maps_P.Get (Positions (pendingRoute), C) .. Length (pendingRoute) =>
              (for all K of Int_Set_Maps_M.Get (Model (pendingRoute), Int_Set_Maps_K.Get (Keys (pendingRoute), J)) =>
                   not Has_Key (Res, K)));

         declare
            routePlans : Int64_Formal_Set renames Element (pendingRoute, C);
         begin
            for C2 in routePlans loop
               pragma Loop_Variant (Increases => Int_Set_P.Get (Positions (routePlans), C2));
               pragma Loop_Invariant
                 (for all I of Res =>
                    Int_Set_Maps_M.Has_Key (Model (pendingRoute), Get (Res, I))
                  and then Contains (Int_Set_Maps_M.Get (Model (pendingRoute), Get (Res, I)), I));
               pragma Loop_Invariant
                 (for all J in 1 .. Int_Set_Maps_P.Get (Positions (pendingRoute), C) - 1 =>
                    (for all K of Int_Set_Maps_M.Get (Model (pendingRoute), Int_Set_Maps_K.Get (Keys (pendingRoute), J)) =>
                         Has_Key (Res, K)
                     and then Get (Res, K) = Int_Set_Maps_K.Get (Keys (pendingRoute), J)));
               pragma Loop_Invariant
                 (if Int_Set_Maps_P.Get (Positions (pendingRoute), C) < Length (pendingRoute)
                  then
                    (for all J in Int_Set_Maps_P.Get (Positions (pendingRoute), C) + 1 .. Length (pendingRoute) =>
                      (for all K of Int_Set_Maps_M.Get (Model (pendingRoute), Int_Set_Maps_K.Get (Keys (pendingRoute), J)) =>
                         not Has_Key (Res, K))));
               pragma Loop_Invariant
                 (for all J in 1 .. Int_Set_P.Get (Positions (routePlans), C2) - 1 =>
                         Has_Key (Res, Int_Set_E.Get (Elements (routePlans), J))
                     and then Get (Res, Int_Set_E.Get (Elements (routePlans), J)) = Key (pendingRoute, C));
               pragma Loop_Invariant
                 (for all J in Int_Set_P.Get (Positions (routePlans), C2) .. Length (routePlans) =>
                         not Has_Key (Res, Int_Set_E.Get (Elements (routePlans), J)));

               Res := Add (Res, Element (routePlans, C2), Key (pendingRoute, C));
            end loop;
         end;
      end loop;
      return Res;
   end Plan_To_Route;
end Route_Aggregator;
