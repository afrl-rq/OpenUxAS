pragma Ada_2012;
with Ada.Containers;
with LMCP_Messages; use LMCP_Messages;
with Common; use Common;
with Daidalus_Response_Mailboxes; use Daidalus_Response_Mailboxes;
with AVTAS.LMCP.Types;
with LMCP_Message_Conversions; use LMCP_Message_Conversions;
with UxAS.Comms.LMCP_Net_Client; use UxAS.Comms.LMCP_Net_Client;
with Ada.Text_IO; use Ada.Text_IO;
with Daidalus_Response;
--  with Ada.Numerics.Big_Numbers.Big_Integers.Big_Integer;
--  use Ada.Numerics.Big_Numbers.Big_Integers.Big_Integer;
package body automatic_response with SPARK_Mode => On is

   ----------------------------
   -- Process_DAIDALUS_Bands --
   ----------------------------
   --  procedure Process_DAIDALUS_Bands
   --    (Mailbox                         : in out Daidalus_Response_Communication;
   --     Current_State                   :     state_parameters;
   --     Divert_State                    :    out state_parameters;
   --     DAIDALUS_Altitude_Bands         :     OrderedIntervalVector;
   --     DAIDALUS_Heading_Bands          :     OrderedIntervalVector;
   --     DAIDALUS_GroundSpeed_Bands      :     OrderedIntervalVector;
   --     Recovery_Altitude_Bands         :     OrderedIntervalVector;
   --     Recovery_Heading_Bands          :     OrderedIntervalVector;
   --     Recovery_GroundSpeed_Bands      :     OrderedIntervalVector;
   --     m_Vehicle_ID                    :     VehicleID_type;
   --     Intruders                       :     Intruder_info_Vector;
   --     DAIDALUS_Altitude_Zones         :     ZoneVector;
   --     DAIDALUS_Heading_Zones          :     ZoneVector;
   --     DAIDALUS_GroundSpeed_Zones      :     ZoneVector;
   --     m_isReady_to_Act                :     Boolean;
   --     m_Action_Time_Thresold_s        :     action_time_sec;
   --     m_Priority_Time_Threshold_s     :     priority_time_sec;
   --     m_Status                        : in out Status_Type;
   --     m_NextWaypoint                  :     Int64;
   --     Altitude_Max_m                  :     Altitude_Type_m;
   --     Altitude_Min_m                  :     Altitude_Type_m;
   --     Altitude_Interval_Buffer_m      :     Altitude_Buffer_Type_m;
   --     Heading_Max_deg                 :     Heading_Type_deg;
   --     Heading_Min_deg                 :     Heading_Type_deg;
   --     Heading_Interval_Buffer_deg     :     Heading_Buffer_Type_deg;
   --     GroundSpeed_Max_mps             :     GroundSpeed_Type_mps;
   --     GroundSpeed_Min_mps             :     GroundSpeed_Type_mps;
   --     GroundSpeed_Interval_Buffer_mps :     GroundSpeed_Buffer_Type_mps;
   --     Is_Tracking_Next_Waypoint       : in out Boolean;
   --     m_MissionCommand                : in out definitions.MissionCommand;
   --     RoW_ghost                       :    out ID_Type;
   --     ConflictResolutionList_ghost    :    out VehicleIDsVector;
   --     SendNewMissionCommand_ghost     :    out Boolean;
   --     Send_Divert_Action_Command_ghost :    out Boolean);
   --
   ----------------------------
   -- Process_DAIDALUS_Bands --
   ----------------------------

   procedure Process_DAIDALUS_Bands
     (Mailbox                         : in out Daidalus_Response_Mailbox;
      Current_State                   :     state_parameters;
      Divert_State                    :    out state_parameters;
      DAIDALUS_Altitude_Bands         :     OrderedIntervalVector32;
      DAIDALUS_Heading_Bands          :     OrderedIntervalVector;
      DAIDALUS_GroundSpeed_Bands      :     OrderedIntervalVector;
      Recovery_Altitude_Bands         :     OrderedIntervalVector32;
      Recovery_Heading_Bands          :     OrderedIntervalVector;
      Recovery_GroundSpeed_Bands      :     OrderedIntervalVector;
      m_Vehicle_ID                    :     VehicleID_type;
      Intruders                       :     Intruder_info_Vector;
      DAIDALUS_Altitude_Zones         :     ZoneVector;
      DAIDALUS_Heading_Zones          :     ZoneVector;
      DAIDALUS_GroundSpeed_Zones      :     ZoneVector;
      m_isReady_to_Act                :     Boolean;
      m_Action_Time_Thresold_s        :     action_time_sec;
      m_Priority_Time_Threshold_s     :     priority_time_sec;
      m_Status                        : aliased in out Status_Type;
      m_NextWaypoint                  :     Int64;
      Altitude_Max_m                  :     Altitude_Type_m;
      Altitude_Min_m                  :     Altitude_Type_m;
      Altitude_Interval_Buffer_m      :     Altitude_Buffer_Type_m;
      Heading_Max_deg                 :     Heading_Type_deg;
      Heading_Min_deg                 :     Heading_Type_deg;
      Heading_Interval_Buffer_deg     :     Heading_Buffer_Type_deg;
      GroundSpeed_Max_mps             :     GroundSpeed_Type_mps;
      GroundSpeed_Min_mps             :     GroundSpeed_Type_mps;
      GroundSpeed_Interval_Buffer_mps :     GroundSpeed_Buffer_Type_mps;
      Is_Tracking_Next_Waypoint       : in out Boolean;
      m_MissionCommand                : in out definitions.MissionCommand;
      RoW_ghost                       :    out ID_Type;
      ConflictResolutionList_ghost    :    out VehicleIDsVector;
      SendNewMissionCommand_ghost     :    out Boolean;
      Send_Divert_Action_Command_ghost :    out Boolean)
   is
      use LMCP_Messages;
      Conflict_Resolution_List : VehicleIDsVector;
      found_acceptable_action_flag : Boolean;
      isSafeToReturnToMissionFlag : Boolean;
      CheckState : state_parameters;
      CutPoint : Integer := MyVectorOfWaypoints.No_Index;
      PriorityStatus : Priority_Type := pStandard;
      RoW : ID_Type;
      m_StatusOld : Status_Type;
      --FlightDirectorAction : VA_Seq;
      --VehicleActionCommand : VehicleActionCommand_w_FlightDirectorAction;
      --  vpimy: integer := 0;
   begin

      --initialize Divert state with the current state
      Divert_State := Current_State;
      --record status on entry
      m_StatusOld := m_Status;
      RoW_ghost := definitions.ID_Type'Last; --initialization
      SendNewMissionCommand_ghost := False; --initialization
      Send_Divert_Action_Command_ghost := False; --initialization

      --Loop over the intruder information and populate a conflict resolution
      --list with those intruders that have a time to violation less than a
      --given threshold. Change priority of resolution based on intruder time to
      --violation against a separate given theshold.
      --  pragma Assert(Integer(MyVectorOfVehicleIDs.Capacity
      --                (Conflict_Resolution_List)) =
      --                  Integer(MyVectorOfIntruderInfo.Capacity(Intruders)));
      for I in MyVectorOfIntruderInfo.First_Index (Intruders) ..
        MyVectorOfIntruderInfo.Last_Index (Intruders) loop
         pragma Loop_Invariant (Integer (MyVectorOfVehicleIDs.Length
                               (Conflict_Resolution_List)) < I);
         if MyVectorOfIntruderInfo.Element (Intruders, I).
                        Intruder_time_to_violation <=
                          m_Action_Time_Thresold_s
         then

            MyVectorOfVehicleIDs.Append (Conflict_Resolution_List,
                                        MyVectorOfIntruderInfo.Element
                                          (Intruders, I).Intruder_ID);
         end if;
         if MyVectorOfIntruderInfo.Element (Intruders, I).
                        Intruder_time_to_violation <=
                          m_Priority_Time_Threshold_s
         then
            PriorityStatus := pHigh;
         end if;

      end loop;
      ConflictResolutionList_ghost := Conflict_Resolution_List;
      --here ends the first subprocedure

      --transition state to InConflict if the conflict resolution list is not
      --empty.  Prepare to set right of way vehicle by finding the intruder with
      --the lowest vehicle ID.
      if not MyVectorOfVehicleIDs.Is_Empty (Conflict_Resolution_List) then
         --initialize right-of-way vehicle with the highest possible ID
         RoW := ID_Type'Last;

         --set state to inconflict
         m_Status := InConflict;
         --  pragma Assert(NeedToResolveConflict(Conflict_Resolution_List));
         pragma Assert (if m_Status = InConflict then
                          NeedToResolveConflict (Conflict_Resolution_List));

         --if the conflict resolution list is not empty then either the altitude
         --, heading, or groundspeed bands has an interval that contains the
         --ownship with a time to violation less than the threshold by nature of
         --DAIDALUS detection and awareness
         if not (Heading_Resolution.Heading_range_restraint
           (Current_State => Current_State,
            Heading_Min_deg => Heading_Min_deg,
            Heading_Max_deg => Heading_Max_deg) and then
           Heading_Resolution.correct_call_sequence
              (Current_State               => Current_State,
               DAIDALUS_Heading_Bands      => DAIDALUS_Heading_Bands,
               Recovery_Heading_bands      => Recovery_Heading_Bands,
               Heading_Max_deg             => Heading_Max_deg,
               Heading_Min_deg             => Heading_Min_deg,
               Heading_Interval_Buffer_deg => Heading_Interval_Buffer_deg)
            and then Altitude_Resolution.correct_call_sequence
              (Current_State            => Current_State,
               DAIDALUS_Altitude_Bands  => DAIDALUS_Altitude_Bands,
               Recovery_Altitude_bands  => Recovery_Altitude_Bands,
               Altitude_Max_m           => Altitude_Max_m,
               Altitude_Min_m           => Altitude_Min_m,
               Altitude_Interval_Buffer => Altitude_Interval_Buffer_m)
            and then speed_resolution.correct_call_sequence
              (Current_State                   => Current_State,
               DAIDALUS_GroundSpeed_Bands      => DAIDALUS_GroundSpeed_Bands,
               Recovery_GroundSpeed_bands      => Recovery_GroundSpeed_Bands,
               GroundSpeed_Max_mps             => GroundSpeed_Max_mps,
               GroundSpeed_Min_mps             => GroundSpeed_Min_mps,
               GroundSpeed_Interval_Buffer_mps => GroundSpeed_Interval_Buffer_mps))
         then
            --  m_Status := definitions.OnHold;
            Put_Line ("Something unexpected occurred. Exiting execution of automatic response.");
            raise Violated_precondition2;
         end if;

         --  pragma Assume (not (MyVectorOfIntervals32.Is_Empty
         --                (DAIDALUS_Altitude_Bands) and MyVectorOfIntervals.
         --                  Is_Empty (DAIDALUS_Heading_Bands) and
         --                  MyVectorOfIntervals.Is_Empty
         --                    (DAIDALUS_GroundSpeed_Bands)));
      else
         --set the right of way vehicle ID to the lowest possible to prevent it
         --from being reassigned.
         RoW := ID_Type'First;
         pragma Assert (not NeedToResolveConflict (Conflict_Resolution_List));
      end if;

      --loop over the intruder ID's setting the right of way to the intruder
      --with the lowest ID
      for I in MyVectorOfVehicleIDs.First_Index (Conflict_Resolution_List) ..
        MyVectorOfVehicleIDs.Last_Index (Conflict_Resolution_List) loop
         if MyVectorOfVehicleIDs.Element (Conflict_Resolution_List, I) < RoW
         then
            RoW := MyVectorOfVehicleIDs.Element (Conflict_Resolution_List, I);
         end if;
         pragma Loop_Invariant (for all J in MyVectorOfVehicleIDs.First_Index
                               (Conflict_Resolution_List) .. I => RoW <=
                                 MyVectorOfVehicleIDs.Element
                                   (Conflict_Resolution_List, J));
      end loop;

      RoW_ghost := RoW;

      --state machine implementation of responses
      case m_Status is
         --On mission, then do nothing
         when OnMission =>
            pragma Assert (Divert_State = Current_State);
            null;
         --when in conflict, do nothing if ownship is right of way, otherwise
         --divert
         when InConflict =>
            --  pragma Assert(m_Status = InConflict);
            pragma Assert (NeedToResolveConflict (Conflict_Resolution_List));
            if m_Vehicle_ID <= RoW
            then
               m_Status := OnHold;
               pragma Assert (PerformedNoActionAsRoWVehicle
                             (Conflict_Resolution_List, RoW, m_Vehicle_ID,
                                Send_Divert_Action_Command_ghost,
                                SendNewMissionCommand_ghost));
            else
               --  pragma Assume (m_Vehicle_ID /= RoW);
               SetDivertState (DAIDALUS_Altitude_Bands, DAIDALUS_Heading_Bands,
                              DAIDALUS_GroundSpeed_Bands,
                              Recovery_Altitude_Bands, Recovery_Heading_Bands,
                              Recovery_GroundSpeed_Bands, Current_State,
                              Divert_State, found_acceptable_action_flag,
                              Altitude_Max_m, Altitude_Min_m,
                              Altitude_Interval_Buffer_m, Heading_Max_deg,
                              Heading_Min_deg, Heading_Interval_Buffer_deg,
                              GroundSpeed_Max_mps, GroundSpeed_Min_mps,
                              GroundSpeed_Interval_Buffer_mps,
                              PriorityStatus);
               Is_Tracking_Next_Waypoint := False;
               Send_Divert_Action_Command_ghost := True;

               -----------------------------------------------------------------
               -- SendDivertCommand(Divert_State, m_Vehicle_ID);--
               -- Initialize local FlightDirectorAction and VehicleActionCommand
               declare
                  use LMCP_Messages;
                  FlightDirectorAction :
                  VehicleAction_Descendant_FlightDirectorAction;
                  VehicleActionCommand :
                  VehicleActionCommand_w_FlightDirectorAction;
               begin
                  --set FlightDirectorAction using the resulting Divert_State---
                  FlightDirectorAction.Altitude_m := Real32 (Divert_State.
                                                               altitude_m);
                  --set altitude type to AGL to match C++ implementation that
                  --differs from documentation of default FlightDirectorAction--
                  FlightDirectorAction.AltitudeType := AGL;
                  FlightDirectorAction.ClimbRate_mps := Real32 (Divert_State.
                    verticalSpeed_mps);
                  FlightDirectorAction.Heading_deg := Real32 (Divert_State.
                                                                heading_deg);
                  FlightDirectorAction.Speed_mps := Real32 (Divert_State.
                                                              groundSpeed_mps);

                  --Set VehicleActionCommand for the diverting vehicle----------
                  VehicleActionCommand.VehicleId := m_Vehicle_ID;
                  Get_Unique_Entity_Send_Message_Id (AVTAS.LMCP.Types.Int64
                                                     (VehicleActionCommand.
                                                          CommandId));
                  VehicleActionCommand.Status := Approved;
                  VehicleActionCommand.VehicleActionList := Add
                    (VehicleActionCommand.VehicleActionList,
                     FlightDirectorAction);
                  sendBroadcastMessage (Mailbox, VehicleActionCommand);

               end;

               -----------------------------------------------------------------
               m_Status := OnHold;
               pragma Assert (Diverted (Conflict_Resolution_List, RoW,
                             m_Vehicle_ID, Send_Divert_Action_Command_ghost,
                             SendNewMissionCommand_ghost));
            end if;

            -- return to mission by sending updated missioncommand if
            -- safe-to-return when previously on hold, otherwise continue mission
            -- if previously on mission-----------------------------------------
         when OnHold =>
            if Is_Tracking_Next_Waypoint then
               m_Status := OnMission;
               pragma Assert (ContinuingLastCommand (Conflict_Resolution_List,
                             Send_Divert_Action_Command_ghost,
                             SendNewMissionCommand_ghost));
            else
               -- handle checking for SafetoReturnToMission----------------------
               SafeToReturn
                 (DAIDALUS_Altitude_Bands, DAIDALUS_Heading_Bands,
                  DAIDALUS_GroundSpeed_Bands,
                  DAIDALUS_Altitude_Zones, DAIDALUS_Heading_Zones,
                  DAIDALUS_GroundSpeed_Zones, Current_State, CheckState,
                  m_NextWaypoint, m_MissionCommand, isSafeToReturnToMissionFlag)
                 ;
               if isSafeToReturnToMissionFlag
               then
                  --send a new mission command containing only the portion of
                  --previous mission command that has not yet be accomplished
                  --Waypoint number of -1 indicates no mission command being
                  --followed.---------------------------------------------------
                  if m_MissionCommand.is_safe_to_access
                  then
                     --Establish a cutpoint on mission command starting at one
                     --before the last waypoint headed to before divert---------
                     for wp in MyVectorOfWaypoints.First_Index
                       (m_MissionCommand.waypoint_list) .. MyVectorOfWaypoints.
                       Last_Index (m_MissionCommand.waypoint_list) loop
                        if MyVectorOfWaypoints.Element
                          (m_MissionCommand.waypoint_list, wp).waypoint_number =
                          m_NextWaypoint
                        then
                           CutPoint := wp;
                           exit;
                        end if;
                     end loop;

                     --  CutPoint := MyVectorOfWaypoints.Find_Index
                     --    (m_MissionCommand.waypoint_list, m_NextWaypoint,
                     --     MyVectorOfWaypoints.
                     --       First_Index (m_MissionCommand.waypoint_list));
                     --remove waypoints up to cutpoint if cutpoint is not the
                     --beginning or end of the waypoint list-------------------
                     if not (CutPoint = MyVectorOfWaypoints.No_Index)
                     then
                        if not (CutPoint = MyVectorOfWaypoints.First_Index
                                (m_MissionCommand.waypoint_list))
                        then
                           MyVectorOfWaypoints.Delete_First (m_MissionCommand.
                                                              waypoint_list,
                                                            Ada.Containers.
                                                     Count_Type (CutPoint - 2));
                        end if;

                        --Alternative cut code----------------------------------
                        --  MyVectorOfWaypoints.Delete(m_MissionCommand.
                        --                               waypoint_list,
                        --                         MyVectorOfWaypoints.First_Index
                        --                         (m_MissionCommand.waypoint_list),
                        --                             Cutpoint-1);
                        --  pragma Assert(MyVectorOfWaypoints.First_Index
                        --                (m_MissionCommand.waypoint_list) in
                        --                  MyVectorofWaypoint

                        --  for I in MyVectorOfWaypoints.First_Index
                        --    (m_MissionCommand.waypoint_list) .. (Cutpoint -2) loop
                        --     MyVectorofWaypoints.Delete(m_MissionCommand.
                        --                                  waypoint_list,
                        --                                MyVectorOfWaypoints.
                        --                                  First_Index
                        --                                    (m_MissionCommand.
                        --                                       waypoint_list));
                        --     pragma Loop_Invariant(MyVectorOfWaypoints.Last_Index
                        --                           (m_MissionCommand.
                        --                                waypoint_list) =
                        --                               MyVectorOfWaypoints.
                        --                                 Last_Index
                        --                             (m_MissionCommand.
                        --                                waypoint_list)'Loop_Entry
                        --                           - I);
                        --  end loop;

                     end if;
                     --Minimum set for waypoint list includes 2 waypoints-------
                     pragma Assume (2 in MyVectorOfWaypoints.First_Index
                                   (m_MissionCommand.waypoint_list) ..
                                     MyVectorOfWaypoints.Last_Index
                                       (m_MissionCommand.waypoint_list));
                     m_MissionCommand.first_waypoint := MyVectorOfWaypoints.
                       Element (m_MissionCommand.waypoint_list, 2).
                       waypoint_number;
                     -----------------------------------------------------------
                     --Transcribe mission command into LMCP.Messages.
                     --MissionCommand-------------------------------------------
                     declare
                        use LMCP_Messages;
                        LMCP_MissionCommand : LMCP_Messages.MissionCommand;
                        waypoint_temp : LMCP_Messages.Waypoint;
                     begin
                        LMCP_MissionCommand.VehicleId := m_MissionCommand.
                          vehicle_id;
                        LMCP_MissionCommand.FirstWaypoint := m_MissionCommand.
                          first_waypoint;
                        case m_MissionCommand.status is
                           when Pending =>
                              LMCP_MissionCommand.Status := Pending;
                           when Approved =>
                              LMCP_MissionCommand.Status := Approved;
                           when InProcess =>
                              LMCP_MissionCommand.Status := InProcess;
                           when Executed =>
                              LMCP_MissionCommand.Status := Executed;
                           when Cancelled =>
                              LMCP_MissionCommand.Status := Cancelled;
                        end case;
                     -- Begin transcription of waypoint list--------------------
                     --  for waypoint of m_MissionCommand.waypoint_list loop
                        for I in MyVectorOfWaypoints.First_Index
                          (m_MissionCommand.waypoint_list) .. MyVectorOfWaypoints.
                          Last_Index (m_MissionCommand.waypoint_list) loop
                           pragma Loop_Invariant (Last (LMCP_MissionCommand.
                                                    WaypointList) <= I);
                           declare
                              waypoint : Waypoint_info := MyVectorOfWaypoints.
                                Element (m_MissionCommand.waypoint_list, I);
                           begin

                              waypoint_temp.Number := Int64
                                (waypoint.waypoint_number);
                              waypoint_temp.NextWaypoint := Int64
                                (waypoint.next_waypoint);
                              waypoint_temp.Speed := Real32 (waypoint.speed);
                              case waypoint.speed_type is
                              when definitions.Airspeed =>
                                 waypoint_temp.SpeedType := Airspeed;
                              when definitions.Groundspeed =>
                                 waypoint_temp.SpeedType := Groundspeed;
                              end case;
                              waypoint_temp.ClimbRate := Real32
                                (waypoint.climb_rate);
                              case waypoint.turn_type is
                              when definitions.TurnShort =>
                                 waypoint_temp.TurnType := TurnShort;
                              when definitions.FlyOver =>
                                 waypoint_temp.TurnType := FlyOver;
                              end case;
                              declare
                                 lmcp_vehicle_action_list : VA_Seq;
                                 vehicle_action_temp : LMCP_Messages.VehicleAction;
                              begin
                                 --  for wp_val of waypoint.vehicle_action_list loop
                                 -- wp_val is MyVectorOfVehicleActions.Element (waypoint.vehicle_action_list, Index)
                                 for Index in MyVectorOfVehicleActions.First_Index
                                   (waypoint.vehicle_action_list) ..
                                   MyVectorOfVehicleActions.Last_Index
                                     (waypoint.vehicle_action_list) loop
                                    pragma Loop_Invariant (Last (lmcp_vehicle_action_list)
                                                           <= Index);
                                    declare
                                       lmcp_associated_task_list : Int64_Seq;
                                    begin
                                       --  pragma Assert
                                       --    (MyVectorOfIntegers.Last_Index
                                       --       (wp_val.AssociatedTaskList) <
                                       --         Positive'Last);
                                       --  pragma Assert (Last
                                       --                 (lmcp_associated_task_list)
                                       --                 <= MyVectorOfIntegers.
                                       --                   Last_Index
                                       --                     (wp_val.AssociatedTaskList));
                                       --  pragma Assert (Last
                                       --                 (lmcp_associated_task_list) =
                                       --                   0);
                                       --  for atl of wp_val.AssociatedTaskList loop
                                       for VectorIndex in MyVectorOfIntegers.First_Index
                                         (MyVectorOfVehicleActions.Element
                                            (waypoint.vehicle_action_list, Index).
                                              AssociatedTaskList)
                                             .. MyVectorOfIntegers.Last_Index
                                               (MyVectorOfVehicleActions.Element
                                                  (waypoint.vehicle_action_list,
                                                   Index).AssociatedTaskList) loop
                                          --  pragma Loop_Invariant (Last
                                          --                           (lmcp_associated_task_list) <=
                                          --                         MyVectorOfIntegers
                                          --                           .Find_Index
                                          --                           (wp_val.AssociatedTaskList,
                                          --                            atl, myvector_index_type'First) - 1);
                                          --  pragma Loop_Invariant (Last
                                          --                         (lmcp_associated_task_list) <=
                                          --                           Integer (MyVectorOfIntegers.Capacity
                                          --                             (wp_val.AssociatedTaskList)));
                                          pragma Loop_Invariant (Last
                                                                 (lmcp_associated_task_list) <=
                                                                   VectorIndex);
                                          --Add for sequences has no postcondition
                                          --thus a loop invariant cannot be used to
                                          --relate the size of the sequence to the
                                          --number of times executed in the loop.
                                          --Instead using an assumption to satisfy
                                          --the precondition on Add----------------
                                          --  pragma Assume (Last
                                          --                 (lmcp_associated_task_list)
                                          --                 < myvector_index_type'Last);
                                          --  lmcp_associated_task_list := Add
                                          --    (lmcp_associated_task_list, Int64
                                          --       (atl));
                                          lmcp_associated_task_list := Add
                                            (lmcp_associated_task_list, MyVectorOfIntegers.
                                               Element
                                                 (MyVectorOfVehicleActions.Element
                                                      (waypoint.vehicle_action_list
                                                       , Index).AssociatedTaskList,
                                                  VectorIndex));
                                       end loop;
                                       vehicle_action_temp.AssociatedTaskList :=
                                         lmcp_associated_task_list;
                                       --Using an assume to relate the size of
                                       --  pragma Assume (Last
                                       --                 (lmcp_vehicle_action_list) <
                                       --                   myvector_index_type'Last);
                                       lmcp_vehicle_action_list := Add
                                         (lmcp_vehicle_action_list,
                                          vehicle_action_temp);
                                    end;
                                 end loop;
                                 waypoint_temp.VehicleActionList :=
                                   lmcp_vehicle_action_list;
                              end;
                              waypoint_temp.ContingencyWaypointA := Int64
                                (waypoint.contingency_waypoint_A);
                              waypoint_temp.ContingencyWaypointB := Int64
                                (waypoint.contingency_waypoint_B);
                              declare
                                 lmcp_associated_task_list : Int64_Seq;
                              begin
                                 --  for wpatl of waypoint.associated_tasks loop
                                 for Index1 in MyVectorOfIntegers.First_Index
                                   (waypoint.associated_tasks) .. MyVectorOfIntegers.
                                   Last_Index (waypoint.associated_tasks) loop
                                    --  pragma Loop_Invariant (Last
                                    --                       (lmcp_associated_task_list)
                                    --                       <= MyVectorOfIntegers.Find_Index
                                    --                         (waypoint.associated_tasks,
                                    --                          wpatl, myvector_index_type'First));
                                    pragma Loop_Invariant (Last
                                                           (lmcp_associated_task_list)
                                                           <= Index1);
                                    lmcp_associated_task_list := Add
                                      (lmcp_associated_task_list, MyVectorOfIntegers.
                                         Element (waypoint.associated_tasks, Index1));
                                 end loop;
                                 waypoint_temp.AssociatedTasks :=
                                   lmcp_associated_task_list;
                              end;
                              waypoint_temp.Latitude := Real64
                                (waypoint.latitude_deg);
                              waypoint_temp.Longitude := Real64
                                (waypoint.longitude_deg);
                              waypoint_temp.Altitude := Real32
                                (waypoint.altitude_m);
                              case waypoint.altitude_type is
                              when definitions.AGL =>
                                 waypoint_temp.AltitudeType := AGL;
                              when definitions.MSL =>
                                 waypoint_temp.AltitudeType := MSL;
                              end case;

                              LMCP_MissionCommand.WaypointList := Add
                                (LMCP_MissionCommand.WaypointList, waypoint_temp);
                           end;

                        end loop;

                        -- End transcription of waypoint list ------------------
                        LMCP_MissionCommand.CommandId := Int64
                          (m_MissionCommand.command_id);
                        declare
                           lmcp_vehicle_action_list : VA_Seq;
                           vehicle_action_temp : LMCP_Messages.VehicleAction;
                        begin
                           -- begin transcription of vehicle_action_list -------
                           --  for val of m_MissionCommand.vehicle_action_list
                           for Index in MyVectorOfVehicleActions.First_Index
                             (m_MissionCommand.vehicle_action_list) ..
                             MyVectorOfVehicleActions.Last_Index
                               (m_MissionCommand.vehicle_action_list) loop
                              pragma Loop_Invariant (Last
                                                     (lmcp_vehicle_action_list)
                                                       <= Index);
                              declare
                                 val : definitions.VehicleAction :=
                                   MyVectorOfVehicleActions.
                                   Element (m_MissionCommand.vehicle_action_list,
                                            Index);
                                 associated_task_list : Int64_Seq;
                              begin
                                 --  for atl of val.AssociatedTaskList loop
                                 for Index2 in MyVectorOfIntegers.First_Index
                                   (val.AssociatedTaskList) .. MyVectorOfIntegers.
                                   Last_Index (val.AssociatedTaskList) loop
                                    pragma Loop_Invariant (Last
                                                           (associated_task_list)
                                                             <= Index2);
                                 --atl is MyVectorOfIntegers.Element (val.AssociatedTaskList, Index2)
                                    associated_task_list := Add
                                      (associated_task_list, Int64
                                         (MyVectorOfIntegers.Element
                                              (val.AssociatedTaskList, Index2)));
                                 end loop;
                                 vehicle_action_temp.AssociatedTaskList :=
                                   associated_task_list;
                              end;
                              lmcp_vehicle_action_list := Add
                                (lmcp_vehicle_action_list,
                                 vehicle_action_temp);
                           end loop;
                           -- end of vehicle_action_list transcription ---------
                           LMCP_MissionCommand.VehicleActionList :=
                             lmcp_vehicle_action_list;
                        end;
                     -- end of LMCP_Messages.MissionCommand transcription-------
                        sendBroadcastMessage (Mailbox, LMCP_MissionCommand);
                        Put_Line ("This is where a MissionCommand should have been sent.");
                     end;

                     -----------------------------------------------------------
                     SendNewMissionCommand_ghost := True;
                     m_Status := OnMission;
                     Is_Tracking_Next_Waypoint := True;
                     pragma Assert (ReturnToMission (Conflict_Resolution_List,
                                   m_StatusOld, m_Status,
                                   Send_Divert_Action_Command_ghost,
                                   SendNewMissionCommand_ghost));
                  end if;
               else
                  pragma Assert (ContinuingLastCommand (Conflict_Resolution_List,
                                Send_Divert_Action_Command_ghost,
                                SendNewMissionCommand_ghost));
               end if;

            end if;
            pragma Assert (Divert_State = Current_State);

      end case;

      pragma Assert (m_Status /= InConflict);
      pragma Assert (if not NeedToResolveConflict (ConflictResolutionList_ghost)
                     then
                (Divert_State = Current_State and then (ContinuingLastCommand
                (ConflictResolutionList_ghost, Send_Divert_Action_Command_ghost,
                      SendNewMissionCommand_ghost) or ReturnToMission
                   (ConflictResolutionList_ghost, m_StatusOld, m_Status,
                    Send_Divert_Action_Command_ghost,
                    SendNewMissionCommand_ghost))) else
                  (PerformedNoActionAsRoWVehicle (ConflictResolutionList_ghost,
                   RoW_ghost, m_Vehicle_ID, Send_Divert_Action_Command_ghost,
                  SendNewMissionCommand_ghost) xor (
                  Diverted (ConflictResolutionList_ghost, RoW_ghost,
                m_Vehicle_ID, Send_Divert_Action_Command_ghost,
                            SendNewMissionCommand_ghost))));
   exception
   when Violated_precondition2 =>
      Put_Line ("Problem satisfying constraints on preconditions for processing DAIDALUS bands.");

   end Process_DAIDALUS_Bands;

end automatic_response;
