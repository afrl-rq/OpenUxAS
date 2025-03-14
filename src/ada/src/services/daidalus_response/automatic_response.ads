with Common; use Common;
with Daidalus_Response_Mailboxes; use Daidalus_Response_Mailboxes;
with set_divert_state; use set_divert_state;
with Heading_Resolution; use Heading_Resolution;
with Altitude_Resolution; use Altitude_Resolution;
with speed_resolution; use speed_resolution;
with definitions; use definitions;
with CheckSafeToReturn; use CheckSafeToReturn;
with Daidalus_Response; use Daidalus_Response;
package automatic_response with SPARK_Mode => On is

   Violated_precondition2 : exception;

   function NeedToResolveConflict (Resolution_list : VehicleIDsVector) return
     Boolean is
       (not (MyVectorOfVehicleIDs.
              Is_Empty (Resolution_list)))
         with Ghost;

   function PerformedNoActionAsRoWVehicle
     (ConflictResolutionList : VehicleIDsVector;
      RoW_ID : ID_Type;
      my_ID : VehicleID_type;
      SentDivertCommand, SentMissionCommand
      : Boolean) return Boolean is
     (NeedToResolveConflict (ConflictResolutionList) and then (my_ID <= RoW_ID)
       and then (not SentDivertCommand) and then
       (not SentMissionCommand)) with Ghost;

   function Diverted (ConflictResolutionList : VehicleIDsVector;
                     RoW_ID : ID_Type; my_ID : VehicleID_type;
                     SentDivertCommand, SentMissionCommand : Boolean) return
     Boolean is
       (NeedToResolveConflict (ConflictResolutionList) and then (my_ID > RoW_ID)
         and then (SentDivertCommand) and then (not SentMissionCommand))
         with Ghost;

   function ReturnToMission (ConflictResolutionList : VehicleIDsVector;
                            EntryState, ExitState : Status_Type;
                            SentDivertCommand, SentMissionCommand : Boolean)
                            return Boolean is
     (not NeedToResolveConflict (ConflictResolutionList) and then
          (EntryState = OnHold) and then (not SentDivertCommand) and then
     (SentMissionCommand) and then (ExitState = OnMission)) with Ghost;

   function ContinuingLastCommand (ConflictResolutionList : VehicleIDsVector;
                                  SentDivertCommand,
                                  SentMissionCommand : Boolean) return Boolean
   is
     (not NeedToResolveConflict (ConflictResolutionList) and then
        not SentDivertCommand and then not SentMissionCommand) with Ghost;

   procedure Process_DAIDALUS_Bands
     (Mailbox : in out Daidalus_Response_Mailbox;
      Current_State : state_parameters;
      Divert_State : out state_parameters;
      DAIDALUS_Altitude_Bands : OrderedIntervalVector32;
      DAIDALUS_Heading_Bands : OrderedIntervalVector;
      DAIDALUS_GroundSpeed_Bands : OrderedIntervalVector;
      Recovery_Altitude_Bands : OrderedIntervalVector32;
      Recovery_Heading_Bands : OrderedIntervalVector;
      Recovery_GroundSpeed_Bands : OrderedIntervalVector;
      m_Vehicle_ID : VehicleID_type;
      Intruders : Intruder_info_Vector;
      DAIDALUS_Altitude_Zones : ZoneVector;
      DAIDALUS_Heading_Zones : ZoneVector;
      DAIDALUS_GroundSpeed_Zones : ZoneVector;
      m_isReady_to_Act : Boolean;
      m_Action_Time_Thresold_s : action_time_sec;
      m_Priority_Time_Threshold_s : priority_time_sec;
      m_Status : aliased in out Status_Type;
      m_NextWaypoint : Int64;
      Altitude_Max_m : Altitude_Type_m;
      Altitude_Min_m : Altitude_Type_m;
      Altitude_Interval_Buffer_m : Altitude_Buffer_Type_m;
      Heading_Max_deg : Heading_Type_deg;
      Heading_Min_deg : Heading_Type_deg;
      Heading_Interval_Buffer_deg : Heading_Buffer_Type_deg;
      GroundSpeed_Max_mps : GroundSpeed_Type_mps;
      GroundSpeed_Min_mps : GroundSpeed_Type_mps;
      GroundSpeed_Interval_Buffer_mps : GroundSpeed_Buffer_Type_mps;
      Is_Tracking_Next_Waypoint : in out Boolean;
      m_MissionCommand : in out MissionCommand;
      RoW_ghost : out ID_Type;
      ConflictResolutionList_ghost : out VehicleIDsVector;
      SendNewMissionCommand_ghost : out Boolean;
      Send_Divert_Action_Command_ghost : out Boolean) with
      Pre => m_isReady_to_Act and then m_Status /= InConflict and then
             m_Priority_Time_Threshold_s < m_Action_Time_Thresold_s and then
             Are_Legitimate_Bands (DAIDALUS_Altitude_Bands) and then
     Are_Legitimate_Bands (DAIDALUS_Heading_Bands) and then
     Are_Legitimate_Bands (DAIDALUS_GroundSpeed_Bands) and then
     Are_Legitimate_Bands (Recovery_Altitude_Bands) and then
     Are_Legitimate_Bands (Recovery_Heading_Bands) and then
     Are_Legitimate_Bands (Recovery_GroundSpeed_Bands) and then
     --  Heading_Resolution.Heading_range_restraint (Current_State, Heading_Min_deg,
     --                                             Heading_Max_deg) and then
     --  Heading_Resolution.correct_call_sequence (Current_State,
     --                                                   DAIDALUS_Heading_Bands,
     --                                                   Recovery_Heading_Bands,
     --                                                   Heading_Max_deg,
     --                                                   Heading_Min_deg,
     --                                           Heading_Interval_Buffer_deg)
     --  and then Altitude_Resolution.correct_call_sequence (Current_State,
     --                                                     DAIDALUS_Altitude_Bands,
     --                                                     Recovery_Altitude_Bands,
     --                                                     Altitude_Max_m,
     --                                                     Altitude_Min_m,
     --                                                     Altitude_Interval_Buffer_m)
     --  and then speed_resolution.correct_call_sequence (Current_State,
     --                                                  DAIDALUS_GroundSpeed_Bands,
     --                                                  Recovery_GroundSpeed_Bands,
     --                                                  GroundSpeed_Max_mps,
     --                                                  GroundSpeed_Min_mps,
     --                                                  GroundSpeed_Interval_Buffer_mps)
     CheckSafeToReturn.SameIndices (DAIDALUS_Altitude_Bands,
                                            DAIDALUS_Altitude_Zones)
     and then CheckSafeToReturn.SameIndices (DAIDALUS_Heading_Bands,
                                            DAIDALUS_Heading_Zones)
     and then CheckSafeToReturn.SameIndices (DAIDALUS_GroundSpeed_Bands,
                                            DAIDALUS_GroundSpeed_Zones)
   , -- and then
     --  not MyVectorOfIntruderInfo.Is_Empty(Intruders),
     --  Exceptional_Cases => (Violated_precondition2 => m_Status = InConflict),
     Post => m_Status = InConflict xor (m_Status /= InConflict and then
             (if not NeedToResolveConflict (ConflictResolutionList_ghost) then
                (Divert_State = Current_State and then (ContinuingLastCommand
                 (ConflictResolutionList_ghost, Send_Divert_Action_Command_ghost,
                      SendNewMissionCommand_ghost) or ReturnToMission
                    (ConflictResolutionList_ghost, m_Status'Old, m_Status,
                    Send_Divert_Action_Command_ghost,
                    SendNewMissionCommand_ghost))) else
                  (PerformedNoActionAsRoWVehicle (ConflictResolutionList_ghost,
                   RoW_ghost, m_Vehicle_ID, Send_Divert_Action_Command_ghost,
                  SendNewMissionCommand_ghost) xor (
                  Diverted (ConflictResolutionList_ghost, RoW_ghost,
                m_Vehicle_ID, Send_Divert_Action_Command_ghost,
                            SendNewMissionCommand_ghost)))));

end automatic_response;
