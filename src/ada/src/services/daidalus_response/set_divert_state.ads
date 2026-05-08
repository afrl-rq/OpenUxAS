with Heading_Resolution; use Heading_Resolution;
with Altitude_Resolution; use Altitude_Resolution;
with speed_resolution; use speed_resolution;
with definitions; use definitions;
package set_divert_state with SPARK_Mode => On is

   --predicate indicating status of setting an acceptable divert altitude
   function Divert_Altitude_Successful (found_acceptable_action_flag : Boolean; 
                                        DAIDALUS_Altitude_Bands : 
                                        OrderedIntervalVector32;
                                        Recovery_Altitude_Bands : 
                                        OrderedIntervalVector32;
                                Divert_State : state_parameters) return Boolean 
   is 
     (Found_Acceptable_Action
                    (found_acceptable_action_flag, DAIDALUS_Altitude_Bands,
         Recovery_Altitude_Bands, Divert_State.altitude_m)) 
     with Ghost;
   
   --predicate indicating status of setting an acceptable divert heading
   function Divert_Heading_Successful (found_acceptable_action_flag : Boolean; 
                                 DAIDALUS_Heading_Bands : OrderedIntervalVector;
                                 Recovery_Heading_Bands : OrderedIntervalVector;
                                 Divert_State : state_parameters) return Boolean
   is
     (Found_Acceptable_Action (found_acceptable_action_flag, 
      DAIDALUS_Heading_Bands, Recovery_Heading_Bands, Divert_State.heading_deg))
     with Ghost;
   
   --predicate indicating status os setting an acceptable divert ground speed
   function Divert_GroundSpeed_Successful (found_acceptable_action_flag : Boolean; 
                             DAIDALUS_GroundSpeed_Bands : OrderedIntervalVector;
                             Recovery_GroundSpeed_Bands : OrderedIntervalVector;
                                          Divert_State : state_parameters) return Boolean is
     (Found_Acceptable_Action (found_acceptable_action_flag, 
      DAIDALUS_GroundSpeed_Bands, Recovery_GroundSpeed_Bands, 
      Divert_State.groundSpeed_mps)) with Ghost;
   
   function Divert_Fallback (found_acceptable_action_flag : Boolean; 
                            DAIDALUS_GroundSpeed_Bands : OrderedIntervalVector;
                            DAIDALUS_Altitude_Bands : OrderedIntervalVector32;
                            Divert_State : state_parameters; 
                            Fallback_speed : GroundSpeed_Type_mps;
                            Fallback_altitude : Altitude_Type_m;
                            Priority : Priority_Type) 
                            return Boolean is
     (case Priority is
         when pStandard =>
            Revert_behavior (found_acceptable_action_flag, 
                            DAIDALUS_GroundSpeed_Bands, Fallback_speed, 
                            Divert_State.groundSpeed_mps),
         when pHigh =>
            Revert_behavior (found_acceptable_action_flag, 
                            DAIDALUS_Altitude_Bands, Fallback_altitude, 
                            Divert_State.altitude_m)) with Ghost;
   
   function Divert_No_Recourse (found_acceptable_action_flag : Boolean;
                               DAIDALUS_GroundSpeed_Bands : OrderedIntervalVector;
                               DAIDALUS_Altitude_Bands : OrderedIntervalVector32;
                               Divert_State : state_parameters;
                               Current_State : state_parameters;
                               Priority : Priority_Type) return Boolean is
     ((case Priority is 
         when pStandard => IsImproperlyConfigured (found_acceptable_action_flag, 
        DAIDALUS_GroundSpeed_Bands, Divert_State.groundSpeed_mps, Current_State.
          groundSpeed_mps),
         when pHigh => IsImproperlyConfigured (found_acceptable_action_flag, 
        DAIDALUS_Altitude_Bands, Divert_State.altitude_m, Current_State.
          altitude_m))) with Ghost;
     
   procedure SetDivertState
     (DAIDALUS_Altitude_Bands : OrderedIntervalVector32;
      DAIDALUS_Heading_Bands : OrderedIntervalVector;
      DAIDALUS_GroundSpeed_Bands : OrderedIntervalVector;
      Recovery_Altitude_Bands : OrderedIntervalVector32;
      Recovery_Heading_Bands : OrderedIntervalVector;
      Recovery_GroundSpeed_Bands : OrderedIntervalVector;
      Current_State : state_parameters;
      Divert_State : out state_parameters;
      found_acceptable_action_flag : out Boolean;
      Altitude_Max_m : Altitude_Type_m;
      Altitude_Min_m : Altitude_Type_m;
      Altitude_Interval_Buffer_m : Altitude_Buffer_Type_m;
      Heading_Max_deg : Heading_Type_deg;
      Heading_Min_deg : Heading_Type_deg;
      Heading_Interval_Buffer_deg : Heading_Buffer_Type_deg;
      GroundSpeed_Max_mps : GroundSpeed_Type_mps;
      GroundSpeed_Min_mps : GroundSpeed_Type_mps;
      GroundSpeed_Interval_Buffer_mps : GroundSpeed_Buffer_Type_mps;
      Priority : Priority_Type) with
     Pre => Are_Legitimate_Bands (DAIDALUS_Altitude_Bands) and then 
            Are_Legitimate_Bands (DAIDALUS_Heading_Bands) and then
            Are_Legitimate_Bands (DAIDALUS_GroundSpeed_Bands) and then 
            Are_Legitimate_Bands (Recovery_Altitude_Bands) and then 
            Are_Legitimate_Bands (Recovery_Heading_Bands) and then 
            Are_Legitimate_Bands (Recovery_GroundSpeed_Bands) and then 
           Altitude_Resolution.correct_call_sequence (Current_State, 
                                                     DAIDALUS_Altitude_Bands, 
                                                     Recovery_Altitude_Bands, 
                                                     Altitude_Max_m, 
                                                     Altitude_Min_m, 
                                                     Altitude_Interval_Buffer_m)
            and then Heading_Resolution.correct_call_sequence (Current_State, 
                                                      DAIDALUS_Heading_Bands, 
                                                      Recovery_Heading_Bands, 
                                                      Heading_Max_deg, 
                                                      Heading_Min_deg, 
                                                      Heading_Interval_Buffer_deg)
            and then speed_resolution.correct_call_sequence (Current_State, 
                                                     DAIDALUS_GroundSpeed_Bands,
                                                     Recovery_GroundSpeed_Bands,
                                                     GroundSpeed_Max_mps, 
                                                     GroundSpeed_Min_mps, 
                                                            GroundSpeed_Interval_Buffer_mps)
            and then Heading_Resolution.Heading_range_restraint (Current_State,
                                                               Heading_Min_deg,
                                                               Heading_Max_deg),
     Post => Divert_Altitude_Successful (found_acceptable_action_flag,
                                        DAIDALUS_Altitude_Bands,
                                        Recovery_Altitude_Bands,
                                        Divert_State) or else
             Divert_Heading_Successful (found_acceptable_action_flag,
                                       DAIDALUS_Heading_Bands,
                                       Recovery_Heading_Bands,
                                       Divert_State) or else
             Divert_GroundSpeed_Successful (found_acceptable_action_flag,
                                           DAIDALUS_GroundSpeed_Bands,
                                           Recovery_GroundSpeed_Bands,
                                           Divert_State) or else
             Divert_Fallback (found_acceptable_action_flag,
                            DAIDALUS_GroundSpeed_Bands, DAIDALUS_Altitude_Bands, 
                            Divert_State, GroundSpeed_Min_mps, Altitude_Max_m, 
                            Priority) or else
             Divert_No_Recourse (found_acceptable_action_flag, 
                          DAIDALUS_GroundSpeed_Bands,
                          DAIDALUS_Altitude_Bands, Divert_State, Current_State,
                        Priority);
end set_divert_state;
