--proves in 63784 steps with GNAT Pro 24.0w SPARK Pro 24.0w GnatStudio 24.0w
with Common; use Common;
with definitions;   use definitions;
package speed_resolution with SPARK_Mode => On is 

   --represents scalar constraints on upper bound, lower bound, range between 
   --upper and lower bound. Additional constraints for range protectcion.
   function scalar_constraints 
     (Upper_limit : GroundSpeed_Type_mps; 
      Lower_limit : GroundSpeed_Type_mps; 
      Interval_Constraint : GroundSpeed_Buffer_Type_mps) return Boolean is 
     (Upper_limit > Lower_limit and then Interval_Constraint > 1.0 and then 
      Lower_limit >= 0.0 and then 
      Upper_limit <= (GroundSpeed_Type_mps'Last - 
            GroundSpeed_Buffer_Type_mps'Last) and then
      Lower_limit >= (GroundSpeed_Type_mps'First + 
            GroundSpeed_Buffer_Type_mps'Last) and then 
      Upper_limit - Lower_limit >= 2.0 * Interval_Constraint);
   
   --represents relational constraints between memeber in an interval band, 
   --assuming the container is not empty additional contraints for range 
   --protection
   function vector_constraints 
     (X : OrderedIntervalVector; 
      Upper_limit : GroundSpeed_Type_mps; 
      Lower_limit : GroundSpeed_Type_mps; 
      Interval_constraint : GroundSpeed_Buffer_Type_mps) return Boolean is 
     (if not MyVectorOfIntervals.Is_Empty (X) then 
          ((for all I in MyVectorOfIntervals.First_Index (X) .. 
               MyVectorOfIntervals.Last_Index (X) =>
        (MyVectorOfIntervals.Element (X, I).LowerBound >= Lower_limit) and then 
        (MyVectorOfIntervals.Element (X, I).UpperBound <= Upper_limit) and then
               (MyVectorOfIntervals.Element (X, I).UpperBound + 
                      GroundSpeed_Buffer_Type_mps'Last <= 
                        GroundSpeed_Type_mps'Last) and then 
             (MyVectorOfIntervals.Element (X, I).LowerBound - 
                  GroundSpeed_Buffer_Type_mps'Last >= 
                    GroundSpeed_Type_mps'First) and then
             (MyVectorOfIntervals.Element (X, I).UpperBound - 
                  MyVectorOfIntervals.Element (X, I).LowerBound
                  >= 2.0 * Interval_constraint) and then
             (MyVectorOfIntervals.Element (X, I).UpperBound >= 
                  MyVectorOfIntervals.Element (X, I).LowerBound +
                2.0 * Interval_constraint) and then 
             (MyVectorOfIntervals.Element (X, I).LowerBound <= MyVectorOfIntervals.
                  Element (X, I).UpperBound - 2.0 * Interval_constraint))));
   
   --predicate indicating if the current altitude is contained within one of 
   --the DAIDALUS_GroundSpeed bands indicating projected loss of well clear
   function Current_GroundSpeed_Exists_in_Bands
     (Current_State : state_parameters; 
      DAIDALUS_GroundSpeed_Bands : OrderedIntervalVector) return Boolean is 
     (for some I in MyVectorOfIntervals.First_Index (DAIDALUS_GroundSpeed_Bands) 
      .. MyVectorOfIntervals.Last_Index (DAIDALUS_GroundSpeed_Bands) 
      => InRange (MyVectorOfIntervals.Element 
        (DAIDALUS_GroundSpeed_Bands, I), Current_State.groundSpeed_mps)); 
   
   --predicate indicating if the constraints on the confict and recover bands 
   --are such that the subprogram to find an altitude resolution can be called 
   --without producing a runtime error.
   function correct_call_sequence 
     (Current_State : state_parameters; 
      DAIDALUS_GroundSpeed_Bands : OrderedIntervalVector; 
      Recovery_GroundSpeed_bands : OrderedIntervalVector; 
      GroundSpeed_Max_mps : GroundSpeed_Type_mps; 
      GroundSpeed_Min_mps : GroundSpeed_Type_mps; 
      GroundSpeed_Interval_Buffer_mps : GroundSpeed_Buffer_Type_mps) return 
     Boolean is
      (scalar_constraints (Upper_limit         => GroundSpeed_Max_mps,
                         Lower_limit         => GroundSpeed_Min_mps,
                         Interval_Constraint => GroundSpeed_Interval_Buffer_mps)
       and then
        vector_constraints (X                   => DAIDALUS_GroundSpeed_Bands,
                         Upper_limit         => GroundSpeed_Max_mps,
                         Lower_limit         => GroundSpeed_Min_mps,
                         Interval_constraint => GroundSpeed_Interval_Buffer_mps)
       and then 
        vector_constraints (X                   => Recovery_GroundSpeed_bands,
                         Upper_limit         => GroundSpeed_Max_mps,
                         Lower_limit         => GroundSpeed_Min_mps,
                         Interval_constraint => GroundSpeed_Interval_Buffer_mps)
       and then
         (MyVectorOfIntervals.Is_Empty (DAIDALUS_GroundSpeed_Bands) or else 
          Current_GroundSpeed_Exists_in_Bands (Current_State,
            DAIDALUS_GroundSpeed_Bands)));
      
   procedure Found_WCV_GroundSpeed_Resolution
     (DAIDALUS_GroundSpeed_Bands : OrderedIntervalVector; 
      Recovery_GroundSpeed_bands : OrderedIntervalVector;
      Current_State : state_parameters; 
      GroundSpeed_Max_mps : GroundSpeed_Type_mps;
      GroundSpeed_Min_mps : GroundSpeed_Type_mps;
      GroundSpeed_Interval_Buffer_mps : GroundSpeed_Buffer_Type_mps;
      Divert_State : out state_parameters; 
      found_acceptable_action_flag : out Boolean)
     with    
       Pre => (Are_Legitimate_Bands (DAIDALUS_GroundSpeed_Bands) and then 
               Are_Legitimate_Bands (Recovery_GroundSpeed_bands) and then 
               correct_call_sequence (Current_State, DAIDALUS_GroundSpeed_Bands, 
                 Recovery_GroundSpeed_bands, GroundSpeed_Max_mps, 
                 GroundSpeed_Min_mps, GroundSpeed_Interval_Buffer_mps)),
   
       Post => (Found_Acceptable_Action (found_acceptable_action_flag, 
                  DAIDALUS_GroundSpeed_Bands, Recovery_GroundSpeed_bands, 
                  Divert_State.groundSpeed_mps) or else 
                IsImproperlyConfigured (found_acceptable_action_flag, 
                  DAIDALUS_GroundSpeed_Bands, Divert_State.groundSpeed_mps, 
                  Current_State.groundSpeed_mps) or else 
                    Revert_behavior (found_acceptable_action_flag, 
                  DAIDALUS_GroundSpeed_Bands, GroundSpeed_Min_mps, 
                  Divert_State.groundSpeed_mps));

end speed_resolution;
