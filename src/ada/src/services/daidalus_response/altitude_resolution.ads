--proves in 44790 steps with GNAT Pro 24.0w SPARK Pro 24.0w GnatStudio 24.0w 
with Common; use Common;
with definitions;
use definitions;
package Altitude_Resolution with SPARK_Mode => On is
   
   pragma Assertion_Policy (Check);
   
   --represents scalar constraints on upper bound, lower bound, range between 
   --upper and lower bound. Additional constraints for range protectcion.
   function scalar_constraints (Upper_limit : Altitude_Type_m; 
                               Lower_limit : Altitude_Type_m; 
                               Interval_Constraint : Altitude_Buffer_Type_m) 
                               return Boolean is 
     (Upper_limit > Lower_limit and then Interval_Constraint > 1.0 and then 
      Lower_limit >= 0.0 and then Upper_limit <= 
      (Altitude_Type_m'Last - Altitude_Buffer_Type_m'Last) and then
      Lower_limit >= (Altitude_Type_m'First + Altitude_Buffer_Type_m'Last) 
      and then Upper_limit - Lower_limit >= 2.0 * Interval_Constraint); 
   
   --represents relational constraints between memebers in an interval band, 
   --assuming the container is not empty additional contraints for range 
   --protection
   function vector_constraints (X : OrderedIntervalVector32;
                               Upper_limit : Altitude_Type_m; 
                               Lower_limit : Altitude_Type_m; 
                               Interval_constraint : Altitude_Buffer_Type_m) 
                               return Boolean is 
     (if not MyVectorOfIntervals32.Is_Empty (X) then 
       ((for all I in MyVectorOfIntervals32.First_Index (X) .. 
         MyVectorOfIntervals32.Last_Index (X) =>
        (MyVectorOfIntervals32.Element (X, I).LowerBound >= Lower_limit) and then 
        (MyVectorOfIntervals32.Element (X, I).UpperBound <= Upper_limit) and then
        (MyVectorOfIntervals32.Element (X, I).UpperBound + 
                 Altitude_Buffer_Type_m'Last <= Altitude_Type_m'Last) and then 
        (MyVectorOfIntervals32.Element (X, I).LowerBound - 
                Altitude_Buffer_Type_m'Last >= Altitude_Type_m'First) and then
        (MyVectorOfIntervals32.Element (X, I).UpperBound - 
           MyVectorOfIntervals32.Element (X, I).LowerBound
                  >= 2.0 * Interval_constraint) and then
        (MyVectorOfIntervals32.Element (X, I).UpperBound >= 
           MyVectorOfIntervals32.Element (X, I).LowerBound +
                 2.0 * Interval_constraint) and then 
        (MyVectorOfIntervals32.Element (X, I).LowerBound <= 
           MyVectorOfIntervals32.Element (X, I).UpperBound -
                  2.0 * Interval_constraint))));
   
   --predicate indicating if the current altitude is contained within one of 
   --the DAIDALUS_Altitude bands indicating projected loss of well clear
   function Current_Altitude_Exists_in_Bands
     (Current_State : state_parameters; 
      DAIDALUS_Altitude_Bands : OrderedIntervalVector32) return Boolean is 
     (for some I in MyVectorOfIntervals32.First_Index (DAIDALUS_Altitude_Bands) 
          .. MyVectorOfIntervals32.Last_Index (DAIDALUS_Altitude_Bands) =>
           InRange (MyVectorOfIntervals32.Element (DAIDALUS_Altitude_Bands, I), 
        Current_State.altitude_m));

   --predicate indicating if the constraints on the confict and recover bands
   --are such that the subprogram to find an altitude resolution can be called 
   --without producing a runtime error.
   function correct_call_sequence
     (Current_State : state_parameters; 
      DAIDALUS_Altitude_Bands : OrderedIntervalVector32; 
      Recovery_Altitude_bands : OrderedIntervalVector32; 
      Altitude_Max_m : Altitude_Type_m;
      Altitude_Min_m : Altitude_Type_m; 
      Altitude_Interval_Buffer : Altitude_Buffer_Type_m) return Boolean is
      (scalar_constraints (Upper_limit         => Altitude_Max_m,
                         Lower_limit         => Altitude_Min_m,
                          Interval_Constraint => Altitude_Interval_Buffer) 
      and then
        vector_constraints (X                   => DAIDALUS_Altitude_Bands,
                           Upper_limit         => Altitude_Max_m,
                           Lower_limit         => Altitude_Min_m,
                           Interval_constraint => Altitude_Interval_Buffer) 
      and then 
        vector_constraints (X                   => Recovery_Altitude_bands,
                           Upper_limit         => Altitude_Max_m,
                           Lower_limit         => Altitude_Min_m,
                           Interval_constraint => Altitude_Interval_Buffer) 
      and then (MyVectorOfIntervals32.Is_Empty (DAIDALUS_Altitude_Bands) or else 
      Current_Altitude_Exists_in_Bands (Current_State, DAIDALUS_Altitude_Bands)));
        
   procedure Found_WCV_Altitude_Resolution 
     (DAIDALUS_Altitude_Bands : OrderedIntervalVector32; 
      Recovery_Altitude_Bands : OrderedIntervalVector32;
      Current_State : state_parameters; 
      Altitude_Max_m : Altitude_Type_m;
      Altitude_Min_m : Altitude_Type_m;
      Altitude_Interval_Buffer_m : Altitude_Buffer_Type_m;
      Divert_State : out state_parameters; 
      found_acceptable_action_flag : out Boolean)
     with    
       Pre => (Are_Legitimate_Bands (DAIDALUS_Altitude_Bands) and then 
               Are_Legitimate_Bands (Recovery_Altitude_Bands) and then 
               correct_call_sequence (Current_State, DAIDALUS_Altitude_Bands,
                 Recovery_Altitude_Bands, Altitude_Max_m, Altitude_Min_m, 
                 Altitude_Interval_Buffer_m)),
         
       Post => (Found_Acceptable_Action (found_acceptable_action_flag, 
                DAIDALUS_Altitude_Bands,
                Recovery_Altitude_Bands, Divert_State.altitude_m) or else
                (IsImproperlyConfigured (found_acceptable_action_flag, 
                   DAIDALUS_Altitude_Bands, Divert_State.altitude_m, 
                   Current_State.altitude_m)) or else
                    (Revert_behavior (found_acceptable_action_flag, 
                     DAIDALUS_Altitude_Bands, Altitude_Max_m, 
                     Divert_State.altitude_m)));

end Altitude_Resolution;
