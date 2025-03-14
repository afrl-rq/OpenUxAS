--proves in 117884 steps with GNAT Pro 24.0w SPARK Pro 24.0w 
--GnatStudio 24.0w, steps=179178 after solving each subprogram separately.
with Common; use Common;
with definitions;
use definitions;
package Heading_Resolution with SPARK_Mode => On
is
   --represents scalar constraints on upper bound, lower bound, the range 
   --between bounds.  Additional constraints for range protection.
   function scalar_constraints (Upper_limit : Heading_Type_deg; 
                               Lower_limit : Heading_Type_deg; 
                               Interval_Constraint : Heading_Buffer_Type_deg)
                               return Boolean is 
     (Upper_limit = 360.0 and then Interval_Constraint > 0.1 and then 
      Lower_limit = 0.0 and then 
      Upper_limit <= (Heading_Type_deg'Last - Heading_Buffer_Type_deg'Last) 
      and then 
      Lower_limit >= (Heading_Type_deg'First + Heading_Buffer_Type_deg'Last) 
      and then
        Upper_limit - Lower_limit >= 2.0 * Interval_Constraint);
   
   --represents relational constraints between memebers in an interval band, 
   --assuming intervals are not empty. Additional constraints for range
   --protection
   function vector_constraints (X : OrderedIntervalVector; 
                               Upper_limit : Heading_Type_deg; 
                               Lower_limit : Heading_Type_deg; 
                               Interval_constraint : Heading_Buffer_Type_deg)
                               return Boolean is 
     (if not MyVectorOfIntervals.Is_Empty (X) then ((for all I in 
      MyVectorOfIntervals.First_Index (X) .. MyVectorOfIntervals.Last_Index (X) =>
          (MyVectorOfIntervals.Element (X, I).LowerBound >= Lower_limit) and then 
        (MyVectorOfIntervals.Element (X, I).UpperBound <= Upper_limit) and then
          (MyVectorOfIntervals.Element (X, I).LowerBound - 
              Heading_Buffer_Type_deg'Last >= Heading_Type_deg'First) and then 
        (MyVectorOfIntervals.Element (X, I).UpperBound + 
             Heading_Buffer_Type_deg'Last <= Heading_Type_deg'Last) and then
        (MyVectorOfIntervals.Element (X, I).UpperBound - 
             MyVectorOfIntervals.Element (X, I).LowerBound
           >= 2.0 * Interval_constraint) and then
        (MyVectorOfIntervals.Element (X, I).UpperBound >= 
             MyVectorOfIntervals.Element (X, I).LowerBound +
             2.0 * Interval_constraint) and then 
        (MyVectorOfIntervals.Element (X, I).LowerBound <= 
             MyVectorOfIntervals.Element (X, I).UpperBound -
             2.0 * Interval_constraint))));
   
   --predicate indicating if the current heading exists amongst interval bands 
   --leading to an impending loss of well clear
   function Current_Heading_Exists_in_Bands
      (Current_State : state_parameters;
      DAIDALUS_Heading_Bands : OrderedIntervalVector)
      return Boolean is (for some I in MyVectorOfIntervals.First_Index
                         (DAIDALUS_Heading_Bands) .. MyVectorOfIntervals.
                           Last_Index (DAIDALUS_Heading_Bands) =>
                            InRange (MyVectorOfIntervals.Element
                            (DAIDALUS_Heading_Bands, I), Current_State.
                             heading_deg));
   
   --predicate indicating whether the constraints on conflict and recovery 
   --intervals are sufficiently enforced to prevent runtime errors
   function correct_call_sequence
              (Current_State : state_parameters; 
              DAIDALUS_Heading_Bands : OrderedIntervalVector; 
              Recovery_Heading_bands : OrderedIntervalVector; 
              Heading_Max_deg : Heading_Type_deg; 
              Heading_Min_deg : Heading_Type_deg; 
              Heading_Interval_Buffer_deg : Heading_Buffer_Type_deg) return 
     Boolean is
       (scalar_constraints
          (Upper_limit         => Heading_Max_deg,
           Lower_limit         => Heading_Min_deg,
           Interval_Constraint => Heading_Interval_Buffer_deg) and then
        vector_constraints
          (X                   => DAIDALUS_Heading_Bands,
           Upper_limit         => Heading_Max_deg,
           Lower_limit         => Heading_Min_deg,
           Interval_constraint => Heading_Interval_Buffer_deg) and then
        vector_constraints
          (X                   => Recovery_Heading_bands,
           Upper_limit         => Heading_Max_deg,
           Lower_limit         => Heading_Min_deg,
           Interval_constraint => Heading_Interval_Buffer_deg) and then 
     (MyVectorOfIntervals.Is_Empty (DAIDALUS_Heading_Bands) or else
           Current_Heading_Exists_in_Bands
             (Current_State          => Current_State,
              DAIDALUS_Heading_Bands => DAIDALUS_Heading_Bands)));
   
   --function to return the angle wrapped version of a heading input in degrees.
   function Angle_Wrap (angle : Heading_Type_deg) return Heading_Type_deg
        is
     ((angle + 360.0) - Real64'Floor ((angle + 360.0) / 360.0) * 360.0)
   with 
       Post => (Angle_Wrap'Result >= 0.0 and Angle_Wrap'Result <= 360.0);
   
   function Heading_range_restraint (Current_State : state_parameters; 
                                    Heading_Min_deg : Heading_Type_deg; 
                                    Heading_Max_deg : Heading_Type_deg) 
                                    return Boolean is
       (Heading_Min_deg <= Current_State.heading_deg and then Current_State.
          heading_deg <= Heading_Max_deg);
   
   --subprogram that attempts to find a resolution to an impending loss of well 
   --clear by setting the divert heading using information from DAIDALUS 
   --conflict band or recovery band information.  Failing to find a good 
   --resolution or a mitigation, the divert heading is set to a known fallback 
   --of the current heading + 180.0 deg or the subprogram safely terminates 
   --due to being called with an improper condfiguration
   procedure Found_WCV_Heading_Resolution 
              (DAIDALUS_Heading_Bands : OrderedIntervalVector;
               Recovery_Heading_Bands : OrderedIntervalVector;
               Current_State : state_parameters;
               Heading_Max_deg : Heading_Type_deg;
               Heading_Min_deg : Heading_Type_deg;
               Heading_Interval_Buffer_deg : Heading_Buffer_Type_deg;
               Divert_State : out state_parameters;
               found_acceptable_action_flag : out Boolean)
     with 
       Pre => (Are_Legitimate_Bands (X => DAIDALUS_Heading_Bands) and then 
               Are_Legitimate_Bands (X => Recovery_Heading_Bands) and then
               correct_call_sequence (Current_State, DAIDALUS_Heading_Bands, 
                 Recovery_Heading_Bands, Heading_Max_deg, Heading_Min_deg, 
                 Heading_Interval_Buffer_deg) and then Heading_range_restraint
              (Current_State, Heading_Min_deg, Heading_Max_deg)), 
       Post => (Found_Acceptable_Action (found_acceptable_action_flag, 
                DAIDALUS_Heading_Bands, Recovery_Heading_Bands, 
                Divert_State.heading_deg) or else IsImproperlyConfigured
                  (found_acceptable_action_flag, DAIDALUS_Heading_Bands, 
                     Divert_State.heading_deg, Current_State.heading_deg) 
                or else Revert_behavior (found_acceptable_action_flag, 
                  DAIDALUS_Heading_Bands,
                    Angle_Wrap (Current_State.heading_deg + 180.0), 
                    Divert_State.heading_deg));
end Heading_Resolution;
