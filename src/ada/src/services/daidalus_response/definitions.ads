--  with Ada.Containers.Formal_Vectors,
--       Ada.Containers.Functional_Vectors;
with SPARK.Containers.Formal.Vectors;
with Common; use Common;

package definitions 
with SPARK_Mode => On
is
   pragma Assertion_Policy (Check);
   
   type zones is (Near, Mid, Far);
   
   Maxsize : constant := 1000;
   subtype SafeReal64 is Real64 range -7_000_000.0 .. 7_000_000.0;
   subtype SafeReal32 is Real32 range -7_000_000.0 .. 7_000_000.0;

   --interval defined by a lower bound and an upper bound.
   --intervals are open on the lower bound and closed on the upper bound.
   type interval is record   
      LowerBound : SafeReal64;
      UpperBound : SafeReal64;
      Classification : zones;
   end record;
   
   type interval32 is record
      LowerBound : SafeReal32;
      UpperBound : SafeReal32;
      Classification : zones;
   end record;
   
-- with Dynamic_Predicate => interval.LowerBound < interval.UpperBound;   
   subtype myvector_index_type is Positive;
   
   package MyVectorOfIntervals is new SPARK.Containers.Formal.Vectors
     (Element_Type => interval, Index_Type => myvector_index_type);
   
   package MyVectorOfIntervals32 is new SPARK.Containers.Formal.Vectors
     (Element_Type  => interval32, Index_Type => myvector_index_type);
   
   package MyVectorOfZones is new SPARK.Containers.Formal.Vectors
     (Element_Type => zones, Index_Type => myvector_index_type);
   
   --instantiated formal vector container
   subtype OrderedIntervalVector is MyVectorOfIntervals.Vector (Maxsize); 
   subtype OrderedIntervalVector32 is MyVectorOfIntervals32.Vector (Maxsize);
   --instantiated formal vector container of zones
   subtype ZoneVector is MyVectorOfZones.Vector (Maxsize);
   --Altitude range in meters
   subtype Altitude_Type_m is SafeReal32 range -100_000.0 .. 100_000.0; 
   --minimum distance between lower and upper bound
   subtype Altitude_Buffer_Type_m is Altitude_Type_m range 
     0.0 .. 0.1 * Altitude_Type_m'Last; 
   --heading range in degrees
   subtype Heading_Type_deg is SafeReal64 range -1_000.0 .. 1_000.0; 
   --minimum distance between lower and upper bound
   subtype Heading_Buffer_Type_deg is Heading_Type_deg range 
     0.0 .. 0.1 * Heading_Type_deg'Last; 
   --Ground speed range in meters per second
   subtype GroundSpeed_Type_mps is SafeReal64 range -1_000.0 .. 1_000.0; 
   --minimum distance between lower and upper bound
   subtype GroundSpeed_Buffer_Type_mps is GroundSpeed_Type_mps range 
     0.0 .. 0.1 * GroundSpeed_Type_mps'Last; 
   --Vertical spped range in meters per second
   subtype VerticalSpeed_Type_mps is SafeReal64 range -1_000.0 .. 1_000.0; 
   --minimum distance between lower and upper bound
   subtype VerticalSpeed_Buffer_Type_mps is VerticalSpeed_Type_mps range 
     0.0 .. 0.1 * VerticalSpeed_Type_mps'Last; 
   subtype latitude_type_deg is SafeReal64 range -90.0 .. 90.0;
   subtype longitude_type_deg is SafeReal64 range -180.0 .. 180.0;
   
   type state_parameters is record
      altitude_m : Altitude_Type_m;
      heading_deg : Heading_Type_deg;
      groundSpeed_mps : GroundSpeed_Type_mps;
      verticalSpeed_mps : VerticalSpeed_Type_mps;
      latitude_deg : latitude_type_deg;
      longitude_deg : longitude_type_deg;
   end record;
   
   --priorty determines nature of response
   type Priority_Type is (pStandard, pHigh);
   
   --status of vehicle
   type Status_Type is (OnMission, OnHold, InConflict);

   subtype ID_Type is Int64 range -1 .. 10000;
   --range of values allowable for UAV ID's
   subtype VehicleID_type is ID_Type range 0 .. ID_Type'Last;
   
   --an array of vehicle ID's
   type Vehicle_IDs is array (1 .. 10) of VehicleID_type;
   
   --time to violation of well clear in seconds;
   subtype ttlowc_sec is SafeReal64 range 0.0 .. SafeReal64'Last;
   
   --time threshold for automatic response to be enacted
   subtype action_time_sec is SafeReal64 range 0.0 .. 120.0;
   
   --time threshold for elevating prioroty of the response;
   subtype priority_time_sec is SafeReal64 range -1.0 .. action_time_sec'Last;
   
   --array of time to violation
   type ttlow_array is array (1 .. 10) of ttlowc_sec;
   
   --record of intruder information
   type Intruder_info is record
      Intruder_ID : VehicleID_type;
      Intruder_time_to_violation : ttlowc_sec;
   end record;
   
   package MyVectorOfIntegers is new SPARK.Containers.Formal.Vectors
     (Element_Type => Int64, Index_Type => myvector_index_type);
   
   subtype Associated_Tasks_List is MyVectorOfIntegers.Vector (8);   

   type VehicleAction is record
      AssociatedTaskList : Associated_Tasks_List;
   end record;
      
   type waypoint_speed_type is (Airspeed, Groundspeed);
   
   type waypoint_turn_type is (TurnShort, FlyOver);
   
   package MyVectorOfVehicleActions is new SPARK.Containers.Formal.Vectors
     (Element_Type => VehicleAction, Index_Type => myvector_index_type);
   
   subtype VehicleActionList is MyVectorOfVehicleActions.Vector (8);
   
   type cmasi_altitude_type is (AGL, MSL);
   
   --record containing Waypoint information
   type Waypoint_info is record
      waypoint_number : Int64;
      next_waypoint : Int64;
      speed : GroundSpeed_Type_mps;
      speed_type : waypoint_speed_type;
      climb_rate : VerticalSpeed_Type_mps;
      turn_type : waypoint_turn_type;
      vehicle_action_list : VehicleActionList;
      contingency_waypoint_A : Int64;
      contingency_waypoint_B : Int64;
      associated_tasks : Associated_Tasks_List;
      latitude_deg : latitude_type_deg;
      longitude_deg : longitude_type_deg;
      altitude_m : Altitude_Type_m;
      altitude_type : cmasi_altitude_type;
   end record;

   package MyVectorOfVehicleIDs is new SPARK.Containers.Formal.Vectors
     (Element_Type => VehicleID_type, Index_Type => myvector_index_type);
   
   package MyVectorOfIntruderInfo is new SPARK.Containers.Formal.Vectors
     (Element_Type => Intruder_info, Index_Type => myvector_index_type);
   
   package MyVectorOfWaypoints is new SPARK.Containers.Formal.Vectors
     (Element_Type => Waypoint_info, Index_Type => myvector_index_type);
   
   subtype WaypointList is MyVectorOfWaypoints.Vector (Maxsize);
   
   subtype VehicleIDsVector is MyVectorOfVehicleIDs.Vector (10);
   
   type CommandStatusType is (Pending, Approved, InProcess, Executed,
                              Cancelled);
   
   type MissionCommand is record
      is_safe_to_access : Boolean := False;
      waypoint_list : WaypointList;
      first_waypoint : Int64;
      command_id : Int64;
      vehicle_id : Int64;
      vehicle_action_list : VehicleActionList;
      status : CommandStatusType; 
   end record;

   -- array of Intruder_info records
   subtype Intruder_info_Vector is MyVectorOfIntruderInfo.Vector (10);
      
   --predicate indicating if a container of interval bands adheres to 
   --constraints defining a proper container. Intervals that next to each other 
   --are allowed to share a value of the upper bound of the lower interval/lower
   --bound of the upper interval otherwise lower and upper bounds of the lower 
   --interval are strictly less than upper and lower bounds of the higher 
   --interval.
   function Are_Legitimate_Bands (X : OrderedIntervalVector) return Boolean is
     ((for all I in MyVectorOfIntervals.First_Index (X) .. 
         MyVectorOfIntervals.Last_Index (X) => 
         (for all J in MyVectorOfIntervals.First_Index (X) .. 
              MyVectorOfIntervals.Last_Index (X) =>
              (if J - I = 1 then MyVectorOfIntervals.Element (X, I).UpperBound <= 
                     MyVectorOfIntervals.Element (X, J).LowerBound
               else (if J > I then MyVectorOfIntervals.Element (X, I).LowerBound
                 < MyVectorOfIntervals.Element (X, J).UpperBound))))
       and then (for all I in MyVectorOfIntervals.First_Index (X) .. 
           MyVectorOfIntervals.Last_Index (X) =>
           (for all J in MyVectorOfIntervals.First_Index (X) .. 
                MyVectorOfIntervals.Last_Index (X) => 
                (if I < J then MyVectorOfIntervals.Element (X, I).
                       LowerBound < MyVectorOfIntervals.Element (X, J).
                       LowerBound)))
       and then (for all I in MyVectorOfIntervals.First_Index (X) .. 
           MyVectorOfIntervals.Last_Index (X) =>
           (for all J in MyVectorOfIntervals.First_Index (X) .. 
                MyVectorOfIntervals.Last_Index (X) =>
                (if I < J then MyVectorOfIntervals.Element (X, I).
                       UpperBound < MyVectorOfIntervals.Element (X, J).
                       UpperBound))));

   function Are_Legitimate_Bands (X : OrderedIntervalVector32) return Boolean is
     ((for all I in MyVectorOfIntervals32.First_Index (X) .. 
         MyVectorOfIntervals32.Last_Index (X) => 
         (for all J in MyVectorOfIntervals32.First_Index (X) .. 
              MyVectorOfIntervals32.Last_Index (X) =>
              (if J - I = 1 then MyVectorOfIntervals32.Element (X, I).UpperBound <= 
                     MyVectorOfIntervals32.Element (X, J).LowerBound
               else (if J > I then MyVectorOfIntervals32.Element (X, I).LowerBound
                 < MyVectorOfIntervals32.Element (X, J).UpperBound))))
       and then (for all I in MyVectorOfIntervals32.First_Index (X) .. 
           MyVectorOfIntervals32.Last_Index (X) =>
           (for all J in MyVectorOfIntervals32.First_Index (X) .. 
                MyVectorOfIntervals32.Last_Index (X) => 
                (if I < J then MyVectorOfIntervals32.Element (X, I).
                       LowerBound < MyVectorOfIntervals32.Element (X, J).
                       LowerBound)))
       and then (for all I in MyVectorOfIntervals32.First_Index (X) .. 
           MyVectorOfIntervals32.Last_Index (X) =>
           (for all J in MyVectorOfIntervals32.First_Index (X) .. 
                MyVectorOfIntervals32.Last_Index (X) =>
                (if I < J then MyVectorOfIntervals32.Element (X, I).
                       UpperBound < MyVectorOfIntervals32.Element (X, J).
                       UpperBound))));
   
   --predicate indicating if a given test value is located within the chosen  
   --interval defined by an open lower bound and closed upper bound 
   function InRange (Test_Interval : interval; Test_Value : SafeReal64) return 
     Boolean is 
       (Test_Value > Test_Interval.LowerBound and then 
        Test_Value <= Test_Interval.UpperBound); 

   function InRange (Test_Interval : interval32; Test_Value : SafeReal32) return 
     Boolean is 
       (Test_Value > Test_Interval.LowerBound and then 
        Test_Value <= Test_Interval.UpperBound); 
   
   --predicate indicating if an acceptable action was found in order to avoid
   --or mitigate an occuring or imminient violation of the well clear volume
   function Found_Acceptable_Action (found_acceptable_action_flag : Boolean;
                                    DAIDALUS_Bands : OrderedIntervalVector;
                                    Recovery_Bands : OrderedIntervalVector;
                                    Divert_State_field : SafeReal64) return Boolean 
   is
     ((found_acceptable_action_flag) and then ((for all I in 
          MyVectorOfIntervals.First_Index (DAIDALUS_Bands) .. 
        MyVectorOfIntervals.Last_Index (DAIDALUS_Bands) => not
        InRange (MyVectorOfIntervals.Element (DAIDALUS_Bands, I), 
          Divert_State_field)) or else (if not 
            MyVectorOfIntervals.Is_Empty (Recovery_Bands) then
          (for some I in MyVectorOfIntervals.First_Index (Recovery_Bands) ..
               MyVectorOfIntervals.Last_Index (Recovery_Bands) =>
                InRange (MyVectorOfIntervals.Element (Recovery_Bands, I),
                  Divert_State_field))))) with Ghost;
   
   function Found_Acceptable_Action (found_acceptable_action_flag : Boolean;
                                     DAIDALUS_Bands : OrderedIntervalVector32;
                                     Recovery_Bands : OrderedIntervalVector32;
                                     Divert_State_field : SafeReal32) return Boolean 
   is
     ((found_acceptable_action_flag) and then ((for all I in 
          MyVectorOfIntervals32.First_Index (DAIDALUS_Bands) .. 
          MyVectorOfIntervals32.Last_Index (DAIDALUS_Bands) => not
          InRange (MyVectorOfIntervals32.Element (DAIDALUS_Bands, I), 
        Divert_State_field)) or else (if not 
            MyVectorOfIntervals32.Is_Empty (Recovery_Bands) then
          (for some I in MyVectorOfIntervals32.First_Index (Recovery_Bands) ..
               MyVectorOfIntervals32.Last_Index (Recovery_Bands) =>
                InRange (MyVectorOfIntervals32.Element (Recovery_Bands, I),
             Divert_State_field))))) with Ghost;

   --predicate indicating that the conflict resolution scenario was improperly 
   --configured where improper is defined as the conflict bands being empty when
   --conflict resolution subprogram is called.
   function IsImproperlyConfigured (found_acceptable_action_flag : Boolean; 
                                   DAIDALUS_X_Bands : OrderedIntervalVector;
                                   Divert_State_field : SafeReal64; 
                                   Current_State_field : SafeReal64)
                                   return Boolean is 
     (found_acceptable_action_flag = False and then 
      MyVectorOfIntervals.Is_Empty (DAIDALUS_X_Bands) and then 
        Divert_State_field = Current_State_field) with Ghost; 
   
   function IsImproperlyConfigured (found_acceptable_action_flag : Boolean; 
                                   DAIDALUS_X_Bands : OrderedIntervalVector32;
                                   Divert_State_field : SafeReal32; 
                                   Current_State_field : SafeReal32)
                                   return Boolean is 
     (found_acceptable_action_flag = False and then 
      MyVectorOfIntervals32.Is_Empty (DAIDALUS_X_Bands) and then 
        Divert_State_field = Current_State_field) with Ghost; 
 
   --predicate indicating that a resolution to impending loss of well clear 
   --could not be found and instead divert state is set to a known default
   function Revert_behavior (found_acceptable_action_flag : Boolean; 
                            DAIDALUS_X_Bands : OrderedIntervalVector;
                            default_action : SafeReal64; 
                            Divert_State_field : SafeReal64) return Boolean is
     (found_acceptable_action_flag = False and then 
      not MyVectorOfIntervals.Is_Empty (DAIDALUS_X_Bands) and then
      Divert_State_field = default_action) with Ghost;

   function Revert_behavior (found_acceptable_action_flag : Boolean; 
                            DAIDALUS_X_Bands : OrderedIntervalVector32;
                            default_action : SafeReal32; 
                            Divert_State_field : SafeReal32) return Boolean is
     (found_acceptable_action_flag = False and then 
      not MyVectorOfIntervals32.Is_Empty (DAIDALUS_X_Bands) and then
      Divert_State_field = default_action) with Ghost;
   
end definitions;
