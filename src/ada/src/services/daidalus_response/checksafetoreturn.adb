pragma Ada_2012;
with Common; use Common;
package body CheckSafeToReturn with SPARK_Mode => On is

   function isSafeToReturn_Altitude
     (DAIDALUS_Altitude_Bands : OrderedIntervalVector32;
      DAIDALUS_Altitude_Classification_Bands : ZoneVector;
      VirtualReturnState : state_parameters) return Boolean
     with
       Pre => SameIndices (DAIDALUS_Altitude_Bands,
                           DAIDALUS_Altitude_Classification_Bands) and then
       Are_Legitimate_Bands (DAIDALUS_Altitude_Bands),
       Post => (if isSafeToReturn_Altitude'Result then
                  AltitudeSafe (DAIDALUS_Altitude_Bands,
                    DAIDALUS_Altitude_Classification_Bands,
                    VirtualReturnState) else
                    not AltitudeSafe (DAIDALUS_Altitude_Bands,
                  DAIDALUS_Altitude_Classification_Bands,
                  VirtualReturnState));

   function isSafeToReturn_Altitude
     (DAIDALUS_Altitude_Bands : OrderedIntervalVector32;
      DAIDALUS_Altitude_Classification_Bands : ZoneVector;
      VirtualReturnState : state_parameters) return Boolean
   is
   begin
      for I in MyVectorOfIntervals32.First_Index (DAIDALUS_Altitude_Bands) ..
        MyVectorOfIntervals32.Last_Index (DAIDALUS_Altitude_Bands) loop
         if InRange (MyVectorOfIntervals32.Element (DAIDALUS_Altitude_Bands, I),
                     VirtualReturnState.altitude_m)
         then
            if MyVectorOfZones.Element (DAIDALUS_Altitude_Classification_Bands,
                                        I) = Near
            then
               pragma Assert (not AltitudeSafe
                              (DAIDALUS_Altitude_Bands,
                                 DAIDALUS_Altitude_Classification_Bands,
                                 VirtualReturnState));
               return False;
            else
               exit;
            end if;
         end if;
         pragma Loop_Invariant (for all J in MyVectorOfIntervals32.First_Index
                               (DAIDALUS_Altitude_Bands) .. I =>
                                  not InRange (MyVectorOfIntervals32.Element
                                 (DAIDALUS_Altitude_Bands, J),
                                 VirtualReturnState.altitude_m));

      end loop;
      pragma Assert (AltitudeSafe (DAIDALUS_Altitude_Bands,
                    DAIDALUS_Altitude_Classification_Bands,
                    VirtualReturnState));
      return True;
   end isSafeToReturn_Altitude;

   function isSafeToReturn_GroundSpeed
     (DAIDALUS_GroundSpeed_Bands : OrderedIntervalVector;
      DAIDALUS_GroundSpeed_Classification_Bands : ZoneVector;
      VirtualReturnState : state_parameters) return Boolean with
     Pre => Are_Legitimate_Bands (DAIDALUS_GroundSpeed_Bands) and then
     SameIndices (DAIDALUS_GroundSpeed_Bands,
                  DAIDALUS_GroundSpeed_Classification_Bands),
     Post => (if isSafeToReturn_GroundSpeed'Result then GroundSpeedSafe
              (DAIDALUS_GroundSpeed_Bands,
                 DAIDALUS_GroundSpeed_Classification_Bands,
                 VirtualReturnState) else not GroundSpeedSafe
                (DAIDALUS_GroundSpeed_Bands,
                 DAIDALUS_GroundSpeed_Classification_Bands,
                 VirtualReturnState));

   function isSafeToReturn_GroundSpeed
     (DAIDALUS_GroundSpeed_Bands : OrderedIntervalVector;
      DAIDALUS_GroundSpeed_Classification_Bands : ZoneVector;
      VirtualReturnState : state_parameters) return Boolean  is
   begin
      for I in MyVectorOfIntervals.First_Index (DAIDALUS_GroundSpeed_Bands) ..
        MyVectorOfIntervals.Last_Index (DAIDALUS_GroundSpeed_Bands) loop
         if InRange (MyVectorOfIntervals.Element (DAIDALUS_GroundSpeed_Bands,
                                                   I), VirtualReturnState.
                       groundSpeed_mps)
         then
            if MyVectorOfZones.Element
              (DAIDALUS_GroundSpeed_Classification_Bands, I) = Near
            then
               pragma Assert (not GroundSpeedSafe (DAIDALUS_GroundSpeed_Bands,
                              DAIDALUS_GroundSpeed_Classification_Bands,
                              VirtualReturnState));
               return False;
            else
               exit;
            end if;
         end if;
         pragma Loop_Invariant (for all J in MyVectorOfIntervals.First_Index
                               (DAIDALUS_GroundSpeed_Bands) ..
                                 I => not InRange (MyVectorOfIntervals.Element
                                   (DAIDALUS_GroundSpeed_Bands, J),
                                   VirtualReturnState.groundSpeed_mps));
      end loop;
      pragma Assert (GroundSpeedSafe (DAIDALUS_GroundSpeed_Bands,
                    DAIDALUS_GroundSpeed_Classification_Bands,
                    VirtualReturnState));
      return True;
   end isSafeToReturn_GroundSpeed;

   function isSafeToReturn_Heading
     (DAIDALUS_Heading_Bands : OrderedIntervalVector;
      DAIDALUS_Heading_Classification_Bands : ZoneVector;
      VirtualReturnState : state_parameters) return Boolean with
     Pre => Are_Legitimate_Bands (DAIDALUS_Heading_Bands) and then
            SameIndices (DAIDALUS_Heading_Bands,
                        DAIDALUS_Heading_Classification_Bands),
     Post => (if isSafeToReturn_Heading'Result then HeadingSafe
             (DAIDALUS_Heading_Bands,
                        DAIDALUS_Heading_Classification_Bands,
                 VirtualReturnState) else not HeadingSafe
                (DAIDALUS_Heading_Bands, DAIDALUS_Heading_Classification_Bands,
                 VirtualReturnState));

   function isSafeToReturn_Heading
     (DAIDALUS_Heading_Bands : OrderedIntervalVector;
      DAIDALUS_Heading_Classification_Bands : ZoneVector;
      VirtualReturnState : state_parameters) return Boolean is
   begin
      for I in MyVectorOfIntervals.First_Index (DAIDALUS_Heading_Bands) ..
        MyVectorOfIntervals.Last_Index (DAIDALUS_Heading_Bands) loop
         if InRange (MyVectorOfIntervals.Element (DAIDALUS_Heading_Bands, I),
                     VirtualReturnState.heading_deg)
         then
            if MyVectorOfZones.Element (DAIDALUS_Heading_Classification_Bands,
                                        I) = Near
            then
               return False;
            else
               exit;
            end if;
         end if;
         pragma Loop_Invariant (for all J in MyVectorOfIntervals.First_Index
                               (DAIDALUS_Heading_Bands) .. I =>
                                  not InRange (MyVectorOfIntervals.Element
                                 (DAIDALUS_Heading_Bands, J),
                                 VirtualReturnState.heading_deg));
      end loop;
      pragma Assert (HeadingSafe (DAIDALUS_Heading_Bands,
                     DAIDALUS_Heading_Classification_Bands,
                     VirtualReturnState));
      return True;
   end isSafeToReturn_Heading;

   ------------------
   -- SafeToReturn --
   ------------------

   procedure SafeToReturn
     (DAIDALUS_Altitude_Bands                   :     OrderedIntervalVector32;
      DAIDALUS_Heading_Bands                    :     OrderedIntervalVector;
      DAIDALUS_GroundSpeed_Bands                :     OrderedIntervalVector;
      DAIDALUS_Altitude_Classification_Bands    :     ZoneVector;
      DAIDALUS_Heading_Classification_Bands     :     ZoneVector;
      DAIDALUS_GroundSpeed_Classification_Bands :     ZoneVector;
      Current_State                             :     state_parameters;
      SyntheticCheckState                       : out state_parameters;
      PreviousMissionWaypoint                   :     Int64;
      Mission_Command                           :     MissionCommand;
      isSafeToReturn                            : out Boolean)
   is
      myFlatEarthObject : FlatEarthObject;
      CurrentStateNorth : Real64;
      CurrentStateEast : Real64;
      WaypointNorth : Real64;
      WaypointEast : Real64;
      dummyFEO : FlatEarthObject;
      dummyState : state_parameters := (altitude_m => 0.0, others => 0.0);
   begin
      SyntheticCheckState := dummyState;
      if not Mission_Command.is_safe_to_access
      then
         SyntheticCheckState := Current_State;
      else
         pragma Assume (for some J in MyVectorOfWaypoints.First_Index
                        (Mission_Command.waypoint_list) .. MyVectorOfWaypoints.
                          Last_Index (Mission_Command.waypoint_list) =>
                          MyVectorOfWaypoints.Element (Mission_Command.waypoint_list,
                            J).waypoint_number = PreviousMissionWaypoint);
         for I in MyVectorOfWaypoints.First_Index (Mission_Command.waypoint_list)
           .. MyVectorOfWaypoints.Last_Index (Mission_Command.waypoint_list) loop
            if MyVectorOfWaypoints.Element (Mission_Command.waypoint_list, I).
              waypoint_number = PreviousMissionWaypoint
            then
               SyntheticCheckState.latitude_deg := MyVectorOfWaypoints.Element
                 (Mission_Command.waypoint_list, I).latitude_deg;
               SyntheticCheckState.longitude_deg := MyVectorOfWaypoints.Element
                 (Mission_Command.waypoint_list, I).longitude_deg;
               SyntheticCheckState.altitude_m := MyVectorOfWaypoints.Element
                 (Mission_Command.waypoint_list, I).altitude_m;
               SyntheticCheckState.groundSpeed_mps := MyVectorOfWaypoints.Element
                 (Mission_Command.waypoint_list, I).speed;
               SyntheticCheckState.verticalSpeed_mps := MyVectorOfWaypoints.Element
                 (Mission_Command.waypoint_list, I).climb_rate;
               exit;
            end if;
         end loop;

         ConvertLatitudeLongitude_deg_ToNorthEast_m
           (myFlatEarthObject, Current_State.latitude_deg, Current_State.
              longitude_deg, CurrentStateNorth, CurrentStateEast);

         ConvertLatitudeLongitude_deg_ToNorthEast_m
           (myFlatEarthObject, SyntheticCheckState.latitude_deg,
            SyntheticCheckState.longitude_deg, WaypointNorth, WaypointEast);
         dummyFEO := myFlatEarthObject;
         pragma Assert (dummyFEO = myFlatEarthObject);
         pragma Assume (Real64 (WaypointEast - CurrentStateEast) in Real64'Range);
         pragma Assume (Real64 (WaypointNorth - CurrentStateNorth) in Real64'Range);
         pragma Assume (not (Float (WaypointEast - CurrentStateEast) = 0.0) and
                          not (Float (WaypointNorth - CurrentStateNorth) = 0.0));
         pragma Assume (Arctan (Float (WaypointNorth - CurrentStateNorth), Float
                        (WaypointEast - CurrentStateEast)) >= -3.14159 / 2.0);
         pragma Assume (Arctan (Float (WaypointNorth - CurrentStateNorth), Float
                        (WaypointEast - CurrentStateEast)) <= 3.14149 / 2.0);
         pragma Assume (Heading_Type_deg (Arctan (Float (WaypointNorth -
                          CurrentStateNorth), Float (WaypointEast -
                              CurrentStateEast))) >= SafeReal64 (-3.14159 / 2.0)
                        and then
                        Heading_Type_deg (Arctan (Float (WaypointNorth -
                            CurrentStateNorth), Float (WaypointEast -
                                CurrentStateEast))) <= SafeReal64 (3.14159 / 2.0));
         SyntheticCheckState.heading_deg := Angle_Wrap
           (Heading_Type_deg (SafeReal64 (Arctan (Float (WaypointNorth -
              CurrentStateNorth), Float (WaypointEast - CurrentStateEast)))
            * convertRadiansToDegrees));
      end if;

      isSafeToReturn := isSafeToReturn_Altitude (DAIDALUS_Altitude_Bands,
                                                 DAIDALUS_Altitude_Classification_Bands,
                                                 SyntheticCheckState) and then
        isSafeToReturn_GroundSpeed (DAIDALUS_GroundSpeed_Bands,
                                    DAIDALUS_GroundSpeed_Classification_Bands,
                                    SyntheticCheckState) and then
        isSafeToReturn_Heading (DAIDALUS_Heading_Bands,
                                DAIDALUS_Heading_Classification_Bands,
                                SyntheticCheckState);
      pragma Assert (if isSafeToReturn_Altitude
                     (DAIDALUS_Altitude_Bands,
                        DAIDALUS_Altitude_Classification_Bands,
                        SyntheticCheckState)
                     then AltitudeSafe
                       (DAIDALUS_Altitude_Bands,
                        DAIDALUS_Altitude_Classification_Bands,
                        SyntheticCheckState));
      pragma Assert (if isSafeToReturn_Heading (DAIDALUS_Heading_Bands,
                     DAIDALUS_Heading_Classification_Bands, SyntheticCheckState)
                     then HeadingSafe
                       (DAIDALUS_Heading_Bands,
                        DAIDALUS_Heading_Classification_Bands,
                        SyntheticCheckState));
      pragma Assert (if isSafeToReturn_GroundSpeed (DAIDALUS_GroundSpeed_Bands,
                     DAIDALUS_GroundSpeed_Classification_Bands, SyntheticCheckState)
                     then GroundSpeedSafe (DAIDALUS_GroundSpeed_Bands,
                       DAIDALUS_GroundSpeed_Classification_Bands, SyntheticCheckState));

   end SafeToReturn;

end CheckSafeToReturn;
