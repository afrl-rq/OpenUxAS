with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with definitions; use definitions;
with Common; use Common;
with Heading_Resolution; use Heading_Resolution;
with FlatEarth; use FlatEarth;
package CheckSafeToReturn with SPARK_Mode => on is

   function SameIndices (A : OrderedIntervalVector; B : ZoneVector) return
   Boolean is
     (MyVectorOfIntervals.Last_Index (A) = MyVectorOfZones.Last_Index (B));

   function SameIndices (A : OrderedIntervalVector32; B : ZoneVector) return
     Boolean is
       (MyVectorOfIntervals32.Last_Index (A) = MyVectorOfZones.Last_Index (B));

   function AltitudeSafe
     (DAIDALUS_Altitude_Bands : OrderedIntervalVector32;
      DAIDALUS_Altitude_Classification_Bands : ZoneVector;
      State : state_parameters) return Boolean is
     ((for all I in MyVectorOfIntervals32.First_Index (DAIDALUS_Altitude_Bands) ..
         MyVectorOfIntervals32.Last_Index (DAIDALUS_Altitude_Bands) => not InRange
       (MyVectorOfIntervals32.Element (DAIDALUS_Altitude_Bands, I),
          State.altitude_m)) or else (for some J in MyVectorOfIntervals32.
             First_Index (DAIDALUS_Altitude_Bands) .. MyVectorOfIntervals32.
           Last_Index (DAIDALUS_Altitude_Bands) =>
            InRange (MyVectorOfIntervals32.Element (DAIDALUS_Altitude_Bands, J),
           State.altitude_m) and then not (MyVectorOfZones.
             Element (DAIDALUS_Altitude_Classification_Bands, J) = Near)))
     with
       Pre => SameIndices (DAIDALUS_Altitude_Bands,
                          DAIDALUS_Altitude_Classification_Bands),   Ghost;

   function GroundSpeedSafe
     (X_Bands : OrderedIntervalVector;
      X_Classification_Bands : ZoneVector;
      State : state_parameters) return Boolean is
     ((for all I in MyVectorOfIntervals.First_Index (X_Bands) ..
         MyVectorOfIntervals.Last_Index (X_Bands) => not InRange
       (MyVectorOfIntervals.Element (X_Bands, I), State.groundSpeed_mps)) or else
         (for some J in MyVectorOfIntervals.First_Index (X_Bands) ..
              MyVectorOfIntervals.Last_Index (X_Bands) =>
               InRange (MyVectorOfIntervals.Element (X_Bands, J), State.
              groundSpeed_mps) and then not (MyVectorOfZones.Element
              (X_Classification_Bands, J) = Near))) with
       Pre => SameIndices (X_Bands, X_Classification_Bands), Ghost;

   function HeadingSafe
     (X_Bands : OrderedIntervalVector;
      X_Classification_Bands : ZoneVector;
      State : state_parameters) return Boolean is
     ((for all I in MyVectorOfIntervals.First_Index (X_Bands) ..
         MyVectorOfIntervals.Last_Index (X_Bands) => not InRange
       (MyVectorOfIntervals.Element (X_Bands, I), State.heading_deg)) or else
         (for some J in MyVectorOfIntervals.First_Index (X_Bands) ..
              MyVectorOfIntervals.Last_Index (X_Bands) =>
               InRange (MyVectorOfIntervals.Element (X_Bands, J), State.
              heading_deg) and then not (MyVectorOfZones.Element
              (X_Classification_Bands, J) = Near))) with
       Pre => SameIndices (X_Bands, X_Classification_Bands), Ghost;

   procedure SafeToReturn
     (DAIDALUS_Altitude_Bands : OrderedIntervalVector32;
      DAIDALUS_Heading_Bands : OrderedIntervalVector;
      DAIDALUS_GroundSpeed_Bands : OrderedIntervalVector;
      DAIDALUS_Altitude_Classification_Bands : ZoneVector;
      DAIDALUS_Heading_Classification_Bands : ZoneVector;
      DAIDALUS_GroundSpeed_Classification_Bands : ZoneVector;
      Current_State : state_parameters;
      SyntheticCheckState : out state_parameters;
      PreviousMissionWaypoint : Int64;
      Mission_Command : MissionCommand;
      isSafeToReturn : out Boolean)  with
       Pre => SameIndices
         (DAIDALUS_Altitude_Bands, DAIDALUS_Altitude_Classification_Bands)
         and then SameIndices
           (DAIDALUS_Heading_Bands, DAIDALUS_Heading_Classification_Bands)
         and then SameIndices
             (DAIDALUS_GroundSpeed_Bands,
              DAIDALUS_GroundSpeed_Classification_Bands) and then
             Are_Legitimate_Bands (DAIDALUS_Altitude_Bands) and then
             Are_Legitimate_Bands (DAIDALUS_Heading_Bands) and then
             Are_Legitimate_Bands (DAIDALUS_GroundSpeed_Bands),
             Post => (if isSafeToReturn then (AltitudeSafe
                  (DAIDALUS_Altitude_Bands,
                         DAIDALUS_Altitude_Classification_Bands, SyntheticCheckState)
                      and then HeadingSafe (DAIDALUS_Heading_Bands,
                        DAIDALUS_Heading_Classification_Bands,
                        SyntheticCheckState) and then GroundSpeedSafe
                        (DAIDALUS_GroundSpeed_Bands,
                         DAIDALUS_GroundSpeed_Classification_Bands,
                         SyntheticCheckState)));

end CheckSafeToReturn;
