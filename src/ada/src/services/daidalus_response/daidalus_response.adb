with Ada.Containers;             use Ada.Containers;
with AVTAS.LMCP.Types;           use AVTAS.LMCP.Types;
with UxAS.Comms.LMCP_Net_Client; use UxAS.Comms.LMCP_Net_Client;
with LMCP_Messages;              use LMCP_Messages;
with Ada.Text_IO;                use Ada.Text_IO;
with Common;                     use Common;
with definitions;      
with SPARK.Containers.Functional.Vectors;
with automatic_response;
with set_divert_state; 
with Heading_Resolution; 
with Altitude_Resolution; 
with speed_resolution; 
with CheckSafeToReturn; 
-- __TODO__
-- Include any other necessary packages.

package body Daidalus_Response with SPARK_Mode is
   
   -- Helper functions ---------------------------------------------------------
   procedure CreateAltitudeBands (LMCP_Altitudes : AltitudeInterval; 
                                  LMCP_AltitudeZone : BandsRegion_seq;
                                  DAIDALUS_Altitude_Bands : aliased out 
                                    definitions.OrderedIntervalVector32) with 
       Exceptional_Cases =>
         (Inconsistent_Message => MyVectorOfIntervals32.Is_Empty
            (DAIDALUS_Altitude_Bands),
          Violated_precondition => 
               not definitions.Are_Legitimate_Bands (DAIDALUS_Altitude_Bands)),
       Post => definitions.Are_Legitimate_Bands (DAIDALUS_Altitude_Bands);
   
   procedure CreateAltitudeBands (LMCP_Altitudes : AltitudeInterval;
                                  LMCP_AltitudeZone : BandsRegion_seq;
                                  DAIDALUS_Altitude_Bands : aliased out 
                                 definitions.OrderedIntervalVector32) is
      result : definitions.OrderedIntervalVector32 := MyVectorOfIntervals32.
      Empty_Vector (definitions.Maxsize);
      --  EmptyVector : definitions.OrderedIntervalVector32;
   begin
      -- Assumption used to bypass setting a precondition given that the 
      -- is true from the message without handling a check to establish the 
      -- property upon reception of the corresponding message ------------------
      --  pragma Assume (Generic_Real32_Sequences.Last
      --                   (LMCP_Altitudes.Altitude) =
      --                     BandsRegion_sequences.Last
      --                     (LMCP_AltitudeZone));
      -- Assumption that the number of intervals is less than an allowable 
      -- maximum. --------------------------------------------------------------
      pragma Assume (BandsRegion_sequences.Last (LMCP_AltitudeZone) <=
                       Integer (MyVectorOfIntervals32.Capacity (result)));
      if Generic_Real32_Sequences.Last (LMCP_Altitudes.Altitude) =
            BandsRegion_sequences.Last (LMCP_AltitudeZone)
      then
         for Index in BandsRegion_sequences.First  .. 
           BandsRegion_sequences.Last (LMCP_AltitudeZone) loop
            pragma Assert (Index in Generic_Real32_Sequences.First .. 
                             Generic_Real32_Sequences.Last 
                               (LMCP_Altitudes.Altitude));
            pragma Loop_Invariant (Integer (MyVectorOfIntervals32.Length (result))
                                   = Index - BandsRegion_sequences.First);
            declare
               temp_interval : definitions.interval32;
            begin
               temp_interval.LowerBound := Generic_Real32_Sequences.Get 
                    (LMCP_Altitudes.Altitude, Index)(1);
               temp_interval.UpperBound := Generic_Real32_Sequences.Get 
                 (LMCP_Altitudes.Altitude, Index)(2);
               case BandsRegion_sequences.Get (LMCP_AltitudeZone, Index) is
                  when LMCP_Messages.MID => temp_interval.Classification := 
                       definitions.Mid;
                  when LMCP_Messages.NEAR => temp_interval.Classification :=
                       definitions.Near;
                  when LMCP_Messages.FAR => temp_interval.Classification :=
                       definitions.Far;
               end case;
               MyVectorOfIntervals32.Append (result, temp_interval);
            end;
         end loop;
         DAIDALUS_Altitude_Bands := result;
         if not definitions.Are_Legitimate_Bands (DAIDALUS_Altitude_Bands) then
            raise Violated_precondition;
         end if;
      else
         --  DAIDALUS_Altitude_Bands := MyVectorOfIntervals32.Empty_Vector;
         DAIDALUS_Altitude_Bands := result;
         raise Inconsistent_Message;
      end if;      
      pragma Assert (definitions.Are_Legitimate_Bands (DAIDALUS_Altitude_Bands));
             
             -- adding a non breaking change
             -- adding a second non breaking change
   end CreateAltitudeBands;

   function CreateZones (LMCP_Zone : BandsRegion_seq) return
     ZoneVector;
   
   function CreateZones (LMCP_Zone : BandsRegion_seq) return 
     ZoneVector is
      result : ZoneVector;
      temp : definitions.zones;
   begin
      -- Assumption that the number of intervals is less than an allowable 
      -- maximum. --------------------------------------------------------------
      pragma Assume (BandsRegion_sequences.Last (LMCP_Zone) <= 
                       Integer (MyVectorOfZones.Capacity (result)));
      for Index in BandsRegion_sequences.First .. 
        BandsRegion_sequences.Last (LMCP_Zone) loop   
         pragma Loop_Invariant (Integer (MyVectorOfZones.Length (result)) < 
                                  Integer (Index));
         case BandsRegion_sequences.Get (LMCP_Zone, Index) is
         when LMCP_Messages.NEAR => temp := definitions.Near;
         when LMCP_Messages.MID => temp := definitions.Mid;
         when LMCP_Messages.FAR => temp := definitions.Far;
         end case;
         MyVectorOfZones.Append (result, temp);
      end loop;
      return result;
   end CreateZones;

   procedure CreateHeadingBands (LMCP_Headings : GroundHeadingInterval; 
                                  LMCP_HeadingZone : BandsRegion_seq;
                                  DAIDALUS_Heading_Bands : aliased out 
                                    definitions.OrderedIntervalVector) with 
       Exceptional_Cases =>
         (Inconsistent_Message => MyVectorOfIntervals.Is_Empty
            (DAIDALUS_Heading_Bands),
          Violated_precondition => 
               not definitions.Are_Legitimate_Bands (DAIDALUS_Heading_Bands)),
       Post => definitions.Are_Legitimate_Bands (DAIDALUS_Heading_Bands);
   
   procedure CreateHeadingBands (LMCP_Headings : GroundHeadingInterval;
                                  LMCP_HeadingZone : BandsRegion_seq;
                                  DAIDALUS_Heading_Bands : aliased out 
                                 definitions.OrderedIntervalVector) is
      result : definitions.OrderedIntervalVector := MyVectorOfIntervals.
      Empty_Vector (definitions.Maxsize);
      --  EmptyVector : definitions.OrderedIntervalVector;
   begin
      -- Assumption used to bypass setting a precondition given that the 
      -- is true from the message without handling a check to establish the 
      -- property upon reception of the corresponding message ------------------
      --  pragma Assume (Generic_Real64_Sequences.Last
      --                   (LMCP_Headings.GroundHeadings) =
      --                     BandsRegion_sequences.Last
      --                     (LMCP_HeadingZone));
      -- Assumption that the number of intervals is less than an allowable 
      -- maximum. --------------------------------------------------------------
      pragma Assume (BandsRegion_sequences.Last (LMCP_HeadingZone) <= 
                       Integer (MyVectorOfIntervals.Capacity (result)));
      if Generic_Real64_Sequences.Last (LMCP_Headings.GroundHeadings) =
        BandsRegion_sequences.Last (LMCP_HeadingZone)
      then
         for Index in BandsRegion_sequences.First  .. 
           BandsRegion_sequences.Last (LMCP_HeadingZone) loop
            pragma Assert (Index in Generic_Real64_Sequences.First .. 
                             Generic_Real64_Sequences.Last 
                               (LMCP_Headings.GroundHeadings));
            pragma Loop_Invariant (Integer (MyVectorOfIntervals.Length (result))
                                   = Index - BandsRegion_sequences.First);
            declare
               temp_interval : definitions.interval;
            begin
               temp_interval.LowerBound := Generic_Real64_Sequences.Get 
                    (LMCP_Headings.GroundHeadings, Index)(1);
               temp_interval.UpperBound := Generic_Real64_Sequences.Get 
                 (LMCP_Headings.GroundHeadings, Index)(2);
               case BandsRegion_sequences.Get (LMCP_HeadingZone, Index) is
                  when LMCP_Messages.MID => temp_interval.Classification := 
                       definitions.Mid;
                  when LMCP_Messages.NEAR => temp_interval.Classification :=
                       definitions.Near;
                  when LMCP_Messages.FAR => temp_interval.Classification :=
                       definitions.Far;
               end case;
               MyVectorOfIntervals.Append (result, temp_interval);
            end;
         end loop;
         DAIDALUS_Heading_Bands := result;
         if not definitions.Are_Legitimate_Bands (DAIDALUS_Heading_Bands) then
            raise Violated_precondition;
         end if;
      else
         --  DAIDALUS_Heading_Bands := MyVectorOfIntervals.Empty_Vector;
         DAIDALUS_Heading_Bands := result;
         raise Inconsistent_Message;
      end if;      
      pragma Assert (definitions.Are_Legitimate_Bands (DAIDALUS_Heading_Bands));
             
   end CreateHeadingBands;

   procedure CreateGroundSpeedBands (LMCP_GroundSpeed : GroundSpeedInterval; 
                                  LMCP_GroundSpeedZone : BandsRegion_seq;
                                  DAIDALUS_GroundSpeed_Bands : aliased out 
                                    definitions.OrderedIntervalVector) with 
       Exceptional_Cases =>
         (Inconsistent_Message => MyVectorOfIntervals.Is_Empty
            (DAIDALUS_GroundSpeed_Bands),
          Violated_precondition => 
               not definitions.Are_Legitimate_Bands (DAIDALUS_GroundSpeed_Bands)),
       Post => definitions.Are_Legitimate_Bands (DAIDALUS_GroundSpeed_Bands);
   
   procedure CreateGroundSpeedBands (LMCP_GroundSpeed : GroundSpeedInterval;
                                  LMCP_GroundSpeedZone : BandsRegion_seq;
                                  DAIDALUS_GroundSpeed_Bands : aliased out 
                                 definitions.OrderedIntervalVector) is
      result : definitions.OrderedIntervalVector := MyVectorOfIntervals.
      Empty_Vector (definitions.Maxsize);
      --  EmptyVector : definitions.OrderedIntervalVector;
   begin
      -- Assumption used to bypass setting a precondition given that the 
      -- is true from the message without handling a check to establish the 
      -- property upon reception of the corresponding message ------------------
      --  pragma Assume (Generic_Real64_Sequences.Last
      --                   (LMCP_GroundSpeed.GroundSpeeds) =
      --                     BandsRegion_sequences.Last
      --                     (LMCP_GroundSpeedZone));
      -- Assumption that the number of intervals is less than an allowable 
      -- maximum. --------------------------------------------------------------
      pragma Assume (BandsRegion_sequences.Last (LMCP_GroundSpeedZone) <= 
                       Integer (MyVectorOfIntervals.Capacity (result)));
      if Generic_Real64_Sequences.Last (LMCP_GroundSpeed.GroundSpeeds) =
        BandsRegion_sequences.Last (LMCP_GroundSpeedZone)
      then
         for Index in BandsRegion_sequences.First  .. 
           BandsRegion_sequences.Last (LMCP_GroundSpeedZone) loop
            pragma Assert (Index in Generic_Real64_Sequences.First .. 
                             Generic_Real64_Sequences.Last 
                               (LMCP_GroundSpeed.GroundSpeeds));
            pragma Loop_Invariant (Integer (MyVectorOfIntervals.Length (result))
                                   = Index - BandsRegion_sequences.First);
            declare
               temp_interval : definitions.interval;
            begin
               temp_interval.LowerBound := Generic_Real64_Sequences.Get 
                    (LMCP_GroundSpeed.GroundSpeeds, Index)(1);
               temp_interval.UpperBound := Generic_Real64_Sequences.Get 
                 (LMCP_GroundSpeed.GroundSpeeds, Index)(2);
               case BandsRegion_sequences.Get (LMCP_GroundSpeedZone, Index) is
                  when LMCP_Messages.MID => temp_interval.Classification := 
                       definitions.Mid;
                  when LMCP_Messages.NEAR => temp_interval.Classification :=
                       definitions.Near;
                  when LMCP_Messages.FAR => temp_interval.Classification :=
                       definitions.Far;
               end case;
               MyVectorOfIntervals.Append (result, temp_interval);
            end;
         end loop;
         DAIDALUS_GroundSpeed_Bands := result;
         if not definitions.Are_Legitimate_Bands (DAIDALUS_GroundSpeed_Bands) then
            raise Violated_precondition;
         end if;
      else
         --  DAIDALUS_GroundSpeed_Bands := MyVectorOfIntervals.Empty_Vector;
         DAIDALUS_GroundSpeed_Bands := result;
         raise Inconsistent_Message;
      end if;      
      pragma Assert (definitions.Are_Legitimate_Bands 
                     (DAIDALUS_GroundSpeed_Bands));
             
   end CreateGroundSpeedBands;   
   
   procedure CreateRecoveryAltitudeBands 
     (LMCP_RecoveryAltitudeBands : AltitudeInterval;
      Recovery_Altitude_Bands : aliased out OrderedIntervalVector32) with 
     Exceptional_Cases => 
       (Violated_precondition => 
          not definitions.Are_Legitimate_Bands (Recovery_Altitude_Bands)),
       Post => definitions.Are_Legitimate_Bands (Recovery_Altitude_Bands);
   
   procedure CreateRecoveryAltitudeBands 
     (LMCP_RecoveryAltitudeBands : AltitudeInterval;
      Recovery_Altitude_Bands : aliased out OrderedIntervalVector32) is
      result : OrderedIntervalVector32;
   begin
      -- Assumption that the number of intervals is less than an allowable 
      -- maximum. --------------------------------------------------------------
      pragma Assume 
        (Generic_Real32_Sequences.Last 
         (LMCP_RecoveryAltitudeBands.Altitude) <=
            Integer (MyVectorOfIntervals32.Capacity (result)));
      for Index in Generic_Real32_Sequences.First .. 
        Generic_Real32_Sequences.Last (LMCP_RecoveryAltitudeBands.Altitude) loop
         pragma Loop_Invariant (Integer (MyVectorOfIntervals32.Length (result)) < 
                                  Index);
         declare
            temp : interval32;
         begin
            temp.LowerBound := Generic_Real32_Sequences.Get 
              (LMCP_RecoveryAltitudeBands.Altitude, Index)(1);
            temp.UpperBound := Generic_Real32_Sequences.Get 
              (LMCP_RecoveryAltitudeBands.Altitude, Index)(2);
            --Zone classification for Recovery bands not utilized.--------------
            temp.Classification := definitions.Near;
            MyVectorOfIntervals32.Append (result, temp);
         end;
      end loop;
      Recovery_Altitude_Bands := result;
      if not definitions.Are_Legitimate_Bands (Recovery_Altitude_Bands) then
         raise Violated_precondition;
      end if;
      pragma Assert (definitions.Are_Legitimate_Bands (Recovery_Altitude_Bands));
   end CreateRecoveryAltitudeBands;
   
   procedure CreateRecoveryHeadingBands 
     (LMCP_RecoveryHeadingBands : GroundHeadingInterval;
      Recovery_Heading_Bands : aliased out OrderedIntervalVector) with 
     Exceptional_Cases => 
       (Violated_precondition => 
          not definitions.Are_Legitimate_Bands (Recovery_Heading_Bands)),
       Post => definitions.Are_Legitimate_Bands (Recovery_Heading_Bands);
   
   procedure CreateRecoveryHeadingBands 
     (LMCP_RecoveryHeadingBands : GroundHeadingInterval;
      Recovery_Heading_Bands : aliased out OrderedIntervalVector) is
      result : OrderedIntervalVector;
   begin
      -- Assumption that the number of intervals is less than an allowable 
      -- maximum. --------------------------------------------------------------
      pragma Assume 
        (Generic_Real64_Sequences.Last 
         (LMCP_RecoveryHeadingBands.GroundHeadings) <=
            Integer (MyVectorOfIntervals.Capacity (result)));
      for Index in Generic_Real64_Sequences.First .. 
        Generic_Real64_Sequences.Last (LMCP_RecoveryHeadingBands.GroundHeadings) loop
         pragma Loop_Invariant (Integer (MyVectorOfIntervals.Length (result)) < 
                                  Index);
         declare
            temp : interval;
         begin
            temp.LowerBound := Generic_Real64_Sequences.Get 
              (LMCP_RecoveryHeadingBands.GroundHeadings, Index)(1);
            temp.UpperBound := Generic_Real64_Sequences.Get 
              (LMCP_RecoveryHeadingBands.GroundHeadings, Index)(2);
            --Zone classification for Recovery bands not utilized.--------------
            temp.Classification := definitions.Near;
            MyVectorOfIntervals.Append (result, temp);
         end;
      end loop;
      Recovery_Heading_Bands := result;
      if not definitions.Are_Legitimate_Bands (Recovery_Heading_Bands) then
         raise Violated_precondition;
      end if;
      pragma Assert (definitions.Are_Legitimate_Bands (Recovery_Heading_Bands));
   end CreateRecoveryHeadingBands;
   
   procedure CreateRecoveryGroundSpeedBands 
     (LMCP_RecoveryGroundSpeedBands : GroundSpeedInterval;
      Recovery_GroundSpeed_Bands : aliased out OrderedIntervalVector) with 
     Exceptional_Cases => 
       (Violated_precondition => 
          not definitions.Are_Legitimate_Bands (Recovery_GroundSpeed_Bands)),
       Post => definitions.Are_Legitimate_Bands (Recovery_GroundSpeed_Bands);
   
   procedure CreateRecoveryGroundSpeedBands 
     (LMCP_RecoveryGroundSpeedBands : GroundSpeedInterval;
      Recovery_GroundSpeed_Bands : aliased out OrderedIntervalVector) is
      result : OrderedIntervalVector;
   begin
      -- Assumption that the number of intervals is less than an allowable 
      -- maximum. --------------------------------------------------------------
      pragma Assume 
        (Generic_Real64_Sequences.Last 
         (LMCP_RecoveryGroundSpeedBands.GroundSpeeds) <=
         Integer (MyVectorOfIntervals.Capacity (result)));
      for Index in Generic_Real64_Sequences.First .. 
        Generic_Real64_Sequences.Last (LMCP_RecoveryGroundSpeedBands.GroundSpeeds) loop
         pragma Loop_Invariant (Integer (MyVectorOfIntervals.Length (result)) < 
                                  Index);
         declare
            temp : interval;
         begin
            temp.LowerBound := Generic_Real64_Sequences.Get 
              (LMCP_RecoveryGroundSpeedBands.GroundSpeeds, Index)(1);
            temp.UpperBound := Generic_Real64_Sequences.Get 
              (LMCP_RecoveryGroundSpeedBands.GroundSpeeds, Index)(2);
            --Zone classification for Recovery bands not utilized.--------------
            temp.Classification := definitions.Near;
            MyVectorOfIntervals.Append (result, temp);
         end;
      end loop;
      Recovery_GroundSpeed_Bands := result;
      if not definitions.Are_Legitimate_Bands (Recovery_GroundSpeed_Bands) then
         raise Violated_precondition;
      end if;
      pragma Assert (definitions.Are_Legitimate_Bands (Recovery_GroundSpeed_Bands));
   end CreateRecoveryGroundSpeedBands;
   
   procedure CreateIntruderInfo (LMCP_Intruders : IDType_seq;
                                 LMCP_ttlowcs : ttlowc_seq;
                                 Intruders : aliased out Intruder_info_Vector) 
     with Exceptional_Cases =>
       (Inconsistent_Message => MyVectorOfIntruderInfo.Is_Empty (Intruders));
   
   procedure CreateIntruderInfo (LMCP_Intruders : IDType_seq;
                                 LMCP_ttlowcs : ttlowc_seq;
                                 Intruders : aliased out Intruder_info_Vector) 
   is
      result : Intruder_info_Vector;
      --  EmptyVector : Intruder_info_Vector;
      --  test1 : MyVectorOfIntruderInfo.Vector := MyVectorOfIntruderInfo.Empty_Vector;
   begin
      if IDType_sequences.Last (LMCP_Intruders) = ttlowc_sequences.Last 
        (LMCP_ttlowcs)
      then
         --Assumption to enforce sequence is less than the maximum capacity of 
         --the Intruder vector being created------------------------------------
         pragma Assume (IDType_sequences.Last (LMCP_Intruders) <= 
                          Integer (MyVectorOfIntruderInfo.Capacity (result)));
         for Index in IDType_sequences.First ..
           IDType_sequences.Last (LMCP_Intruders) loop
            pragma Loop_Invariant (Integer (MyVectorOfIntruderInfo.Length 
                                   (result)) < Index);
            declare
               temp : definitions.Intruder_info;
            begin
               temp.Intruder_ID := IDType_sequences.Get (LMCP_Intruders, Index);
               temp.Intruder_time_to_violation := ttlowc_sequences. Get 
                 (LMCP_ttlowcs, Index);
               definitions.MyVectorOfIntruderInfo.Append
                 (result, temp);
            end;
         end loop;
         Intruders := result;
      else
         --  Intruders := MyVectorOfIntruderInfo.Empty_Vector;
         Intruders := result;
         raise Inconsistent_Message;
      end if;
      
   end CreateIntruderInfo;
   
   procedure ArePreconditionsSatisfied 
     (DAIDALUS_Altitude_Bands : definitions.OrderedIntervalVector32;
      DAIDALUS_Heading_Bands : definitions.OrderedIntervalVector;
      DAIDALUS_GroundSpeed_Bands : definitions.OrderedIntervalVector;
      Recovery_Altitude_Bands : definitions.OrderedIntervalVector32;
      Recovery_Heading_Bands : definitions.OrderedIntervalVector;
      Recovery_GroundSpeed_Bands : definitions.OrderedIntervalVector;
      DAIDALUS_Altitude_Zones : definitions.ZoneVector;
      DAIDALUS_Heading_Zones : definitions.ZoneVector;
      DAIDALUS_GroundSpeed_Zones : definitions.ZoneVector;
      State_ReadyToAct : Boolean;
      State_Status : definitions.Status_Type;
      Config_PriorityTimeThreshold_sec : definitions.priority_time_sec;
      Config_ActionTimeThreshold_sec : definitions.action_time_sec;
      IsSatisfied : aliased out Boolean) with 
     Exceptional_Cases => 
       (Violated_precondition => IsSatisfied = False), 
       Post => 
         IsSatisfied and then State_Status /= InConflict and then
       State_ReadyToAct and then 
       Config_PriorityTimeThreshold_sec < Config_ActionTimeThreshold_sec and then
       definitions.Are_Legitimate_Bands (DAIDALUS_Altitude_Bands) and then
       definitions.Are_Legitimate_Bands (DAIDALUS_Heading_Bands) and then
       definitions.Are_Legitimate_Bands (DAIDALUS_GroundSpeed_Bands) and then
       definitions.Are_Legitimate_Bands (Recovery_Altitude_Bands) and then
       definitions.Are_Legitimate_Bands (Recovery_Heading_Bands) and then
       definitions.Are_Legitimate_Bands (Recovery_GroundSpeed_Bands) and then
     CheckSafeToReturn.SameIndices (DAIDALUS_Altitude_Bands,
                                            DAIDALUS_Altitude_Zones)
     and then CheckSafeToReturn.SameIndices (DAIDALUS_Heading_Bands,
                                            DAIDALUS_Heading_Zones)
     and then CheckSafeToReturn.SameIndices (DAIDALUS_GroundSpeed_Bands,
                                            DAIDALUS_GroundSpeed_Zones);

   procedure ArePreconditionsSatisfied 
     (DAIDALUS_Altitude_Bands : definitions.OrderedIntervalVector32;
      DAIDALUS_Heading_Bands : definitions.OrderedIntervalVector;
      DAIDALUS_GroundSpeed_Bands : definitions.OrderedIntervalVector;
      Recovery_Altitude_Bands : definitions.OrderedIntervalVector32;
      Recovery_Heading_Bands : definitions.OrderedIntervalVector;
      Recovery_GroundSpeed_Bands : definitions.OrderedIntervalVector;
      DAIDALUS_Altitude_Zones : definitions.ZoneVector;
      DAIDALUS_Heading_Zones : definitions.ZoneVector;
      DAIDALUS_GroundSpeed_Zones : definitions.ZoneVector;
      State_ReadyToAct : Boolean;
      State_Status : definitions.Status_Type;
      Config_PriorityTimeThreshold_sec : definitions.priority_time_sec;
      Config_ActionTimeThreshold_sec : definitions.action_time_sec;
      IsSatisfied : aliased out Boolean) is
   begin
      if State_ReadyToAct and then 
        State_Status /= definitions.InConflict and then
        Config_PriorityTimeThreshold_sec < Config_ActionTimeThreshold_sec and then
        definitions.Are_Legitimate_Bands (DAIDALUS_Altitude_Bands) and then
        definitions.Are_Legitimate_Bands (DAIDALUS_Heading_Bands) and then
        definitions.Are_Legitimate_Bands (DAIDALUS_GroundSpeed_Bands) and then
        definitions.Are_Legitimate_Bands (Recovery_Altitude_Bands) and then
        definitions.Are_Legitimate_Bands (Recovery_Heading_Bands) and then
        definitions.Are_Legitimate_Bands (Recovery_GroundSpeed_Bands) and then
        CheckSafeToReturn.SameIndices (DAIDALUS_Altitude_Bands,
                                                DAIDALUS_Altitude_Zones)
        and then CheckSafeToReturn.SameIndices (DAIDALUS_Heading_Bands,
                                                DAIDALUS_Heading_Zones)
        and then CheckSafeToReturn.SameIndices (DAIDALUS_GroundSpeed_Bands,
                                                DAIDALUS_GroundSpeed_Zones)
      then 
         IsSatisfied := True;
      else
         IsSatisfied := False;
         --  Put_Line ("Checking the preconditions is the problem.");
         raise Violated_precondition;
      end if;
   end ArePreconditionsSatisfied;
   
   -- __TODO__
   -- Include any local types or use clauses you would like to have.
   --
   -- __Example__
   --
   -- use all type Pos64_Nat64_Maps.Formal_Model.M.Map;
   -- use Pos64_Vectors.Formal_Model.M;

   -- __TODO__
   -- Declare and define bodies for any local subprograms used internally in the
   -- body of the package. This may include helper subprograms or ghost code
   -- (e.g. lemmas) to help with proof.
   -- 
   -- __Example__
   -- 
   -- procedure Lemma_Mod_Incr (A : Natural; B : Positive) with
   --   Ghost,
   --   Pre => A < Integer'Last,
   --   Post =>
   --     (if A mod B = B - 1 then (A + 1) mod B = 0
   --        else (A + 1) mod B = A mod B + 1);
   -- 
   -- procedure Lemma_Mod_Incr (A : Natural; B : Positive) is null;
   --
   -- --------------------
   -- -- Construct_Path --
   -- --------------------
   -- 
   -- procedure Construct_Path (...) with
   --   Pre => ...
   --   Post => ...;
   -- 
   -- procedure Construct_Path (...)
   -- is
   --   ...
   -- begin
   --   ...
   -- end Construct_Path;

   -- __TODO__
   -- Define bodies for any subprograms declared in the package specification.
   -- These are likely to include procedures to handle SPARK-compatible LMCP
   -- messages (by convention named `Handle_<MessageType>`), along with other
   -- SPARK subprograms needed by the service. Note that procedures that send
   -- SPARK-compatible LMCP messages directly should include the service's
   -- mailbox as a parameter. Also, as a general tip for proof, subprograms that
   -- have complex contracts and operate on the state should in their
   -- implementations rely on helper subprograms that operate over *only* the
   -- required fields of the state and have contracts that can be leveraged for
   -- proof of the original subprogram's contract. This modularizes proof and
   -- minimizes context for the provers, making proof more tractable.
   -- 
   -- __Example Stubs__
   --
   -- ---------------------------
   -- -- Handle_MissionCommand --
   -- ---------------------------
   --
   -- procedure Handle_MissionCommand
   --   (State : in out <Service_Name>_State;
   --    MC : MissionCommand)
   -- is
   --    ...
   -- begin
   --    ...
   -- end Handle_MissionCommand;
   --
   -- ---------------------
   -- -- Produce_Segment --
   -- ---------------------
   -- 
   -- procedure Produce_Segment
   --   (State : in out <Service_Name>_State;
   --    Config : <Service_Name>_Configuration_Data;
   --    Mailbox : in out <Service_Name>_Mailbox)
   -- is
   --   ...
   -- begin
   --   ...
   --   Construct_Path (...);
   --   ... 
   -- end Produce_Segment;
   procedure Process_WellclearViolation_Message 
     (m_DAIDALUSResponseServiceState : in out Daidalus_Response_State;
      m_DAIDALUSResponseServiceConfig : Daidalus_Response_Configuration_Data;
      m_DAIDALUSResponseServiceMailbox : in out Daidalus_Response_Mailbox;
      WCV_Intervals : LMCP_Messages.WellClearViolationIntervals) is
      WCVdata : WCV_data;
      BandsSurrogate : aliased definitions.OrderedIntervalVector;
      BandsSurrogate32 : aliased definitions.OrderedIntervalVector32;
      IntrudersSurrogate : aliased definitions.Intruder_info_Vector;
      PreconditionsMetSurrogate : aliased Boolean;
      StatusSurrogate : aliased definitions.Status_Type;
      DivertStateSurrogate : definitions.state_parameters;
      RoWghostSurrogate : definitions.ID_Type;
      ConflictResolutionListghostSurrogate : definitions.VehicleIDsVector;
      SendNewMissionCommandghostSurrogate : Boolean;
      SendDivertActionCommandghostSurrogate : Boolean;
   begin
      if Common.Int64 (WCV_Intervals.EntityID) = m_DAIDALUSResponseServiceConfig.
        VehicleID
      then
         --Configure WCVdata object with paramater for automatic response-------
         WCVdata.CurrentState.altitude_m := WCV_Intervals.CurrentAltitude;
         WCVdata.CurrentState.groundSpeed_mps := 
           WCV_Intervals.CurrentGroundSpeed;
         WCVdata.CurrentState.heading_deg := WCV_Intervals.CurrentHeading;
         WCVdata.CurrentState.verticalSpeed_mps := WCV_Intervals.
           CurrentVerticalSpeed;
         WCVdata.CurrentState.latitude_deg := WCV_Intervals.CurrentLatitude;
         WCVdata.CurrentState.longitude_deg := WCV_Intervals.CurrentLongitude;
         CreateAltitudeBands
           (LMCP_Altitudes          => WCV_Intervals.WCVAltitudeIntervals,
            LMCP_AltitudeZone       => WCV_Intervals.WCVAltitudeRegions,
            DAIDALUS_Altitude_Bands => BandsSurrogate32); 
         WCVdata.AltitudeBands := BandsSurrogate32;
         WCVdata.AltitudeZones := CreateZones 
           (WCV_Intervals.WCVAltitudeRegions);
         CreateHeadingBands 
           (LMCP_Headings          => WCV_Intervals.WCVGroundHeadingIntervals,
            LMCP_HeadingZone       => WCV_Intervals.WCVGroundHeadingRegions,
            DAIDALUS_Heading_Bands => BandsSurrogate);
         WCVdata.HeadingBands := BandsSurrogate;
         WCVdata.HeadingZones := CreateZones 
           (WCV_Intervals.WCVGroundHeadingRegions);
         CreateGroundSpeedBands
           (LMCP_GroundSpeed           => WCV_Intervals.WCVGroundSpeedIntervals,
            LMCP_GroundSpeedZone       => WCV_Intervals.WCVGroundSpeedRegions,
            DAIDALUS_GroundSpeed_Bands => BandsSurrogate);
         WCVdata.GroundspeedBands := BandsSurrogate;
         WCVdata.GroundspeedZones := CreateZones 
           (WCV_Intervals.WCVGroundSpeedRegions);
         CreateIntruderInfo
           (LMCP_Intruders => WCV_Intervals.EntityList,
            LMCP_ttlowcs   => WCV_Intervals.TimeToViolationList,
            Intruders      => IntrudersSurrogate);
         WCVdata.IntrudersInfo := IntrudersSurrogate;

         --TODO finish else ladder for throwing an exception-----------------
         ArePreconditionsSatisfied
              (DAIDALUS_Altitude_Bands          => WCVdata.AltitudeBands,
               DAIDALUS_Heading_Bands           => WCVdata.HeadingBands,
               DAIDALUS_GroundSpeed_Bands       => WCVdata.GroundspeedBands,
               Recovery_Altitude_Bands          => WCVdata.RAltitudeBands,
               Recovery_Heading_Bands           => WCVdata.RHeadingBands,
               Recovery_GroundSpeed_Bands       => WCVdata.RGroundspeedBands,
               DAIDALUS_Altitude_Zones          => WCVdata.AltitudeZones,
               DAIDALUS_Heading_Zones           => WCVdata.HeadingZones,
               DAIDALUS_GroundSpeed_Zones       => WCVdata.GroundspeedZones,
               State_ReadyToAct                 => 
                 m_DAIDALUSResponseServiceState.ReadyToAct,
               State_Status                     => 
                 m_DAIDALUSResponseServiceState.Status,
               Config_PriorityTimeThreshold_sec => 
                 m_DAIDALUSResponseServiceConfig.PriorityTimeThreshold,
               Config_ActionTimeThreshold_sec   => 
                 m_DAIDALUSResponseServiceConfig.ActionTimeThreshold,
               IsSatisfied                      => PreconditionsMetSurrogate);
         m_DAIDALUSResponseServiceState.PreconditionsMet := 
           PreconditionsMetSurrogate;
         if  m_DAIDALUSResponseServiceState.PreconditionsMet 
         then
            StatusSurrogate := m_DAIDALUSResponseServiceState.Status;
            automatic_response.Process_DAIDALUS_Bands
              (Mailbox                          => 
                 m_DAIDALUSResponseServiceMailbox,
               Current_State                    => WCVdata.CurrentState,
               Divert_State                     => DivertStateSurrogate,
               DAIDALUS_Altitude_Bands          => WCVdata.AltitudeBands,
               DAIDALUS_Heading_Bands           => WCVdata.HeadingBands,
               DAIDALUS_GroundSpeed_Bands       => WCVdata.GroundspeedBands,
               Recovery_Altitude_Bands          => WCVdata.RAltitudeBands,
               Recovery_Heading_Bands           => WCVdata.RHeadingBands,
               Recovery_GroundSpeed_Bands       => WCVdata.RGroundspeedBands,
               m_Vehicle_ID                     => 
                 m_DAIDALUSResponseServiceConfig.VehicleID,
               Intruders                        => WCVdata.IntrudersInfo,
               DAIDALUS_Altitude_Zones         => WCVdata.AltitudeZones,
               DAIDALUS_Heading_Zones           => WCVdata.HeadingZones,
               DAIDALUS_GroundSpeed_Zones       => WCVdata.GroundspeedZones,
               m_isReady_to_Act                 => 
                 m_DAIDALUSResponseServiceState.ReadyToAct,
               m_Action_Time_Thresold_s         => 
                 m_DAIDALUSResponseServiceConfig.ActionTimeThreshold,
               m_Priority_Time_Threshold_s      => 
                 m_DAIDALUSResponseServiceConfig.PriorityTimeThreshold,
               m_Status                         => 
                 StatusSurrogate,
               m_NextWaypoint                   => 
                 m_DAIDALUSResponseServiceState.NextWaypoint,
               Altitude_Max_m                   => 
                 m_DAIDALUSResponseServiceState.Altitude_Max_m,
               Altitude_Min_m                   => 
                 m_DAIDALUSResponseServiceState.Altitude_Min_m,
               Altitude_Interval_Buffer_m       => 
                 m_DAIDALUSResponseServiceState.Altitude_Interval_Buffer_m,
               Heading_Max_deg                  => 
                 m_DAIDALUSResponseServiceState.Heading_Max_deg,
               Heading_Min_deg                  => 
                 m_DAIDALUSResponseServiceState.Heading_Min_deg,
               Heading_Interval_Buffer_deg      => 
                 m_DAIDALUSResponseServiceState.Heading_Interval_Buffer_deg,
               GroundSpeed_Max_mps              => 
                 m_DAIDALUSResponseServiceState.GroundSpeed_Max_mps,
               GroundSpeed_Min_mps              => 
                 m_DAIDALUSResponseServiceState.GroundSpeed_Min_mps,
               GroundSpeed_Interval_Buffer_mps  => 
                 m_DAIDALUSResponseServiceState.GroundSpeed_Interval_Buffer_mps,
               Is_Tracking_Next_Waypoint        => 
                 m_DAIDALUSResponseServiceState.IsTrackingNextWaypoint,
               m_MissionCommand                 => 
                 m_DAIDALUSResponseServiceState.MissionCommand,
               RoW_ghost                        => RoWghostSurrogate,
               ConflictResolutionList_ghost     => 
                 ConflictResolutionListghostSurrogate,
               SendNewMissionCommand_ghost      => 
                 SendNewMissionCommandghostSurrogate,
               Send_Divert_Action_Command_ghost => 
                 SendDivertActionCommandghostSurrogate);
            m_DAIDALUSResponseServiceState.Status := StatusSurrogate;

         end if;
        
      end if;
   exception
      when Inconsistent_Message =>
         Put_Line 
           ("Problem with data in WellClearViolation message. Not activating automatic response. ");
      when Violated_precondition =>
         Put_Line ("Problem satisfying preconditions to ensure absence of runtime errors in functional behavior.");
      --  when automatic_response.Violated_precondition2 =>
      --     Put_Line ("Problem with satisfying preconditions on contraints.");
      
   end Process_WellclearViolation_Message;
   
   procedure Process_DAIDALUSConfiguration_Message 
     (m_DAIDALUSResponseServiceState : in out Daidalus_Response_State;
      m_DAIDALUSResponseServiceConfig : Daidalus_Response_Configuration_Data;
      ConfigurationMessage : LMCP_Messages.DAIDALUSConfiguration)
   is 
   begin
      --Set State parameters from DAIDALUSConfiguration message when message is 
      --for configured ownship -------------------------------------------------
      if Common.Int64 (ConfigurationMessage.EntityID) = 
        m_DAIDALUSResponseServiceConfig.VehicleID 
      then
         m_DAIDALUSResponseServiceState.ReadyToAct := True;
         m_DAIDALUSResponseServiceState.Altitude_Min_m := ConfigurationMessage.
           MinAltitude;
         m_DAIDALUSResponseServiceState.Altitude_Max_m := ConfigurationMessage.
           MaxAltitude;
         m_DAIDALUSResponseServiceState.Altitude_Interval_Buffer_m := 
           ConfigurationMessage.AltitudeStep / 2.0;
         m_DAIDALUSResponseServiceState.Heading_Interval_Buffer_deg := 
           ConfigurationMessage.TrackStep / 2.0;
         m_DAIDALUSResponseServiceState.GroundSpeed_Interval_Buffer_mps := 
           ConfigurationMessage.GroundSpeedStep / 2.0;
         m_DAIDALUSResponseServiceState.GroundSpeed_Min_mps :=
           ConfigurationMessage.MinGroundSpeed;
         m_DAIDALUSResponseServiceState.GroundSpeed_Max_mps := 
           ConfigurationMessage.MaxGroundSpeed;
         
      end if;

   end Process_DAIDALUSConfiguration_Message;
   
   procedure Process_MissionCommand_Message 
     (m_DAIDALUSResponseServiceState : in out Daidalus_Response_State;
      m_DAIDALUSResponseServiceConfig : Daidalus_Response_Configuration_Data;
      MissionCommandMessage : LMCP_Messages.MissionCommand) is
      SettingState : definitions.MissionCommand;
      use all type Int64_Seq; 
      use all type VA_Seq;
      use all type WP_Seq;
   begin
      if MissionCommandMessage.VehicleId = m_DAIDALUSResponseServiceConfig.
        VehicleID
      then
         SettingState.command_id := Common.Int64 (MissionCommandMessage.CommandId);
         SettingState.vehicle_id := Common.Int64 (MissionCommandMessage.VehicleId);
         case MissionCommandMessage.Status is
         when Pending => SettingState.status := definitions.Pending;
         when Approved => SettingState.status := definitions.Approved;
         when InProcess => SettingState.status := definitions.InProcess;
         when Executed => SettingState.status := definitions.Executed;
         when Cancelled => SettingState.status := definitions.Cancelled;
         end case;
         declare
            temp_val : definitions.VehicleActionList;
         begin
         
            if Last (MissionCommandMessage.VehicleActionList) <= Integer 
                    (MyVectorOfVehicleActions.Capacity (temp_val))
            then
               --  for val of MissionCommandMessage.VehicleActionList loop
               for Index1 in Positive'First .. Last 
                 (MissionCommandMessage.VehicleActionList) loop
                  pragma Loop_Invariant (Integer (MyVectorOfVehicleActions.
                                           Length (temp_val)) = (Index1 - 1));
                  declare
                     val : LMCP_Messages.VehicleAction := Get 
                       (MissionCommandMessage.VehicleActionList, Index1);
                     temp_atl : definitions.Associated_Tasks_List;
                     vehicleaction : definitions.VehicleAction;
                  begin
                     --Due to transcription from sequence to vector an assumption
                     --is used to convey the relationship between the capacity of 
                     --vector and the number of items transcribed------------------
                     if Last (val.AssociatedTaskList) <= Integer
                             (MyVectorOfIntegers.Capacity (temp_atl))
                     then
                        pragma Assert (MyVectorOfIntegers.Length (temp_atl) = 0);
                        --  for atl of val.AssociatedTaskList loop
                        --     Increment_Ghost_Index (Index);
                        for Index2 in Positive'First .. Last (val.AssociatedTaskList) loop
                           pragma Loop_Invariant (Integer (MyVectorOfIntegers.
                                                    Length (temp_atl)) = 
                                                  (Index2 - 1));
                           --  MyVectorOfIntegers.Append (temp_atl, Common.Int64 (atl));
                           MyVectorOfIntegers.Append (temp_atl, Get 
                                                      (val.AssociatedTaskList, 
                                                         Index2));
                        end loop;
                        vehicleaction.AssociatedTaskList := temp_atl;
                        MyVectorOfVehicleActions.Append (temp_val, vehicleaction);
                     else
                        raise Inconsistent_Message;
                     end if;
                     
                  end;
               end loop;
            else
               raise Inconsistent_Message;
            end if;
                    
            SettingState.vehicle_action_list := temp_val;
         end;
         SettingState.first_waypoint := Common.Int64 (MissionCommandMessage.FirstWaypoint);
         declare
            temp_waypoint_list : definitions.WaypointList;
         begin
            --  for waypointlist of MissionCommandMessage.WaypointList loop
            for II in WP_Sequences.First .. 
              Last (MissionCommandMessage.WaypointList) loop
               pragma Loop_Invariant (Integer (MyVectorOfWaypoints.Length 
                                      (temp_waypoint_list)) = II - 1);
               declare
                  waypointlist : LMCP_Messages.Waypoint := Get 
                    (MissionCommandMessage.WaypointList, II);
                  temp_waypoint : definitions.Waypoint_info;
               begin
                  temp_waypoint.waypoint_number := Common.Int64 
                    (waypointlist.Number);
                  temp_waypoint.next_waypoint := Common.Int64 
                    (waypointlist.NextWaypoint);
                  if waypointlist.Speed in Common.Real32 
                    (GroundSpeed_Type_mps'First) .. Common.Real32 
                    (GroundSpeed_Type_mps'Last) 
                  then
                     temp_waypoint.speed := definitions.GroundSpeed_Type_mps 
                       (waypointlist.Speed);
                  else
                     raise Improper_Configuration;
                  end if;
                  case waypointlist.SpeedType is
                  when Airspeed => temp_waypoint.speed_type := 
                       definitions.Airspeed;
                  when Groundspeed => temp_waypoint.speed_type :=
                       definitions.Groundspeed;
                  end case;
                  if waypointlist.ClimbRate in Common.Real32 
                    (VerticalSpeed_Type_mps'First) .. Common.Real32 
                    (VerticalSpeed_Type_mps'Last) 
                  then
                     temp_waypoint.climb_rate := definitions.VerticalSpeed_Type_mps 
                       (waypointlist.ClimbRate);
                  else 
                     raise Improper_Configuration;
                  end if;
                  case waypointlist.TurnType is 
                  when TurnShort => temp_waypoint.turn_type := 
                       definitions.TurnShort;
                  when FlyOver => temp_waypoint.turn_type := 
                       definitions.FlyOver;
                  end case;
                  declare
                     val : definitions.VehicleActionList;
                  begin
                     --  for lmcp_val of waypointlist.VehicleActionList loop
                     if Last (waypointlist.VehicleActionList) <= Integer 
                       (MyVectorOfVehicleActions.Capacity (val))
                     then
                           
                        for Index1 in Positive'First .. Last 
                          (waypointlist.VehicleActionList) loop
                           pragma Loop_Invariant (Integer 
                                                  (MyVectorOfVehicleActions.
                                                       Length (val)) = 
                                                    Index1 - 1);
                           declare
                              lmcp_val : LMCP_Messages.VehicleAction := Get 
                                (waypointlist.VehicleActionList, Index1);
                              val_atl : definitions.Associated_Tasks_List;
                              vehicleaction : definitions.VehicleAction;
                           begin
                              if Last (lmcp_val.AssociatedTaskList) <= Integer 
                                (MyVectorOfIntegers.Capacity (val_atl)) 
                              then
                                 for Index2 in Positive'First .. Last 
                                   (lmcp_val.AssociatedTaskList) loop
                                    pragma Loop_Invariant (Integer 
                                                           (MyVectorOfIntegers.
                                                                Length (val_atl)) 
                                                           = Index2 - 1);
                                    MyVectorOfIntegers.Append (val_atl, Get 
                                                               (lmcp_val.
                                                                    AssociatedTaskList
                                                                  , Index2));
                                 end loop;
                                 vehicleaction.AssociatedTaskList := val_atl;
                                 MyVectorOfVehicleActions.Append 
                                   (val, vehicleaction);
                              else
                                 raise Inconsistent_Message;
                              end if;
                                 
                           end;
                           
                        end loop;
                        temp_waypoint.vehicle_action_list := val;
                     else
                        raise Inconsistent_Message;
                     end if;
                  end;
                  temp_waypoint.contingency_waypoint_A := Common.Int64 
                    (waypointlist.ContingencyWaypointA);
                  temp_waypoint.contingency_waypoint_B := Common.Int64 
                    (waypointlist.ContingencyWaypointB);
                  declare
                     wp_atl : definitions.Associated_Tasks_List;
                  begin
                     if Last (waypointlist.AssociatedTasks) <= Integer 
                       (MyVectorOfIntegers.Capacity (wp_atl))
                     then
                           
                        --  for atl of waypointlist.AssociatedTasks loop
                        for Index in Positive'First .. Last 
                          (waypointlist.AssociatedTasks) loop
                           pragma Loop_Invariant (Integer (MyVectorOfIntegers.
                                                    Length (wp_atl)) = Index 
                                                  - 1);
                           MyVectorOfIntegers.Append (wp_atl, Get 
                                                      (waypointlist.AssociatedTasks, Index));
                        end loop;
                        temp_waypoint.associated_tasks := wp_atl;
                     else
                        raise Inconsistent_Message;
                     end if;
                        
                  end;
                  if waypointlist.Longitude in definitions.
                    longitude_type_deg'First .. definitions.
                      longitude_type_deg'Last
                  then
                     temp_waypoint.longitude_deg := definitions.
                       longitude_type_deg (waypointlist.Longitude);
                  else 
                     raise Improper_Configuration;
                  end if;
                  if waypointlist.Latitude in definitions.
                    latitude_type_deg'First ..  definitions.
                      latitude_type_deg'Last
                  then
                     temp_waypoint.latitude_deg := definitions.latitude_type_deg 
                       (waypointlist.Latitude);
                  else
                     raise Improper_Configuration;
                  end if;
                  if waypointlist.Altitude in Altitude_Type_m'First .. 
                    Altitude_Type_m'Last
                  then
                     temp_waypoint.altitude_m := definitions.Altitude_Type_m 
                       (waypointlist.Altitude);
                  else
                     raise Improper_Configuration;
                  end if;
                  case waypointlist.AltitudeType is
                     when MSL => temp_waypoint.altitude_type := definitions.MSL;
                     when AGL => temp_waypoint.altitude_type := definitions.AGL;
                  end case;
                  if MyVectorOfWaypoints.Length (temp_waypoint_list) < 
                    MyVectorOfWaypoints.Capacity (temp_waypoint_list) 
                  then
                     MyVectorOfWaypoints.Append 
                       (temp_waypoint_list, temp_waypoint);
                  else
                     raise Improper_Configuration;
                  end if;
                  
               end;
            end loop;
            SettingState.waypoint_list := temp_waypoint_list;
         end;
         SettingState.is_safe_to_access := True;
         m_DAIDALUSResponseServiceState.MissionCommand := SettingState;
      end if;
   exception
      when Inconsistent_Message =>
         --  SettingState.is_safe_to_access := False;
         Put_Line ("Problem with data in MissionCommand message.  Unable to return to Mission.");
      when Improper_Configuration =>
         --  SettingState.is_safe_to_access := False;
         Put_Line ("MissionCommand message does not conform to current configuration parameters. Unalbe to return to Mission.");
      
   end Process_MissionCommand_Message;

end Daidalus_Response;
