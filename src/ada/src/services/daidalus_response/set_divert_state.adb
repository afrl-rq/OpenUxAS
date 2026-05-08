package body set_divert_state with
SPARK_Mode => On is

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
      Priority : Priority_Type) is

      --found_acceptable_action_flag : Boolean;

   begin
      case Priority is
      when pStandard =>
         Found_WCV_Altitude_Resolution
           (DAIDALUS_Altitude_Bands  => DAIDALUS_Altitude_Bands,
            Recovery_Altitude_Bands  => Recovery_Altitude_Bands,
            Current_State            => Current_State,
            Altitude_Max_m           => Altitude_Max_m,
            Altitude_Min_m           => Altitude_Min_m,
            Altitude_Interval_Buffer_m => Altitude_Interval_Buffer_m,
            Divert_State             => Divert_State,
            found_acceptable_action_flag    => found_acceptable_action_flag);
         pragma Assert (if found_acceptable_action_flag then
                           Divert_Altitude_Successful
                          (found_acceptable_action_flag   =>
                             found_acceptable_action_flag,
                           DAIDALUS_Altitude_Bands =>
                             DAIDALUS_Altitude_Bands,
                           Recovery_Altitude_Bands => Recovery_Altitude_Bands,
                           Divert_State            => Divert_State));
         if not found_acceptable_action_flag then
            Found_WCV_Heading_Resolution
              (DAIDALUS_Heading_Bands      => DAIDALUS_Heading_Bands,
               Recovery_Heading_Bands      => Recovery_Heading_Bands,
               Current_State               => Current_State,
               Heading_Max_deg             => Heading_Max_deg,
               Heading_Min_deg             => Heading_Min_deg,
               Heading_Interval_Buffer_deg => Heading_Interval_Buffer_deg,
               Divert_State                => Divert_State,
               found_acceptable_action_flag       =>
                 found_acceptable_action_flag);
            pragma Assert (if found_acceptable_action_flag then
                              Divert_Heading_Successful
                             (found_acceptable_action_flag  =>
                                found_acceptable_action_flag,
                              DAIDALUS_Heading_Bands => DAIDALUS_Heading_Bands,
                              Recovery_Heading_Bands => Recovery_Heading_Bands,
                              Divert_State           => Divert_State));
            if not found_acceptable_action_flag then
               Found_WCV_GroundSpeed_Resolution
                 (DAIDALUS_GroundSpeed_Bands      => DAIDALUS_GroundSpeed_Bands,
                  Recovery_GroundSpeed_bands      => Recovery_GroundSpeed_Bands,
                  Current_State                   => Current_State,
                  GroundSpeed_Max_mps             => GroundSpeed_Max_mps,
                  GroundSpeed_Min_mps             => GroundSpeed_Min_mps,
                  GroundSpeed_Interval_Buffer_mps =>
                    GroundSpeed_Interval_Buffer_mps,
                  Divert_State                    => Divert_State,
                  found_acceptable_action_flag           =>
                    found_acceptable_action_flag);
               pragma Assert (if found_acceptable_action_flag then
                                 Divert_GroundSpeed_Successful
                                (found_acceptable_action_flag      =>
                                   found_acceptable_action_flag,
                                 DAIDALUS_GroundSpeed_Bands =>
                                   DAIDALUS_GroundSpeed_Bands,
                                 Recovery_GroundSpeed_Bands =>
                                   Recovery_GroundSpeed_Bands,
                                 Divert_State               => Divert_State));
            end if;
         end if;

         pragma Assert (if not found_acceptable_action_flag and then not
                      MyVectorOfIntervals.Is_Empty (DAIDALUS_GroundSpeed_Bands)
                       then Divert_Fallback (found_acceptable_action_flag,
                         DAIDALUS_GroundSpeed_Bands, DAIDALUS_Altitude_Bands,
                         Divert_State, GroundSpeed_Min_mps, Altitude_Max_m,
                         Priority));
         pragma Assert (if not found_acceptable_action_flag and then
                      MyVectorOfIntervals.Is_Empty (DAIDALUS_GroundSpeed_Bands)
                       then Divert_No_Recourse (found_acceptable_action_flag,
                         DAIDALUS_GroundSpeed_Bands, DAIDALUS_Altitude_Bands,
                         Divert_State, Current_State, Priority));
      when pHigh =>
         Found_WCV_GroundSpeed_Resolution (DAIDALUS_GroundSpeed_Bands,
                                          Recovery_GroundSpeed_Bands,
                                          Current_State, GroundSpeed_Max_mps,
                                          GroundSpeed_Min_mps,
                                          GroundSpeed_Interval_Buffer_mps,
                                          Divert_State,
                                          found_acceptable_action_flag);
         pragma Assert (if found_acceptable_action_flag then
                          Divert_GroundSpeed_Successful (
                         found_acceptable_action_flag,
                         DAIDALUS_GroundSpeed_Bands, Recovery_GroundSpeed_Bands,
                         Divert_State));
         if not found_acceptable_action_flag then
            Found_WCV_Heading_Resolution (DAIDALUS_Heading_Bands,
                                          Recovery_Heading_Bands,
                                          Current_State, Heading_Max_deg,
                                          Heading_Min_deg,
                                          Heading_Interval_Buffer_deg,
                                          Divert_State,
                                          found_acceptable_action_flag);
            pragma Assert (if found_acceptable_action_flag then
                             Divert_Heading_Successful (
                            found_acceptable_action_flag,
                            DAIDALUS_Heading_Bands, Recovery_Heading_Bands,
                            Divert_State));
            if not found_acceptable_action_flag then
               Found_WCV_Altitude_Resolution (DAIDALUS_Altitude_Bands,
                                             Recovery_Altitude_Bands,
                                             Current_State, Altitude_Max_m,
                                             Altitude_Min_m,
                                             Altitude_Interval_Buffer_m,
                                             Divert_State,
                                             found_acceptable_action_flag);
               pragma Assert (if found_acceptable_action_flag then
                                 Divert_Altitude_Successful
                              (found_acceptable_action_flag,
                               DAIDALUS_Altitude_Bands, Recovery_Altitude_Bands,
                               Divert_State));
            end if;
         end if;

         pragma Assert (if not found_acceptable_action_flag and then not
                       MyVectorOfIntervals32.Is_Empty (DAIDALUS_Altitude_Bands)
                       then Divert_Fallback (found_acceptable_action_flag,
                         DAIDALUS_GroundSpeed_Bands, DAIDALUS_Altitude_Bands,
                         Divert_State, GroundSpeed_Min_mps, Altitude_Max_m,
                         Priority));
         pragma Assert (if not found_acceptable_action_flag and then
                         MyVectorOfIntervals32.Is_Empty (DAIDALUS_Altitude_Bands)
                       then Divert_No_Recourse (found_acceptable_action_flag,
                         DAIDALUS_GroundSpeed_Bands, DAIDALUS_Altitude_Bands,
                         Divert_State, Current_State, Priority));

         --  pragma Assert(Divert_Altitude_Successful(found_acceptable_action_flag,
         --                DAIDALUS_Altitude_Bands,
         --                Recovery_Altitude_Bands,
         --                Divert_State) or else
         --                Divert_Heading_Successful(found_acceptable_action_flag,
         --                  DAIDALUS_Heading_Bands,
         --                  Recovery_Heading_Bands,
         --                  Divert_State) or else
         --                Divert_GroundSpeed_Successful(found_acceptable_action_flag,
         --                  DAIDALUS_GroundSpeed_Bands,
         --                  Recovery_GroundSpeed_Bands,
         --                  Divert_State) or else
         --                Divert_Fallback(found_acceptable_action_flag,
         --                  DAIDALUS_GroundSpeed_Bands, DAIDALUS_Altitude_Bands,
         --                  Divert_State, GroundSpeed_Min_mps, Altitude_Max_m,
         --                  Priority) or else
         --                Divert_No_Recourse(found_acceptable_action_flag,
         --                  DAIDALUS_GroundSpeed_Bands,
         --                  DAIDALUS_Altitude_Bands, Divert_State, Current_State,
         --                  Priority));

      end case;

      --  pragma Assert(Divert_Altitude_Successful(found_acceptable_action_flag,
      --                                    DAIDALUS_Altitude_Bands,
      --                                    Recovery_Altitude_Bands,
      --                                    Divert_State) or else
      --         Divert_Heading_Successful(found_acceptable_action_flag,
      --                                   DAIDALUS_Heading_Bands,
      --                                   Recovery_Heading_Bands,
      --                                   Divert_State) or else
      --         Divert_GroundSpeed_Successful(found_acceptable_action_flag,
      --                                       DAIDALUS_GroundSpeed_Bands,
      --                                       Recovery_GroundSpeed_Bands,
      --                                       Divert_State) or else
      --         Divert_Fallback(found_acceptable_action_flag,
      --                        DAIDALUS_GroundSpeed_Bands, DAIDALUS_Altitude_Bands,
      --                        Divert_State, GroundSpeed_Min_mps, Altitude_Max_m,
      --                        Priority) or else
      --         Divert_No_Recourse(found_acceptable_action_flag,
      --                      DAIDALUS_GroundSpeed_Bands,
      --                      DAIDALUS_Altitude_Bands, Divert_State, Current_State,
      --                    Priority));

   end SetDivertState;

end set_divert_state;
