package body speed_resolution 
with SPARK_Mode => On is

   --predicate indicating whether or not the divert groundspeed is contained 
   --within a single recovery band interval
   function Successful_Placement_in_Recovery_Bands
     (Recovery_GroundSpeed_bands : OrderedIntervalVector;
      Divert_state : state_parameters) 
      return Boolean is
     (for some I in MyVectorOfIntervals.First_Index (Recovery_GroundSpeed_bands) 
      .. MyVectorOfIntervals.Last_Index (Recovery_GroundSpeed_bands) =>
           InRange (MyVectorOfIntervals.Element (Recovery_GroundSpeed_bands, I), 
        Divert_state.groundSpeed_mps)); 
   
   function Conflict_and_Recovery_Complimentary_Nature
     (Divert_State : state_parameters;
      DAIDALUS_GroundSpeed_Bands : OrderedIntervalVector;
      Recovery_GroundSpeed_Bands : OrderedIntervalVector;
      GroundSpeed_Min_mps : GroundSpeed_Type_mps;
      GroundSpeed_Max_mps : GroundSpeed_Type_mps) return Boolean is
     (if (Divert_State.groundSpeed_mps >= GroundSpeed_Min_mps) and then
        (Divert_State.groundSpeed_mps <= GroundSpeed_Max_mps) and then (not
        MyVectorOfIntervals.Is_Empty (DAIDALUS_GroundSpeed_Bands)) and then
        (not MyVectorOfIntervals.Is_Empty (Recovery_GroundSpeed_Bands)) and then
        (for all I in
          MyVectorOfIntervals.First_Index (DAIDALUS_GroundSpeed_Bands) ..
          MyVectorOfIntervals.Last_Index (DAIDALUS_GroundSpeed_Bands) =>
        not InRange (MyVectorOfIntervals.Element (DAIDALUS_GroundSpeed_Bands, I),
             Divert_State.groundSpeed_mps)) then
        (Successful_Placement_in_Recovery_Bands
             (Recovery_GroundSpeed_Bands, Divert_State))) with
     Ghost;   
   
   --Predicate describing the expected nature of recovery band intervals
   --recovery bands, when present, are the complement to conflict bands. Thus 
   --given the current state is in a conflict band there exists a recovery band 
   --interval that is completly above or below the current groundspeed
   function Expected_Recovery_Bands_Nature
     (Recovery_GroundSpeed_bands : OrderedIntervalVector;
      Current_state : state_parameters) return Boolean is
     ((for some I in MyVectorOfIntervals.First_Index
      (Recovery_GroundSpeed_bands)
          .. MyVectorOfIntervals.Last_Index (Recovery_GroundSpeed_bands) =>
        MyVectorOfIntervals.Element (Recovery_GroundSpeed_bands, I).LowerBound >
         Current_state.groundSpeed_mps and MyVectorOfIntervals.Element 
         (Recovery_GroundSpeed_bands, I).UpperBound > Current_state.
          groundSpeed_mps) or else
            (for some I in MyVectorOfIntervals.First_Index
               (Recovery_GroundSpeed_bands) .. MyVectorOfIntervals.Last_Index
               (Recovery_GroundSpeed_bands) =>
                    MyVectorOfIntervals.Element (Recovery_GroundSpeed_bands, I).
                   LowerBound < Current_state.groundSpeed_mps and
                    MyVectorOfIntervals.Element (Recovery_GroundSpeed_bands, I).
                   UpperBound < Current_state.groundSpeed_mps)) with Ghost;

   --sub program to determine which conflict band the current groundspeed is in 
   --and then set the divert groundspeed just past the upper bound of that 
   --interval loops over intervals starting from the lowest to highest
   procedure Find_Initial
     (Divert_State : state_parameters;
      is_Found : in out Boolean;
      initial_band_index : in out myvector_index_type;
      DAIDALUS_GroundSpeed_Bands : OrderedIntervalVector;
      Current_state : state_parameters;
      GroundSpeed_Max_mps : GroundSpeed_Type_mps;
      GroundSpeed_Min_mps : GroundSpeed_Type_mps;
      GroundSpeed_Interval_Buffer_mps : GroundSpeed_Buffer_Type_mps) with
     Pre => Are_Legitimate_Bands (DAIDALUS_GroundSpeed_Bands) and then 
            not MyVectorOfIntervals.Is_Empty (DAIDALUS_GroundSpeed_Bands)
            and then scalar_constraints
                      (Upper_limit         => GroundSpeed_Max_mps,
                       Lower_limit         => GroundSpeed_Min_mps,
                       Interval_Constraint => GroundSpeed_Interval_Buffer_mps)
            and then vector_constraints
                       (X                   => DAIDALUS_GroundSpeed_Bands,
                        Upper_limit         => GroundSpeed_Max_mps,
                        Lower_limit         => GroundSpeed_Min_mps,
                        Interval_constraint => GroundSpeed_Interval_Buffer_mps) 
            and then Divert_State = Current_state
            and then Current_GroundSpeed_Exists_in_Bands 
                       (Current_state, DAIDALUS_GroundSpeed_Bands)
            and then initial_band_index = MyVectorOfIntervals.First_Index
             (DAIDALUS_GroundSpeed_Bands) and then is_Found = False,
     Post => initial_band_index in MyVectorOfIntervals.First_Index
             (DAIDALUS_GroundSpeed_Bands) .. MyVectorOfIntervals.Last_Index
               (DAIDALUS_GroundSpeed_Bands) and then is_Found = True 
             and then InRange (MyVectorOfIntervals.Element 
                              (DAIDALUS_GroundSpeed_Bands, initial_band_index), 
                              Current_state.groundSpeed_mps);
   
   procedure Find_Initial
     (Divert_State : state_parameters;
      is_Found : in out Boolean;
      initial_band_index : in out myvector_index_type;
      DAIDALUS_GroundSpeed_Bands : OrderedIntervalVector;
      Current_state : state_parameters;
      GroundSpeed_Max_mps : GroundSpeed_Type_mps;
      GroundSpeed_Min_mps : GroundSpeed_Type_mps;
      GroundSpeed_Interval_Buffer_mps : GroundSpeed_Buffer_Type_mps) is
   begin
      
      for I in MyVectorOfIntervals.First_Index (DAIDALUS_GroundSpeed_Bands) ..
        MyVectorOfIntervals.Last_Index (DAIDALUS_GroundSpeed_Bands) loop

         if InRange (MyVectorOfIntervals.Element (DAIDALUS_GroundSpeed_Bands, I),
                     Divert_State.groundSpeed_mps) 
         then
            if not is_Found then
               initial_band_index := I;
               is_Found := True;
            end if;
         end if;
         pragma Loop_Invariant (not is_Found or else (initial_band_index <= I and 
                               then initial_band_index in MyVectorOfIntervals.
                                 First_Index (DAIDALUS_GroundSpeed_Bands) ..
                                 MyVectorOfIntervals.Last_Index
                                   (DAIDALUS_GroundSpeed_Bands)));
         pragma Loop_Invariant (not is_Found or else InRange (MyVectorOfIntervals.
                                 Element (DAIDALUS_GroundSpeed_Bands, 
                                   initial_band_index), Current_state.
                                 groundSpeed_mps));
         pragma Loop_Invariant (is_Found or else 
                                 (for all J in MyVectorOfIntervals.First_Index
                                  (DAIDALUS_GroundSpeed_Bands) .. I => 
                                       not InRange (MyVectorOfIntervals.Element
                                    (DAIDALUS_GroundSpeed_Bands, J), 
                                    Divert_State.groundSpeed_mps)));
         pragma Loop_Invariant (is_Found or else (for some J in I .. 
                                 MyVectorOfIntervals.Last_Index
                                   (DAIDALUS_GroundSpeed_Bands) =>
                                 InRange (MyVectorOfIntervals.Element
                                   (DAIDALUS_GroundSpeed_Bands, J), 
                                   Divert_State.groundSpeed_mps)));
      end loop;
      pragma Assert (is_Found);
   end Find_Initial;
   
   --subprogram that given the index to the conflict band that contains the 
   --current ground speed, loops in reverse and sets the divert groundspeed
   --to just under the lower band of the current conflict band being 
   --interrogated if before modification the divert groundspeed is contained in 
   --the conflict band.   
   procedure Slow_Down 
     (Divert_state : in out state_parameters;
      initial_band_index : myvector_index_type;
      DAIDALUS_GroundSpeed_Bands : OrderedIntervalVector;
      Current_State : state_parameters;
      GroundSpeed_Max_mps : GroundSpeed_Type_mps;
      GroundSpeed_Min_mps : GroundSpeed_Type_mps;
      GroundSpeed_Interval_Buffer_mps : GroundSpeed_Buffer_Type_mps) with
     Pre => Are_Legitimate_Bands (DAIDALUS_GroundSpeed_Bands) and then 
            not MyVectorOfIntervals.Is_Empty (DAIDALUS_GroundSpeed_Bands)
            and then scalar_constraints 
                       (Upper_limit         => GroundSpeed_Max_mps,
                        Lower_limit         => GroundSpeed_Min_mps,
                        Interval_Constraint => GroundSpeed_Interval_Buffer_mps) 
            and then vector_constraints 
                       (X                   => DAIDALUS_GroundSpeed_Bands,
                        Upper_limit         => GroundSpeed_Max_mps,
                        Lower_limit         => GroundSpeed_Min_mps,
                        Interval_constraint => GroundSpeed_Interval_Buffer_mps) 
           and then initial_band_index in MyVectorOfIntervals.First_Index
             (DAIDALUS_GroundSpeed_Bands) .. MyVectorOfIntervals.Last_Index
             (DAIDALUS_GroundSpeed_Bands)
           and then Divert_state = Current_State  and then 
             Current_GroundSpeed_Exists_in_Bands (Current_State, 
                                                 DAIDALUS_GroundSpeed_Bands) 
           and then InRange (MyVectorOfIntervals.Element 
                            (DAIDALUS_GroundSpeed_Bands, initial_band_index), 
                            Current_State.groundSpeed_mps),
     Post => Divert_state.groundSpeed_mps < Current_State.groundSpeed_mps 
             and then (for all I in MyVectorOfIntervals.First_Index
                       (DAIDALUS_GroundSpeed_Bands) .. MyVectorOfIntervals.
                         Last_Index (DAIDALUS_GroundSpeed_Bands)
                       => not InRange (MyVectorOfIntervals.Element
                                      (DAIDALUS_GroundSpeed_Bands, I), 
                                      Divert_state.groundSpeed_mps));
   
   procedure Slow_Down 
     (Divert_state : in out state_parameters;
      initial_band_index : myvector_index_type;
      DAIDALUS_GroundSpeed_Bands : OrderedIntervalVector;
      Current_State : state_parameters;
      GroundSpeed_Max_mps : GroundSpeed_Type_mps;
      GroundSpeed_Min_mps : GroundSpeed_Type_mps;
      GroundSpeed_Interval_Buffer_mps : GroundSpeed_Buffer_Type_mps) is
      --  isMovedFromIC : Boolean := False with Ghost;
   begin
      for I in reverse MyVectorOfIntervals.First_Index 
        (DAIDALUS_GroundSpeed_Bands) .. initial_band_index loop
         pragma Assert (MyVectorOfIntervals.Element 
                       (DAIDALUS_GroundSpeed_Bands, I).LowerBound >= 
                         GroundSpeed_Min_mps);
         pragma Assert (MyVectorOfIntervals.Element (DAIDALUS_GroundSpeed_Bands, 
                       I).LowerBound - GroundSpeed_Interval_Buffer_mps >= 
                         GroundSpeed_Type_mps'First);
         if InRange (MyVectorOfIntervals.Element (DAIDALUS_GroundSpeed_Bands, I),
                     Divert_state.groundSpeed_mps) 
         then
            Divert_state.groundSpeed_mps := MyVectorOfIntervals.Element
              (DAIDALUS_GroundSpeed_Bands, I).LowerBound - 
              GroundSpeed_Interval_Buffer_mps;
            pragma Assert (not InRange (MyVectorOfIntervals.Element 
                          (DAIDALUS_GroundSpeed_Bands, I), Divert_state.
                            groundSpeed_mps));
            pragma Assert (Divert_state.groundSpeed_mps < Current_State.
                             groundSpeed_mps);
            pragma Assert (Divert_state.groundSpeed_mps < 
                             MyVectorOfIntervals.Element 
                               (DAIDALUS_GroundSpeed_Bands, I).LowerBound);
         end if;
         pragma Loop_Invariant (Divert_state.groundSpeed_mps <= Divert_state.
                                 groundSpeed_mps'Loop_Entry);
         pragma Loop_Invariant (for all J in I .. initial_band_index =>
                                  not InRange (MyVectorOfIntervals.Element
                                 (DAIDALUS_GroundSpeed_Bands, J), Divert_state.
                                   groundSpeed_mps));
         pragma Loop_Invariant (for all J in initial_band_index .. 
                                 MyVectorOfIntervals.Last_Index
                                   (DAIDALUS_GroundSpeed_Bands) => 
                                  not InRange (MyVectorOfIntervals.Element
                                 (DAIDALUS_GroundSpeed_Bands, J), 
                                 Divert_state.groundSpeed_mps));
      end loop;     
   end Slow_Down;
      
   --subprogram that resets the divert groundspeed to the current groundspeed 
   --and then starting from the conflict band containing the current groundspeed
   --sets the divert groundspeed higher than the upper bound of the conflict 
   --band being interrogated if prior to modification the divert groundspeed is 
   --contained
   procedure Reset_to_Initial_and_Speed_Up 
     (Divert_state : in out state_parameters;
      initial_band_index : myvector_index_type;
      DAIDALUS_GroundSpeed_Bands : OrderedIntervalVector;
      Current_State : state_parameters;
      GroundSpeed_Max_mps : GroundSpeed_Type_mps;
      GroundSpeed_Min_mps : GroundSpeed_Type_mps;
      GroundSpeed_Interval_Buffer_mps : GroundSpeed_Buffer_Type_mps) with
     Pre => (Are_Legitimate_Bands (DAIDALUS_GroundSpeed_Bands) and then not 
               MyVectorOfIntervals.Is_Empty (DAIDALUS_GroundSpeed_Bands)
             and then scalar_constraints
                       (Upper_limit         => GroundSpeed_Max_mps,
                        Lower_limit         => GroundSpeed_Min_mps,
                        Interval_Constraint => GroundSpeed_Interval_Buffer_mps)
             and then vector_constraints
                       (X                   => DAIDALUS_GroundSpeed_Bands,
                        Upper_limit         => GroundSpeed_Max_mps,
                        Lower_limit         => GroundSpeed_Min_mps,
                        Interval_constraint => GroundSpeed_Interval_Buffer_mps)
             and then Current_GroundSpeed_Exists_in_Bands
               (Current_State, DAIDALUS_GroundSpeed_Bands)
             and then Divert_state = Current_State
             and then initial_band_index in MyVectorOfIntervals.First_Index
               (DAIDALUS_GroundSpeed_Bands) ..
               MyVectorOfIntervals.Last_Index (DAIDALUS_GroundSpeed_Bands)
             and then InRange (MyVectorOfIntervals.Element 
               (DAIDALUS_GroundSpeed_Bands, initial_band_index), Current_State.
                 groundSpeed_mps)),
     Post => (Divert_state.groundSpeed_mps > Current_State.groundSpeed_mps 
              and then (for all I in MyVectorOfIntervals.First_Index
               (DAIDALUS_GroundSpeed_Bands) .. MyVectorOfIntervals.Last_Index 
               (DAIDALUS_GroundSpeed_Bands) => not InRange (MyVectorOfIntervals.
                    Element (DAIDALUS_GroundSpeed_Bands, I), 
                  Divert_state.groundSpeed_mps)));
   
   procedure Reset_to_Initial_and_Speed_Up 
     (Divert_state : in out state_parameters;
      initial_band_index : myvector_index_type;
      DAIDALUS_GroundSpeed_Bands : OrderedIntervalVector;
      Current_State : state_parameters;
      GroundSpeed_Max_mps : GroundSpeed_Type_mps;
      GroundSpeed_Min_mps : GroundSpeed_Type_mps;
      GroundSpeed_Interval_Buffer_mps : GroundSpeed_Buffer_Type_mps) is
   begin
      for I in initial_band_index .. MyVectorOfIntervals.Last_Index 
        (DAIDALUS_GroundSpeed_Bands) loop
         if InRange (MyVectorOfIntervals.Element (DAIDALUS_GroundSpeed_Bands, I),
                     Divert_state.groundSpeed_mps) 
         then
            pragma Assert (MyVectorOfIntervals.Element 
                          (DAIDALUS_GroundSpeed_Bands, I).UpperBound <= 
                            GroundSpeed_Max_mps);
            Divert_state.groundSpeed_mps := MyVectorOfIntervals.Element 
              (DAIDALUS_GroundSpeed_Bands, I).UpperBound +
              GroundSpeed_Interval_Buffer_mps;
         end if;
         pragma Assert (for all J in initial_band_index .. I - 1 =>
                          not InRange (MyVectorOfIntervals.Element 
                         (DAIDALUS_GroundSpeed_Bands, J), Divert_state.
                           groundSpeed_mps));
         pragma Loop_Invariant (Divert_state.groundSpeed_mps >= 
                                 Divert_state.groundSpeed_mps'Loop_Entry);
         pragma Loop_Invariant (for all J in MyVectorOfIntervals.First_Index 
                               (DAIDALUS_GroundSpeed_Bands) .. 
                                 initial_band_index =>
                                    not InRange (MyVectorOfIntervals.Element 
                                 (DAIDALUS_GroundSpeed_Bands, J), Divert_state.
                                   groundSpeed_mps));
         pragma Loop_Invariant (for all J in initial_band_index .. I =>
                                  not InRange (MyVectorOfIntervals.Element 
                                 (DAIDALUS_GroundSpeed_Bands, J), Divert_state.
                                   groundSpeed_mps));
      end loop;
   
   end Reset_to_Initial_and_Speed_Up;
   
   --subprogram that searches the recovery intervals for one that has upper and 
   --lower bounds less than the current groundspeed from highest to lowest. Sets
   --the divert groundspeed to be contained by the first interval matching the 
   --criteria.
   procedure Recovery_Slow_Down 
     (Divert_State : in out state_parameters;
      Recovery_GroundSpeed_bands : OrderedIntervalVector;
      Current_State : state_parameters;
      GroundSpeed_Max_mps : GroundSpeed_Type_mps;
      GroundSpeed_Min_mps : GroundSpeed_Type_mps;
      GroundSpeed_Interval_Buffer_mps : GroundSpeed_Buffer_Type_mps;
      is_Recovery_Found : in out Boolean;
      local_acceptable_action_flag : in out Boolean) with
     Pre => Are_Legitimate_Bands (Recovery_GroundSpeed_bands) and then 
            is_Recovery_Found = False and then 
            not MyVectorOfIntervals.Is_Empty (Recovery_GroundSpeed_bands) 
            and then scalar_constraints
                       (Upper_limit         => GroundSpeed_Max_mps,
                        Lower_limit         => GroundSpeed_Min_mps,
                        Interval_Constraint => GroundSpeed_Interval_Buffer_mps)
            and then vector_constraints
                       (X                   => Recovery_GroundSpeed_bands,
                        Upper_limit         => GroundSpeed_Max_mps,
                        Lower_limit         => GroundSpeed_Min_mps,
                        Interval_constraint => GroundSpeed_Interval_Buffer_mps)
            and then Divert_State.groundSpeed_mps > GroundSpeed_Max_mps,
     Post => (is_Recovery_Found and then local_acceptable_action_flag and then
              Successful_Placement_in_Recovery_Bands
                (Recovery_GroundSpeed_bands    => Recovery_GroundSpeed_bands,
                 Divert_state               => Divert_State))
              or else (Divert_State.groundSpeed_mps > GroundSpeed_Max_mps 
              and then not is_Recovery_Found and then 
              (for all I in MyVectorOfIntervals.First_Index
                 (Recovery_GroundSpeed_bands) .. MyVectorOfIntervals.Last_Index
                   (Recovery_GroundSpeed_bands) => not 
                 (MyVectorOfIntervals.Element (Recovery_GroundSpeed_bands, I).
                      LowerBound < Current_State.groundSpeed_mps and then 
                        MyVectorOfIntervals.Element
                    (Recovery_GroundSpeed_bands, I).UpperBound < Current_State.
                      groundSpeed_mps)));
   
   procedure Recovery_Slow_Down 
     (Divert_State : in out state_parameters;
      Recovery_GroundSpeed_bands : OrderedIntervalVector;
      Current_State : state_parameters;
      GroundSpeed_Max_mps : GroundSpeed_Type_mps;
      GroundSpeed_Min_mps : GroundSpeed_Type_mps;
      GroundSpeed_Interval_Buffer_mps : GroundSpeed_Buffer_Type_mps;
      is_Recovery_Found : in out Boolean;
      local_acceptable_action_flag : in out Boolean) is
   begin
   
      for I in reverse MyVectorOfIntervals.First_Index
        (Recovery_GroundSpeed_bands) .. MyVectorOfIntervals.Last_Index
        (Recovery_GroundSpeed_bands) loop
         if MyVectorOfIntervals.Element (Recovery_GroundSpeed_bands, I).
           LowerBound < Current_State.groundSpeed_mps and then 
           MyVectorOfIntervals.Element (Recovery_GroundSpeed_bands, I).
           UpperBound < Current_State.groundSpeed_mps 
         then
            pragma Assert (MyVectorOfIntervals.Element 
                          (Recovery_GroundSpeed_bands, I).LowerBound >= 
                            GroundSpeed_Min_mps);
            Divert_State.groundSpeed_mps := MyVectorOfIntervals.Element 
              (Recovery_GroundSpeed_bands, I).UpperBound -
              GroundSpeed_Interval_Buffer_mps / 2.0;
            pragma Assert (MyVectorOfIntervals.Element
                          (Recovery_GroundSpeed_bands, I).UpperBound - 
                            MyVectorOfIntervals.Element
                              (Recovery_GroundSpeed_bands, I).LowerBound
                          >= 2.0 * GroundSpeed_Interval_Buffer_mps);
            pragma Assert (InRange (MyVectorOfIntervals.Element 
                          (Recovery_GroundSpeed_bands, I), Divert_State.
                            groundSpeed_mps));
            is_Recovery_Found := True;
            local_acceptable_action_flag := True;   
            pragma Assert (Successful_Placement_in_Recovery_Bands
                          (Recovery_GroundSpeed_bands      => 
                             Recovery_GroundSpeed_bands,
                           Divert_state                    => Divert_State));   
            exit;
         end if;
   
         pragma Loop_Invariant (Divert_State.groundSpeed_mps = Divert_State.
                                 groundSpeed_mps'Loop_Entry);
         pragma Loop_Invariant (not is_Recovery_Found);
         pragma Loop_Invariant (for all J in I .. MyVectorOfIntervals.Last_Index 
                               (Recovery_GroundSpeed_bands)  =>
                                  not (MyVectorOfIntervals.Element
                                 (Recovery_GroundSpeed_bands, J).LowerBound < 
                                   Current_State.groundSpeed_mps and then 
                                 MyVectorOfIntervals.Element
                                   (Recovery_GroundSpeed_bands, J).UpperBound < 
                                   Current_State.groundSpeed_mps));
      end loop;
   end Recovery_Slow_Down;
   
   --subprogram that searches for a recovery interval that has upper and lower 
   --bounds greater than the current speed, looping from lowest to highest.
   --sets the divert groundspeed to be contained by the first interval that 
   --matches the criteria.
   procedure Recovery_Speed_Up 
     (Divert_State : in out state_parameters;
      Recovery_GroundSpeed_bands : OrderedIntervalVector;
      Current_State : state_parameters;
      GroundSpeed_Max_mps : GroundSpeed_Type_mps;
      GroundSpeed_Min_mps : GroundSpeed_Type_mps;
      GroundSpeed_Interval_Buffer_mps : GroundSpeed_Buffer_Type_mps;
      is_Recovery_Found : in out Boolean;
      local_acceptable_action_flag : in out Boolean) with
     Pre => (Are_Legitimate_Bands (Recovery_GroundSpeed_bands) and then not
             MyVectorOfIntervals.Is_Empty (Recovery_GroundSpeed_bands)
             and then scalar_constraints
                        (Upper_limit         => GroundSpeed_Max_mps,
                         Lower_limit         => GroundSpeed_Min_mps,
                         Interval_Constraint => GroundSpeed_Interval_Buffer_mps)
             and then vector_constraints
                        (X                   => Recovery_GroundSpeed_bands,
                         Upper_limit         => GroundSpeed_Max_mps,
                         Lower_limit         => GroundSpeed_Min_mps,
                         Interval_constraint => GroundSpeed_Interval_Buffer_mps)
             and then Expected_Recovery_Bands_Nature
                        (Recovery_GroundSpeed_bands      => 
                           Recovery_GroundSpeed_bands,
                         Current_state                   => Current_State) 
             and then is_Recovery_Found = False 
             and then Divert_State.groundSpeed_mps > GroundSpeed_Max_mps 
             and then 
             (for all I in MyVectorOfIntervals.First_Index
                    (Recovery_GroundSpeed_bands) .. MyVectorOfIntervals.
                    Last_Index (Recovery_GroundSpeed_bands)
                    => not (MyVectorOfIntervals.Element
                    (Recovery_GroundSpeed_bands, I).LowerBound < 
                    Current_State.groundSpeed_mps and then 
             MyVectorOfIntervals.Element (Recovery_GroundSpeed_bands, I).
                    UpperBound < Current_State.groundSpeed_mps))),
     Post => (is_Recovery_Found and then local_acceptable_action_flag and then
              Successful_Placement_in_Recovery_Bands
                (Recovery_GroundSpeed_bands      => Recovery_GroundSpeed_bands,
                 Divert_state                    => Divert_State));
   
   procedure Recovery_Speed_Up 
     (Divert_State : in out state_parameters;
      Recovery_GroundSpeed_bands : OrderedIntervalVector;
      Current_State : state_parameters;
      GroundSpeed_Max_mps : GroundSpeed_Type_mps;
      GroundSpeed_Min_mps : GroundSpeed_Type_mps;
      GroundSpeed_Interval_Buffer_mps : GroundSpeed_Buffer_Type_mps;
      is_Recovery_Found : in out Boolean;
      local_acceptable_action_flag : in out Boolean) is
   begin
      for I in MyVectorOfIntervals.First_Index (Recovery_GroundSpeed_bands) ..
        MyVectorOfIntervals.Last_Index (Recovery_GroundSpeed_bands) loop
         if MyVectorOfIntervals.Element (Recovery_GroundSpeed_bands, I).
           LowerBound > Current_State.groundSpeed_mps
           and then MyVectorOfIntervals.Element (Recovery_GroundSpeed_bands, I).
           UpperBound > Current_State.groundSpeed_mps
         then
            pragma Assert (MyVectorOfIntervals.Element
                          (Recovery_GroundSpeed_bands, I).UpperBound <=
                            GroundSpeed_Max_mps);
            pragma Assert (MyVectorOfIntervals.Element
                          (Recovery_GroundSpeed_bands, I).UpperBound -
                            MyVectorOfIntervals.Element
                              (Recovery_GroundSpeed_bands, I).LowerBound >=
                            2.0 * GroundSpeed_Interval_Buffer_mps);
            Divert_State.groundSpeed_mps := MyVectorOfIntervals.Element 
              (Recovery_GroundSpeed_bands, I).LowerBound +
              GroundSpeed_Interval_Buffer_mps / 2.0;
            pragma Assert (Divert_State.groundSpeed_mps > MyVectorOfIntervals.
                            Element (Recovery_GroundSpeed_bands, I).LowerBound 
                          and then Divert_State.groundSpeed_mps < 
                            MyVectorOfIntervals.Element
                              (Recovery_GroundSpeed_bands, I).UpperBound);
            pragma Assert (InRange (MyVectorOfIntervals.Element 
                          (Recovery_GroundSpeed_bands, I), Divert_State.
                            groundSpeed_mps));
            pragma Assert (Successful_Placement_in_Recovery_Bands
                          (Recovery_GroundSpeed_bands, Divert_State));
            is_Recovery_Found := True;
            local_acceptable_action_flag := True;
            pragma Assert (is_Recovery_Found);
            exit;
         end if;
   
         pragma Loop_Invariant (Divert_State.groundSpeed_mps > 
                                 GroundSpeed_Max_mps);
         pragma Loop_Invariant (not is_Recovery_Found);
         pragma Loop_Invariant (for all J in MyVectorOfIntervals.First_Index
                               (Recovery_GroundSpeed_bands) .. I =>
                                  not (MyVectorOfIntervals.Element 
                                 (Recovery_GroundSpeed_bands, J).LowerBound > 
                                   Current_State.groundSpeed_mps and then
                                 MyVectorOfIntervals.Element 
                                   (Recovery_GroundSpeed_bands, J).UpperBound > 
                                   Current_State.groundSpeed_mps));
      end loop;
   end Recovery_Speed_Up;
   
   -----------------------------------
   -- Found_WCV_GroundSpeed_Resolution --
   -----------------------------------

   procedure Found_WCV_GroundSpeed_Resolution
     (DAIDALUS_GroundSpeed_Bands : OrderedIntervalVector;
      Recovery_GroundSpeed_bands : OrderedIntervalVector;
      Current_State : state_parameters;
      GroundSpeed_Max_mps : GroundSpeed_Type_mps;
      GroundSpeed_Min_mps : GroundSpeed_Type_mps;
      GroundSpeed_Interval_Buffer_mps : GroundSpeed_Buffer_Type_mps;
      Divert_State : out state_parameters;
      found_acceptable_action_flag : out Boolean)
   is
      local_acceptable_action_flag : Boolean;
      is_Found : Boolean := False;
      is_Recovery_Found : Boolean := False;
      initial_band : myvector_index_type := myvector_index_type'First;

   begin
      --initialized the divert state to the current state
      Divert_State := Current_State; 

      if not MyVectorOfIntervals.Is_Empty (DAIDALUS_GroundSpeed_Bands) then
   
         --find the conflict band that contains the current groundspeed by 
         --looping from lowest to highest
         Find_Initial 
           (Divert_State             => Divert_State,
            is_Found                 => is_Found,
            initial_band_index       => initial_band,
            DAIDALUS_GroundSpeed_Bands  => DAIDALUS_GroundSpeed_Bands,
            Current_state            => Current_State,
            GroundSpeed_Max_mps           => GroundSpeed_Max_mps,
            GroundSpeed_Min_mps           => GroundSpeed_Min_mps,
            GroundSpeed_Interval_Buffer_mps => GroundSpeed_Interval_Buffer_mps);
         pragma Assert (is_Found);
         
         --using the index to the conflict band containing the current 
         --groundspeed loop over that band and all bands with bounds higher to 
         --move the divert groundspeed greater than the upperbound of the 
         --current conflict band if contained prior to modification
         Slow_Down
           (Divert_state                    => Divert_State,
            initial_band_index              => initial_band,
            DAIDALUS_GroundSpeed_Bands      => DAIDALUS_GroundSpeed_Bands,
            Current_State                   => Current_State,
            GroundSpeed_Max_mps             => GroundSpeed_Max_mps,
            GroundSpeed_Min_mps             => GroundSpeed_Min_mps,
            GroundSpeed_Interval_Buffer_mps => GroundSpeed_Interval_Buffer_mps);
   
         --check if the divert groundspeed was lowered beyond the minimum 
         --groundspeed if so, reset divert groundspeed to current and look for a
         --groundspeed greater than the current as conflict resolution         
         if Divert_State.groundSpeed_mps < GroundSpeed_Min_mps then
            Divert_State := Current_State;
            Reset_to_Initial_and_Speed_Up
              (Divert_state                    => Divert_State,
               initial_band_index              => initial_band,
               DAIDALUS_GroundSpeed_Bands      => DAIDALUS_GroundSpeed_Bands,
               Current_State                   => Current_State,
               GroundSpeed_Max_mps             => GroundSpeed_Max_mps,
               GroundSpeed_Min_mps             => GroundSpeed_Min_mps,
               GroundSpeed_Interval_Buffer_mps => 
                 GroundSpeed_Interval_Buffer_mps);
   
         else
            pragma Assume (Conflict_and_Recovery_Complimentary_Nature
                          (Divert_State, DAIDALUS_GroundSpeed_Bands, 
                             Recovery_GroundSpeed_bands, GroundSpeed_Min_mps,
                             GroundSpeed_Max_mps));
            local_acceptable_action_flag := (if MyVectorOfIntervals.Is_Empty
                                             (Recovery_GroundSpeed_bands) then 
                                             True else 
                                         Successful_Placement_in_Recovery_Bands  
                                               (Recovery_GroundSpeed_bands,
                                                Divert_State));
            pragma Assert (Found_Acceptable_Action (local_acceptable_action_flag, 
                         DAIDALUS_GroundSpeed_Bands, Recovery_GroundSpeed_bands,
                         Divert_State.groundSpeed_mps));
         end if;
   
         --if divert groundspeed is greather than the maximum, then failed to 
         --find a good resolution using conflict bands alone, check 
         --recovery bands for a mitigation strategy
         if Divert_State.groundSpeed_mps > GroundSpeed_Max_mps then
            local_acceptable_action_flag := False;
            if not MyVectorOfIntervals.Is_Empty (Recovery_GroundSpeed_bands) then
               --check the recovery bands for a grounspeed less than the current
               --groundspeed as a mitigation
               Recovery_Slow_Down
                 (Divert_State                    => Divert_State,
                  Recovery_GroundSpeed_bands      => Recovery_GroundSpeed_bands,
                  Current_State                   => Current_State,
                  GroundSpeed_Max_mps             => GroundSpeed_Max_mps,
                  GroundSpeed_Min_mps             => GroundSpeed_Min_mps,
                  GroundSpeed_Interval_Buffer_mps => 
                    GroundSpeed_Interval_Buffer_mps,
                  is_Recovery_Found => is_Recovery_Found,
                  local_acceptable_action_flag    => 
                    local_acceptable_action_flag);
               --  pragma Assert(is_Recovery_Found or not is_Recovery_Found);
               --  pragma Assert(if is_Recovery_Found then Successful_Placement_in_Recovery_Bands(Recovery_GroundSpeed_bands      => Recovery_GroundSpeed_bands,
               --                                                                                 Divert_state                    => Divert_State,
               --                                                                                 GroundSpeed_Max_mps             => GroundSpeed_Max_mps,
               --                                                                                 GroundSpeed_Min_mps             => GroundSpeed_Min_mps,
               --                                                                                 GroundSpeed_Interval_Buffer_mps => GroundSpeed_Interval_Buffer_mps));
               --  pragma Assert(if (Successful_Placement_in_Recovery_Bands(Recovery_GroundSpeed_bands      => Recovery_GroundSpeed_bands,
               --                                                          Divert_state                    => Divert_State,
               --                                                          GroundSpeed_Max_mps             => GroundSpeed_Max_mps,
               --                                                          GroundSpeed_Min_mps             => GroundSpeed_Min_mps,
               --                                                          GroundSpeed_Interval_Buffer_mps => GroundSpeed_Interval_Buffer_mps))
               --                then Found_Mitigation(local_acceptable_action_flag, Recovery_GroundSpeed_bands, Divert_State.groundSpeed_mps));
               --  --
               --  pragma Assert(if is_Recovery_Found then Found_Mitigation(found_guaranteed_flag => local_acceptable_action_flag,
               --                                                           Recovery_X_Bands      => Recovery_GroundSpeed_bands,
               --                                                           Divert_State_field    => Divert_State.groundSpeed_mps));
                  
               pragma Assume (if not is_Recovery_Found then 
                               (for some I in reverse MyVectorOfIntervals.
                                First_Index (Recovery_GroundSpeed_bands) ..
                                    MyVectorOfIntervals.Last_Index 
                                  (Recovery_GroundSpeed_bands) 
                                => (MyVectorOfIntervals.Element 
                                    (Recovery_GroundSpeed_bands, I).LowerBound >
                                      Current_State.groundSpeed_mps and then
                                    MyVectorOfIntervals.Element 
                                      (Recovery_GroundSpeed_bands, I).UpperBound
                                    > Current_State.groundSpeed_mps)));
               if not is_Recovery_Found then
                  --mitigation not found by searching for a recovery band 
                  --interval with bounds less than the current groundspeed. 
                  --Instead, check for a recovery band interval with bounds 
                  --greater than the current groundspeed
                  Recovery_Speed_Up
                    (Divert_State                    => Divert_State,
                     Recovery_GroundSpeed_bands      => 
                       Recovery_GroundSpeed_bands,
                     Current_State                   => Current_State,
                     GroundSpeed_Max_mps             => GroundSpeed_Max_mps,
                     GroundSpeed_Min_mps             => GroundSpeed_Min_mps,
                     GroundSpeed_Interval_Buffer_mps => 
                       GroundSpeed_Interval_Buffer_mps,
                     is_Recovery_Found               => is_Recovery_Found,
                     local_acceptable_action_flag    => 
                         local_acceptable_action_flag);
                  
                  pragma Assert (is_Recovery_Found);
   
               end if;
               pragma Assert (local_acceptable_action_flag);
               pragma Assert (Found_Acceptable_Action
                             (local_acceptable_action_flag, 
                                DAIDALUS_GroundSpeed_Bands, 
                                Recovery_GroundSpeed_bands, 
                                Divert_State.groundSpeed_mps));   
            else
               local_acceptable_action_flag := False;
               Divert_State.groundSpeed_mps := GroundSpeed_Min_mps;
               pragma Assert (Revert_behavior (local_acceptable_action_flag, 
                             DAIDALUS_GroundSpeed_Bands, GroundSpeed_Min_mps, 
                             Divert_State.groundSpeed_mps));
   
            end if;
         else
            pragma Assume (Conflict_and_Recovery_Complimentary_Nature
                          (Divert_State, DAIDALUS_GroundSpeed_Bands, 
                             Recovery_GroundSpeed_bands, GroundSpeed_Min_mps,
                             GroundSpeed_Max_mps));
            local_acceptable_action_flag := (if MyVectorOfIntervals.Is_Empty 
                                            (Recovery_GroundSpeed_bands) then
                                             True else 
                                          Successful_Placement_in_Recovery_Bands
                                            (Recovery_GroundSpeed_bands,
                                             Divert_State));
            pragma Assert (local_acceptable_action_flag);
            pragma Assert (for all I in MyVectorOfIntervals.First_Index 
                           (DAIDALUS_GroundSpeed_Bands) .. MyVectorOfIntervals.
                             Last_Index (DAIDALUS_GroundSpeed_Bands) =>
                              not InRange (MyVectorOfIntervals.Element 
                             (DAIDALUS_GroundSpeed_Bands, I), 
                             Divert_State.groundSpeed_mps));
            pragma Assert (Found_Acceptable_Action (local_acceptable_action_flag,
                         DAIDALUS_GroundSpeed_Bands, Recovery_GroundSpeed_bands,
                         Divert_State.groundSpeed_mps));
         end if;
   
      else
         local_acceptable_action_flag := False;
         pragma Assert (IsImproperlyConfigured (local_acceptable_action_flag,
                       DAIDALUS_GroundSpeed_Bands, Divert_State.groundSpeed_mps,
                       Current_State.groundSpeed_mps));
   
      end if;
   
      found_acceptable_action_flag := local_acceptable_action_flag;
      pragma Assert (Found_Acceptable_Action (found_acceptable_action_flag, 
                    DAIDALUS_GroundSpeed_Bands, Recovery_GroundSpeed_bands, 
                    Divert_State.groundSpeed_mps) or else 
                    IsImproperlyConfigured (found_acceptable_action_flag, 
                      DAIDALUS_GroundSpeed_Bands, Divert_State.groundSpeed_mps, 
                      Current_State.groundSpeed_mps) or else
                    Revert_behavior (found_acceptable_action_flag, 
                      DAIDALUS_GroundSpeed_Bands,  GroundSpeed_Min_mps, 
                      Divert_State.groundSpeed_mps));
   end Found_WCV_GroundSpeed_Resolution;
   
end speed_resolution;
