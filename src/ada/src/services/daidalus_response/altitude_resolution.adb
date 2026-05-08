package body Altitude_Resolution
with SPARK_Mode => On is

   pragma Assertion_Policy (Check);

   --predicate indicting whether or not the divert altitude is contained within
   --a single recovery band interval
   function Successful_Placement_in_Recovery_Bands
     (Recovery_Altitude_Bands : OrderedIntervalVector32;
      Divert_state : state_parameters) return Boolean is
     (for some I in MyVectorOfIntervals32.First_Index (Recovery_Altitude_Bands) ..
       MyVectorOfIntervals32.Last_Index (Recovery_Altitude_Bands) =>
           InRange (MyVectorOfIntervals32.Element (Recovery_Altitude_Bands, I),
        Divert_state.altitude_m));

   --Predicate describing the relationship between conflict bands and recovery
   --bands reported out by DAIDALUS.  Recovery bands are presented when the
   --conflict bands fully saturate the range.  Recovery bands represent the
   --portion of the range that will exit the well clear violation the quickest
   --despite the full range not being able to avoid a well clear violation.
   --Recovery bands and conflict bands are then reported out as complimentary
   --to each other.
   function Conflict_and_Recovery_Complimentary_Nature
     (Divert_State : state_parameters;
      DAIDALUS_Altitude_Bands : OrderedIntervalVector32;
      Recovery_Altitude_Bands : OrderedIntervalVector32;
      Altitude_Min_m : Altitude_Type_m;
      Altitude_Max_m : Altitude_Type_m) return Boolean is
     (if (Divert_State.altitude_m >= Altitude_Min_m) and then
        (Divert_State.altitude_m <= Altitude_Max_m) and then (not
        MyVectorOfIntervals32.Is_Empty (DAIDALUS_Altitude_Bands)) and then
        (not MyVectorOfIntervals32.Is_Empty (Recovery_Altitude_Bands)) and then
        (for all I in
          MyVectorOfIntervals32.First_Index (DAIDALUS_Altitude_Bands) ..
          MyVectorOfIntervals32.Last_Index (DAIDALUS_Altitude_Bands) =>
              not InRange (MyVectorOfIntervals32.Element (DAIDALUS_Altitude_Bands, I)
           , Divert_State.altitude_m)) then
        (Successful_Placement_in_Recovery_Bands
             (Recovery_Altitude_Bands, Divert_State))) with
     Ghost;

   --Predicate describing the expected nature of recovery band intervals
   --recovery bands, when present, are the complement to conflict bands. Thus
   --given the current state is in a conflict band there exists a recovery band
   --interval that is completly above or below the current altitude
   function Expected_Recovery_Bands_Nature
     (Recovery_Altitude_Bands : OrderedIntervalVector32;
      Current_state : state_parameters) return Boolean is
     ((for some I in MyVectorOfIntervals32.First_Index (Recovery_Altitude_Bands) ..
        MyVectorOfIntervals32.Last_Index (Recovery_Altitude_Bands) =>
         MyVectorOfIntervals32.Element (Recovery_Altitude_Bands, I).LowerBound >
        Current_state.altitude_m and then
          MyVectorOfIntervals32.Element (Recovery_Altitude_Bands, I).UpperBound >
        Current_state.altitude_m) or else
         (for some I in MyVectorOfIntervals32.First_Index (Recovery_Altitude_Bands)
           .. MyVectorOfIntervals32.Last_Index (Recovery_Altitude_Bands) =>
           MyVectorOfIntervals32.Element (Recovery_Altitude_Bands, I).LowerBound <
              Current_state.altitude_m and
           MyVectorOfIntervals32.Element (Recovery_Altitude_Bands, I).UpperBound <
                 Current_state.altitude_m)) with
           Ghost;
   procedure Find_Initial_and_Climb
     (Divert_state : in out state_parameters;
      is_Found : in out Boolean;
      initial_band_index : in out myvector_index_type;
      DAIDALUS_Altitude_Bands : OrderedIntervalVector32;
      Current_state : state_parameters;
      Altitude_Max_m : Altitude_Type_m;
      Altitude_Min_m : Altitude_Type_m;
      Altitude_Interval_Buffer_m : Altitude_Buffer_Type_m) with
        Pre => Are_Legitimate_Bands (DAIDALUS_Altitude_Bands) and then
            not MyVectorOfIntervals32.Is_Empty (DAIDALUS_Altitude_Bands) and then
            scalar_constraints
               (Upper_limit         => Altitude_Max_m,
                Lower_limit         => Altitude_Min_m,
                Interval_Constraint => Altitude_Interval_Buffer_m) and then
             vector_constraints
                (X                   => DAIDALUS_Altitude_Bands,
                 Upper_limit         => Altitude_Max_m,
                 Lower_limit         => Altitude_Min_m,
                 Interval_constraint => Altitude_Interval_Buffer_m) and then
             Divert_state = Current_state and then
             Current_Altitude_Exists_in_Bands (Current_state,
                                              DAIDALUS_Altitude_Bands) and then
             initial_band_index =
              MyVectorOfIntervals32.First_Index (DAIDALUS_Altitude_Bands) and then
           is_Found = False and then Current_state = Divert_state,
     Post => (initial_band_index in
                MyVectorOfIntervals32.First_Index (DAIDALUS_Altitude_Bands) ..
                MyVectorOfIntervals32.Last_Index (DAIDALUS_Altitude_Bands) and then
             (for all I in MyVectorOfIntervals32.First_Index
                                                 (DAIDALUS_Altitude_Bands) ..
              MyVectorOfIntervals32.Last_Index (DAIDALUS_Altitude_Bands) =>
              not InRange (MyVectorOfIntervals32.Element
             (DAIDALUS_Altitude_Bands, I), Divert_state.altitude_m)) and then
             (Divert_state.altitude_m > Current_state.altitude_m) and then
             (is_Found = True) and then
             InRange (MyVectorOfIntervals32.Element
                (DAIDALUS_Altitude_Bands, initial_band_index),
                Current_state.altitude_m));

   --subprogram to determine which conflict band the current altitude is in and
   --then set the divert altitude just past the upper bound of that interval
   --loops over intervals starting from the lowest to highest
   procedure Find_Initial_and_Climb
     (Divert_state : in out state_parameters;
      is_Found : in out Boolean;
      initial_band_index : in out myvector_index_type;
      DAIDALUS_Altitude_Bands : OrderedIntervalVector32;
      Current_state : state_parameters;
      Altitude_Max_m : Altitude_Type_m;
      Altitude_Min_m : Altitude_Type_m;
      Altitude_Interval_Buffer_m : Altitude_Buffer_Type_m) is
   begin

      for I in MyVectorOfIntervals32.First_Index (DAIDALUS_Altitude_Bands) ..
        MyVectorOfIntervals32.Last_Index (DAIDALUS_Altitude_Bands) loop

         if InRange (MyVectorOfIntervals32.Element (DAIDALUS_Altitude_Bands, I),
                     Divert_state.altitude_m)
         then
            pragma Assert (MyVectorOfIntervals32.Element
                          (DAIDALUS_Altitude_Bands, I).UpperBound +
                           Altitude_Buffer_Type_m'Last <= Altitude_Type_m'Last);
            Divert_state.altitude_m := MyVectorOfIntervals32.Element
              (DAIDALUS_Altitude_Bands, I).UpperBound +
              Altitude_Interval_Buffer_m;
            pragma Assert (MyVectorOfIntervals32.Element
                          (DAIDALUS_Altitude_Bands, I).UpperBound <
                            Divert_state.altitude_m);
            if not is_Found then
               initial_band_index := I;
               is_Found := True;
            end if;
         end if;
         pragma Assert (if is_Found then (for all J in
                        MyVectorOfIntervals32.First_Index (DAIDALUS_Altitude_Bands)
                        .. I - 1 => not InRange (MyVectorOfIntervals32.
                        Element (DAIDALUS_Altitude_Bands, J),
                        Divert_state.altitude_m)));

         pragma Loop_Invariant (not is_Found or else
                                 (initial_band_index <= I and then
                                  initial_band_index in
                                    MyVectorOfIntervals32.First_Index
                                      (DAIDALUS_Altitude_Bands) ..
                                    MyVectorOfIntervals32.Last_Index
                                    (DAIDALUS_Altitude_Bands)));
         pragma Loop_Invariant (not is_Found or else
                               InRange (MyVectorOfIntervals32.Element
                                 (DAIDALUS_Altitude_Bands, initial_band_index),
                                 Current_state.altitude_m));
         pragma Loop_Invariant (is_Found or else (Current_state = Divert_state
                               and then Current_Altitude_Exists_in_Bands
                                 (Divert_state, DAIDALUS_Altitude_Bands)));
         pragma Loop_Invariant (not is_Found or else Divert_state.altitude_m >
                                 Current_state.altitude_m);
         pragma Loop_Invariant (is_Found or else (for all J in
                                 MyVectorOfIntervals32.First_Index (
                                   DAIDALUS_Altitude_Bands) .. I =>
                                  not InRange (MyVectorOfIntervals32.Element
                                 (DAIDALUS_Altitude_Bands, J),
                                 Divert_state.altitude_m)));
         pragma Loop_Invariant (not InRange (MyVectorOfIntervals32.Element
                               (DAIDALUS_Altitude_Bands, I),
                               Divert_state.altitude_m));
         pragma Loop_Invariant (for all J in MyVectorOfIntervals32.
                                 First_Index (DAIDALUS_Altitude_Bands) .. I =>
                                  not InRange (MyVectorOfIntervals32.Element
                                 (DAIDALUS_Altitude_Bands, J),
                                 Divert_state.altitude_m));
      end loop;
      pragma Assert (for all I in MyVectorOfIntervals32.First_Index
                    (DAIDALUS_Altitude_Bands) .. MyVectorOfIntervals32.Last_Index
                    (DAIDALUS_Altitude_Bands) =>
                       not InRange (MyVectorOfIntervals32.Element
                      (DAIDALUS_Altitude_Bands, I), Divert_state.altitude_m));
      pragma Assert (is_Found);
   end Find_Initial_and_Climb;

   --subprogram that given the index to the conflict band that contains the c
   --urrent altitude, loops in reverse and sets the divert altitude to just
   --under the lower band of the current conflict band being interrogated.
   procedure Reset_to_Initial_and_Descend
     (Divert_state : in out state_parameters;
      initial_band_index : myvector_index_type;
      DAIDALUS_Altitude_Bands : OrderedIntervalVector32;
      Current_State : state_parameters;
      Altitude_Max_m : Altitude_Type_m;
      Altitude_Min_m : Altitude_Type_m;
      Altitude_Interval_Buffer_m : Altitude_Buffer_Type_m) with
     Pre => (Are_Legitimate_Bands (DAIDALUS_Altitude_Bands) and then
             not MyVectorOfIntervals32.Is_Empty (DAIDALUS_Altitude_Bands) and then
             scalar_constraints
                (Upper_limit         => Altitude_Max_m,
                 Lower_limit         => Altitude_Min_m,
                 Interval_Constraint => Altitude_Interval_Buffer_m) and then
             vector_constraints
                 (X                   => DAIDALUS_Altitude_Bands,
                  Upper_limit         => Altitude_Max_m,
                  Lower_limit         => Altitude_Min_m,
                  Interval_constraint => Altitude_Interval_Buffer_m) and then
              Current_Altitude_Exists_in_Bands (Current_State,
               DAIDALUS_Altitude_Bands) and then
              Divert_state = Current_State and then initial_band_index in
              MyVectorOfIntervals32.First_Index (DAIDALUS_Altitude_Bands) ..
               MyVectorOfIntervals32.Last_Index (DAIDALUS_Altitude_Bands) and then
              InRange (MyVectorOfIntervals32.Element (DAIDALUS_Altitude_Bands,
                  initial_band_index), Current_State.altitude_m)),
     Post => (Divert_state.altitude_m < Current_State.altitude_m and then
             (for all I in MyVectorOfIntervals32.First_Index
                 (DAIDALUS_Altitude_Bands) ..
              MyVectorOfIntervals32.Last_Index (DAIDALUS_Altitude_Bands) => not
              InRange (MyVectorOfIntervals32.Element (DAIDALUS_Altitude_Bands, I),
                   Divert_state.altitude_m)));

   procedure Reset_to_Initial_and_Descend
     (Divert_state : in out state_parameters;
      initial_band_index : myvector_index_type;
      DAIDALUS_Altitude_Bands : OrderedIntervalVector32;
      Current_State : state_parameters;
      Altitude_Max_m : Altitude_Type_m;
      Altitude_Min_m : Altitude_Type_m;
      Altitude_Interval_Buffer_m : Altitude_Buffer_Type_m) is
   begin
      for I in reverse MyVectorOfIntervals32.First_Index (DAIDALUS_Altitude_Bands)
        .. initial_band_index loop
         if InRange (MyVectorOfIntervals32.Element (DAIDALUS_Altitude_Bands, I),
                    Divert_state.altitude_m)
         then
            pragma Assert (MyVectorOfIntervals32.Element
                          (DAIDALUS_Altitude_Bands, I).LowerBound >=
                            Altitude_Min_m);
            Divert_state.altitude_m := MyVectorOfIntervals32.Element
              (DAIDALUS_Altitude_Bands, I).LowerBound -
              Altitude_Interval_Buffer_m;
            pragma Assert (Divert_state.altitude_m < MyVectorOfIntervals32.
                            Element (DAIDALUS_Altitude_Bands, I).LowerBound);
            pragma Assert (for all J in I .. MyVectorOfIntervals32.Last_Index
                          (DAIDALUS_Altitude_Bands) =>
                             not InRange (MyVectorOfIntervals32.Element
                            (DAIDALUS_Altitude_Bands, J),
                            Divert_state.altitude_m));
         end if;

         pragma Loop_Invariant (Divert_state.altitude_m <=
                                 Divert_state.altitude_m'Loop_Entry);
         pragma Loop_Invariant (for all J in initial_band_index  ..
                                 MyVectorOfIntervals32.Last_Index
                                   (DAIDALUS_Altitude_Bands) =>
                                  not InRange (MyVectorOfIntervals32.Element
                                 (DAIDALUS_Altitude_Bands, J),
                                 Divert_state.altitude_m));
         pragma Loop_Invariant (for all J in I .. initial_band_index =>
                                  not InRange (MyVectorOfIntervals32.Element
                                 (DAIDALUS_Altitude_Bands, J),
                                 Divert_state.altitude_m));
      end loop;

   end Reset_to_Initial_and_Descend;

   --subprogram that attempts to find a recovery interval that has both bounds
   --above the currrent altitude. divert altitutde is set to be just within the
   --first recovery interval that meets that criterion looping from lowest to
   --highest
      procedure Recovery_Climb
     (Divert_State : in out state_parameters;
      Recovery_Altitude_Bands : OrderedIntervalVector32;
      Current_State : state_parameters;
      Altitude_Max_m : Altitude_Type_m;
      Altitude_Min_m : Altitude_Type_m;
      Altitude_Interval_Buffer_m : Altitude_Buffer_Type_m;
      is_Recovery_Found_by_Climb : in out Boolean;
      acceptable_action_found : in out Boolean) with
     Pre => (Are_Legitimate_Bands (Recovery_Altitude_Bands) and then
             not MyVectorOfIntervals32.Is_Empty (Recovery_Altitude_Bands)
             and then scalar_constraints
                         (Upper_limit         => Altitude_Max_m,
                          Lower_limit         => Altitude_Min_m,
                          Interval_Constraint => Altitude_Interval_Buffer_m)
             and then vector_constraints
                         (X                   => Recovery_Altitude_Bands,
                          Upper_limit         => Altitude_Max_m,
                          Lower_limit         => Altitude_Min_m,
                          Interval_constraint => Altitude_Interval_Buffer_m)
             and then is_Recovery_Found_by_Climb = False and then
             Divert_State.altitude_m < Altitude_Min_m and then not
                 MyVectorOfIntervals32.Is_Empty (Recovery_Altitude_Bands)),
     Post => ((is_Recovery_Found_by_Climb and then acceptable_action_found
              and then Successful_Placement_in_Recovery_Bands
                (Recovery_Altitude_Bands    => Recovery_Altitude_Bands,
                 Divert_state               => Divert_State))
              or else (not is_Recovery_Found_by_Climb and then
                    Divert_State.altitude_m < Altitude_Min_m and then
                  (for all I in
                     MyVectorOfIntervals32.First_Index (Recovery_Altitude_Bands) ..
                       MyVectorOfIntervals32.Last_Index (Recovery_Altitude_Bands)
                   => not
                     (MyVectorOfIntervals32.Element (Recovery_Altitude_Bands, I).
                          LowerBound > Current_State.altitude_m and then
                      MyVectorOfIntervals32.Element (Recovery_Altitude_Bands, I).
                        UpperBound > Current_State.altitude_m))));
   procedure Recovery_Climb
     (Divert_State : in out state_parameters;
      Recovery_Altitude_Bands : OrderedIntervalVector32;
      Current_State : state_parameters;
      Altitude_Max_m : Altitude_Type_m;
      Altitude_Min_m : Altitude_Type_m;
      Altitude_Interval_Buffer_m : Altitude_Buffer_Type_m;
      is_Recovery_Found_by_Climb : in out Boolean;
      acceptable_action_found : in out Boolean) is
   begin
      for I in MyVectorOfIntervals32.First_Index (Recovery_Altitude_Bands) ..
        MyVectorOfIntervals32.Last_Index (Recovery_Altitude_Bands) loop
         if MyVectorOfIntervals32.Element (Recovery_Altitude_Bands, I).LowerBound >
          Current_State.altitude_m and then MyVectorOfIntervals32.Element
           (Recovery_Altitude_Bands, I).UpperBound > Current_State.altitude_m
         then
            pragma Assert (MyVectorOfIntervals32.Element
                          (Recovery_Altitude_Bands, I).UpperBound <=
                            Altitude_Max_m);
            Divert_State.altitude_m := MyVectorOfIntervals32.Element
              (Recovery_Altitude_Bands, I).LowerBound +
              Altitude_Interval_Buffer_m / 2.0;
            pragma Assert (Divert_State.altitude_m = MyVectorOfIntervals32.Element
                          (Recovery_Altitude_Bands, I).LowerBound +
                            Altitude_Interval_Buffer_m / 2.0);
            pragma Assert (MyVectorOfIntervals32.Element
                          (Recovery_Altitude_Bands, I).UpperBound -
                            MyVectorOfIntervals32.Element
                              (Recovery_Altitude_Bands, I).LowerBound >=
                            2.0 * Altitude_Interval_Buffer_m);
            pragma Assert (Divert_State.altitude_m > MyVectorOfIntervals32.Element
                            (Recovery_Altitude_Bands, I).LowerBound and then
                           Divert_State.altitude_m < MyVectorOfIntervals32.Element
                             (Recovery_Altitude_Bands, I).UpperBound);
            pragma Assert (InRange (MyVectorOfIntervals32.Element
                        (Recovery_Altitude_Bands, I), Divert_State.altitude_m));
            pragma Assert (Successful_Placement_in_Recovery_Bands
                          (Recovery_Altitude_Bands, Divert_State));
            is_Recovery_Found_by_Climb := True;
            acceptable_action_found := True;
            pragma Assert (is_Recovery_Found_by_Climb);
            exit;
         end if;

         pragma Loop_Invariant (Divert_State.altitude_m < Altitude_Min_m);
         pragma Loop_Invariant (not is_Recovery_Found_by_Climb);
         pragma Loop_Invariant (for all J in MyVectorOfIntervals32.First_Index
                               (Recovery_Altitude_Bands) .. I =>
                                  not (MyVectorOfIntervals32.Element
                                 (Recovery_Altitude_Bands, J).LowerBound >
                                   Current_State.altitude_m and then
                                 MyVectorOfIntervals32.Element
                                   (Recovery_Altitude_Bands, J).UpperBound >
                                   Current_State.altitude_m));
      end loop;
   end Recovery_Climb;

   --subprogram that finds a recovery band with both bounds less than the
   --current altitude. loops over recovery bands from highest to lowest
   procedure Recovery_Descend
     (Divert_State : in out state_parameters;
      Recovery_Altitude_Bands : OrderedIntervalVector32;
      Current_State : state_parameters;
      Altitude_Max_m : Altitude_Type_m;
      Altitude_Min_m : Altitude_Type_m;
      Altitude_Interval_Buffer_m : Altitude_Buffer_Type_m;
      is_Recovery_Found : in out Boolean;
      acceptable_action_found : in out Boolean) with
     Pre => Are_Legitimate_Bands (Recovery_Altitude_Bands) and then
            is_Recovery_Found = False and then not
            MyVectorOfIntervals32.Is_Empty (Recovery_Altitude_Bands) and then
           scalar_constraints (Upper_limit         => Altitude_Max_m,
                              Lower_limit         => Altitude_Min_m,
                              Interval_Constraint => Altitude_Interval_Buffer_m)
            and then vector_constraints
                         (X                   => Recovery_Altitude_Bands,
                          Upper_limit         => Altitude_Max_m,
                          Lower_limit         => Altitude_Min_m,
                          Interval_constraint => Altitude_Interval_Buffer_m)
            and then Divert_State.altitude_m < Altitude_Min_m and then not
                MyVectorOfIntervals32.Is_Empty (Recovery_Altitude_Bands)
            and then Expected_Recovery_Bands_Nature
                      (Recovery_Altitude_Bands    => Recovery_Altitude_Bands,
                       Current_state              => Current_State)
            and then (for all I in MyVectorOfIntervals32.First_Index
                     (Recovery_Altitude_Bands) .. MyVectorOfIntervals32.Last_Index
                      (Recovery_Altitude_Bands) => not
                       (MyVectorOfIntervals32.Element (Recovery_Altitude_Bands, I)
                         .LowerBound > Current_State.altitude_m and then
                        MyVectorOfIntervals32.Element (Recovery_Altitude_Bands, I)
                         .UpperBound > Current_State.altitude_m)),
     Post => (is_Recovery_Found and then acceptable_action_found and then
                Successful_Placement_in_Recovery_Bands
                  (Recovery_Altitude_Bands    => Recovery_Altitude_Bands,
                   Divert_state               => Divert_State));

   procedure Recovery_Descend
     (Divert_State : in out state_parameters;
      Recovery_Altitude_Bands : OrderedIntervalVector32;
      Current_State : state_parameters;
      Altitude_Max_m : Altitude_Type_m;
      Altitude_Min_m : Altitude_Type_m;
      Altitude_Interval_Buffer_m : Altitude_Buffer_Type_m;
      is_Recovery_Found : in out Boolean;
      acceptable_action_found : in out Boolean) is
   begin

      for I in reverse MyVectorOfIntervals32.First_Index (Recovery_Altitude_Bands)
        .. MyVectorOfIntervals32.Last_Index (Recovery_Altitude_Bands) loop
         if MyVectorOfIntervals32.Element (Recovery_Altitude_Bands, I).LowerBound <
           Current_State.altitude_m and then MyVectorOfIntervals32.Element
           (Recovery_Altitude_Bands, I).UpperBound < Current_State.altitude_m
         then
            pragma Assert (MyVectorOfIntervals32.Element
                          (Recovery_Altitude_Bands, I).LowerBound >=
                            Altitude_Min_m);
            Divert_State.altitude_m := MyVectorOfIntervals32.Element
              (Recovery_Altitude_Bands, I).UpperBound -
              Altitude_Interval_Buffer_m / 2.0;

            pragma Assert (MyVectorOfIntervals32.Element
                          (Recovery_Altitude_Bands, I).UpperBound -
                            MyVectorOfIntervals32.Element
                              (Recovery_Altitude_Bands, I).LowerBound
                            >= 2.0 * Altitude_Interval_Buffer_m);
            pragma Assert (Divert_State.altitude_m < MyVectorOfIntervals32.Element
                           (Recovery_Altitude_Bands, I).UpperBound and then
                          Divert_State.altitude_m > MyVectorOfIntervals32.Element
                            (Recovery_Altitude_Bands, I).LowerBound);
            pragma Assert (InRange (MyVectorOfIntervals32.Element
                        (Recovery_Altitude_Bands, I), Divert_State.altitude_m));

            is_Recovery_Found := True;
            acceptable_action_found := True;
            pragma Assert (is_Recovery_Found);
            pragma Assert (Successful_Placement_in_Recovery_Bands
                         (Recovery_Altitude_Bands    => Recovery_Altitude_Bands,
                          Divert_state               => Divert_State));

            exit;
         end if;

         pragma Loop_Invariant (Divert_State.altitude_m =
                                 Divert_State.altitude_m'Loop_Entry);
         pragma Loop_Invariant (not is_Recovery_Found);
         pragma Loop_Invariant (for all J in reverse I ..
                                 MyVectorOfIntervals32.Last_Index
                                   (Recovery_Altitude_Bands)  =>
                                  not (MyVectorOfIntervals32.Element
                                 (Recovery_Altitude_Bands, J).LowerBound <
                                   Current_State.altitude_m and then
                                 MyVectorOfIntervals32.Element
                                   (Recovery_Altitude_Bands, J).UpperBound <
                                   Current_State.altitude_m));

      end loop;
      pragma Assert (if (for some I in MyVectorOfIntervals32.First_Index
                    (Recovery_Altitude_Bands) .. MyVectorOfIntervals32.Last_Index
                    (Recovery_Altitude_Bands) =>
                      (MyVectorOfIntervals32.Element (Recovery_Altitude_Bands, I).
                           LowerBound < Current_State.altitude_m and then
                       MyVectorOfIntervals32.Element (Recovery_Altitude_Bands, I).
                           UpperBound < Current_State.altitude_m)) then
                           is_Recovery_Found);
   end Recovery_Descend;

   -----------------------------------
   -- Found_WCV_Altitude_Resolution --
   -----------------------------------

   procedure Found_WCV_Altitude_Resolution
     (DAIDALUS_Altitude_Bands : OrderedIntervalVector32;
      Recovery_Altitude_Bands : OrderedIntervalVector32;
      Current_State : state_parameters;
      Altitude_Max_m : Altitude_Type_m;
      Altitude_Min_m : Altitude_Type_m;
      Altitude_Interval_Buffer_m : Altitude_Buffer_Type_m;
      Divert_State : out state_parameters;
      found_acceptable_action_flag : out Boolean)
   is
      --local inidicator for guaranteed collision avoidance
      local_acceptable_action_flag : Boolean;
      is_Found : Boolean := False;
      is_Recovery_Found : Boolean := False;
      initial_band : myvector_index_type := myvector_index_type'First;

   begin
      --initialize divert state with the current state
      Divert_State := Current_State;

      if not MyVectorOfIntervals32.Is_Empty (DAIDALUS_Altitude_Bands) then

         --find the index to the conflict band that contains the current
         --altitude and move the divert altitude higher if in a conflict band
         Find_Initial_and_Climb
           (Divert_state             => Divert_State,
            is_Found                 => is_Found,
            initial_band_index       => initial_band,
            DAIDALUS_Altitude_Bands  => DAIDALUS_Altitude_Bands,
            Current_state            => Current_State,
            Altitude_Max_m           => Altitude_Max_m,
            Altitude_Min_m           => Altitude_Min_m,
            Altitude_Interval_Buffer_m => Altitude_Interval_Buffer_m);
         pragma Assert (is_Found);

         --check if the divert altitude was raised beyond the maximum altitude
         --if so, look for an altitude less than the current as conflict
         --resolution
         if Divert_State.altitude_m > Altitude_Max_m then
            --reset divert state to current state
            Divert_State := Current_State;
            --starting at conflict band containing the current altitude, move
            --divert altitude down if in a conflict band
            Reset_to_Initial_and_Descend
              (Divert_state             => Divert_State,
               initial_band_index       => initial_band,
               DAIDALUS_Altitude_Bands  => DAIDALUS_Altitude_Bands,
               Current_State            => Current_State,
               Altitude_Max_m           => Altitude_Max_m,
               Altitude_Min_m           => Altitude_Min_m,
               Altitude_Interval_Buffer_m => Altitude_Interval_Buffer_m);

         else
            --if divert altitude is not above the maximum, then found a
            --resolution by climbing
            pragma Assume (Conflict_and_Recovery_Complimentary_Nature
                          (Divert_State, DAIDALUS_Altitude_Bands,
                             Recovery_Altitude_Bands, Altitude_Min_m,
                             Altitude_Max_m));
            local_acceptable_action_flag :=
              (if MyVectorOfIntervals32.Is_Empty (Recovery_Altitude_Bands) then
                    True else Successful_Placement_in_Recovery_Bands
                               (Recovery_Altitude_Bands, Divert_State));
            pragma Assert (Found_Acceptable_Action (local_acceptable_action_flag
                           , DAIDALUS_Altitude_Bands, Recovery_Altitude_Bands,
                          Divert_State.altitude_m));
         end if;

         --check if the divert altitude was lowered beyond the minimum altitude
         --if so, finding a resolution using only the conflict bands has failed.
         --Use recovery bands to mitigate loss of well clear (LOWC)
         if Divert_State.altitude_m < Altitude_Min_m then
            local_acceptable_action_flag := False;
            if not MyVectorOfIntervals32.Is_Empty (Recovery_Altitude_Bands) then
               --check for a recovery band with bounds greater than current
               --altitude to set divert altitude
               Recovery_Climb
                 (Divert_State               => Divert_State,
                  Recovery_Altitude_Bands    => Recovery_Altitude_Bands,
                  Current_State              => Current_State,
                  Altitude_Max_m             => Altitude_Max_m,
                  Altitude_Min_m             => Altitude_Min_m,
                  Altitude_Interval_Buffer_m   => Altitude_Interval_Buffer_m,
                  is_Recovery_Found_by_Climb => is_Recovery_Found,
                  acceptable_action_found     => local_acceptable_action_flag);

               -- Relate to the solver that if recovery was not found by
               --climbing, it must by found by descending since recovery
               --information available
               pragma Assume (if not is_Recovery_Found then
                               (for some I in reverse
                                  MyVectorOfIntervals32.First_Index
                                    (Recovery_Altitude_Bands) ..
                                    MyVectorOfIntervals32.Last_Index
                                  (Recovery_Altitude_Bands) =>
                                    (MyVectorOfIntervals32.Element
                                         (Recovery_Altitude_Bands, I).LowerBound
                                     < Current_State.altitude_m and then
                                     MyVectorOfIntervals32.Element
                                       (Recovery_Altitude_Bands, I).UpperBound <
                                           Current_State.altitude_m)));
               if not is_Recovery_Found then
                  Recovery_Descend
                   (Divert_State               => Divert_State,
                    Recovery_Altitude_Bands    => Recovery_Altitude_Bands,
                    Current_State              => Current_State,
                    Altitude_Max_m             => Altitude_Max_m,
                    Altitude_Min_m             => Altitude_Min_m,
                    Altitude_Interval_Buffer_m => Altitude_Interval_Buffer_m,
                    is_Recovery_Found          => is_Recovery_Found,
                    acceptable_action_found    => local_acceptable_action_flag);

                  pragma Assert (is_Recovery_Found);
               end if;

               pragma Assert (local_acceptable_action_flag);
               pragma Assert (Found_Acceptable_Action
                             (local_acceptable_action_flag,
                              DAIDALUS_Altitude_Bands, Recovery_Altitude_Bands,
                              Divert_State.altitude_m));
            else
               local_acceptable_action_flag := False;
               --default/reversionary behavior is to climb to maxium altitude
               Divert_State.altitude_m := Altitude_Max_m;
               pragma Assert (Revert_behavior
                             (local_acceptable_action_flag,
                                DAIDALUS_Altitude_Bands, Altitude_Max_m,
                                Divert_State.altitude_m));

            end if;
         else
               --found divert altitude by descending
               pragma Assume (Conflict_and_Recovery_Complimentary_Nature
                          (Divert_State, DAIDALUS_Altitude_Bands,
                             Recovery_Altitude_Bands, Altitude_Min_m,
                             Altitude_Max_m));
               local_acceptable_action_flag := (if MyVectorOfIntervals32.Is_Empty
                                               (Recovery_Altitude_Bands)    then
                                                   True else
                                          Successful_Placement_in_Recovery_Bands
                                               (Recovery_Altitude_Bands,
                                                Divert_State));
            pragma Assert (local_acceptable_action_flag);
            pragma Assert (Found_Acceptable_Action (local_acceptable_action_flag,
                          DAIDALUS_Altitude_Bands, Recovery_Altitude_Bands,
                          Divert_State.altitude_m));
         end if;

      else
         --no conflict band information therefore this algorithm should not be
         --called.
         local_acceptable_action_flag := False;
         pragma Assert (IsImproperlyConfigured (local_acceptable_action_flag,
                       DAIDALUS_Altitude_Bands, Divert_State.altitude_m,
                       Current_State.altitude_m));

      end if;

      found_acceptable_action_flag := local_acceptable_action_flag;
      --prove the post condition
      pragma Assert (Found_Acceptable_Action (found_acceptable_action_flag,
                    DAIDALUS_Altitude_Bands, Recovery_Altitude_Bands,
                    Divert_State.altitude_m) or else
                    IsImproperlyConfigured (found_acceptable_action_flag,
                      DAIDALUS_Altitude_Bands, Divert_State.altitude_m,
                      Current_State.altitude_m) or else
                    Revert_behavior (found_acceptable_action_flag,
                      DAIDALUS_Altitude_Bands, Altitude_Max_m,
                      Divert_State.altitude_m));

   end Found_WCV_Altitude_Resolution;
end Altitude_Resolution;
