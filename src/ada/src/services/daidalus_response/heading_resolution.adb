package body Heading_Resolution with SPARK_Mode => On
is

   ----------------------------------
   -- Found_WCV_Heading_Resolution --
   ----------------------------------

   --predicate indicting whether or not the divert altitude is contained within
   --a single recovery band interval
   function Successful_Placement_in_Recovery_Bands
     (Recovery_Heading_Bands : OrderedIntervalVector;
      Divert_State : state_parameters) return Boolean
   is
     (for some I in MyVectorOfIntervals.First_Index (Recovery_Heading_Bands) ..
          MyVectorOfIntervals.Last_Index (Recovery_Heading_Bands) =>
           InRange (MyVectorOfIntervals.Element (Recovery_Heading_Bands, I),
        Divert_State.heading_deg));

   --Predicate describing the relationship between conflict bands and recovery
   --bands reported out by DAIDALUS.  Recovery bands are presented when the
   --conflict bands fully saturate the range.  Recovery bands represent the
   --portion of the range that will exit the well clear violation the quickest
   --despite the full range not being able to avoid a well clear violation.
   --Recovery bands and conflict bands are then reported out as complimentary
   --to each other.
   function Conflict_and_Recovery_Complimentary_Nature
     (Divert_State : state_parameters;
      DAIDALUS_Heading_Bands : OrderedIntervalVector;
      Recovery_Heading_Bands : OrderedIntervalVector;
      Heading_Min_deg : Heading_Type_deg;
      Heading_Max_deg : Heading_Type_deg) return Boolean is
     (if (Divert_State.heading_deg >= Heading_Min_deg) and then
        (Divert_State.heading_deg <= Heading_Max_deg) and then (not
        MyVectorOfIntervals.Is_Empty (DAIDALUS_Heading_Bands)) and then
        (not MyVectorOfIntervals.Is_Empty (Recovery_Heading_Bands)) and then
        (for all I in
          MyVectorOfIntervals.First_Index (DAIDALUS_Heading_Bands) ..
          MyVectorOfIntervals.Last_Index (DAIDALUS_Heading_Bands) =>
           not InRange (MyVectorOfIntervals.Element (DAIDALUS_Heading_Bands, I),
             Divert_State.heading_deg)) then
        (Successful_Placement_in_Recovery_Bands
             (Recovery_Heading_Bands, Divert_State))) with Ghost;

   --Predicate describing the expected nature of recovery band intervals
   --recovery bands, when present, are the complement to conflict bands. Thus
   --given the current state is in a conflict band there exists a recovery band
   --interval that is completly above or below the current heading in degrees.
   function Recovery_Bands_Nature
     (Recovery_Heading_Bands : OrderedIntervalVector;
      Current_State : state_parameters) return Boolean is
     (for some I in MyVectorOfIntervals.First_Index (Recovery_Heading_Bands) ..
          MyVectorOfIntervals.Last_Index (Recovery_Heading_Bands) =>
          (Current_State.heading_deg > MyVectorOfIntervals.Element
           (Recovery_Heading_Bands, I).LowerBound and then
           Current_State.heading_deg > MyVectorOfIntervals.Element
             (Recovery_Heading_Bands, I).UpperBound) or else
        (Current_State.heading_deg < MyVectorOfIntervals.Element
             (Recovery_Heading_Bands, I).LowerBound and then
         Current_State.heading_deg < MyVectorOfIntervals.Element
           (Recovery_Heading_Bands, I).UpperBound)) with Ghost;

   --subprogram to loop over the conflict intervals to determine the index of
   --the conflict interval that contains the current heading and once found,
   --set the divert heading to being greater than the upper bound of the current
   --conflict interval when the divert heading is contained.
   procedure Find_Initial_and_Turn_Right
     (Divert_State : in out state_parameters;
      is_Found : in out Boolean;
      initial_band_index : in out myvector_index_type;
      DAIDALUS_Heading_Bands : OrderedIntervalVector;
      Current_State : state_parameters;
      Heading_Max_deg : Heading_Type_deg;
      Heading_Min_deg : Heading_Type_deg;
      Heading_Interval_Buffer_Deg : Heading_Buffer_Type_deg) with
     Pre => Are_Legitimate_Bands (DAIDALUS_Heading_Bands) and then not
     MyVectorOfIntervals.Is_Empty (DAIDALUS_Heading_Bands) and then
     scalar_constraints
       (Upper_limit         => Heading_Max_deg,
        Lower_limit         => Heading_Min_deg,
        Interval_Constraint => Heading_Interval_Buffer_Deg) and then
       vector_constraints
         (X                   => DAIDALUS_Heading_Bands,
          Upper_limit         => Heading_Max_deg,
          Lower_limit         => Heading_Min_deg,
          Interval_constraint => Heading_Interval_Buffer_Deg) and then
         Current_Heading_Exists_in_Bands
           (Current_State          => Current_State,
            DAIDALUS_Heading_Bands => DAIDALUS_Heading_Bands) and then
           is_Found = False and then
           initial_band_index = myvector_index_type'First and then
           Divert_State.heading_deg = Current_State.heading_deg,
      Post => (is_Found and then initial_band_index in
                  MyVectorOfIntervals.First_Index (DAIDALUS_Heading_Bands) ..
                    MyVectorOfIntervals.Last_Index (DAIDALUS_Heading_Bands)
                and then (InRange (MyVectorOfIntervals.Element
                  (DAIDALUS_Heading_Bands, initial_band_index), Current_State.
                    heading_deg))
                and then (Divert_State.heading_deg > Current_State.heading_deg)
                and then (for all I in MyVectorOfIntervals.First_Index
                  (DAIDALUS_Heading_Bands)
                   .. MyVectorOfIntervals.Last_Index (DAIDALUS_Heading_Bands) =>
                        not InRange (MyVectorOfIntervals.Element
                      (DAIDALUS_Heading_Bands, I), Divert_State.heading_deg)));

   procedure Find_Initial_and_Turn_Right
     (Divert_State : in out state_parameters;
      is_Found : in out Boolean;
      initial_band_index : in out myvector_index_type;
      DAIDALUS_Heading_Bands : OrderedIntervalVector;
      Current_State : state_parameters;
      Heading_Max_deg : Heading_Type_deg;
      Heading_Min_deg : Heading_Type_deg;
      Heading_Interval_Buffer_Deg : Heading_Buffer_Type_deg)
   is
   begin
      for I in MyVectorOfIntervals.First_Index (DAIDALUS_Heading_Bands) ..
        MyVectorOfIntervals.Last_Index (DAIDALUS_Heading_Bands) loop
         if InRange (MyVectorOfIntervals.Element (DAIDALUS_Heading_Bands, I),
                     Divert_State.heading_deg)
         then
            pragma Assert (MyVectorOfIntervals.Element (DAIDALUS_Heading_Bands, I)
                          .UpperBound + Heading_Buffer_Type_deg'Last <=
                            Heading_Type_deg'Last);
            Divert_State.heading_deg := MyVectorOfIntervals.Element
              (DAIDALUS_Heading_Bands, I).UpperBound +
              Heading_Interval_Buffer_Deg;
            pragma Assert (MyVectorOfIntervals.Element (DAIDALUS_Heading_Bands, I)
                          .UpperBound < Divert_State.heading_deg);
            if not is_Found then
               initial_band_index := I;
               is_Found := True;
            end if;
         end if;
         pragma Assert (for all J in MyVectorOfIntervals.First_Index
                       (DAIDALUS_Heading_Bands) .. I - 1 =>
                          not InRange (MyVectorOfIntervals.Element
                        (DAIDALUS_Heading_Bands, J), Divert_State.heading_deg));
         pragma Loop_Invariant (not is_Found or else (initial_band_index <= I
                               and then Divert_State.heading_deg >
                                 Current_State.heading_deg));
         pragma Loop_Invariant (not is_Found or else InRange
                                (MyVectorOfIntervals.Element
                                   (DAIDALUS_Heading_Bands,
                                   initial_band_index), Current_State.
                                 heading_deg));
         pragma Loop_Invariant (is_Found or else
                                 (Divert_State.heading_deg = Current_State.
                                    heading_deg));
         pragma Loop_Invariant (is_Found or else
                                 (for all J in MyVectorOfIntervals.First_Index
                                  (DAIDALUS_Heading_Bands) .. I =>
                                       not InRange (MyVectorOfIntervals.Element
                                    (DAIDALUS_Heading_Bands, J), Divert_State.
                                      heading_deg)));
         pragma Loop_Invariant (not is_Found or else not InRange
                               (MyVectorOfIntervals.Element
                                  (DAIDALUS_Heading_Bands, I), Divert_State.
                                    heading_deg));
         pragma Loop_Invariant (for all J in MyVectorOfIntervals.First_Index
                               (DAIDALUS_Heading_Bands) .. I =>
                                  not InRange (MyVectorOfIntervals.Element
                                 (DAIDALUS_Heading_Bands, J), Divert_State.
                                   heading_deg));
      end loop;
   end Find_Initial_and_Turn_Right;

   --subprogram to deal with angle wrapping when looking to set a divert heading
   --by turning right.  When the previously setting the divert heading to more
   --than 360 degrees, the angle wrapped version of the divert heading is
   --checked against the the interval bands starting with the lowest interval
   --and continues to set the divert heading to just past the upperbound.
   --Loop exits early if the angle wrapped divert heading is not in the first
   --interval band
   procedure Check_Angle_Wrapped_Turn_Right
     (Divert_State : in out state_parameters;
      DAIDALUS_Heading_Bands : OrderedIntervalVector;
      Heading_Max_deg : Heading_Type_deg;
      Heading_Min_deg : Heading_Type_deg;
      Heading_Interval_Buffer_deg : Heading_Buffer_Type_deg) with
     Pre => Are_Legitimate_Bands (X => DAIDALUS_Heading_Bands) and then
            not MyVectorOfIntervals.Is_Empty (DAIDALUS_Heading_Bands) and then
            scalar_constraints
              (Upper_limit         => Heading_Max_deg,
               Lower_limit         => Heading_Min_deg,
               Interval_Constraint => Heading_Interval_Buffer_deg)
            and then vector_constraints
                       (X                   => DAIDALUS_Heading_Bands,
                        Upper_limit         => Heading_Max_deg,
                        Lower_limit         => Heading_Min_deg,
                        Interval_constraint => Heading_Interval_Buffer_deg)
            and then Divert_State.heading_deg >= Heading_Min_deg
            and then Divert_State.heading_deg <= Heading_Min_deg +
            Heading_Interval_Buffer_deg,
     Post => Divert_State.heading_deg >= Heading_Min_deg and then
            (for all I in MyVectorOfIntervals.First_Index
              (DAIDALUS_Heading_Bands) .. MyVectorOfIntervals.Last_Index
              (DAIDALUS_Heading_Bands) => not InRange (MyVectorOfIntervals.
                          Element (DAIDALUS_Heading_Bands, I), Divert_State.
                  heading_deg));

   procedure Check_Angle_Wrapped_Turn_Right
     (Divert_State : in out state_parameters;
      DAIDALUS_Heading_Bands : OrderedIntervalVector;
      Heading_Max_deg : Heading_Type_deg;
      Heading_Min_deg : Heading_Type_deg;
      Heading_Interval_Buffer_deg : Heading_Buffer_Type_deg) is
   begin

      --Asertion relating the minimum size of an interval. Useful for proving
      --adjustments to Divert_state move only a single interval
      pragma Assert (MyVectorOfIntervals.First_Element (DAIDALUS_Heading_Bands).
                      UpperBound >= MyVectorOfIntervals.First_Element
                        (DAIDALUS_Heading_Bands).LowerBound +
                      2.0 * Heading_Interval_Buffer_deg);
      --Assertion providing that either the angle wrapped divert heading is
      --contained in the first conflict band or it is less than the lower bound
      --of the first conflict band.  This is reasoned knowing that in the worst
      --case, the angle wrapped divert angle occurred when the previous upper
      --bound was 360.0 degree and the first interval's lower bound would be 0.0
      --degrees, thus divert = 0.0 + buffer where the worst case first interval
      --upper bound is 0.0 + 2.0 * buffer.
      pragma Assert (InRange (MyVectorOfIntervals.Element
                     (DAIDALUS_Heading_Bands,
                    MyVectorOfIntervals.First_Index (DAIDALUS_Heading_Bands)),
                    Divert_State.heading_deg)
                    or else (Divert_State.heading_deg <= MyVectorOfIntervals.
                        Element (DAIDALUS_Heading_Bands, MyVectorOfIntervals.
                            First_Index (DAIDALUS_Heading_Bands)).LowerBound));
      for I in MyVectorOfIntervals.First_Index (DAIDALUS_Heading_Bands) ..
        MyVectorOfIntervals.Last_Index (DAIDALUS_Heading_Bands) loop
         if InRange (MyVectorOfIntervals.Element (DAIDALUS_Heading_Bands, I),
                     Divert_State.heading_deg)
         then
            --Assertion to show the addition that follows does not overflow the
            --type.
            pragma Assert (MyVectorOfIntervals.Element
                          (DAIDALUS_Heading_Bands, I).UpperBound <=
                            Heading_Max_deg);

            Divert_State.heading_deg := MyVectorOfIntervals.Element
              (DAIDALUS_Heading_Bands, I).UpperBound +
              Heading_Interval_Buffer_deg;
            pragma Assert (Divert_State.heading_deg > MyVectorOfIntervals.
                             Element (DAIDALUS_Heading_Bands, I).UpperBound);
            --The next three assertions together show that the adjustment to the
            --divert heading places the divert less that the next interval's
            --upper bound and greater than the current upper bound.  Thus either
            --contained by the next interval or less than the lower bound of the
            --next interval
            pragma Assert (if I < MyVectorOfIntervals.Last_Index
                          (DAIDALUS_Heading_Bands) then
                            (MyVectorOfIntervals.Element
                             (DAIDALUS_Heading_Bands, I + 1).LowerBound >=
                                 MyVectorOfIntervals.Element
                               (DAIDALUS_Heading_Bands, I).UpperBound));
            pragma Assert (if I < MyVectorOfIntervals.Last_Index
                          (DAIDALUS_Heading_Bands) then
                            (MyVectorOfIntervals.Element
                             (DAIDALUS_Heading_Bands, I + 1).LowerBound +
                                 2.0 * Heading_Interval_Buffer_deg
                             <= MyVectorOfIntervals.Element
                               (DAIDALUS_Heading_Bands, I + 1).UpperBound));
            pragma Assert (if I < MyVectorOfIntervals.Last_Index
                          (DAIDALUS_Heading_Bands) then
                            (Divert_State.heading_deg < MyVectorOfIntervals.
                           Element (DAIDALUS_Heading_Bands, I + 1).UpperBound));
         else
            --if the else branch is executed then exit the loop early.
            --Assertion shows that if the else branch is reached then the divert
            --heading is lower than the current interval's lower bound.
            pragma Assert (Divert_State.heading_deg <= MyVectorOfIntervals.
                            Element (DAIDALUS_Heading_Bands, I).LowerBound);
            --Asertion showing for all intervals before the current, the divert
            --heading is not contained either from previously being moved out of
            --previous intervals or never being contained in any prior interval.
            pragma Assert (for all J in MyVectorOfIntervals.First_Index
                          (DAIDALUS_Heading_Bands) .. I =>
                             not InRange (MyVectorOfIntervals.Element
                            (DAIDALUS_Heading_Bands, J), Divert_State.
                              heading_deg));

            pragma Assert (MyVectorOfIntervals.Element
                           (DAIDALUS_Heading_Bands, I).UpperBound <
                             Divert_State.heading_deg or else
                          Divert_State.heading_deg <= MyVectorOfIntervals.
                            Element (DAIDALUS_Heading_Bands, I).LowerBound);
            pragma Assert (if Divert_State.heading_deg <= MyVectorOfIntervals.
                            Element (DAIDALUS_Heading_Bands, I).LowerBound then
                            (for all J in I .. MyVectorOfIntervals.Last_Index
                             (DAIDALUS_Heading_Bands) =>
                                  not InRange (MyVectorOfIntervals.Element
                               (DAIDALUS_Heading_Bands, J), Divert_State.
                                 heading_deg)));
            exit;
         end if;

         pragma Assert (for all J in MyVectorOfIntervals.First_Index
                       (DAIDALUS_Heading_Bands) .. I - 1 =>
                         MyVectorOfIntervals.Element (DAIDALUS_Heading_Bands, I)
                        .UpperBound > MyVectorOfIntervals.Element
                           (DAIDALUS_Heading_Bands, J).UpperBound);
         pragma Loop_Invariant (Divert_State.heading_deg > MyVectorOfIntervals.
                                Element (DAIDALUS_Heading_Bands, I).UpperBound);
         pragma Loop_Invariant (for all J in MyVectorOfIntervals.First_Index
                               (DAIDALUS_Heading_Bands) .. I =>
                                  not InRange (MyVectorOfIntervals.Element
                                 (DAIDALUS_Heading_Bands, J), Divert_State.
                                   heading_deg));
         pragma Loop_Invariant (if I < MyVectorOfIntervals.Last_Index
                               (DAIDALUS_Heading_Bands) then
                                  InRange (MyVectorOfIntervals.Element
                                 (DAIDALUS_Heading_Bands, I + 1), Divert_State.
                                   heading_deg)
                               or else Divert_State.heading_deg <=
                                 MyVectorOfIntervals.Element
                                   (DAIDALUS_Heading_Bands, I + 1).LowerBound);
      end loop;

      pragma Assert (for all I in MyVectorOfIntervals.First_Index
                    (DAIDALUS_Heading_Bands) .. MyVectorOfIntervals.Last_Index
                    (DAIDALUS_Heading_Bands) =>
                       not InRange (MyVectorOfIntervals.Element
                      (DAIDALUS_Heading_Bands, I), Divert_State.heading_deg));
   end Check_Angle_Wrapped_Turn_Right;

   --subprogram that starting with the conflict band containing the current
   --heading loops over the intervals from highest to lowest and sets the divert
   --heading to just below the lower bound if the current interval contained the
   --divert heading, prior to adjustment.
   procedure Turn_Left
     (DAIDALUS_Heading_Bands : OrderedIntervalVector;
      initial_band_index : myvector_index_type;
      Heading_Max_deg : Heading_Type_deg;
      Heading_Min_deg : Heading_Type_deg;
      Heading_Interval_Buffer_deg : Heading_Buffer_Type_deg;
      Current_State : state_parameters;
      Divert_State : in out state_parameters) with
     Pre => Are_Legitimate_Bands (DAIDALUS_Heading_Bands) and then not
            MyVectorOfIntervals.Is_Empty (DAIDALUS_Heading_Bands)
            and then scalar_constraints
                       (Upper_limit         => Heading_Max_deg,
                        Lower_limit         => Heading_Min_deg,
                        Interval_Constraint => Heading_Interval_Buffer_deg)
            and then vector_constraints
                       (X                   => DAIDALUS_Heading_Bands,
                        Upper_limit         => Heading_Max_deg,
                        Lower_limit         => Heading_Min_deg,
                        Interval_constraint => Heading_Interval_Buffer_deg)
            and then (initial_band_index in MyVectorOfIntervals.First_Index
                      (DAIDALUS_Heading_Bands) .. MyVectorOfIntervals.Last_Index
                      (DAIDALUS_Heading_Bands))
            and then (InRange (MyVectorOfIntervals.Element
                              (DAIDALUS_Heading_Bands, initial_band_index),
                              Current_State.heading_deg))
            and then Divert_State.heading_deg = Current_State.heading_deg,
     Post => (for all I in MyVectorOfIntervals.First_Index
             (DAIDALUS_Heading_Bands) .. MyVectorOfIntervals.Last_Index
             (DAIDALUS_Heading_Bands)
             => not InRange (MyVectorOfIntervals.Element
               (DAIDALUS_Heading_Bands, I), Divert_State.heading_deg));

   procedure Turn_Left
     (DAIDALUS_Heading_Bands : OrderedIntervalVector;
      initial_band_index : myvector_index_type;
      Heading_Max_deg : Heading_Type_deg;
      Heading_Min_deg : Heading_Type_deg;
      Heading_Interval_Buffer_deg : Heading_Buffer_Type_deg;
      Current_State : state_parameters;
      Divert_State : in out state_parameters) is
   begin
      for I in reverse MyVectorOfIntervals.First_Index (DAIDALUS_Heading_Bands)
        .. initial_band_index loop
         if InRange (MyVectorOfIntervals.Element (DAIDALUS_Heading_Bands, I),
                     Divert_State.heading_deg)
         then
            pragma Assert (MyVectorOfIntervals.Element (DAIDALUS_Heading_Bands,
                           I).LowerBound >= Heading_Min_deg);
            Divert_State.heading_deg := MyVectorOfIntervals.Element
              (DAIDALUS_Heading_Bands, I).LowerBound -
              Heading_Interval_Buffer_deg;
            pragma Assert (Divert_State.heading_deg <= MyVectorOfIntervals.
                            Element (DAIDALUS_Heading_Bands, I).LowerBound);
            pragma Assert (not InRange (MyVectorOfIntervals.Element
                          (DAIDALUS_Heading_Bands, I), Divert_State.
                            heading_deg));
         end if;
         pragma Loop_Invariant (Divert_State.heading_deg <
                                 Current_State.heading_deg);
         pragma Loop_Invariant (for all J in I .. MyVectorOfIntervals.Last_Index
                               (DAIDALUS_Heading_Bands) =>
                                  not InRange (MyVectorOfIntervals.Element
                                 (DAIDALUS_Heading_Bands, J), Divert_State.
                                   heading_deg));
      end loop;
   end Turn_Left;

   --subprogram to check the angle wrapped divert heading to handle the edge
   --case when previously the diver heading was set to below the minimum heading
   --loops over conflict intervals from highest to lowest and continually
   --adjusts the divert heading to below the lower bound if prior to adjustment
   --the interval contained the divert heading.  Loop terminates early if the
   --angle wrapped divert heading is not contained in the highest interval
   procedure Check_Angle_Wrapped_Turn_Left
     (DAIDALUS_Heading_Bands : OrderedIntervalVector;
      Heading_Max_deg : Heading_Type_deg;
      Heading_Min_deg : Heading_Type_deg;
      Heading_Interval_Buffer_deg : Heading_Buffer_Type_deg;
      Divert_State : in out state_parameters) with
     Pre => Are_Legitimate_Bands (DAIDALUS_Heading_Bands) and then
            not MyVectorOfIntervals.Is_Empty (DAIDALUS_Heading_Bands) and then
            scalar_constraints
              (Upper_limit         => Heading_Max_deg,
               Lower_limit         => Heading_Min_deg,
               Interval_Constraint => Heading_Interval_Buffer_deg)
            and then vector_constraints
                       (X                   => DAIDALUS_Heading_Bands,
                        Upper_limit         => Heading_Max_deg,
                        Lower_limit         => Heading_Min_deg,
                        Interval_constraint => Heading_Interval_Buffer_deg)
            and then Divert_State.heading_deg >= Heading_Max_deg -
             Heading_Interval_Buffer_deg,
     Post => True and then
             (for all I in MyVectorOfIntervals.First_Index
                    (DAIDALUS_Heading_Bands)
                    .. MyVectorOfIntervals.Last_Index (DAIDALUS_Heading_Bands)
              => not InRange (MyVectorOfIntervals.Element
                (DAIDALUS_Heading_Bands, I), Divert_State.heading_deg));

   procedure Check_Angle_Wrapped_Turn_Left
     (DAIDALUS_Heading_Bands : OrderedIntervalVector;
      Heading_Max_deg : Heading_Type_deg;
      Heading_Min_deg : Heading_Type_deg;
      Heading_Interval_Buffer_deg : Heading_Buffer_Type_deg;
      Divert_State : in out state_parameters) is
   begin

      pragma Assert (MyVectorOfIntervals.Last_Element (DAIDALUS_Heading_Bands).
                      UpperBound <= Heading_Max_deg);

      pragma Assert (MyVectorOfIntervals.Element (DAIDALUS_Heading_Bands,
                    MyVectorOfIntervals.Last_Index (DAIDALUS_Heading_Bands)).
                      LowerBound < Divert_State.heading_deg);

      pragma Assert (InRange (MyVectorOfIntervals.Element
                     (DAIDALUS_Heading_Bands, MyVectorOfIntervals.Last_Index
                        (DAIDALUS_Heading_Bands)),
                    Divert_State.heading_deg) or else
                    MyVectorOfIntervals.Element (DAIDALUS_Heading_Bands,
                      MyVectorOfIntervals.Last_Index (DAIDALUS_Heading_Bands)).
                      UpperBound < Divert_State.heading_deg);

      for I in reverse MyVectorOfIntervals.First_Index (DAIDALUS_Heading_Bands)
        .. MyVectorOfIntervals.Last_Index (DAIDALUS_Heading_Bands) loop
         if InRange (MyVectorOfIntervals.Element (DAIDALUS_Heading_Bands, I),
                     Divert_State.heading_deg)
         then
            pragma Assert (MyVectorOfIntervals.Element (DAIDALUS_Heading_Bands,
                           I).LowerBound >= Heading_Min_deg);
            Divert_State.heading_deg := MyVectorOfIntervals.Element
              (DAIDALUS_Heading_Bands, I).LowerBound -
              Heading_Interval_Buffer_deg;
            pragma Assert (Divert_State.heading_deg < MyVectorOfIntervals.
                            Element (DAIDALUS_Heading_Bands, I).LowerBound);
            --  pragma Assert(not InRange(MyVectorOfIntervals.Element
            --                (DAIDALUS_Heading_Bands, I), Divert_State.
            --                  heading_deg));
            pragma Assert (if I > MyVectorOfIntervals.First_Index
                          (DAIDALUS_Heading_Bands) then
                            (MyVectorOfIntervals.Element
                             (DAIDALUS_Heading_Bands, I).LowerBound >=
                                 MyVectorOfIntervals.Element
                               (DAIDALUS_Heading_Bands, I - 1).UpperBound));
            pragma Assert (if I > MyVectorOfIntervals.First_Index
                          (DAIDALUS_Heading_Bands) then
                            (MyVectorOfIntervals.Element
                             (DAIDALUS_Heading_Bands, I - 1).LowerBound <=
                                 MyVectorOfIntervals.Element
                               (DAIDALUS_Heading_Bands, I - 1).UpperBound -
                                 2.0 * Heading_Interval_Buffer_deg));
            pragma Assert (if I > MyVectorOfIntervals.First_Index
                          (DAIDALUS_Heading_Bands) then
                            (Divert_State.heading_deg > MyVectorOfIntervals.
                           Element (DAIDALUS_Heading_Bands, I - 1).LowerBound));
         else
            pragma Assert (Divert_State.heading_deg > MyVectorOfIntervals.
                             Element (DAIDALUS_Heading_Bands, I).UpperBound);
            pragma Assert (for all J in I .. MyVectorOfIntervals.Last_Index
                          (DAIDALUS_Heading_Bands) =>
                             not InRange (MyVectorOfIntervals.Element
                            (DAIDALUS_Heading_Bands, J), Divert_State.
                              heading_deg));
            pragma Assert (Divert_State.heading_deg > MyVectorOfIntervals.
                             Element (DAIDALUS_Heading_Bands, I).UpperBound or
                           else
                          Divert_State.heading_deg <= MyVectorOfIntervals.
                            Element  (DAIDALUS_Heading_Bands, I).LowerBound);
            pragma Assert (if Divert_State.heading_deg > MyVectorOfIntervals.
                            Element (DAIDALUS_Heading_Bands, I).UpperBound then
                            (for all J in MyVectorOfIntervals.First_Index
                             (DAIDALUS_Heading_Bands) .. I =>
                                  not InRange (MyVectorOfIntervals.Element
                               (DAIDALUS_Heading_Bands, J), Divert_State.
                                 heading_deg)));
            exit;
         end if;

         pragma Assert (for all J in I .. MyVectorOfIntervals.Last_Index
                       (DAIDALUS_Heading_Bands) => MyVectorOfIntervals.
                         Element (DAIDALUS_Heading_Bands, I).LowerBound <=
                         MyVectorOfIntervals.Element (DAIDALUS_Heading_Bands, J)
                        .LowerBound);
         pragma Loop_Invariant (Divert_State.heading_deg < MyVectorOfIntervals.
                                Element (DAIDALUS_Heading_Bands, I).LowerBound);
         pragma Loop_Invariant (for all J in I .. MyVectorOfIntervals.Last_Index
                               (DAIDALUS_Heading_Bands) =>
                                  not InRange (MyVectorOfIntervals.Element
                                 (DAIDALUS_Heading_Bands, J), Divert_State.
                                   heading_deg));
         pragma Loop_Invariant (if I > MyVectorOfIntervals.First_Index
                               (DAIDALUS_Heading_Bands) then
                                 (InRange (MyVectorOfIntervals.Element
                                  (DAIDALUS_Heading_Bands, I - 1), Divert_State.
                                    heading_deg)
                                  or else (Divert_State.heading_deg >
                                        MyVectorOfIntervals.Element
                                  (DAIDALUS_Heading_Bands, I - 1).UpperBound)));

      end loop;

      pragma Assert (for all I in MyVectorOfIntervals.First_Index
                    (DAIDALUS_Heading_Bands) .. MyVectorOfIntervals.Last_Index
                    (DAIDALUS_Heading_Bands)
                    => not InRange (MyVectorOfIntervals.Element
                      (DAIDALUS_Heading_Bands, I), Divert_State.heading_deg));
   end Check_Angle_Wrapped_Turn_Left;

   --subprogram that loops over the recovery intervals from lowest to highest
   --and sets the divert heading to inside the first recovery interval that has
   --both bounds greater than the current heading. Loop terminates early if
   --divert heading is set.
   procedure Recovery_Right
     (Recovery_Heading_Bands : OrderedIntervalVector;
      Current_State : state_parameters;
      Divert_State : in out state_parameters;
      Heading_Max_deg : Heading_Type_deg;
      Heading_Min_deg : Heading_Type_deg;
      Heading_Interval_Buffer_deg : Heading_Buffer_Type_deg;
      is_Recovery_Found : in out Boolean;
      local_acceptable_action_flag : in out Boolean)
     with
       Pre => (Are_Legitimate_Bands (Recovery_Heading_Bands) and then
              is_Recovery_Found = False and then
              not MyVectorOfIntervals.Is_Empty (Recovery_Heading_Bands) and then
              scalar_constraints
                (Upper_limit         => Heading_Max_deg,
                 Lower_limit         => Heading_Min_deg,
                 Interval_Constraint => Heading_Interval_Buffer_deg) and then
              vector_constraints
                (X                   => Recovery_Heading_Bands,
                 Upper_limit         => Heading_Max_deg,
                 Lower_limit         => Heading_Min_deg,
                 Interval_constraint => Heading_Interval_Buffer_deg)),
      Post => ((not is_Recovery_Found and then
               (for all J in MyVectorOfIntervals.First_Index
                  (Recovery_Heading_Bands) .. MyVectorOfIntervals.Last_Index
                  (Recovery_Heading_Bands) => not (MyVectorOfIntervals.Element
                    (Recovery_Heading_Bands, J).LowerBound >
                      Current_State.heading_deg and then
                        MyVectorOfIntervals.Element (Recovery_Heading_Bands, J).
                      UpperBound > Current_State.heading_deg))) or else
                 (is_Recovery_Found and then local_acceptable_action_flag
                  and then (Divert_State.heading_deg > Current_State.
                        heading_deg) and then
                    Successful_Placement_in_Recovery_Bands
                      (Recovery_Heading_Bands      => Recovery_Heading_Bands,
                       Divert_State               => Divert_State)));

   procedure Recovery_Right
     (Recovery_Heading_Bands : OrderedIntervalVector;
      Current_State : state_parameters;
      Divert_State : in out state_parameters;
      Heading_Max_deg : Heading_Type_deg;
      Heading_Min_deg : Heading_Type_deg;
      Heading_Interval_Buffer_deg : Heading_Buffer_Type_deg;
      is_Recovery_Found : in out Boolean;
      local_acceptable_action_flag : in out Boolean) is
   begin
      pragma Assert (for all I in MyVectorOfIntervals.First_Index
                    (Recovery_Heading_Bands) .. MyVectorOfIntervals.Last_Index
                    (Recovery_Heading_Bands) =>
                      MyVectorOfIntervals.Element (Recovery_Heading_Bands, I).
                      LowerBound + Heading_Interval_Buffer_deg <=
                        MyVectorOfIntervals.Element (Recovery_Heading_Bands, I).
                      UpperBound);
      for I in MyVectorOfIntervals.First_Index (Recovery_Heading_Bands) ..
        MyVectorOfIntervals.Last_Index (Recovery_Heading_Bands) loop
         if MyVectorOfIntervals.Element (Recovery_Heading_Bands, I).LowerBound >
           Current_State.heading_deg and then
           MyVectorOfIntervals.Element (Recovery_Heading_Bands, I).UpperBound >
           Current_State.heading_deg
         then
            pragma Assert (MyVectorOfIntervals.Element
                           (Recovery_Heading_Bands, I).UpperBound <=
                             Heading_Max_deg);
            Divert_State.heading_deg := MyVectorOfIntervals.Element
              (Recovery_Heading_Bands, I).LowerBound +
              Heading_Interval_Buffer_deg / 2.0;
            pragma Assert (InRange (MyVectorOfIntervals.Element
                          (Recovery_Heading_Bands, I),
                          Divert_State.heading_deg));
            is_Recovery_Found := True;
            local_acceptable_action_flag := True;
            exit;
         end if;
         pragma Loop_Invariant (not is_Recovery_Found or else
                               Divert_State.heading_deg > Current_State.
                                 heading_deg);
         pragma Loop_Invariant (not is_Recovery_Found or else
                               InRange (MyVectorOfIntervals.Element
                                 (Recovery_Heading_Bands, I), Divert_State.
                                   heading_deg));
         pragma Loop_Invariant (for all J in MyVectorOfIntervals.First_Index
                               (Recovery_Heading_Bands) .. I =>
                                  not (MyVectorOfIntervals.Element
                                 (Recovery_Heading_Bands, J).LowerBound >
                                   Current_State.heading_deg and then
                                 MyVectorOfIntervals.Element
                                   (Recovery_Heading_Bands, J).UpperBound >
                                   Current_State.heading_deg));
      end loop;
   end Recovery_Right;

   --subprogram that loops over the recovery intervals from highest to lowest
   --and sets the divert heading to within the first recovery interval
   --that has bounds less than the current heading.  Loop terminates early if
   --the divert heading is set.
   procedure Recovery_Left
     (Recovery_Heading_Bands : OrderedIntervalVector;
      Current_State : state_parameters;
      Divert_State : in out state_parameters;
      Heading_Max_deg : Heading_Type_deg;
      Heading_Min_deg : Heading_Type_deg;
      Heading_Interval_Buffer_deg : Heading_Buffer_Type_deg;
      is_Recovery_Found : in out Boolean;
      local_acceptable_action_flag : in out Boolean)
     with
       Pre => Are_Legitimate_Bands (Recovery_Heading_Bands) and then
              is_Recovery_Found = False and then
              not MyVectorOfIntervals.Is_Empty (Recovery_Heading_Bands) and then
              scalar_constraints
                (Upper_limit         => Heading_Max_deg,
                 Lower_limit         => Heading_Min_deg,
                 Interval_Constraint => Heading_Interval_Buffer_deg) and then
              vector_constraints
                (X                   => Recovery_Heading_Bands,
                 Upper_limit         => Heading_Max_deg,
                 Lower_limit         => Heading_Min_deg,
                 Interval_constraint => Heading_Interval_Buffer_deg) and then
              Recovery_Bands_Nature
                (Recovery_Heading_Bands      => Recovery_Heading_Bands,
                 Current_State               => Current_State)
              and then (for all I in MyVectorOfIntervals.First_Index
                  (Recovery_Heading_Bands) .. MyVectorOfIntervals.Last_Index
                  (Recovery_Heading_Bands) =>
                  not (MyVectorOfIntervals.Element (Recovery_Heading_Bands, I).
                                 LowerBound > Current_State.heading_deg and then
                       MyVectorOfIntervals.Element (Recovery_Heading_Bands, I).
                         UpperBound > Current_State.heading_deg)),
        Post => (Divert_State.heading_deg < Current_State.heading_deg and then
                is_Recovery_Found and then local_acceptable_action_flag and then
                Successful_Placement_in_Recovery_Bands
                  (Recovery_Heading_Bands      => Recovery_Heading_Bands,
                   Divert_State                => Divert_State));

   procedure Recovery_Left
     (Recovery_Heading_Bands : OrderedIntervalVector;
      Current_State : state_parameters;
      Divert_State : in out state_parameters;
      Heading_Max_deg : Heading_Type_deg;
      Heading_Min_deg : Heading_Type_deg;
      Heading_Interval_Buffer_deg : Heading_Buffer_Type_deg;
      is_Recovery_Found : in out Boolean;
      local_acceptable_action_flag : in out Boolean) is
   begin
      pragma Assert (for some I in MyVectorOfIntervals.First_Index
                     (Recovery_Heading_Bands) .. MyVectorOfIntervals.Last_Index
                     (Recovery_Heading_Bands) => MyVectorOfIntervals.Element
                     (Recovery_Heading_Bands, I).LowerBound < Current_State.
                       heading_deg and then MyVectorOfIntervals.Element
                         (Recovery_Heading_Bands, I).UpperBound < Current_State.
                       heading_deg);
      for I in reverse MyVectorOfIntervals.First_Index (Recovery_Heading_Bands)
        .. MyVectorOfIntervals.Last_Index (Recovery_Heading_Bands) loop
         if MyVectorOfIntervals.Element (Recovery_Heading_Bands, I).LowerBound <
           Current_State.heading_deg and then MyVectorOfIntervals.Element
           (Recovery_Heading_Bands, I).UpperBound < Current_State.heading_deg
         then
            pragma Assert (MyVectorOfIntervals.Element (Recovery_Heading_Bands,
                          I).LowerBound >= Heading_Min_deg);
            Divert_State.heading_deg := MyVectorOfIntervals.Element
              (Recovery_Heading_Bands, I).UpperBound -
              Heading_Interval_Buffer_deg / 2.0;
            is_Recovery_Found := True;
            local_acceptable_action_flag := True;
            pragma Assert (MyVectorOfIntervals.Element (Recovery_Heading_Bands,
                           I).LowerBound <= MyVectorOfIntervals.Element
                            (Recovery_Heading_Bands, I).UpperBound -
                            2.0 * Heading_Interval_Buffer_deg);
            pragma Assert (InRange (MyVectorOfIntervals.Element
                          (Recovery_Heading_Bands, I),
                           Divert_State.heading_deg));
            pragma Assert (Divert_State.heading_deg < Current_State.heading_deg);
            exit;
         end if;
      end loop;
      pragma Assert (Divert_State.heading_deg < Current_State.heading_deg);
   end Recovery_Left;

   procedure Found_WCV_Heading_Resolution
     (DAIDALUS_Heading_Bands : OrderedIntervalVector;
      Recovery_Heading_Bands : OrderedIntervalVector;
      Current_State : state_parameters;
      Heading_Max_deg : Heading_Type_deg;
      Heading_Min_deg : Heading_Type_deg;
      Heading_Interval_Buffer_deg : Heading_Buffer_Type_deg;
      Divert_State : out state_parameters;
      found_acceptable_action_flag : out Boolean)
   is
      local_acceptable_action_flag : Boolean := True;
      is_Found : Boolean := False;
      is_Recovery_Found : Boolean := False;
      initial_band_index : myvector_index_type := myvector_index_type'First;

   begin
      --initialize divert state with the current state
      Divert_State := Current_State;
      --override vertical speed. When a heading change is chosen as resolution,
      --it should be a single mode change
      Divert_State.verticalSpeed_mps := 0.0;

      --check for conflict band information
      if not MyVectorOfIntervals.Is_Empty (DAIDALUS_Heading_Bands) then
         --attempt to find a good resolution using conflict band information to
         --turn to the right
         Find_Initial_and_Turn_Right
           (Divert_State                => Divert_State,
            is_Found                    => is_Found,
            initial_band_index          => initial_band_index,
            DAIDALUS_Heading_Bands      => DAIDALUS_Heading_Bands,
            Current_State               => Current_State,
            Heading_Max_deg             => Heading_Max_deg,
            Heading_Min_deg             => Heading_Min_deg,
            Heading_Interval_Buffer_Deg => Heading_Interval_Buffer_deg);
         pragma Assert (is_Found);
         --if divert heading is greater than max, check the angle wrapped
         --version of the angle
         if Divert_State.heading_deg > Heading_Max_deg then
            --angle wrapping to keep in range of 0 to 360 degrees
            Divert_State.heading_deg := Angle_Wrap (Divert_State.heading_deg);
            --since the maximum upperbound on conflict interval is 360.0 degrees
            --and divert state is adjusted by adding a buffer, the angle wrapped
            --version of the of the divert heading must be less than 0.0 +
            --buffer
            pragma Assert (Divert_State.heading_deg >= Heading_Min_deg);
            --assumption needed to explain angle wrapping over heading_max_deg
            --(360.0)
            pragma Assume (Divert_State.heading_deg <= Heading_Min_deg +
                            Heading_Interval_Buffer_deg);
            pragma Assert (Divert_State.heading_deg <= Heading_Max_deg);
            Check_Angle_Wrapped_Turn_Right
              (Divert_State                => Divert_State,
               DAIDALUS_Heading_Bands      => DAIDALUS_Heading_Bands,
               Heading_Max_deg             => Heading_Max_deg,
               Heading_Min_deg             => Heading_Min_deg,
               Heading_Interval_Buffer_deg => Heading_Interval_Buffer_deg);
         else
            pragma Assert (Divert_State.heading_deg >= Heading_Min_deg);
            pragma Assert (Divert_State.heading_deg <= Heading_Max_deg);
         end if;

         if Divert_State.heading_deg > Heading_Max_deg then
            --Checking right twice, to handle angle wrap cases, has failed to
            --result in a heading in range of allowed.  Therefore, entire range
            --is saturated as conflict with no recovery bands => enact fallback
            --behavoir
            local_acceptable_action_flag := False;
            --  pragma Assert(Current_State.heading_deg <= Heading_Max_deg);
            --  pragma Assert(Current_State.heading_deg >= Heading_Min_deg);
            Divert_State.heading_deg := Angle_Wrap (Current_State.heading_deg
                                               +  180.0);
         else

            pragma Assert (Divert_State.heading_deg <= Heading_Max_deg);
            pragma Assert (Current_State.heading_deg >= Heading_Min_deg and then
                          Current_State.heading_deg <= Heading_Max_deg);
            if (Angle_Wrap (Divert_State.heading_deg) >
                  Angle_Wrap (Current_State.heading_deg) and then
                Angle_Wrap (Divert_State.heading_deg) -
                  Angle_Wrap (Current_State.heading_deg) <= 180.0) or
              (Angle_Wrap (Current_State.heading_deg) >
                   Angle_Wrap (Divert_State.heading_deg) and then
               Angle_Wrap (Current_State.heading_deg) -
                   Angle_Wrap (Divert_State.heading_deg) >= 180.0)
            then

               --  pragma Assert (Divert_State.heading_deg >= Heading_Min_deg);
               pragma Assume (Conflict_and_Recovery_Complimentary_Nature
                             (Divert_State, DAIDALUS_Heading_Bands,
                                Recovery_Heading_Bands, Heading_Min_deg,
                                Heading_Max_deg));
               local_acceptable_action_flag := (if MyVectorOfIntervals.Is_Empty
                                                (Recovery_Heading_Bands) then
                                                   True
                                                else
                                          Successful_Placement_in_Recovery_Bands
                                            (Recovery_Heading_Bands,
                                             Divert_State));
               pragma Assert (Found_Acceptable_Action
                             (local_acceptable_action_flag,
                             DAIDALUS_Heading_Bands, Recovery_Heading_Bands,
                             Divert_State.heading_deg));
            else
               --reset divert heading to current heading and attempt to find
               --good resolution by turning left
               Divert_State.heading_deg := Current_State.heading_deg;
               Turn_Left
                 (DAIDALUS_Heading_Bands      => DAIDALUS_Heading_Bands,
                  initial_band_index          => initial_band_index,
                  Heading_Max_deg             => Heading_Max_deg,
                  Heading_Min_deg             => Heading_Min_deg,
                  Heading_Interval_Buffer_deg => Heading_Interval_Buffer_deg,
                  Current_State               => Current_State,
                  Divert_State                => Divert_State);
               --if the divert heading is less than the minimum heading, check
               --the angle wrapped version of the divert heading
               if Divert_State.heading_deg < Heading_Min_deg then
                  --angle wrapping to keep divert heading in range of 0.0 to
                  --360.0 degrees
                  Divert_State.heading_deg := Angle_Wrap (angle => Divert_State.
                                                           heading_deg);
                  --since the minimum heading is angle wrapped version of
                  --maximum heading and the divert heading is set by
                  --subrtracting a buffer from upper bound use an assumption to
                  --relate this information to solvers.

                  --angle wrap will put Divert_State.heading_deg just inside
                  --the maximum
                  pragma Assume (Divert_State.heading_deg >= Heading_Max_deg -
                                  Heading_Interval_Buffer_deg);
                  Check_Angle_Wrapped_Turn_Left
                    (DAIDALUS_Heading_Bands      => DAIDALUS_Heading_Bands,
                     Heading_Max_deg             => Heading_Max_deg,
                     Heading_Min_deg             => Heading_Min_deg,
                     Heading_Interval_Buffer_deg => Heading_Interval_Buffer_deg,
                     Divert_State                => Divert_State);
               end if;

               if Divert_State.heading_deg < Heading_Min_deg
               then
                  local_acceptable_action_flag := False;
                  Divert_State.heading_deg := Angle_Wrap
                    (Current_State.heading_deg + 180.0);
               else

                  --if the divert heading is a left turn less than 180.0 degrees
                  --from the current heading then found a good resoltution.
                  if (Angle_Wrap (Divert_State.heading_deg) >
                        Angle_Wrap (Current_State.heading_deg) and then
                      Angle_Wrap (Divert_State.heading_deg) -
                        Angle_Wrap (Current_State.heading_deg) > 180.0) or
                    (Angle_Wrap (Current_State.heading_deg) >
                         Angle_Wrap (Divert_State.heading_deg) and then
                     Angle_Wrap (Current_State.heading_deg) -
                         Angle_Wrap (Divert_State.heading_deg) < 180.0)
                  then

                     pragma Assert (Found_Acceptable_Action
                                   (local_acceptable_action_flag,
                                      DAIDALUS_Heading_Bands,
                                      Recovery_Heading_Bands,
                                   Divert_State.heading_deg));
                  else
                     --cannot find a good resolution and instead find a
                     --mitigation using recovery information; preferencing a
                     --right turn
                     pragma Assert (vector_constraints (Recovery_Heading_Bands,
                                    Heading_Max_deg, Heading_Min_deg,
                                    Heading_Interval_Buffer_deg)); --new
                     if not MyVectorOfIntervals.Is_Empty (Recovery_Heading_Bands)
                     then
                        local_acceptable_action_flag := False;
                        --                    Divert_State := Current_State;
                        pragma Assert (vector_constraints (Recovery_Heading_Bands,
                                       Heading_Max_deg, Heading_Min_deg,
                                       Heading_Interval_Buffer_deg));
                        pragma Assume (Recovery_Bands_Nature
                                       (Recovery_Heading_Bands => Recovery_Heading_Bands,
                                        Current_State => Current_State));
                        pragma Assert (vector_constraints (Recovery_Heading_Bands,
                                       Heading_Max_deg, Heading_Min_deg,
                                       Heading_Interval_Buffer_deg));
                        Recovery_Right
                         (Recovery_Heading_Bands      => Recovery_Heading_Bands,
                          Current_State               => Current_State,
                          Divert_State                => Divert_State,
                          Heading_Max_deg             => Heading_Max_deg,
                          Heading_Min_deg             => Heading_Min_deg,
                          Heading_Interval_Buffer_deg =>
                             Heading_Interval_Buffer_deg,
                          is_Recovery_Found           => is_Recovery_Found,
                          local_acceptable_action_flag =>
                            local_acceptable_action_flag);
                        --expected that recovery bands are complementary to
                        --conflict bands. Thus if current heading is in a
                        --conflict band, if there is no resolution band that
                        --exists to the right, it must exist to the left since
                        --recovery is not empty

                        --if not in recovery right, must be in recovery left
                        --  pragma Assume (Recovery_Bands_Nature
                        --                (Recovery_Heading_Bands      =>
                        --                   Recovery_Heading_Bands,
                        --                 Current_State               =>
                        --                   Current_State));
                        pragma Assert (scalar_constraints (Heading_Max_deg,
                                       Heading_Min_deg,
                                       Heading_Interval_Buffer_deg));
                        pragma Assert (vector_constraints (Recovery_Heading_Bands,
                                       Heading_Max_deg, Heading_Min_deg,
                                       Heading_Interval_Buffer_deg));
                        if not is_Recovery_Found then
                           Recovery_Left
                             (Recovery_Heading_Bands       =>
                                Recovery_Heading_Bands,
                              Current_State                => Current_State,
                              Divert_State                 => Divert_State,
                              Heading_Max_deg              => Heading_Max_deg,
                              Heading_Min_deg              => Heading_Min_deg,
                              Heading_Interval_Buffer_deg  =>
                                Heading_Interval_Buffer_deg,
                              is_Recovery_Found            => is_Recovery_Found,
                              local_acceptable_action_flag =>
                                local_acceptable_action_flag);
                           pragma Assert (is_Recovery_Found);
                        end if;
                        --at this point, recovery bands are present and either a
                        --recovery band to the left or the right has been found.
                        --Thus a mitigation to the loss of well clear has been
                        --determined
                        pragma Assert (Found_Acceptable_Action
                                      (local_acceptable_action_flag,
                                         DAIDALUS_Heading_Bands,
                                         Recovery_Heading_Bands, Divert_State.
                                           heading_deg));
                     else
                        --Here the conflict bands were not empty, but neither a
                        --mitigation or good resolution was found.  As a result,
                        --revert to a known fall back behavior or turning 180
                        --degrees
                        local_acceptable_action_flag := False;
                        Divert_State.heading_deg := Angle_Wrap
                          (Current_State.heading_deg + 180.0);
                        pragma Assert (Revert_behavior
                                      (local_acceptable_action_flag,
                                       DAIDALUS_Heading_Bands,
                                       Angle_Wrap (Current_State.heading_deg +
                                             180.0), Divert_State.heading_deg));
                     end if;

                  end if;
               end if;

            end if;

         end if;
      else
         --these functions to set a divert action is triggered by a time
         --threshold on the loss of well clear.  This action should
         --be accompanied by by corresponding conflict bands.  However, in this
         --branch the routine was called without conflict band information.
         local_acceptable_action_flag := False;
         pragma Assert (IsImproperlyConfigured
                       (found_acceptable_action_flag =>
                          local_acceptable_action_flag,
                        DAIDALUS_X_Bands      => DAIDALUS_Heading_Bands,
                        Divert_State_field    => Divert_State.heading_deg,
                        Current_State_field   => Current_State.heading_deg));
      end if;
      found_acceptable_action_flag := local_acceptable_action_flag;
      pragma Assert (Found_Acceptable_Action (found_acceptable_action_flag,
                    DAIDALUS_Heading_Bands, Recovery_Heading_Bands, Divert_State
                    .heading_deg) or else Revert_behavior
                      (found_acceptable_action_flag,
                       DAIDALUS_Heading_Bands,
                       Angle_Wrap (Current_State.heading_deg + 180.0),
                       Divert_State.
                           heading_deg) or else IsImproperlyConfigured
                      (found_acceptable_action_flag, DAIDALUS_Heading_Bands,
                       Divert_State.heading_deg, Current_State.heading_deg));

   end Found_WCV_Heading_Resolution;

end Heading_Resolution;
