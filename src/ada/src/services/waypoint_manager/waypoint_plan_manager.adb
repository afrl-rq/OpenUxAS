with Ada.Containers;             use Ada.Containers;
with AVTAS.LMCP.Types;           use AVTAS.LMCP.Types;
with UxAS.Comms.LMCP_Net_Client; use UxAS.Comms.LMCP_Net_Client;
with LMCP_Messages;              use LMCP_Messages;
with Ada.Text_IO;                use Ada.Text_IO;

package body Waypoint_Plan_Manager with SPARK_Mode is

   ----------------------------------
   -- Extract_Mission_Command_Info --
   ----------------------------------

   procedure Extract_MissionCommand_Maps
     (State : in out Waypoint_Plan_Manager_State;
      MC : MissionCommand);

   procedure Extract_MissionCommand_Maps
     (State : in out Waypoint_Plan_Manager_State;
      MC : MissionCommand)
   is
      WP : Waypoint;
   begin

      Clear (State.Id_To_Next_Id);
      Clear (State.Id_To_Waypoint);

      for I in WP_Sequences.First .. Last (MC.WaypointList) loop
         WP := Get (MC.WaypointList, I);
         if WP.Number > 0 and then WP.NextWaypoint >= 0 then
            if not Contains (State.Id_To_Next_Id, Pos64 (WP.Number)) then
               Insert (State.Id_To_Next_Id, Pos64 (WP.Number), Nat64 (WP.NextWaypoint));
            end if;
            if not Contains (State.Id_To_Waypoint, Pos64 (WP.Number)) then
               Insert (State.Id_To_Waypoint, Pos64 (WP.Number), WP);
            end if;
         end if;
         pragma Loop_Invariant (Integer (Length (State.Id_To_Next_Id)) <= I - WP_Sequences.First + 1);
         pragma Loop_Invariant (Integer (Length (State.Id_To_Waypoint)) <= I - WP_Sequences.First + 1);
      end loop;
   end Extract_MissionCommand_Maps;

   ---------------------------
   -- Handle_MissionCommand --
   ---------------------------

   procedure Handle_MissionCommand
     (State : in out Waypoint_Plan_Manager_State;
      MC : MissionCommand)
   is
      First_Id : Pos64 := MC.FirstWaypoint;
      Id_List : Pos64_Vector;
      Ids : Pos64_Nat64_Map;
      function Successor (M : Pos64_Nat64_Map; K : Pos64) return Nat64 renames Element;
   begin

      State.MC := MC;
      Extract_MissionCommand_Maps (State, MC);
      State.New_Command := True;
      State.Next_Segment_Id := First_Id;
      State.Next_First_Id := First_Id;
      Clear (State.Prefix);
      Clear (State.Cycle);
      Ids := State.Id_To_Next_Id;

      -- Look for First_Id.
      if Contains (Ids, First_Id) then
         if Successor (Ids, First_Id) = 0
             or else not Contains (Ids, Pos64 (Successor (Ids, First_Id)))
             or else Successor (Ids, First_Id) = First_Id
         then
            -- First_Id has no successors. Return.
            Append (State.Prefix, First_Id);
            return;
         else
            -- First_Id has a successor. Continue.
            Append (Id_List, First_Id);
            Append (Id_List, Successor (Ids, First_Id));
            Delete (Ids, First_Id);
         end if;
      else
         -- First_Id not found. Return with no segment.
         State.Next_Segment_Id := 0;
         State.Next_First_Id := 0;
         return;
      end if;

      pragma Assert (not Is_Empty (Id_List));
      pragma Assert (Length (Id_List) = 2);

      -- Look for a predecessor to First_Id.
      -- We already checked that it does not point to itself.
      for Id of Ids loop
         if Successor (Ids, Id) = First_Element (Id_List) then
            if Last_Element (Id_List) = Id then
               -- First_Id cycles with its precedessor. Return.
               State.Next_Segment_Id := Id;
               Reverse_Elements (Id_List);
               State.Cycle := Id_List;
               return;
            else
               -- First_Id has a precessor. Continue.
               State.Next_Segment_Id := Id;
               Prepend (Id_List, Id);
               Delete (Ids, Id);
               exit;
            end if;
         end if;
         pragma Loop_Invariant (not Is_Empty (Id_List));
         pragma Loop_Invariant (Length (Id_List) <= 3);
         pragma Loop_Invariant (Length (Ids) <= Max - 1);
      end loop;

      -- Search for successors until done.
      while Length (Ids) > 0 loop
         if Contains (Ids, Last_Element (Id_List)) then
            if Successor (Ids, Last_Element (Id_List)) = 0
              or else not Contains (State.Id_To_Next_Id, Pos64 (Successor (Ids, Last_Element (Id_List))))
              or else Successor (Ids, Last_Element (Id_List)) = Last_Element (Id_List)
            then
               -- Candidate successor is 0 , unknown , or points to itself.
               -- Return with a prefix only.
               State.Prefix := Id_List;
               return;
            elsif Contains (Id_List, Successor (Ids, Last_Element (Id_List))) then
               -- Found a cycle. Compute prefix & cycle. Return.
               declare
                  Index : Pos64_Vectors.Extended_Index;
                  Next_Id : Pos64 := Successor (Ids, Last_Element (Id_List));
               begin
                  Index := Find_Index (Id_List, Next_Id);
                  for I in First_Index (Id_List) .. Index - 1 loop
                     Append (State.Prefix, Element (Id_List, I));
                     pragma Loop_Invariant (Integer (Length (State.Prefix)) = I - First_Index (Id_List) + 1);
                  end loop;
                  for I in Index .. Last_Index (Id_List) loop
                     Append (State.Cycle, Element (Id_List, I));
                     pragma Loop_Invariant (Integer (Length (State.Cycle)) = I - Index + 1);
                  end loop;
                  return;
               end;
            else
               -- Found a successor that's not a cycle.
               declare
                  Id : Pos64 := Last_Element (Id_List);
               begin
                  Append (Id_List, Successor (Ids, Id));
                  Delete (Ids, Id);
               end;
            end if;
         else
            -- Can't find a successor. Return a prefix.
            State.Prefix := Id_List;
            return;
         end if;

         pragma Loop_Invariant (not Is_Empty (Id_List));
         pragma Loop_Invariant (Length (Id_List) <= Max - Length (Ids) + 1);
      end loop;

      -- No more successors. Return a prefix.
      State.Prefix := Id_List;

   end Handle_MissionCommand;

   ---------------------
   -- Produce_Segment --
   ---------------------

   procedure Produce_Segment
     (State : in out Waypoint_Plan_Manager_State;
      Config : Waypoint_Plan_Manager_Configuration_Data;
      Mailbox : in out Waypoint_Plan_Manager_Mailbox)
   is
      Id : constant Pos64 := State.Next_Segment_Id;
      First_Id : constant Pos64 := State.Next_First_Id;
      Prefix : constant Pos64_Vector := State.Prefix;
      Cycle : constant Pos64_Vector := State.Cycle;
      Len : constant Positive := Positive (Config.NumberWaypointsToServe);
      Overlap : constant Positive := Positive (Config.NumberWaypointsOverlap);

      I : Natural := 1;
      C : Pos64_Vectors.Extended_Index;
      In_Prefix : Boolean;
   begin

      State.New_Command := False;
      State.Next_Segment_Id := 0;
      State.Next_First_Id := 0;
      Clear (State.Segment);

      C := Find_Index (Prefix, Id);
      In_Prefix := (if C /= Pos64_Vectors.No_Index then True else False);

      while C in First_Index (Prefix) .. Last_Index (Prefix) and then I <= Len loop
         pragma Loop_Invariant (Natural (Length (State.Segment)) < I);
         Append (State.Segment, Element (Prefix, C));
         C := Iter_Next (Prefix, C);
         I := I + 1;
      end loop;

      C := (if In_Prefix then First_Index (Cycle) else Find_Index (Cycle, Id));

      while C in First_Index (Cycle) .. Last_Index (Cycle) and then I <= Len loop
         pragma Loop_Invariant (Natural (Length (State.Segment)) < I);
         Append (State.Segment, Element (Cycle, C));
         C := Iter_Next (Cycle, C);
         if not Iter_Has_Element (Cycle, C) then
            C := First_Index (Cycle);
         end if;
         I := I + 1;
      end loop;

      if Integer (Length (State.Segment)) > Overlap then
         State.Next_Segment_Id := Element (State.Segment, Last_Index (State.Segment) - Overlap + 1);
         State.Next_First_Id := Element (State.Segment, Last_Index (State.Segment) - Overlap + 2);
      end if;

      declare
         MC_Out : MissionCommand := State.MC;
         WP_List : WP_Seq;
         Id : Pos64;
         WP : Waypoint;
      begin
         MC_Out.FirstWaypoint := First_Id;
         for I in First_Index (State.Segment) .. Last_Index (State.Segment) loop
            Id := Element (State.Segment, I);
            if Contains (State.Id_To_Waypoint, Id) then
               WP := Element (State.Id_To_Waypoint, Id);
               if I = Last_Index (State.Segment) then
                  WP.NextWaypoint := WP.Number;
                  -- TODO: Extend SPARK messages to handle
                  -- VehicleAction -> NavigationAction -> LoiterAction
                  -- VehicleAction -> PayloadAction -> GimbalAngleAction
               end if;
               -- WP.TurnType := Config.TurnType;
               WP_List := Add (WP_List, WP);
            end if;
            pragma Loop_Invariant
              (Integer (Length (WP_List)) <= I - First_Index (State.Segment) + 1);
         end loop;
         MC_Out.WaypointList := WP_List;
         sendBroadcastMessage (Mailbox, MC_Out);
      end;

   end Produce_Segment;

end Waypoint_Plan_Manager;
