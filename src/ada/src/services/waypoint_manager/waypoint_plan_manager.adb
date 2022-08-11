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
      State.New_Command := True;
      State.Next_Segment_Id := First_Id;
      State.Next_First_Id := First_Id;
      Extract_MissionCommand_Maps (State, MC);
      Clear (State.Prefix);
      Clear (State.Cycle);
      Ids := State.Id_To_Next_Id;

      -- Handle FirstID
      if Contains (Ids, First_Id) then
         if Successor (Ids, First_Id) = 0
             or else not Contains (Ids, Pos64 (Successor (Ids, First_Id)))
             or else Successor (Ids, First_Id) = First_Id
         then
            -- FirstID has no successors or points to itself. Return.
            Append (State.Prefix, First_Id);
            return;
         else
            -- FirstID has a successor. Continue.
            Append (Id_List, First_Id);
            Append (Id_List, Successor (Ids, First_Id));
            Delete (Ids, First_Id);
         end if;
      else
         -- FirstID not known. Return.
         State.Next_Segment_Id := 0;
         State.Next_First_Id := 0;
         return;
      end if;

      -- Try to find a predecessor for FirstID.
      -- We already checked that FirstID does not point to itself.
      pragma Assert (not Is_Empty (Id_List));
      pragma Assert (Length (Id_List) = 2);

      for Id of Ids loop
         if Successor (Ids, Id) = First_Element (Id_List) then
            if Last_Element (Id_List) = Id then
               -- FirstID cycles with its precedessor. Return.
               State.Next_Segment_Id := Id;
               Reverse_Elements (Id_List);
               State.Cycle := Id_List;
               return;
            else
               -- FirstID has precessor that starts the segment. Continue.
               State.Next_Segment_Id := Id;
               Prepend (Id_List, Id);
               Delete (Ids, Id);
               exit;
            end if;
         end if;
         pragma Loop_Invariant (Length (Id_List) <= 3);
         pragma Loop_Invariant (Length (Ids) <= Max - 1);
         pragma Loop_Invariant (not Is_Empty (Id_List));
      end loop;

      while Length (Ids) > 0 loop

         if Contains (Ids, Last_Element (Id_List)) then
            if Successor (Ids, Last_Element (Id_List)) = 0
              or else not Contains (State.Id_To_Next_Id, Pos64 (Successor (Ids, Last_Element (Id_List))))
              or else Successor (Ids, Last_Element (Id_List)) = Last_Element (Id_List)
            then
               -- No more elements found, so no cycle. Return a prefix only.
               State.Prefix := Id_List;
               return;
            elsif Contains (Id_List, Successor (Ids, Last_Element (Id_List))) then
               -- Found a cycle. Copy first part of Id_List to prefix and the rest to cycle.
               declare
                  Index : Pos64_Vectors.Extended_Index;
                  Next_ID : Pos64 := Successor (Ids, Last_Element (Id_List));
               begin
                  Index := Find_Index (Id_List, Next_ID);
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
               -- Found a successor that's not a cycle
               declare
                  Id : Pos64 := Last_Element (Id_List);
               begin
                  Append (Id_List, Successor (Ids, Id));
                  Delete (Ids, Id);
               end;
            end if;
         else
            -- Can't find successor. Nothing to add. No cycle.
            State.Prefix := Id_List;
            return;
         end if;

         pragma Loop_Invariant (not Is_Empty (Id_List));
         pragma Loop_Invariant (Length (Id_List) <= Max - Length (Ids) + 1);
      end loop;

      -- There are no more keys in the map, so everything is in the prefix.
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

      State.Next_Segment_Id := 0;
      State.Next_First_Id := 0;
      State.New_Command := False;
      Clear (State.Segment);

      C := Find_Index (Prefix, Id);
      In_Prefix := (if C /= Pos64_Vectors.No_Index then True else False);

      while C in First_Index (Prefix) .. Last_Index (Prefix) and then I <= Len loop
         pragma Loop_Invariant (Natural (Length (State.Segment)) < I);
         Append (State.Segment, Element (Prefix, C));
         C := Iter_Next (Prefix, C);
         I := I + 1;
      end loop;

      if In_Prefix then
         C := First_Index (Cycle);
      else
         C := Find_Index (Cycle, Id);
      end if;

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

   procedure Print (MC : MissionCommand)
   is
   begin
      Put_Line ("MissionCommand.FirstWaypoint: " & MC.FirstWaypoint'Image);
      Put ("MissionCommand.WaypointList: ");
      for WP of MC.WaypointList loop
         Put ("[" & WP.Number'Image & ", " & WP.NextWaypoint'Image & "] ");
      end loop;
      New_Line;
   end Print;

   procedure Print (WPs : Pos64_WP_Map);

   procedure Print (WPs : Pos64_WP_Map)
   is
      WP : Waypoint;
   begin
      Put ("WPs: ");
      for K of WPs loop
         WP := Element (WPs, K);
         Put ("[" & K'Image & ", (" & WP.Number'Image & ", " & WP.NextWaypoint'Image & ",)]");
      end loop;
      New_Line;

   end Print;

   procedure Print (Succ_IDs : Pos64_Nat64_Map);

   procedure Print (Succ_IDs : Pos64_Nat64_Map)
   is
   begin
      Put ("Succ_IDs: ");
      for K of Succ_IDs loop
         Put ("[" & K'Image & ", " & Element (Succ_IDs, K)'Image & "]");
      end loop;
      New_Line;

   end Print;

   procedure Print (Id_List : Pos64_Vector);

   procedure Print (Id_List : Pos64_Vector)
   is
   begin
      for K of Id_List loop
         Put (K'Image & ", ");
      end loop;
      New_Line;

   end Print;

   procedure Print (State : Waypoint_Plan_Manager_State)
   is
   begin
      Print (State.MC);
      Print (State.Id_To_Waypoint);
      Print (State.Id_To_Next_Id);
      Put_Line ("New_Command : " & (if State.New_Command then "True" else "False"));
      Put_Line ("Next_Segment_ID : " & State.Next_Segment_Id'Image);
      Put_Line ("Next_FirstID : " & State.Next_First_Id'Image);
      Put ("Prefix : ");
      Print (State.Prefix);
      Put ("Cycle : ");
      Print (State.Cycle);
      Put ("Segment : ");
      Print (State.Segment);
      Put_Line ("Headed_To_First_ID : " & State.Headed_To_First_Id'Image);

   end Print;

end Waypoint_Plan_Manager;
