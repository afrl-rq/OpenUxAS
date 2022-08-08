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

      Clear (State.Succ_IDs);
      Clear (State.WPs);

      for I in WP_Sequences.First .. Last (MC.WaypointList) loop
         WP := Get (MC.WaypointList, I);
         if WP.Number > 0 and then WP.NextWaypoint >= 0 then
            if not Contains (State.Succ_IDs, Pos64 (WP.Number)) then
               Insert (State.Succ_IDs, Pos64 (WP.Number), Nat64 (WP.NextWaypoint));
            end if;
            if not Contains (State.WPs, Pos64 (WP.Number)) then
               Insert (State.WPs, Pos64 (WP.Number), WP);
            end if;
         end if;
         pragma Loop_Invariant (Integer (Length (State.Succ_IDs)) <= I - WP_Sequences.First + 1);
         pragma Loop_Invariant (Integer (Length (State.WPs)) <= I - WP_Sequences.First + 1);
      end loop;
   end Extract_MissionCommand_Maps;

   ---------------------------
   -- Handle_MissionCommand --
   ---------------------------

   procedure Handle_MissionCommand
     (State : in out Waypoint_Plan_Manager_State;
      MC : MissionCommand)
   is
      First_ID : Pos64 := MC.FirstWaypoint;
      M : ID_Map;
      ID_List : ID_Vector;
   begin

      State.MC := MC;
      State.New_Command := True;
      State.Next_Segment_ID := First_ID;
      State.Next_First_ID := First_ID;
      Extract_MissionCommand_Maps (State, MC);
      Clear (State.Prefix);
      Clear (State.Cycle);
      M := State.Succ_IDs;

      -- Handle FirstID
      if Contains (State.Succ_IDs, First_ID) then
         if Element (State.Succ_IDs, First_ID) = 0
             or else not Contains (State.Succ_IDs, Pos64 (Element (State.Succ_IDs, First_ID))) -- Had to add Positive here for SPARK
             or else Element (State.Succ_IDs, First_ID) = First_ID
         then
            -- FirstID has no successors or points to itself. Return.
            Append (State.Prefix, First_ID);
            return;
         else
            -- FirstID has a successor. Continue.
            Append (ID_List, First_ID);
            Append (ID_List, Element (State.Succ_IDs, First_ID));
            Delete (M, First_ID);
         end if;
      else
         -- FirstID not known. Return.
         State.Next_Segment_ID := 0;
         State.Next_First_ID := 0;
         return;
      end if;

      -- Try to find a predecessor for FirstID.
      -- We already checked that FirstID does not point to itself.
      pragma Assert (not Is_Empty (ID_List));
      pragma Assert (Length (ID_List) = 2);
      pragma Assert (Length (M) <= Max - 1);

      for K of M loop
         if Element (M, K) = First_Element (ID_List) then
            if Last_Element (ID_List) = K then
               -- FirstID cycles with its precedessor. Return.
               State.Next_First_ID := K;
               State.Cycle := ID_List;
               return;
            else
               -- FirstID has precessor that starts the segment. Continue.
               State.Next_Segment_ID := K;
               Prepend (ID_List, K);
               Delete (M, K);
               exit;
            end if;
         end if;
         pragma Loop_Invariant (Length (ID_List) <= 3);
         pragma Loop_Invariant (Length (M) <= Max - 1);
         pragma Loop_Invariant (not Is_Empty (ID_List));
      end loop;

      pragma Assert (Length (M) <= Max - 1);

      while Length (M) > 0 loop

         if Contains (M, Last_Element (ID_List)) then
            if Element (M, Last_Element (ID_List)) = 0
              or Element (M, Last_Element (ID_List)) = Last_Element (ID_List)
            then
               -- No more elements found, so no cycle. Return a prefix only.
               State.Prefix := ID_List;
               return;
            elsif Contains (ID_List, Element (M, Last_Element (ID_List))) then
               -- Found a cycle. Copy first part of ID_List to prefix and the rest to cycle.
               declare
                  Index : ID_Vectors.Extended_Index;
                  Next_ID : Pos64 := Element (M, Last_Element (ID_List));
               begin
                  Index := Find_Index (ID_List, Next_ID);
                  for I in First_Index (ID_List) .. Index - 1 loop
                     Append (State.Prefix, Element (ID_List, I));
                     pragma Loop_Invariant (Integer (Length (State.Prefix)) = I - First_Index (ID_List) + 1);
                  end loop;
                  for I in Index .. Last_Index (ID_List) loop
                     Append (State.Cycle, Element (ID_List, I));
                     pragma Loop_Invariant (Integer (Length (State.Cycle)) = I - Index + 1);
                  end loop;
                  return;
               end;
            else
               -- Found a successor that's not a cycle
               declare
                  ID : Pos64 := Last_Element (ID_List);
               begin
                  Append (ID_List, Element (M, ID));
                  Delete (M, ID);
               end;
            end if;
         else
            -- Can't find successor. Nothing to add. No cycle.
            State.Prefix := ID_List;
            return;
         end if;

         pragma Loop_Invariant (not Is_Empty (ID_List));
         pragma Loop_Invariant (Length (ID_List) <= Max - Length (M) + 1);
      end loop;

      -- There are no more keys in the map, so everything is in the prefix.
      State.Prefix := ID_List;

   end Handle_MissionCommand;

   ---------------------
   -- Produce_Segment --
   ---------------------

   procedure Produce_Segment
     (State : in out Waypoint_Plan_Manager_State;
      Config : Waypoint_Plan_Manager_Configuration_Data;
      Mailbox : in out Waypoint_Plan_Manager_Mailbox)
   is
      ID : constant Pos64 := State.Next_Segment_ID;
      First_ID : constant Pos64 := State.Next_First_ID;
      Prefix : constant ID_Vector := State.Prefix;
      Cycle : constant ID_Vector := State.Cycle;
      Len : constant Positive := Positive (Config.NumberWaypointsToServe);
      Overlap : constant Positive := Positive (Config.NumberWaypointsOverlap);

      I : Natural := 1;
      C : ID_Vectors.Extended_Index;
      In_Prefix : Boolean;
   begin

      State.Next_Segment_ID := 0;
      State.Next_First_ID := 0;
      State.New_Command := False;
      Clear (State.Segment);

      C := Find_Index (Prefix, ID);
      if C /= ID_Vectors.No_Index then
         In_Prefix := True;
      else
         In_Prefix := False;
      end if;

      while C in First_Index (Prefix) .. Last_Index (Prefix) and then I <= Len loop
         pragma Loop_Invariant (Natural (Length (State.Segment)) < I);
         Append (State.Segment, Element (Prefix, C));
         if I = Len - Overlap + 1 and (not Is_Empty (Cycle) or C /= Last_Index (Prefix)) then
            State.Next_Segment_ID := Element (Prefix, C);
         end if;
         if I = Len - Overlap + 2 then
            State.Next_First_ID := Element (Prefix, C);
         end if;
         C := Iter_Next (Prefix, C);
         I := I + 1;
      end loop;

      if In_Prefix then
         C := First_Index (Cycle);
      else
         C := Find_Index (Cycle, ID);
      end if;

      while C in First_Index (Cycle) .. Last_Index (Cycle) and then I <= Len loop
         pragma Loop_Invariant (Natural (Length (State.Segment)) < I);
         Append (State.Segment, Element (Cycle, C));
         if I = Len - Overlap + 1 then
            State.Next_Segment_ID := Element (Cycle, C);
         end if;
         if I = Len - Overlap + 2 then
            State.Next_First_ID := Element (Cycle, C);
         end if;
         C := Iter_Next (Cycle, C);
         if not Iter_Has_Element (Cycle, C) then
            C := First_Index (Cycle);
         end if;

         I := I + 1;
      end loop;

      declare
         MC_Out : MissionCommand := State.MC;
         WP_List : WP_Seq;
         ID : Pos64;
         WP : Waypoint;
      begin
         MC_Out.FirstWaypoint := First_ID;
         for I in First_Index (State.Segment) .. Last_Index (State.Segment) loop
            ID := Element (State.Segment, I);
            if Contains (State.WPs, ID) then
               WP := Element (State.WPs, ID);
               if I = Last_Index (State.Segment) then
                  WP.NextWaypoint := WP.Number;
                  -- TODO: Extend SPARK messages to handle
                  -- VehicleAction -> NavigationAction -> LoiterAction
                  -- VehicleAction -> PayloadAction -> GimbalAngleAction
               end if;
               WP.TurnType := Config.TurnType;
               WP_List := Add (WP_List, WP);
            end if;
            pragma Loop_Invariant
              (Integer (Length (WP_List)) <= I - First_Index (State.Segment) + 1);
         end loop;
         MC_Out.WaypointList := WP_List;
         sendBroadcastMessage (Mailbox, MC_Out);
      end;

   end Produce_Segment;

   procedure Print (MC : MissionCommand);

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

   procedure Print (WPs : WP_Map);

   procedure Print (WPs : WP_Map)
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

   procedure Print (Succ_IDs : ID_Map);

   procedure Print (Succ_IDs : ID_Map)
   is
   begin
      Put ("Succ_IDs: ");
      for K of Succ_IDs loop
         Put ("[" & K'Image & ", " & Element (Succ_IDs, K)'Image & "]");
      end loop;
      New_Line;

   end Print;

   procedure Print (ID_List : ID_Vector);

   procedure Print (ID_List : ID_Vector)
   is
   begin
      for K of ID_List loop
         Put (K'Image & ", ");
      end loop;
      New_Line;

   end Print;

   procedure Print (State : Waypoint_Plan_Manager_State)
   is
   begin
      Print (State.MC);
      Print (State.WPs);
      Print (State.Succ_IDs);
      Put_Line ("New_Command : " & (if State.New_Command then "True" else "False"));
      Put_Line ("Next_Segment_ID : " & State.Next_Segment_ID'Image);
      Put_Line ("Next_FirstID : " & State.Next_First_ID'Image);
      Put ("Prefix : ");
      Print (State.Prefix);
      Put ("Cycle : ");
      Print (State.Cycle);
      Put ("Segment : ");
      Print (State.Segment);
      Put_Line ("Headed_To_First_ID : " & State.Headed_To_First_ID'Image);

   end Print;

end Waypoint_Plan_Manager;
