with Ada.Containers;             use Ada.Containers;
with AVTAS.LMCP.Types;           use AVTAS.LMCP.Types;
with UxAS.Comms.LMCP_Net_Client; use UxAS.Comms.LMCP_Net_Client;
with LMCP_Messages;              use LMCP_Messages;
with Ada.Text_IO;                use Ada.Text_IO;

package body Waypoint_Plan_Manager with SPARK_Mode is

   ----------------------------------
   -- Extract_Mission_Command_Info --
   ----------------------------------

   procedure Extract_Mission_Command_Info
     (State : in out Waypoint_Plan_Manager_State;
      MC : MissionCommand);

   procedure Extract_Mission_Command_Info
     (State : in out Waypoint_Plan_Manager_State;
      MC : MissionCommand)
   is
      WP : Waypoint;
   begin

      State.MC := MC;
      Clear (State.Succ_IDs);
      Clear (State.WPs);

      for I in WP_Sequences.First .. Last (MC.WaypointList) loop
         WP := Get (MC.WaypointList, I);
         if WP.Number > 0 and then WP.NextWaypoint >= 0 then
            if not Contains (State.Succ_IDs, Positive64 (WP.Number)) then
               Insert (State.Succ_IDs, Positive64 (WP.Number), Natural64 (WP.NextWaypoint));
            end if;
            if not Contains (State.WPs, Positive64 (WP.Number)) then
               Insert (State.WPs, Positive64 (WP.Number), WP);
            end if;
         end if;
         pragma Loop_Invariant (Integer (Length (State.Succ_IDs)) <= I - WP_Sequences.First + 1);
         pragma Loop_Invariant (Integer (Length (State.WPs)) <= I - WP_Sequences.First + 1);
      end loop;
   end Extract_Mission_Command_Info;

   ---------------------------
   -- Handle_MissionCommand --
   ---------------------------

   procedure Handle_MissionCommand
     (State : in out Waypoint_Plan_Manager_State;
      MC : MissionCommand)
   is
      FirstID : Positive64 := MC.FirstWaypoint;
      M : WP_ID_Map;
      ID_List : WP_ID_Vector;
   begin

      State.New_Command := True;
      State.Next_Segment_ID := FirstID;
      State.Next_FirstID := FirstID;
      Extract_Mission_Command_Info (State, MC);
      M := State.Succ_IDs;
      Clear (State.Prefix);
      Clear (State.Cycle);

      -- Handle FirstID
      if Contains (State.Succ_IDs, FirstID) then
         if Element (State.Succ_IDs, FirstID) = 0
             or else not Contains (State.Succ_IDs, Positive64 (Element (State.Succ_IDs, FirstID))) -- Had to add Positive here for SPARK
             or else Element (State.Succ_IDs, FirstID) = FirstID
         then
            -- FirstID has no successors or points to itself. Return.
            Append (State.Prefix, FirstID);
            return;
         else
            -- FirstID has a successor. Continue.
            Append (ID_List, FirstID);
            Append (ID_List, Element (State.Succ_IDs, FirstID));
            Delete (M, FirstID);
         end if;
      else
         -- FirstID not known. Return.
         State.Next_Segment_ID := 0;
         State.Next_FirstID := 0;
         return;
      end if;

      -- Try to find a predecessor for FirstID.
      -- We already checked that FirstID does not point to itself.
      pragma Assert (not Is_Empty (ID_List));
      pragma Assert (Length (ID_List) = 2);

      for K of M loop
         if Element (M, K) = First_Element (ID_List) then
            if Last_Element (ID_List) = K then
               -- FirstID cycles with its precedessor. Return.
               State.Next_FirstID := K;
               State.Cycle := ID_List;
               return;
            else
               -- FirstID has precessor that starts the segment. Continue.
               State.Next_Segment_ID := K;
               Prepend (ID_List, K);
               Delete (M, K);
            end if;
            exit;
         end if;
         pragma Loop_Invariant (Length (ID_List) <= 3);
         pragma Loop_Invariant (not Is_Empty (ID_List));
      end loop;

      while Length (M) > 0 loop

         if Contains (M, Last_Element (ID_List)) then
            if Element (M, Last_Element (ID_List)) = 0
              or Element (M, Last_Element (ID_List)) = Last_Element (ID_List)
            then
               -- No more elements found, so no cycle. Return a prefix only.
               State.Prefix := ID_List;
               return;
            elsif Contains (ID_List, Element (M, Last_Element (ID_List))) then
               -- Found a cycle. Copy first part of ID_List to prefix and rest to cycle.
               declare
                  Index : WP_ID_Vectors.Extended_Index;
                  Next_ID : Positive64 := Element (M, Last_Element (ID_List));
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
                  ID : Positive64 := Last_Element (ID_List);
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

         pragma Loop_Invariant (Length (M) <= ID_List.Capacity - 4);
         pragma Loop_Invariant
           (Length (ID_List) <= 3 + Length (M'Loop_Entry) - Length (M));
         pragma Loop_Invariant (not Is_Empty (ID_List));
      end loop;

      -- If we ran out of keys in the map without returning yet, then there was
      -- no "0" element in the map. Everything is in the prefix (no cycle).
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
      ID : constant Positive64 := State.Next_Segment_ID;
      FirstID : constant Positive64 := State.Next_FirstID;
      Prefix : constant WP_ID_Vector := State.Prefix;
      Cycle : constant WP_ID_Vector := State.Cycle;
      Len : constant Positive := Positive (Config.NumberWaypointsToServe);
      Overlap : constant Positive := Positive (Config.NumberWaypointOverlap);

      I : Natural := 1;
      C : WP_ID_Vectors.Extended_Index;
      -- Seg_Out : WP_ID_Vector;
      In_Prefix : Boolean;
   begin

      State.Next_Segment_ID := 0;
      State.Next_FirstID := 0;
      State.New_Command := False;
      Clear (State.Segment);

      C := Find_Index (Prefix, ID);
      if C /= WP_ID_Vectors.No_Index then
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
            State.Next_FirstID := Element (Prefix, C);
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
            State.Next_FirstID := Element (Cycle, C);
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
      begin
         MC_Out.FirstWaypoint := FirstID;
         for WP_ID of State.Segment loop
            WP_List := Add (WP_List, Element (State.WPs, WP_ID));
         end loop;
         MC_Out.WaypointList := WP_List;
         sendBroadcastMessage (Mailbox, MC_Out);
         -- TODO: Modify last waypoint to point to itself
      end;

   end Produce_Segment;

   procedure Print (MC : MissionCommand);

   procedure Print (MC : MissionCommand)
   is
   begin
      Put_Line ("MissionCommand.FirstWaypoint: " & MC.FirstWaypoint'Image);
      Put ("MissionCommand.WaypointList IDs: ");
      for WP of MC.WaypointList loop
         Put ("[" & WP.Number'Image & ", " & WP.NextWaypoint'Image & "]");
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

   procedure Print (Succ_IDs : WP_ID_Map);

   procedure Print (Succ_IDs : WP_ID_Map)
   is
   begin
      Put ("Succ_IDs: ");
      for K of Succ_IDs loop
         Put ("[" & K'Image & ", " & Element (Succ_IDs, K)'Image & "]");
      end loop;
      New_Line;

   end Print;

   procedure Print (ID_List : WP_ID_Vector);

   procedure Print (ID_List : WP_ID_Vector)
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
      Put_Line ("Next_FirstID : " & State.Next_FirstID'Image);
      Put ("Prefix : ");
      Print (State.Prefix);
      Put ("Cycle : ");
      Print (State.Cycle);
      Put ("Segment : ");
      Print (State.Segment);
      Put_Line ("AV_WP_ID : " & State.AV_WP_ID'Image);

   end Print;

end Waypoint_Plan_Manager;
