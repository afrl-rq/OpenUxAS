with Ada.Containers;             use Ada.Containers;
with AVTAS.LMCP.Types;
with UxAS.Comms.LMCP_Net_Client; use UxAS.Comms.LMCP_Net_Client;
with LMCP_Messages;              use LMCP_Messages;

package body Waypoint_Plan_Manager with SPARK_Mode is

   function To_WP_ID_Map (MC : MissionCommand) return WP_ID_Map
     with Pre => Length (MC.WaypointList) <= Max;

   ------------------
   -- To_WP_ID_Map --
   ------------------

   function To_WP_ID_Map (MC : MissionCommand) return WP_ID_Map is
      M : WP_ID_Map;
      WP : Waypoint;
   begin

      for I in WP_Sequences.First .. Last (MC.WaypointList) loop
         WP := Get (MC.WaypointList, I);
         if WP.Number > 0 and then WP.NextWaypoint >= 1 and then not Contains (M, WP.Number)
         then
            Insert (M, WP.Number, WP.NextWaypoint);
         end if;
         pragma Loop_Invariant (Integer (Length (M)) <= I - WP_Sequences.First + 1);
      end loop;

      return M;

   end To_WP_ID_Map;

   -----------------------------
   -- Process_Mission_Command --
   -----------------------------

   procedure Process_Mission_Command
     (This : in out Waypoint_Plan_Manager_State)
   is
      FirstID : Positive64 := Positive64 (This.Original_MC.FirstWaypoint);
      M : WP_ID_Map := To_WP_ID_Map (This.Original_MC);
      ID_List : WP_ID_Vector;
   begin

      This.ID_Map := M;
      This.New_Command := True;
      This.Next_Seg_ID := FirstID;
      This.Second_Element_Is_FirstID := False;
      Clear (This.Prefix);
      Clear (This.Cycle);

      -- Handle FirstID
      if Contains (This.ID_Map, FirstID) then
         -- FirstID has no successors or points to itself. Return it.
         if Element (This.ID_Map, FirstID) = 0
             or else not Contains (This.ID_Map, Positive64 (Element (This.ID_Map, FirstID))) -- Had to add Positive here for SPARK
             or else Element (This.ID_Map, FirstID) = FirstID
         then
            Append (This.Prefix, FirstID);
            return;
         -- Continue with first two IDs in ID_List and FirstID removed from M
         else
            Append (ID_List, FirstID);
            Append (ID_List, Element (This.ID_Map, FirstID));
            Delete (M, FirstID);
         end if;
      else
         -- FirstID is not valid. Return an empty WP_Command
         return;
      end if;

      -- Try to find a predecessor for FirstID.
      -- We already checked that FirstID does not point to itself.
      -- The predecessor points to FirstID, and if FirstID points to the
      -- predecessor, return a cycle.
      pragma Assert (not Is_Empty (ID_List));
      pragma Assert (Length (ID_List) = 2);

      for K of M loop
         if Element (M, K) = First_Element (ID_List) then
            -- FirstID cycles with its precedessor. Return.
            if Last_Element (ID_List) = K then
               This.Cycle := ID_List;
               This.Second_Element_Is_FirstID := True; -- Derek says this is right
               return;
            -- First ID has precessor that starts the segment. Continue.
            else
               This.Next_Seg_ID := K;
               Prepend (ID_List, K);
               This.Second_Element_Is_FirstID := True;
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
            -- No more elements found, so no cycle. Return a prefix only.
            then
               This.Prefix := ID_List;
               return;
            elsif Contains (ID_List, Element (M, Last_Element (ID_List))) then
               -- Found a cycle. Copy first part of ID_List to prefix and rest to cycle.
               declare
                  Index : WP_ID_Vectors.Extended_Index;
                  Next_ID : Positive64 := Element (M, Last_Element (ID_List));
               begin
                  Index := Find_Index (ID_List, Next_ID);
                  for I in First_Index (ID_List) .. Index - 1 loop
                     Append (This.Prefix, Element (ID_List, I));
                     pragma Loop_Invariant (Integer (Length (This.Prefix)) = I - First_Index (ID_List) + 1);
                  end loop;
                  for I in Index .. Last_Index (ID_List) loop
                     Append (This.Cycle, Element (ID_List, I));
                     pragma Loop_Invariant (Integer (Length (This.Cycle)) = I - Index + 1);
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
            This.Prefix := ID_List;
            return;
         end if;

         pragma Loop_Invariant (Length (M) <= ID_List.Capacity - 4);
         pragma Loop_Invariant
           (Length (ID_List) <= 3 + Length (M'Loop_Entry) - Length (M));
         pragma Loop_Invariant (not Is_Empty (ID_List));
      end loop;

      -- If we ran out of keys in the map without returning yet, then there was
      -- no "0" element in the map. Everything is in the prefix (no cycle)
      This.Prefix := ID_List;

   end Process_Mission_Command;

   procedure Update_Segment
     (State : in out Waypoint_Plan_Manager_State;
      Config : Waypoint_Plan_Manager_Configuration_Data)
   is
      ID : constant Positive64 := State.Next_Seg_ID;
      Prefix : constant WP_ID_Vector := State.Prefix;
      Cycle : constant WP_ID_Vector := State.Cycle;
      Len : constant Positive := Positive (Config.NumberWaypointsToServe);
      Overlap : constant Positive := Positive (Config.NumberWaypointOverlap);

      I : Natural := 1;
      C : WP_ID_Vectors.Extended_Index;
      Seg_Out : WP_ID_Vector;
      In_Prefix : Boolean;
   begin

      State.Next_Seg_ID := 0;

      C := Find_Index (Prefix, ID);
      if C /= WP_ID_Vectors.No_Index then
         In_Prefix := True;
      else
         In_Prefix := False;
      end if;

      while C in First_Index (Prefix) .. Last_Index (Prefix) and then I <= Len loop
         pragma Loop_Invariant (Natural (Length (Seg_Out)) < I);
         Append (Seg_Out, Element (Prefix, C));
         if I = Len - Overlap  + 1 and (not Is_Empty (Cycle) or C /= Last_Index (Prefix)) then
            State.Next_Seg_ID := Element (Prefix, C);
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
         pragma Loop_Invariant (Natural (Length (Seg_Out)) < I);
         Append (Seg_Out, Element (Cycle, C));
         if I = Len - Overlap + 1 then
            State.Next_Seg_ID := Element (Cycle, C);
         end if;
         C := Iter_Next (Cycle, C);
         if not Iter_Has_Element (Cycle, C) then
            C := First_Index (Cycle);
         end if;

         I := I + 1;
      end loop;

      if State.New_Command then
         State.New_Command := False;
      else
         State.Second_Element_Is_FirstID := True;
      end if;

      State.Current_Segment := Seg_Out;

   end Update_Segment;

end Waypoint_Plan_Manager;