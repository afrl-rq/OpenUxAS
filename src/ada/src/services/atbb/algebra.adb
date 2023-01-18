with Ada.Containers;    use Ada.Containers;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings;       use Ada.Strings;
with Ada.Text_IO;       use Ada.Text_IO;
with Int64_Parsing;     use Int64_Parsing;

package body Algebra with SPARK_Mode is

   type Int64_Seq_Arr is array (Children_Index range <>) of Int64_Seq;

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Next_Actions
     (Assignment             : Int64_Seq;
      Algebra                : not null access constant Algebra_Tree_Cell;
      Result                 : out Int64_Seq;
      Encounter_Executed_Out : out Boolean);

   function Depth (T : access constant Algebra_Tree_Cell) return Big_Natural is
   begin
      if T = null then
         return 0;
      else
         if T.Node_Kind /= Operator then
            return 1;
         else
            declare
               Max_Depth : Big_Natural := 0;
            begin
               for J in T.all.Collection.Children'Range loop
                  Max_Depth := Max (Max_Depth, Depth (T.all.Collection.Children (J)));

                  pragma Loop_Invariant
                    (for all K in T.all.Collection.Children'First .. J =>
                       Max_Depth >= Depth (T.all.Collection.Children (K)));
               end loop;
               return Max_Depth + 1;
            end;
         end if;
      end if;
   end Depth;

   ---------------
   -- Free_Tree --
   ---------------

   procedure Free_Tree (T : in out Algebra_Tree)
   is
      procedure Internal_Free is new Ada.Unchecked_Deallocation
        (Algebra_Tree_Cell, Algebra_Tree);
   begin
      if T /= null then
         if T.all.Node_Kind = Operator then
            declare
               Children : Algebra_Tree_Array := Algebra_Tree_Array (T.all.Collection.Children);
            begin
               for J in Children'Range loop
                  Free_Tree (Children (J));

                  pragma Loop_Invariant (for all K in Children'First .. J => Children (K) = null);
               end loop;
            end;
         end if;
         Internal_Free (T);
      end if;
   end Free_Tree;

   -----------------------------
   -- Get_Next_Objectives_Ids --
   -----------------------------

   function Get_Next_Objectives_Ids
     (Assignment : Int64_Seq;
      Algebra    : not null access constant Algebra_Tree_Cell)
      return Int64_Seq
   is
      pragma SPARK_Mode (Off);
      Encounter_Executed : Boolean;
      Result             : Int64_Seq;
   begin
      Next_Actions (Assignment, Algebra, Result, Encounter_Executed);
      return Result;
   end Get_Next_Objectives_Ids;

   ------------------
   -- Next_Actions --
   ------------------

   procedure Next_Actions
     (Assignment             : Int64_Seq;
      Algebra                : not null access constant Algebra_Tree_Cell;
      Result                 : out Int64_Seq;
      Encounter_Executed_Out : out Boolean)
   is
      pragma SPARK_Mode (Off);
      ResultThis : Int64_Seq;
   begin
      case Algebra.Node_Kind is

         when Action =>
            declare
               ActionFound : constant Boolean :=
                 (for some TaskOptionId of Assignment =>
                    (TaskOptionId = Algebra.TaskOptionId));
            begin

               --  If the action has already been executed, we assign
               --  True to Encounter_Executed_Out.
               if ActionFound then
                  Encounter_Executed_Out := True;

               --  Otherwise, the action is an objective, so we add it to
               --  ResultThis. Encounter_Executed_Out is set to False;
               else
                  ResultThis := Add (ResultThis, Algebra.TaskOptionId);
                  Encounter_Executed_Out := False;
               end if;
            end;
         when Operator =>

            declare
               Num_Children       : Children_Number renames
                 Algebra.Collection.Num_Children;
               Children_Results   : Int64_Seq_Arr (1 .. Num_Children);
               --  We will need to store the results of this procedure for all
               --  children of the current node.
               Encounter_Executed : Boolean;
               --  Result of this procedure called on the children of the
               --  current node.
            begin
               Encounter_Executed_Out := False;

               case Algebra.Operator_Kind is
               when Sequential =>
                  --  Encounter_Executed_Out is set to true in the case where
                  --  the first child has no next objectives, i.e. it has
                  --  been executed.
                  Encounter_Executed_Out := True;

                  for J in 1 .. Num_Children loop
                     Next_Actions
                       (Assignment,
                        Algebra.Collection.Children (J),
                        Children_Results (J),
                        Encounter_Executed);

                     --  If this child has next objectives, we set ResultThis
                     --  to those objectives.
                     if Length (Children_Results (J)) > 0 then
                        ResultThis := Children_Results (J);

                        --  If this child is the first child, we set
                        --  Encounter_Executed_Out to Encounter_Executed.
                        --  Encounter_Executed_Out can be true when
                        --  the first child is an operator.
                        if J = 1 then
                           Encounter_Executed_Out := Encounter_Executed;
                        end if;
                        exit;
                     end if;
                  end loop;

               when Alternative =>
                  for J in 1 .. Num_Children loop
                     Next_Actions
                       (Assignment,
                        Algebra.Collection.Children (J),
                        Children_Results (J),
                        Encounter_Executed);

                     --  If this child has been executed, even partially,
                     --  no other child can be executed.
                     if Encounter_Executed then
                        Encounter_Executed_Out := True;
                        ResultThis := Children_Results (J);
                        exit;
                     end if;
                  end loop;

                  --  If no child has been executed, every action is a
                  --  candidate for the next assignment.
                  if not Encounter_Executed_Out then
                     for J in 1 .. Num_Children loop
                        for TaskOptionId of Children_Results (J) loop
                           pragma Assume (Length (ResultThis) < Count_Type'Last);
                           ResultThis := Add (ResultThis, TaskOptionId);
                        end loop;
                     end loop;
                  end if;

               when Parallel =>
                  for J in 1 .. Num_Children loop
                     Next_Actions
                       (Assignment,
                        Algebra.Collection.Children (J),
                        Children_Results (J),
                        Encounter_Executed);

                     --  All actions are candidate in a parallel assignment
                     for TaskOptionId of Children_Results (J) loop
                        pragma Assume (Length (ResultThis) < Count_Type'Last);
                        ResultThis := Add (ResultThis, TaskOptionId);
                     end loop;

                     --  If a child has been executed, Encounter_Executed_Out
                     --  is set to True.
                     if Encounter_Executed then
                        Encounter_Executed_Out := True;
                     end if;
                  end loop;
               when Undefined =>
                  raise Program_Error;
               end case;
            end;
         when Undefined =>
            raise Program_Error;
      end case;
      Result := ResultThis;
   end Next_Actions;

   -------------------
   -- Parse_Formula --
   -------------------

   procedure Parse_Formula
     (Formula : Unbounded_String;
      Algebra : out Algebra_Tree;
      Error   : in out Boolean;
      Message : in out Unbounded_String)
   is
      package Unb renames Common.Unbounded_Strings_Subprograms;
      Kind          : Node_Kind_Type := Undefined;
      Operator_Kind : Operator_Kind_Type := Undefined;
      form          : Unbounded_String := Formula;
   begin
      Algebra := null;
      if Element (form, 1) in '.' | '|' | '+' then
         if Element (form, 2) /= '(' or else Element (form, Length (form)) /= ')' then
            Append_To_Msg (Message, "Substring " & '"');
            Append_To_Msg (Message, form);
            Append_To_Msg (Message, '"' & ": character '" & Element (form, 1) & "should be followed by '(' and substring should end with ')'. ");
            Error := True;
            return;
         end if;
      elsif Element (form, 1) /= 'p' then
         Append_To_Msg (Message, "Substring " & '"');
         Append_To_Msg (Message, form);
         Append_To_Msg (Message, '"' & ": substring should begin with '.', '|', '+', or 'p'. ");
         Error := True;
         return;
      end if;

      if Element (form, 1) = '.' then
         Kind := Operator;
         Operator_Kind := Sequential;
         form := To_Unbounded_String (Unb.Slice (form, 3, Unb.Index (form, ")", Backward) - 1));

      elsif Element (form, 1) = '+' then
         Kind := Operator;
         Operator_Kind := Alternative;
         form := To_Unbounded_String (Unb.Slice (form, 3, Unb.Index (form, ")", Backward) - 1));

      elsif Element (form, 1) = '|' then
         Kind := Operator;
         Operator_Kind := Parallel;
         form := To_Unbounded_String (Unb.Slice (form, 3, Unb.Index (form, ")", Backward) - 1));

      elsif Element (form, 1) = 'p' then
         Kind := Action;

         if Index (form, ")", Backward) = 0 then
            form := To_Unbounded_String (Slice (form, 2, Length (form)));
            pragma Assert (Length (form) <= Length (Formula) and then Length (form) < Natural'Last);
         else
            form := To_Unbounded_String (Unb.Slice (form, 2, Unb.Index (form, ")", Backward) - 1));
         end if;
      end if;

      if Kind = Action then
         declare
            Str           : constant String := To_String (form);
            ActionID      : Int64;
            Parsing_Error : Boolean;
         begin
            if Str'Last = Integer'Last then
               Append_To_Msg (Message, "Substring " & '"');
               Append_To_Msg (Message, Str);
               Append_To_Msg (Message, '"' & ": substring is too long. ");
               Error := True;
               return;
            end if;

            Parse_Int64 (Str, ActionID, Parsing_Error);

            if Parsing_Error then
               Append_To_Msg (Message, "Substring " & '"');
               Append_To_Msg (Message, Str);
               Append_To_Msg (Message, '"' & ": does not correspond to an Int64. ");
               Error := True;
               return;
            end if;

            Algebra := new Algebra_Tree_Cell'(Node_Kind     => Action,
                                              TaskOptionId  => ActionID);
         end;

      else
         declare
            numParenthesis : Natural := 0;
            Children_Arr   : Algebra_Tree_Array (1 .. Max_Children);
            numChildren    : Children_Number := 0;
         begin
            for J in 1 .. Length (form) - 1 loop

               if Element (form, J) in '+' | '.' | '|' then

                  if numParenthesis = 0 then
                     declare
                        iEnd              : Natural := J + 1;
                        numParenthesisTmp : Natural := 0;
                     begin
                        if Element (form, iEnd) /= '(' then
                           Append_To_Msg (Message, "Substring " & '"');
                           Append_To_Msg (Message, Slice (form, J, Length (form)));
                           Append_To_Msg (Message, '"' & ": character '" & Element (form, 1) & "should be followed by '('. ");
                           Error := True;
                        else
                           while iEnd <= Length (form) loop
                              if Element (form, iEnd) = '(' then
                                 if numParenthesisTmp = Natural'Last then
                                    Append_To_Msg (Message, "Substring " & '"');
                                    Append_To_Msg (Message, form);
                                    Append_To_Msg (Message, '"' & ": substring has too many opening parentheses. ");
                                    Error := True;
                                    exit;
                                 else
                                    numParenthesisTmp := numParenthesisTmp + 1;
                                 end if;

                              elsif Element (form, iEnd) = ')' then
                                 numParenthesisTmp := numParenthesisTmp - 1;
                              end if;

                              if numParenthesisTmp = 0 then
                                 exit;
                              end if;

                              pragma Loop_Invariant (iEnd in J + 1 .. Length (form));
                              pragma Loop_Invariant (numParenthesisTmp >= 1);
                              pragma Loop_Variant (Increases => iEnd);
                              iEnd := iEnd + 1;
                           end loop;
                           if not Error then
                              if numParenthesisTmp /= 0 then
                                 Append_To_Msg (Message, "Substring " & '"');
                                 Append_To_Msg (Message, Slice (form, J, Length (form)));
                                 Append_To_Msg (Message, '"' & ": substring is missing one or several closing parentheses. ");
                                 Error := True;
                              else
                                 if numChildren = Children_Number'Last then
                                    Append_To_Msg (Message, "Substring " & '"');
                                    Append_To_Msg (Message, form);
                                    Append_To_Msg (Message, '"' & ": substring defines too many children. ");
                                    Error := True;
                                 else
                                    numChildren := numChildren + 1;
                                    Parse_Formula (To_Unbounded_String (Slice (form, J, iEnd)),
                                                   Children_Arr (numChildren),
                                                   Error,
                                                   Message);
                                    if Error then
                                       exit;
                                    end if;
                                 end if;
                              end if;
                           end if;
                        end if;
                     end;
                  end if;

               elsif Element (form, J) = 'p' then
                  if numParenthesis = 0 then
                     declare
                        iEnd : Natural := J + 2;
                     begin
                        while iEnd <= Length (form) loop
                           pragma Loop_Invariant (iEnd in J + 2 .. Length (form));
                           pragma Loop_Variant (Increases => iEnd);
                           if Element (form, iEnd) in ')' | ' ' then
                              exit;
                           end if;
                           iEnd := iEnd + 1;
                        end loop;
                        if numChildren = Children_Number'Last then
                           Append_To_Msg (Message, "Substring " & '"');
                           Append_To_Msg (Message, form);
                           Append_To_Msg (Message, '"' & ": substring defines too many children. ");
                           Error := True;
                        else
                           numChildren := numChildren + 1;
                           Parse_Formula (To_Unbounded_String (Slice (form, J, iEnd - 1)),
                                          Children_Arr (numChildren),
                                          Error,
                                          Message);
                           if Error then
                              exit;
                           end if;
                        end if;
                     end;
                  end if;

               elsif Element (form, J) = '(' then
                  if numParenthesis = Natural'Last then
                     Append_To_Msg (Message, "Substring " & '"');
                     Append_To_Msg (Message, form);
                     Append_To_Msg (Message, '"' & ": substring has too many opening parentheses. ");
                     Error := True;
                  else
                     numParenthesis := numParenthesis + 1;
                  end if;

               elsif Element (form, J) = ')' then
                  if numParenthesis = 0 then
                     Append_To_Msg (Message, "Substring " & '"');
                     Append_To_Msg (Message, form);
                     Append_To_Msg (Message, '"' & ": substring has too many closing parentheses. ");
                     Error := True;
                  else
                     numParenthesis := numParenthesis - 1;
                  end if;
               end if;
               pragma Loop_Invariant (for all K in 1 .. numChildren => Children_Arr (K) /= null);
               pragma Loop_Invariant (for all K in numChildren + 1 .. Children_Arr'Last => Children_Arr (K) = null);
            end loop;
            if not Error then
               declare
                  Nb_Children : constant Children_Number := numChildren;
                  Children    : Children_Collection (Nb_Children)
                    := (Num_Children => Nb_Children,
                        Children     => Children_Array (Children_Arr (1 .. Nb_Children)));
               begin
                  Algebra := new Algebra_Tree_Cell'(Node_Kind     => Operator,
                                                    Operator_Kind => Operator_Kind,
                                                    Collection    => Children);
               end;
            else
               for J in 1 .. numChildren loop
                  Free_Tree (Children_Arr (J));
                  pragma Loop_Invariant (for all K in 1 .. J => Children_Arr (K) = null);
               end loop;
            end if;
         end;
      end if;
   end Parse_Formula;

   ----------------
   -- Print_Tree --
   ----------------

   procedure Print_Tree (Algebra : not null access constant Algebra_Tree_Cell) is
      pragma SPARK_Mode (Off);
      procedure Print_Tree_Aux (Node : not null access constant Algebra_Tree_Cell; I : Natural);

      procedure Print_Tree_Aux (Node : not null access constant Algebra_Tree_Cell; I : Natural) is
         Prefix : constant String := I * " | ";
      begin
         if Node.Node_Kind = Action then
            Put_Line (Prefix & "Action node: " & Node.TaskOptionId'Image);
         else
            Put_Line (Prefix & "Operator " & Node.Operator_Kind'Image & ":");
            for J in 1 .. Node.Collection.Num_Children loop
               Print_Tree_Aux (Node.Collection.Children (J), I + 1);
            end loop;
         end if;
      end Print_Tree_Aux;
   begin
      Print_Tree_Aux (Algebra, 0);
   end Print_Tree;

end Algebra;
