with Ada.Containers; use Ada.Containers;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package body Algebra with SPARK_Mode is

   type Int64_Seq_Arr is array (Children_Index range <>) of Int64_Seq;

   procedure Next_Actions
     (Assignment             : Int64_Seq;
      Algebra                : not null access constant Algebra_Tree_Cell;
      Result                 : out Int64_Seq;
      Encounter_Executed_Out : out Boolean);

   ------------------
   -- Next_Actions --
   ------------------

   procedure Next_Actions
     (Assignment             : Int64_Seq;
      Algebra                : not null access constant Algebra_Tree_Cell;
      Result                 : out Int64_Seq;
      Encounter_Executed_Out : out Boolean)
   is
      ResultThis : Int64_Seq;
   begin
      case Algebra.Node_Kind is

         when Action =>
            declare
               ActionFound : Boolean :=
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

   function Get_Next_Objectives_Ids
     (Assignment : Int64_Seq;
      Algebra    : access constant Algebra_Tree_Cell)
      return Int64_Seq
   is
      Encounter_Executed : Boolean;
      Result             : Int64_Seq;
   begin
      Next_Actions (Assignment, Algebra, Result, Encounter_Executed);
      return Result;
   end Get_Next_Objectives_Ids;

   procedure Parse_Formula
     (Formula : Unbounded_String;
      Algebra : out Algebra_Tree)
   is
      Kind          : Node_Kind_Type := Undefined;
      Operator_Kind : Operator_Kind_Type := Undefined;
      Form          : Unbounded_String := Formula;
   begin
      for J in 1 .. Length (Form) loop

         if Element (Form, J) = '.' then
            Kind := Operator;
            Operator_Kind := Sequential;
            Form := To_Unbounded_String (Slice (Form, J + 2, Index (Form, ")", Backward) - 1));
            exit;

         elsif Element (Form, J) = '+' then
            Kind := Operator;
            Operator_Kind := Alternative;
            Form := To_Unbounded_String (Slice (Form, J + 2, Index (Form, ")", Backward) - 1));
            exit;

         elsif Element (Form, J) = '|' then
            Kind := Operator;
            Operator_Kind := Parallel;
            Form := To_Unbounded_String (Slice (Form, J + 2, Index (Form, ")", Backward) - 1));
            exit;

         elsif Element (Form, J) = 'p' then
            Kind := Action;

            if Index (Form, ")", Backward) = 0 then
               Form := To_Unbounded_String (Slice (Form, J + 1, Length (Form)));
            else
               Form := To_Unbounded_String (Slice (Form, J + 1, Index (Form, ")", Backward) - 1));
            end if;

            exit;

         end if;
      end loop;

      if Kind = Action then
         declare
            ActionID : Int64 :=
              Int64'Value (To_String (Form));
         begin
            Algebra := new Algebra_Tree_Cell'(Node_Kind     => Action,
                                              TaskOptionId  => ActionID);
         end;

      else
         declare
            numParenthesis : Natural := 0;
            Children_Array : Algebra_Tree_Array (1 .. Max_Children);
            numChildren    : Children_Number := 0;
         begin
            for J in 1 .. Length (Form) loop

               if Element (Form, J) in '+' | '.' | '|' then

                  if numParenthesis = 0 then
                     declare
                        iEnd : Natural := J + 1;
                        numParenthesisTmp : Natural := 0;
                     begin
                        while iEnd <= Length (Form) loop
                           if Element (Form, iEnd) = '(' then
                              numParenthesisTmp := numParenthesisTmp + 1;
                           elsif Element (Form, iEnd) = ')' then
                              numParenthesisTmp := numParenthesisTmp - 1;
                           end if;
                           if numParenthesisTmp = 0 then
                              exit;
                           end if;
                           iEnd := iEnd + 1;
                        end loop;
                        numChildren := numChildren + 1;
                        Parse_Formula (To_Unbounded_String (Slice (Form, J, iEnd)),
                                       Children_Array (numChildren));
                     end;
                  end if;

               elsif Element (Form, J) = 'p' then
                  if numParenthesis = 0 then
                     declare
                        iEnd : Natural := J + 2;
                     begin
                        while iEnd <= Length (Form) loop
                           if Element (Form, iEnd) in ')' | ' ' then
                              exit;
                           end if;
                           iEnd := iEnd + 1;
                        end loop;

                        numChildren := numChildren + 1;
                        Parse_Formula (To_Unbounded_String (Slice (Form, J, iEnd)),
                                       Children_Array (numChildren));
                     end;
                  end if;

               elsif Element (Form, J) = '(' then
                  numParenthesis := numParenthesis + 1;

               elsif Element (Form, J) = ')' then
                  numParenthesis := numParenthesis - 1;
               end if;
            end loop;
            declare
               Nb_Children : constant Children_Number := numChildren;
               Children    : Children_Collection (Nb_Children);
            begin
               Children.Children := Children_Array (1 .. Nb_Children);
               Algebra := new Algebra_Tree_Cell'(Node_Kind     => Operator,
                                                 Operator_Kind => Operator_Kind,
                                                 Collection    => Children);
            end;
         end;
      end if;
   end Parse_Formula;

   procedure Print_Tree (Algebra : access constant Algebra_Tree_Cell) is
      procedure Print_Tree_Aux (Node : access constant Algebra_Tree_Cell; I : Natural) is
         Prefix : String := I * " | ";
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
