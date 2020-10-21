with Common;  use Common;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Algebra with SPARK_Mode is

   Max_Children : constant := 50;

   type Node_Kind_Type is (Action, Operator, Undefined);
   --  Nodes can be either Action nodes or Operator nodes
   type Operator_Kind_Type is (Sequential, Alternative, Parallel, Undefined);
   --  In case of Operator nodes, there are 3 possibilities

   type Algebra_Tree_Cell (Node_Kind : Node_Kind_Type);
   type Algebra_Tree is access Algebra_Tree_Cell;
   --  Cell type and its corresponding pointer

   subtype Children_Number is Integer range 0 .. Max_Children;
   subtype Children_Index is Children_Number range 1 .. Children_Number'Last;
   type Algebra_Tree_Array is array (Children_Index range <>) of Algebra_Tree;

   type Children_Collection (Num_Children : Children_Number := 0) is record
      Children : Algebra_Tree_Array (1 .. Num_Children);
   end record;
   --  This type is used to store the pointers to the children of an Operator
   --  node.

   type Algebra_Tree_Cell (Node_Kind : Node_Kind_Type) is record
      case Node_Kind is
         when Action =>
            TaskOptionId : Int64;
         when Operator =>
            Operator_Kind : Operator_Kind_Type;
            Collection    : Children_Collection;
         when Undefined =>
            null;
      end case;
   end record;

   procedure Parse_Formula
     (Formula : Unbounded_String;
      Algebra : out Algebra_Tree);

   procedure Print_Tree
     (Algebra : access constant Algebra_Tree_Cell);

   function Get_Next_Objectives_Ids
     (Assignment : Int64_Seq;
      Algebra    : access constant Algebra_Tree_Cell)
      return Int64_Seq;
   --  Returns a sequence of TaskOptionIds corresponding to the next possible
   --  actions considering Assignment.

end Algebra;
