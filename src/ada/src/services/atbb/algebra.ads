with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Common;                use Common;
with Ada.Unchecked_Deallocation;

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
   type Children_Array is new Algebra_Tree_Array
   with Predicate => (for all Child of Children_Array => Child /= null);

   type Children_Collection (Num_Children : Children_Number := 0) is record
      Children : Children_Array (1 .. Num_Children);
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
      Algebra : out Algebra_Tree;
      Error   : in out Boolean;
      Message : in out Unbounded_String)
   with
     Pre  => Length (Formula) > 1,
     Post => (if not Error then Algebra /= null);

   procedure Print_Tree
     (Algebra : not null access constant Algebra_Tree_Cell);

   function Is_Present
     (Algebra      : not null access constant Algebra_Tree_Cell;
      TaskOptionId : Int64)
      return Boolean
   is
      (case Algebra.Node_Kind is
       when Action   => TaskOptionId = Algebra.TaskOptionId,
       when Operator => (for some J in 1 .. Algebra.Collection.Num_Children => Is_Present (Algebra.Collection.Children (J), TaskOptionId)),
       when Undefined => False);
   pragma Annotate (GNATprove, Terminating, Is_Present);

   function Get_Next_Objectives_Ids
     (Assignment : Int64_Seq;
      Algebra    : not null access constant Algebra_Tree_Cell)
      return Int64_Seq
   with
     Post =>
       (for all ObjectiveId of Get_Next_Objectives_Ids'Result =>
          (Is_Present (Algebra, ObjectiveId)
             and then
           not Contains (Assignment, Int64_Sequences.First, Last (Assignment), ObjectiveId)));
   pragma Annotate (GNATprove, Terminating, Get_Next_Objectives_Ids);
   --  Returns a sequence of TaskOptionIds corresponding to the next possible
   --  actions considering Assignment.

   procedure Free_Tree (X : in out Algebra_Tree) with
     Depends => (X => X),
     Post    => X = null;

end Algebra;
