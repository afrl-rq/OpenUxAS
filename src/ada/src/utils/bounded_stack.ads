with SPARK.Containers.Formal.Unbounded_Vectors;
with Common; use Common;
with SPARK.Containers.Types; use SPARK.Containers.Types;

generic
   type Element_Type is private;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
package Bounded_Stack with SPARK_Mode is

   type Stack is private with Default_Initial_Condition => Size (Stack) = 0;

   subtype Positive_Count_Type is Count_Type range 1 .. Count_Type'Last;

   function Size (S : Stack) return Count_Type;

   function Get (S : Stack; I : Positive_Count_Type) return Element_Type
     with Ghost, Pre => I <= Size (S);

   function Element_Logic_Equal (Left, Right : Element_Type) return Boolean
   with
     Ghost,
     Import,
     Global => null,
     Annotate => (GNATprove, Logical_Equal),
     Annotate => (GNATprove, Always_Return);

   procedure Push (S : in out Stack; E : Element_Type) with
     Pre  => Size (S) < Count_Type'Last,
     Post =>
       Size (S) = Size (S'Old) + 1
         and then
       (for all I in 1 .. Size (S'Old) => Element_Logic_Equal (Get (S, I), Get (S'Old, I)))
         and then
           Element_Logic_Equal (Get (S, Size (S)), E);

   procedure Pop (S : in out Stack; E : out Element_Type) with
     Pre  => Size (S) > 0,
     Post =>
       Size (S) = Size (S'Old) - 1
         and then
       (for all I in 1 .. Size (S) => Element_Logic_Equal (Get (S, I), Get (S'Old, I)))
         and then
       Element_Logic_Equal (E, Get (S'Old, Size (S'Old)));

private

   package Element_Vects is new SPARK.Containers.Formal.Unbounded_Vectors
        (Index_Type   => Positive_Count_Type,
         Element_Type => Element_Type);
   type Stack is record
      Elements : Element_Vects.Vector;
   end record;
   use Element_Vects;

   function Size (S : Stack) return Count_Type is (Length (S.Elements));

   function Get (S : Stack; I : Positive_Count_Type) return Element_Type is (Element (S.Elements, I));

end Bounded_Stack;
