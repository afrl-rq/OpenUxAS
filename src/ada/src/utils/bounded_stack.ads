with Ada.Containers.Functional_Maps;
with Ada.Containers.Functional_Vectors;
with Common; use Common;
with Ada.Containers; use Ada.Containers;

generic
   type Element_Type is private;
package Bounded_Stack with SPARK_Mode is

   Capacity : constant Integer := 200;
   Empty    : constant Integer := 0;

   subtype Extent is Integer range Empty .. Capacity;
   subtype Index is Extent range 1 .. Capacity;

   type Stack is private;

   function Size (S : Stack) return Extent;

   function Element (S : Stack; I : Index) return Element_Type
     with Ghost, Pre => I <= Size (S);

   procedure Push (S : in out Stack; E : Element_Type) with
     Pre  => Size (S) < Capacity,
     Post =>
       Size (S) = Size (S'Old) + 1
         and then
       (for all I in 1 .. Size (S'Old) => Element (S, I) = Element (S'Old, I))
         and then
           Element (S, Size (S)) = E;

   procedure Pop (S : in out Stack; E : out Element_Type) with
     Pre  => Size (S) > Empty,
     Post =>
       Size (S) = Size (S'Old) - 1
         and then
       (for all I in 1 .. Size (S) => Element (S, I) = Element (S'Old, I))
         and then
       E = Element (S'Old, Size (S'Old));

private

   type Content_Array is array (Index) of Element_Type with Relaxed_Initialization;

   type Stack is record
      Top     : Extent := 0;
      Content : Content_Array;
   end record
     with Predicate => (for all I in 1 .. Top => Content (I)'Initialized);

   function Size (S : Stack) return Extent is (S.Top);

   function Element (S : Stack; I : Index) return Element_Type is (S.Content (I));

end Bounded_Stack;
