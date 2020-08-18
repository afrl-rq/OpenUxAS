with Ada.Containers.Functional_Vectors;
with Ada.Containers.Functional_Sets;
with Ada.Containers.Functional_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces;

package Common with SPARK_Mode is

   type UInt32 is new Interfaces.Unsigned_32;

   type Int64 is new Integer;
   type Real32 is new Interfaces.IEEE_Float_32;
   type Real64 is new Interfaces.IEEE_Float_64;

   function Get_TaskID   (TaskOptionID : Int64) return Int64 is (TaskOptionID / 100000);
   --  Retrieve the TaskId from a TaskOptionId
   function Get_OptionID (TaskOptionID : Int64) return Int64 is (TaskOptionID rem 100000);
   --  Retrieve the OptionId from a TaskOptionId
   function Get_TaskOptionID (TaskID, OptionID : Int64) return Int64 is (TaskID * 100000 + OptionID);
   --  Generate TaskOptionID from TaskID and OptionID

   function Int64_Hash (X : Int64) return Ada.Containers.Hash_Type is
     (Ada.Containers.Hash_Type'Mod (X));

   package Int64_Sequences is new Ada.Containers.Functional_Vectors
     (Index_Type   => Positive,
      Element_Type => Int64);
   type Int64_Seq is new Int64_Sequences.Sequence;

   package Int64_Sets is new Ada.Containers.Functional_Sets (Int64);
   type Int64_Set is new Int64_Sets.Set;

   package Int64_Maps is new Ada.Containers.Functional_Maps
     (Int64, Int64);
   type Int64_Map is new Int64_Maps.Map;

   --  Messages are unbounded strings. To avoid having to prove that messages
   --  do not overflow Integer'Last, we use a procedure which will truncate
   --  the message if it is too long. We can justify that this should not
   --  happen in practice.

   procedure Append_To_Msg
     (Msg  : in out Unbounded_String;
      Tail : String);
   --  Append Tail to Msg if there is enough room in the unbounded string
end Common;
