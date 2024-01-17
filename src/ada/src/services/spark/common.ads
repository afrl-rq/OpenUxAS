with SPARK.Containers.Functional.Vectors;
with SPARK.Containers.Functional.Sets;
with SPARK.Containers.Functional.Maps;
with SPARK.Containers.Types; use SPARK.Containers.Types;
with SPARK.Big_Integers;
with Ada.Strings;
with Ada.Strings.Maps;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with Interfaces;

package Common with SPARK_Mode is

   type UInt32 is new Interfaces.Unsigned_32;

   type Int64 is new Interfaces.Integer_64;
   type Real32 is new Interfaces.IEEE_Float_32;
   type Real64 is new Interfaces.IEEE_Float_64;

   function Get_TaskID   (TaskOptionID : Int64) return Int64 is (TaskOptionID / 100_000) with
     Pre => TaskOptionID in 0 .. 9_999_999_999;
   --  Retrieve the TaskId from a TaskOptionId
   function Get_OptionID (TaskOptionID : Int64) return Int64 is (TaskOptionID rem 100_000) with
     Pre => TaskOptionID in 0 .. 9_999_999_999;
   --  Retrieve the OptionId from a TaskOptionId
   function Get_TaskOptionID (TaskID, OptionID : Int64) return Int64 is (TaskID * 100_000 + OptionID)
   with Pre => TaskID in 0 .. 99_999 and then OptionID in 0 .. 99_999;
   --  Generate TaskOptionID from TaskID and OptionID

   function Int64_Hash (X : Int64) return SPARK.Containers.Types.Hash_Type is
     (SPARK.Containers.Types.Hash_Type'Mod (X));

   package Int64_Sequences is new SPARK.Containers.Functional.Vectors
     (Index_Type   => Positive,
      Element_Type => Int64);
   type Int64_Seq is new Int64_Sequences.Sequence;

   package Int64_Sets is new SPARK.Containers.Functional.Sets (Int64);
   type Int64_Set is new Int64_Sets.Set;

   package Int64_Maps is new SPARK.Containers.Functional.Maps
     (Int64, Int64);
   type Int64_Map is new Int64_Maps.Map;

   package Count_Type_To_Big_Integer_Conversions is new
     SPARK.Big_Integers.Signed_Conversions (Count_Type);

   function "+" (R : Count_Type) return SPARK.Big_Integers.Big_Integer renames
     Count_Type_To_Big_Integer_Conversions.To_Big_Integer;

   --  Messages are unbounded strings. To avoid having to prove that messages
   --  do not overflow Integer'Last, we use a procedure which will truncate
   --  the message if it is too long. We can justify that this should not
   --  happen in practice.

   procedure Append_To_Msg
     (Msg  : in out Unbounded_String;
      Tail : String);

   procedure Append_To_Msg
     (Msg  : in out Unbounded_String;
      Tail : Unbounded_String);

   procedure Append_To_Msg
     (Msg  : in out Unbounded_String;
      Tail : Character);
   --  Append Tail to Msg if there is enough room in the unbounded string

   package Unbounded_Strings_Subprograms with Annotate => (GNATprove, Always_Return) is

      function Index
        (Source  : Unbounded_String;
         Pattern : String;
         Going   : Ada.Strings.Direction := Ada.Strings.Forward;
         Mapping : Ada.Strings.Maps.Character_Mapping := Ada.Strings.Maps.Identity) return Natural
      with
        Pre    => Pattern'Length /= 0,
        Post   => Index'Result in 1 .. Length (Source) - Pattern'Length + 1,
        Global => null;

      function Index
        (Source  : Unbounded_String;
         Pattern : String;
         From    : Positive;
         Going   : Ada.Strings.Direction := Ada.Strings.Forward;
         Mapping : Ada.Strings.Maps.Character_Mapping := Ada.Strings.Maps.Identity) return Natural
      with
        Pre    =>
          (if Length (Source) /= 0
           then From <= Length (Source))
             and then Pattern'Length /= 0,
        Post   => Index'Result in 0 |
                                  From .. Length (Source) - Pattern'Length + 1,
        Global => null;

      function Slice
        (Source : Unbounded_String;
         Low    : Positive;
         High   : Natural) return String
      with
        Pre    => Low - 1 <= Length (Source) and then High <= Length (Source),
        Post   =>
          Slice'Result'Length = Natural'Max (0, High - Low + 1)
            and then Slice'Result'First = 1
            and then (for all J in Slice'Result'Range =>
                        (Element (Source, Low - 1 + J) = Slice'Result (J))),
        Global => null;
   private
      pragma SPARK_Mode (Off);

      function Index
        (Source  : Unbounded_String;
         Pattern : String;
         Going   : Ada.Strings.Direction := Ada.Strings.Forward;
         Mapping : Ada.Strings.Maps.Character_Mapping := Ada.Strings.Maps.Identity) return Natural
      is (Ada.Strings.Unbounded.Index (Source, Pattern, Going, Mapping));

      function Index
        (Source  : Unbounded_String;
         Pattern : String;
         From    : Positive;
         Going   : Ada.Strings.Direction := Ada.Strings.Forward;
         Mapping : Ada.Strings.Maps.Character_Mapping := Ada.Strings.Maps.Identity) return Natural
      is (Ada.Strings.Unbounded.Index (Source, Pattern, From, Going, Mapping));

      function Slice
        (Source : Unbounded_String;
         Low    : Positive;
         High   : Natural) return String
      is (Ada.Strings.Unbounded.Slice (Source, Low, High));
   end Unbounded_Strings_Subprograms;
end Common;
