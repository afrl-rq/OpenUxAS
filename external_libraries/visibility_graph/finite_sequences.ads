-- -----------------------------------------------------------------------------
-- finite_Ssquences.ads               Dependable Computing
-- Corresponds to the finite_sequences theory in the PVS prelude
-- -----------------------------------------------------------------------------
generic
   type T is private; -- Generic type for sequence elements
   Default_Value : T;
package Finite_Sequences with SPARK_Mode is

   -- Declare a fixed-size zero-based array type
   -- Max_Length is the maximum allowable length of a finite sequence
   Max_Length : constant Natural := 1024; -- Adjustable limit
   type Fixed_Seq_Array is array (Integer range 0 .. Max_Length - 1) of T;

   -- Record type for finite sequences
   -- PVS: finite_sequence: TYPE = [# length: nat, seq: [below[length] -> T] #]
   type Finseq(Length: Natural) is record
      Seq: Fixed_Seq_Array; -- Full array; logical length is managed separately
   end record;

   -- Empty sequence
   function Empty_Seq return Finseq is (Length => 0, Seq => (others => Default_Value));

   -- Sequence application
   function Seq_Apply(Fs: Finseq; Index: Natural) return T is (Fs.Seq(Index))
      with Pre => Index < Fs.Length;

   -- Concatenation of two sequences
   function Concat(Fs1, Fs2: Finseq) return Finseq;

   -- Slicing operation
   function Slice(Fs: Finseq; M, N : Integer) return Finseq;

   -- Extract single element from a singleton sequence
   function Extract1(Fs: Finseq) return T is (Fs.Seq(0))
      with Pre => Fs.Length = 1;

end Finite_Sequences;
