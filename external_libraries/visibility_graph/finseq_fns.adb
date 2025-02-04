package body Finseq_Fns with SPARK_Mode is

   function Cdr (Seq : Nonempty_Seq) return Finseq is
      Result : Finseq(Length => Seq.Length - 1);
   begin
      Result.Seq := (others => Default_Value);
      for I in 0 .. Integer(Seq.Length - 2) loop
         Result.Seq(I) := Seq.Seq(I + 1);
      end loop;
      return Result;
   end Cdr;

   function Remove (Seq : Finseq; Idx : Natural) return Finseq is
      Result : Finseq (Length => Seq.Length - 1);
   begin
      Result.Seq := (others => Default_Value);
      for I in 0 .. Integer(Result.Length - 1) loop
         if I < Idx then
            Result.Seq(I) := Seq.Seq(I);
         else
            Result.Seq(I) := Seq.Seq(I + 1);
         end if;
      end loop;
      return Result;
   end Remove;

   function Count (Seq : Finseq; E : T) return Natural is
      Result : Natural := 0;
   begin
      for I in 0 .. Integer(Seq.Length - 1) loop
         if Seq.Seq(I) = E then
            Result := Result + 1;
         end if;
      end loop;
      return Result;
   end Count;

   function Sum_Of_Counts (Seq : Finseq; S : Fixed_Seq_Array) return Natural is
      Result : Natural := 0;
   begin
      for I in S'Range loop
         Result := Result + Count(Seq, S(I)); -- Add the count of each element
      end loop;
      return Result;
   end Sum_Of_Counts;

   function Reverse_Seq (Seq : Finseq) return Finseq is
      Result : Finseq (Length => Seq.Length);
   begin
      Result.Seq := (others => Default_Value);
      for I in 0 .. Integer(Seq.Length - 1) loop
         Result.Seq(I) := Seq.Seq(Integer(Seq.Length - 1) - I);
      end loop;
      return Result;
   end Reverse_Seq;

   function Reverse_Rec (Seq : Finseq) return Finseq is
      Result : Finseq (Length => Seq.Length);
      Current_Length : Natural := 0;
   begin
      Result.Seq := (others => Default_Value);
      for I in 0 .. Integer(Seq.Length - 1) loop
         Result.Seq(Current_Length) := Seq.Seq(Integer(Seq.Length - 1) - I);
         Current_Length := Current_Length + 1;
      end loop;
      return Result;
   end Reverse_Rec;

end Finseq_Fns;
