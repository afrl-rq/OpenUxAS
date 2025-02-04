package body Finite_Sequences with SPARK_Mode is

   function Concat (Fs1, Fs2 : Finseq) return Finseq is
      New_Length : Natural := Fs1.Length + Fs2.Length;
      Result     : Finseq (Length => Fs1.Length + Fs2.Length);
   begin
      -- Ensure Result's length does not exceed Max_Length
      pragma Assert(New_Length <= Max_Length);

      Result.Seq := (others => Default_Value);
      for I in 0 .. Integer(Fs1.Length - 1) loop
         Result.Seq(I) := Fs1.Seq(I);
      end loop;
      for I in 0 .. Integer(Fs2.Length - 1) loop
         Result.Seq(Integer(Fs1.Length) + I) := Fs2.Seq(I);
      end loop;
      return Result;
   end Concat;

   function Slice (Fs : Finseq; M, N : Integer) return Finseq is
      Start  : Integer := Integer'Max(M, 0);
      End_Index : Integer := Integer'Min(N, Integer(Fs.Length - 1));
      Length : Natural := Natural(End_Index - Start + 1);
      Result : Finseq (Length => Natural(Integer'Min(N, Integer(Fs.Length - 1)) - Integer'Max(M, 0) + 1));
   begin
      pragma Assert(Result.Length = Length);
      Result.Seq := (others => Default_Value);
      if M > N or M >= Integer(Fs.Length) then
         return Empty_Seq;
      else
         for I in 0 .. Integer(Length - 1) loop
            Result.Seq(I) := Fs.Seq(Start + I);
         end loop;
         return Result;
      end if;
   end Slice;

end Finite_Sequences;
