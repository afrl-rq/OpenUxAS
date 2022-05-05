package body Common with SPARK_Mode is

   -------------------
   -- Append_To_Msg --
   -------------------

   procedure Append_To_Msg (Msg : in out Unbounded_String; Tail : String) is
   begin
      if Length (Msg) < Natural'Last  and then Tail'Length > 0 then
         declare
            Max_Length : constant Positive := Natural'Last - Length (Msg);
         begin
            if Max_Length <= Tail'Length then
               Append (Msg, Tail (Tail'First .. Tail'First - 1 + Max_Length));
            else
               Append (Msg, Tail);
            end if;
         end;
      end if;
   end Append_To_Msg;

   procedure Append_To_Msg (Msg  : in out Unbounded_String; Tail : Unbounded_String) is
   begin
      if Length (Msg) < Natural'Last then
         declare
            Max_Length : constant Positive := Natural'Last - Length (Msg);
         begin
            Append (Msg, Slice (Tail, 1, Natural'Min (Length (Tail), Max_Length)));
         end;
      end if;
   end Append_To_Msg;

   procedure Append_To_Msg (Msg  : in out Unbounded_String; Tail : Character) is
   begin
      if Length (Msg) < Natural'Last then
         Append (Msg, Tail);
      end if;
   end Append_To_Msg;

end Common;
