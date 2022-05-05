package body Int64_Parsing with SPARK_Mode is

   function Char_To_Int (C : Character) return Int64 is
     (case C is
         when '0'    => 0,
         when '1'    => 1,
         when '2'    => 2,
         when '3'    => 3,
         when '4'    => 4,
         when '5'    => 5,
         when '6'    => 6,
         when '7'    => 7,
         when '8'    => 8,
         when '9'    => 9,
         when others => raise Program_Error)
   with Pre => C in '0' .. '9';
   subtype Digit is Int64 range 0 .. 9;
   function Int_To_Char (V : Digit) return Character is
     (case V is
         when 0 => '0',
         when 1 => '1',
         when 2 => '2',
         when 3 => '3',
         when 4 => '4',
         when 5 => '5',
         when 6 => '6',
         when 7 => '7',
         when 8 => '8',
         when 9 => '9');

   procedure Parse_Int64 (S : String; V : out Int64; Error : out Boolean) is
      Is_Pos : constant Boolean := S'Length = 0 or else S (S'First) /= '-';
      FirstZ : constant Integer := (if Is_Pos then S'First else S'First + 1);
      First  : Integer;
   begin
      V := 0;
      Error := True;
      if FirstZ > S'Last then
         return;
      end if;

      First :=
        (if FirstZ > S'Last or else S'Last - FirstZ <= 18
         then FirstZ else S'Last - 18);

      for I in FirstZ .. First - 1 loop
         if S (I) /= '0' then
            return;
         end if;
         pragma Loop_Invariant (for all K in FirstZ .. I => S (K) = '0');
      end loop;

      for I in 1 .. 19 loop
         if S (I - 1 + First) not in '0' .. '9' then
            return;
         end if;
         if I = 19 and then
            (abs (V) > Int64'Last / 10
             or else (abs (V) = Int64'Last / 10 and then S (I - 1 + First) > (if Is_Pos then '7' else '8')))
         then
            return;
         end if;

         V := V * 10 + (if Is_Pos then 1 else -1) * Char_To_Int (S (I - 1 + First));

         if I - 1 + First = S'Last then
            Error := False;
            return;
         end if;
      end loop;
   end Parse_Int64;

   function Print_Int64 (V : Int64) return String is
      res : String (1 .. 20) := (others => '0');
      X   : Int64 := V;
      top : Natural := 20;
   begin
      if V = 0 then
         return "0";
      end if;

      for I in 1 .. 19 loop
         res (top) := Int_To_Char (abs (X rem 10));
         top := top - 1;
         X := X / 10;
         exit when X = 0;
      end loop;

      if V < 0 then
         res (top) := '-';
         top := top - 1;
      end if;

      return (res (top + 1 .. 20));
   end Print_Int64;
end Int64_Parsing;
