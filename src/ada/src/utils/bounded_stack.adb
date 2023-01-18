package body Bounded_Stack with SPARK_Mode is

   procedure Push (S : in out Stack; E : Element_Type) is
   begin
      Append (S.Elements, E);
   end Push;

   procedure Pop (S : in out Stack; E : out Element_Type) is
   begin
      E := Last_Element (S.Elements);
      Delete_Last (S.Elements);
   end Pop;

end Bounded_Stack;
