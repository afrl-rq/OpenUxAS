package body Bounded_Stack with SPARK_Mode is

   procedure Push (S : in out Stack; E : Element_Type) is
   begin
      S.Content (S.Top + 1) := E;
      S.Top := S.Top + 1;
   end Push;

   procedure Pop (S : in out Stack; E : out Element_Type) is
   begin
      E := S.Content (S.Top);
      S.Top := S.Top - 1;
   end Pop;

end Bounded_Stack;
