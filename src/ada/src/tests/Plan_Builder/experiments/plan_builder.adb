with Ada.Text_IO; use Ada.Text_IO;

package body Plan_Builder is

   procedure Initialize is
   begin
      --  Add initialization code here
      Put_Line ("Plan_Builder initialized");
   end Initialize;

   procedure Finalize is
   begin
      --  Add cleanup code here
      Put_Line ("Plan_Builder finalized");
   end Finalize;
end Plan_Builder;