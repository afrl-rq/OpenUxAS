with Ada.Text_IO; use Ada.Text_IO;
with Plan_Builder; use Plan_Builder;

package State_Serializer with SPARK_Mode is

   -- Procedure to write the full content of Plan_Builder_State to a file
   procedure Write_State_To_File
     (State     : Plan_Builder_State;
      File_Name : String);

end State_Serializer;
