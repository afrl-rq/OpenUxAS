--
--  Copyright (C) 2008, AdaCore
--
with Plan_Builder.Tests;         use Plan_Builder.Tests;
with AUnit.Test_Caller;

package body Plan_Builder_Suite is

   package Caller is new AUnit.Test_Caller (Plan_Builder.Tests.Test);

   function Suite return Access_Test_Suite is
      Ret : constant Access_Test_Suite := AUnit.Test_Suites.New_Suite;
   begin
      Ret.Add_Test
        (Caller.Create ("Test Process Task Implementation Response",
        Test_Process_Task_Implementation_Response'Access));
      Ret.Add_Test
        (Caller.Create ("Test Send Next Task Implementation Request",
        Test_Send_Next_Task_Implementation_Request'Access));
      Ret.Add_Test
        (Caller.Create ("Test Process Task Assignment Summary",
        Test_Process_Task_Assignment_Summary'Access));
      Ret.Add_Test
        (Caller.Create ("Test Check Next Task Implementation Request",
        Test_Check_Next_Task_Implementation_Request'Access));
      Ret.Add_Test
        (Caller.Create ("Test Add Loiters To Mission Commands",
        Test_Add_Loiters_To_Mission_Commands'Access));
      return Ret;
   end Suite;

end Plan_Builder_Suite;