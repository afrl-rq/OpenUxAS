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
        (Caller.Create ("Test Process Task Assignment Summary",
        Test_Process_Task_Assignment_Summary'Access));
      Ret.Add_Test
        (Caller.Create ("Test Process Task Implementation Response 1",
        Test_Process_Task_Implementation_Response_Vehicle_Exists_WPList_Empty_Test'Access));
      Ret.Add_Test
        (Caller.Create ("Test Process Task Implementation Response 2",
        Test_Process_Task_Implementation_Response_Vehicle_Exists_WPList_NotEmpty'Access));
      Ret.Add_Test
        (Caller.Create ("Test Process Task Implementation Response 3",
        Test_Process_Task_Implementation_Response_Vehicle_DoesNotExists'Access));
      Ret.Add_Test
        (Caller.Create ("Test Send Next Task Implementation Request",
        Test_Send_Next_Task_Implementation_Request'Access));
      Ret.Add_Test
        (Caller.Create ("Test Check Next Task Implementation Request",
        Test_Check_Next_Task_Implementation_Request'Access));
      Ret.Add_Test
        (Caller.Create ("Test Add Loiters To Mission Commands 1",
        Test_Add_Loiters_To_Mission_Commands_Does_Not_Contain_Loiters_Hover'Access));
      Ret.Add_Test
        (Caller.Create ("Test Add Loiters To Mission Commands 2",
        Test_Add_Loiters_To_Mission_Commands_Does_Not_Contain_Loiters_Circular'Access));
      Ret.Add_Test
        (Caller.Create ("Test Add Loiters To Mission Commands 3",
        Test_Add_Loiters_To_Mission_Commands_Contains_Loiters'Access));
      Ret.Add_Test
        (Caller.Create ("Test Add Loiters To Mission Commands 4",
        Test_Add_Loiters_To_Mission_Commands_Multiple_Vehicles'Access));
      return Ret;
   end Suite;

end Plan_Builder_Suite;