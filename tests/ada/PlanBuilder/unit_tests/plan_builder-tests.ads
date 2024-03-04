--
--  Copyright (C) 2024, AdaCore
--
with AUnit.Test_Fixtures;

package Plan_Builder.Tests is

   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   procedure Test_Process_Task_Assignment_Summary (T : in out Test);

   procedure Test_Process_Task_Implementation_Response_Vehicle_Exists_WPList_Empty_Test (T : in out Test);

   procedure Test_Process_Task_Implementation_Response_Vehicle_Exists_WPList_NotEmpty (T : in out Test);

   procedure Test_Process_Task_Implementation_Response_Vehicle_DoesNotExists (T : in out Test);

   procedure Test_Send_Next_Task_Implementation_Request (T : in out Test);

   procedure Test_Check_Next_Task_Implementation_Request (T : in out Test);

   procedure Test_Add_Loiters_To_Mission_Commands_Does_Not_Contain_Loiters_Hover (T : in out Test);

   procedure Test_Add_Loiters_To_Mission_Commands_Does_Not_Contain_Loiters_Circular (T : in out Test);

   procedure Test_Add_Loiters_To_Mission_Commands_Contains_Loiters (T : in out Test);

   procedure Test_Add_Loiters_To_Mission_Commands_Multiple_Vehicles (T : in out Test);

end Plan_Builder.Tests;
