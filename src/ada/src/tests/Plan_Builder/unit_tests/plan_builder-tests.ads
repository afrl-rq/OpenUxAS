--
--  Copyright (C) 2024, AdaCore
--
with AUnit.Test_Fixtures;

package Plan_Builder.Tests is

   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   procedure Test_Process_Task_Implementation_Response (T : in out Test);

   procedure Test_Send_Next_Task_Implementation_Request (T : in out Test);

   procedure Test_Process_Task_Assignment_Summary (T : in out Test);

   procedure Test_Check_Next_Task_Implementation_Request (T : in out Test);

   procedure Test_Add_Loiters_To_Mission_Commands (T : in out Test);

end Plan_Builder.Tests;
