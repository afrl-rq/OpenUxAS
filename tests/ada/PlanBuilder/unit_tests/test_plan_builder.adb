--
--  Copyright (C) 2024, AdaCore
--
with AUnit.Reporter.Text;
with AUnit.Run;
with Plan_Builder_Suite; use Plan_Builder_Suite;

procedure Test_Plan_Builder is
   procedure Runner is new AUnit.Run.Test_Runner (Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   Runner (Reporter);
end Test_Plan_Builder;