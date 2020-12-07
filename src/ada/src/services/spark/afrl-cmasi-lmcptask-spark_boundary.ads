with AFRL.impact.AngledAreaSearchTask;
with AFRL.impact.ImpactLineSearchTask;
with AFRL.impact.ImpactPointSearchTask;

package AFRL.CMASI.lmcptask.SPARK_Boundary with SPARK_Mode is
   pragma Annotate (GNATprove, Terminating, SPARK_Boundary);
   --  This package introduces a private type hiding an access to a
   --  lmcptask.

   type Task_Kind is (AngledAreaSearchTask, ImpactLineSearchTask, ImpactPointSearchTask, Other_Task);

   type Task_Kind_And_Id (Kind : Task_Kind := Other_Task) is record
      case Kind is
      when AngledAreaSearchTask =>
         SearchAreaID : Int64;
      when ImpactLineSearchTask =>
         LineID : Int64;
      when ImpactPointSearchTask =>
         SearchLocationID : Int64;
      when Other_Task =>
         null;
      end case;
   end record;

   function Get_Kind_And_Id (X : lmcptask_Any) return Task_Kind_And_Id with
     Global => null,
     SPARK_Mode => Off;

private
   pragma SPARK_Mode (Off);

   function Get_Kind_And_Id (X : lmcptask_Any) return Task_Kind_And_Id is
     (if X.getLMCPTypeName = AFRL.impact.AngledAreaSearchTask.Subscription then
        (Kind         => AngledAreaSearchTask,
         SearchAreaID => AFRL.impact.AngledAreaSearchTask.AngledAreaSearchTask (X.all).getSearchAreaID)
      elsif X.getLMCPTypeName = AFRL.impact.ImpactLineSearchTask.Subscription then
        (Kind   => ImpactLineSearchTask,
         LineID => AFRL.impact.ImpactLineSearchTask.ImpactLineSearchTask (X.all).getLineID)
      elsif X.getLMCPTypeName = AFRL.impact.ImpactPointSearchTask.Subscription then
        (Kind             => ImpactPointSearchTask,
         SearchLocationID => AFRL.impact.ImpactPointSearchTask.ImpactPointSearchTask (X.all).getSearchLocationID)
      else (Kind => Other_Task));

end AFRL.CMASI.lmcptask.SPARK_Boundary;
