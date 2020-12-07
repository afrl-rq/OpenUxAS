with Common_Formal_Containers; use Common_Formal_Containers;

package AFRL.CMASI.AutomationResponse.SPARK_Boundary with SPARK_Mode is
   pragma Annotate (GNATprove, Terminating, SPARK_Boundary);

   function Get_WaypointEntity_Set
     (Response : AutomationResponse) return Int64_Set
     with Global => null;

end AFRL.CMASI.AutomationResponse.SPARK_Boundary;
