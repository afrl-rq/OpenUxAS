with Common_Formal_Containers; use Common_Formal_Containers;

package afrl.cmasi.AutomationResponse.SPARK_Boundary with SPARK_Mode is
   pragma Annotate (GNATprove, Terminating, SPARK_Boundary);

   function Get_WaypointEntitySet
     (Response : AutomationResponse) return Int64_Set
     with Global => null;

end afrl.cmasi.AutomationResponse.SPARK_Boundary;
