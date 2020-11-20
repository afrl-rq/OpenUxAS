with DOM.Core;
with AFRL.CMASI.AutomationResponse; use AFRL.CMASI.AutomationResponse;

with Common_Formal_Containers; use Common_Formal_Containers;

package UxAS.Comms.LMCP_Net_Client.Service.Example_Spark_Service is

   type Example_Spark_Service is new Service_Base with private;
   type Example_Spark_Service_Ref is access all Example_Spark_Service;
   
   Type_Name : constant String := "ExampleSparkService";
   
   Directory_Name : constant String := "";
   
   function Registry_Service_Type_Names return Service_Type_Names_List;

   function Create return Any_Service;
   
private
   
   type Configuration_Data is record
      AutomationIds : Int64_Set;
   end record;
   
   type Example_Spark_Service is new Service_Base with record
      Configs : Configuration_Data;
   end record;
   
   procedure Handle_AutomationResponse_Msg
     (This     : in out Example_Spark_Service;
      Response : Object_Any);
   
   procedure Handle_MissionCommand_Msg
     (This    : in out Example_Spark_Service;
      Command : Object_Any);
   
   overriding
   procedure Process_Received_LMCP_Message
     (This             : in out Example_Spark_Service;
      Received_Message : not null Any_LMCP_Message;
      Should_Terminate : out Boolean);
   
   overriding
   procedure Configure
     (This     : in out Example_Spark_Service;
      XML_Node : DOM.Core.Element;
      Result   : out Boolean);
   
   overriding
   procedure Initialize
     (This   : in out Example_Spark_Service;
      Result : out Boolean);
   
   procedure Construct 
     (This : in out Example_Spark_Service);

end UxAS.Comms.LMCP_Net_Client.Service.Example_Spark_Service;
