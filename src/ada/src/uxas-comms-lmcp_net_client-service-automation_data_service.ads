with DOM.Core;
with AFRL.CMASI.AutomationResponse; use AFRL.CMASI.AutomationResponse;

with Common_Formal_Containers; use Common_Formal_Containers;

package uxas.comms.lmcp_net_client.service.Automation_Data_Service is

   type Automation_Data_Service is new Service_Base with private;
   type Automation_Data_Service_Ref is access all Automation_Data_Service;
   
   Type_Name : constant String := "AutomationDataService";
   
   Directory_Name : constant String := "";
   
   function Registry_Service_Type_Names return Service_Type_Names_List;

   function Create return Any_Service;
   
private
   
   type Configuration_Data is record
      AutomationIds : Int64_Set;
   end record;
   
   type Automation_Data_Service is new Service_Base with record
      Configs : Configuration_Data;
   end record;
   
   procedure Handle_AutomationResponse_Msg
     (This     : in out Automation_Data_Service;
      Response : Object_Any);
   
   procedure Handle_MissionCommand_Msg
     (This    : in out Automation_Data_Service;
      Command : Object_Any);
   
   overriding
   procedure Process_Received_LMCP_Message
     (This             : in out Automation_Data_Service;
      Received_Message : not null Any_LMCP_Message;
      Should_Terminate : out Boolean);
   
   overriding
   procedure Configure
     (This     : in out Automation_Data_Service;
      XML_Node : DOM.Core.Element;
      Result   : out Boolean);
   
   overriding
   procedure Initialize
     (This   : in out Automation_Data_Service;
      Result : out Boolean);
   
   procedure Construct 
     (This : in out Automation_Data_Service);


end uxas.comms.lmcp_net_client.service.Automation_Data_Service;
