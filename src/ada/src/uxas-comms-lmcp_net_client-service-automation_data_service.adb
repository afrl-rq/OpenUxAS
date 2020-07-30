with Ada.Text_IO; use Ada.Text_IO;

with AVTAS.Lmcp.Object.SPARK_Boundary;              use AVTAS.Lmcp.Object.SPARK_Boundary;
with AFRl.CMASI.AutomationResponse; use AFRl.CMASI.AutomationResponse;
with AFRL.cmasi.AutomationResponse.SPARK_Boundary; use AFRL.cmasi.AutomationResponse.SPARK_Boundary;
with AFRL.CMASI.MissionCommand; use AFRL.CMASI.MissionCommand;


with UxAS.Comms.LMCP_Net_Client.Service.Automation_Data_Service.SPARK;

package body uxas.comms.lmcp_net_client.service.Automation_Data_Service is
   
   
   procedure Handle_AutomationResponse_Msg
     (This     : in out Automation_Data_Service;
      Response : Object_Any)
   is
   begin
      This.Configs.AutomationIds :=
         Int64_Sets.Union(This.Configs.AutomationIds,
             Get_WaypointEntitySet(AutomationResponse(Response.all)));
   end;
   
   procedure Handle_MissionCommand_Msg
     (This    : in out Automation_Data_Service;
      Command : Object_Any)
   is
      Result : Boolean;
   begin
      SPARK.Handle_MissionCommand(This, Wrap(Command), Result);
   end;
   
   overriding
   procedure Process_Received_LMCP_Message
     (This             : in out Automation_Data_Service;
      Received_Message : not null Any_LMCP_Message;
      Should_Terminate : out Boolean)
   is
   begin
      if Received_Message.Payload.all in AutomationResponse'Class then
         This.Handle_AutomationResponse_Msg (Received_Message.Payload);
      end if;
         
      if Received_Message.Payload.all in MissionCommand'Class then
         This.Handle_MissionCommand_Msg (Received_Message.Payload);
         
      end if;

      Should_Terminate := False;
      end Process_Received_LMCP_Message;
      
      function Registry_Service_Type_Names return Service_Type_Names_List is
        (Service_Type_Names_List'(1 => Instance (Service_Type_Name_Max_Length, Content => Type_Name)));

   
   function Create return Any_Service is
      Result : Automation_Data_Service_Ref;
   begin
      Result := new Automation_Data_Service;
      Result.Construct; -- Specific to Ada version
      return Any_Service (Result);
   end Create;
   
   
   overriding
   procedure Configure
     (This     : in out Automation_Data_Service;
      XML_Node : DOM.Core.Element;
      Result   : out Boolean) 
   is
      pragma Unreferenced (XML_Node);
      Unused : Boolean;
   begin

      This.Add_Subscription_Address (AFRL.CMASI.AutomationResponse.Subscription, Unused);
      This.Add_Subscription_Address (AFRL.CMASI.MissionCommand.Subscription, Unused);
      
      Result := True;
   end;
   
   
   overriding
   procedure Initialize
     (This   : in out Automation_Data_Service;
      Result : out Boolean)
   is
      pragma Unreferenced (This); -- since not doing the Timers
   begin
      Result := True;
   end Initialize;
   
   procedure Construct
     (This : in out Automation_Data_Service)
   is
   begin
      This.Construct_Service
        (Service_Type        => Type_Name,
         Work_Directory_Name => Directory_Name);
   end Construct;
   
   -----------------------------
   -- Package Executable Part --
   -----------------------------

   --  This is the executable part for the package, invoked automatically and only once.
begin
   --  All concrete service subclasses must call this procedure in their
   --  own package like this, with their own params.
   Register_Service_Creation_Function_Pointers (Registry_Service_Type_Names, Create'Access);

end uxas.comms.lmcp_net_client.service.Automation_Data_Service;
