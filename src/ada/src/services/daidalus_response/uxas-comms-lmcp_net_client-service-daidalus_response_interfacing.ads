with DOM.Core;

with Daidalus_Response;               use Daidalus_Response;
with Daidalus_Response_Mailboxes; use Daidalus_Response_Mailboxes;

-- __TODO__
-- Include any additional necessary packages.

with Common; use Common;

package UxAS.Comms.LMCP_Net_Client.Service.Daidalus_Response_Interfacing is

   type Daidalus_Response_Service is new Service_Base with private;

   type Daidalus_Response_Service_Ref is access all Daidalus_Response_Service;

   Type_Name : constant String := "DAIDALUSResponseService";

   Directory_Name : constant String := "";

   function Registry_Service_Type_Names return Service_Type_Names_List;

   function Create return Any_Service;

private

   type Daidalus_Response_Service is new Service_Base with record
      -- __TODO__
      -- Include any additional fields needed by the service that are not part
      -- of its state or configuration. Often there will not be any, but you
      -- might have some for hard-coded service-specific values, e.g. for timers
      -- as in the Waypoint_Plan_Manager or Automation_Request_Validator.
      --
      -- __Example__
      -- Timer        : Common.Int64 := 0;
      -- Time_Elapsed : Boolean := False;
      -- Min_Time_Between_Commands_ms : Common.Int64 := 1000;
      Config  : Daidalus_Response_Configuration_Data;
      Mailbox : Daidalus_Response_Mailbox;
      State   : Daidalus_Response_State;
   end record;

   overriding
   procedure Configure
     (This     : in out Daidalus_Response_Service;
      XML_Node : DOM.Core.Element;
      Result   : out Boolean);

   overriding
   procedure Initialize
     (This   : in out Daidalus_Response_Service;
      Result : out Boolean);

   overriding
   procedure Process_Received_LMCP_Message
     (This             : in out Daidalus_Response_Service;
      Received_Message : not null Any_LMCP_Message;
      Should_Terminate : out Boolean);

end UxAS.Comms.LMCP_Net_Client.Service.Daidalus_Response_Interfacing;
