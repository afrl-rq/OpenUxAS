with DOM.Core;

with Route_Aggregator;                               use Route_Aggregator;
with Route_Aggregator_Communication;                 use Route_Aggregator_Communication;
with Common;                                         use Common;

with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;

with AVTAS.LMCP.Types;
with AFRL.CMASI.EntityState;                         use AFRL.CMASI.EntityState;
with Afrl.Cmasi.EntityConfiguration;                 use Afrl.Cmasi.EntityConfiguration;
with Uxas.Messages.Lmcptask.UniqueAutomationRequest; use Uxas.Messages.Lmcptask.UniqueAutomationRequest;
with UxAS.Messages.Lmcptask.TaskPlanOptions;         use UxAS.Messages.Lmcptask.TaskPlanOptions;

package UxAS.Comms.LMCP_Net_Client.Service.Route_Aggregation is

   type Route_Aggregator_Service is new Service_Base with private;

   Type_Name : constant String := "RouteAggregatorService";

   Directory_Name : constant String := "";

   --  static const std::vector<std::string>
   --  s_registryServiceTypeNames()
   function Registry_Service_Type_Names return Service_Type_Names_List;

   --  static ServiceBase*
   --  create()
   function Create return Any_Service;

private

   type Route_Aggregator_Service is new Service_Base with record

      --  the following types are defined in SPARK code
      Mailbox : Route_Aggregator_Mailbox;
      State   : Route_Aggregator_State;
      Config  : Route_Aggregator_Configuration_Data;
   end record;

   overriding
   procedure Configure
     (This     : in out Route_Aggregator_Service;
      XML_Node : DOM.Core.Element;
      Result   : out Boolean);

   overriding
   procedure Initialize
     (This   : in out Route_Aggregator_Service;
      Result : out Boolean);

   overriding
   procedure Process_Received_LMCP_Message
     (This             : in out Route_Aggregator_Service;
      Received_Message : not null Any_LMCP_Message;
      Should_Terminate : out Boolean);

end UxAS.Comms.LMCP_Net_Client.Service.Route_Aggregation;
