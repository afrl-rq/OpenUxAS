with Ada.Characters.Handling;
with DOM.Core.Elements;
with LMCP_Messages;
with LMCP_Message_Conversions;

with AFRL.CMASI.AirVehicleConfiguration;             use AFRL.CMASI.AirVehicleConfiguration;
with AFRL.CMASI.AirVehicleState;                     use AFRL.CMASI.AirVehicleState;
with AFRL.CMASI.AutomationRequest;                   use AFRL.CMASI.AutomationRequest;
with AFRL.Impact.ImpactAutomationRequest;            use AFRL.Impact.ImpactAutomationRequest;
with AFRL.Vehicles.GroundVehicleConfiguration;       use AFRL.Vehicles.GroundVehicleConfiguration;
with AFRL.Vehicles.GroundVehicleState;               use AFRL.Vehicles.GroundVehicleState;
with AFRL.Vehicles.SurfaceVehicleConfiguration;      use AFRL.Vehicles.SurfaceVehicleConfiguration;
with AFRL.Vehicles.SurfaceVehicleState;              use AFRL.Vehicles.SurfaceVehicleState;
with UxAS.Messages.Route.RoutePlanResponse;          use UxAS.Messages.Route.RoutePlanResponse;
with UxAS.Messages.Route.RouteRequest;               use UxAS.Messages.Route.RouteRequest;
with AFRL.CMASI.EntityConfiguration;                 use AFRL.CMASI.EntityConfiguration;
with AFRL.CMASI.EntityState;                         use AFRL.CMASI.EntityState;
with AVTAS.LMCP.Types;
with UxAS.Messages.lmcptask.TaskPlanOptions;         use UxAS.Messages.lmcptask.TaskPlanOptions;
with UxAS.Messages.lmcptask.UniqueAutomationRequest; use UxAS.Messages.lmcptask.UniqueAutomationRequest;
with Ada.Containers; use Ada.Containers;

with DOM.Core;

with Common;                                         use Common;
with Plan_Builder;                               use Plan_Builder;
with Plan_Builder_Communication;                 use Plan_Builder_Communication;

package UxAS.Comms.LMCP_Net_Client.Service.Plan_Builder is

   type Plan_Builder_Service is new Service_Base with private;

   Type_Name : constant String := "PlanBuilderService";

   Directory_Name : constant String := "";

   --  static const std::vector<std::string>
   --  s_registryServiceTypeNames()
   function Registry_Service_Type_Names return Service_Type_Names_List;

   --  static ServiceBase*
   --  create()
   function Create return Any_Service;

private

   type Plan_Builder_Service is new Service_Base with record

      --  the following types are defined in SPARK code
      Mailbox : Plan_Builder_Mailbox;
      State   : Plan_Builder_State;
      Config  : Plan_Builder_Configuration_Data;
   end record;

   overriding
   procedure Configure
     (This     : in out Plan_Builder_Service;
      XML_Node : DOM.Core.Element;
      Result   : out Boolean);

   overriding
   procedure Initialize
     (This   : in out Plan_Builder_Service;
      Result : out Boolean);

   overriding
   procedure Process_Received_LMCP_Message
     (This             : in out Plan_Builder_Service;
      Received_Message : not null Any_LMCP_Message;
      Should_Terminate : out Boolean);

end UxAS.Comms.LMCP_Net_Client.Service.Plan_Builder;
