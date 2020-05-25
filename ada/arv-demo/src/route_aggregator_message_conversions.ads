with Route_Aggregator_Messages;

with Afrl.Cmasi.Waypoint;                   use Afrl.Cmasi.Waypoint;
with Uxas.Messages.Route.RoutePlan;         use Uxas.Messages.Route.RoutePlan;
with Uxas.Messages.Route.RoutePlanResponse; use Uxas.Messages.Route.RoutePlanResponse;
with Afrl.Cmasi.VehicleAction;              use Afrl.Cmasi.VehicleAction;
with Afrl.Cmasi.KeyValuePair;               use Afrl.Cmasi.KeyValuePair;
with Afrl.Cmasi.Location3D;                 use Afrl.Cmasi.Location3D;
with Afrl.Cmasi.EntityState;                use Afrl.Cmasi.EntityState;
with Uxas.Messages.Lmcptask.UniqueAutomationRequest; use Uxas.Messages.Lmcptask.UniqueAutomationRequest;
with Uxas.Messages.Route.RouteRequest;      use Uxas.Messages.Route.RouteRequest;
with Uxas.Messages.Route.RoutePlanRequest;  use Uxas.Messages.Route.RoutePlanRequest;
with Uxas.Messages.Route.RouteConstraints;  use Uxas.Messages.Route.RouteConstraints;
with Uxas.Messages.lmcptask.TaskPlanOptions; use Uxas.Messages.lmcptask.TaskPlanOptions;
with AVTAS.LMCP.Object;

package Route_Aggregator_Message_Conversions is

   function As_RouteConstraints_Message (Msg : not null RouteConstraints_Any) return Route_Aggregator_Messages.RouteConstraints;

   function As_Location3D_Message (Msg : not null Location3D_Any) return Route_Aggregator_Messages.Location3D;

   function As_VehicleAction_Message (Msg : not null VehicleAction_Any) return Route_Aggregator_Messages.VehicleAction;

   function As_KeyValuePair_Message (Msg : not null KeyValuePair_Acc) return Route_Aggregator_Messages.KeyValuePair;

   function As_Waypoint_Message (Msg : not null Waypoint_Any) return Route_Aggregator_Messages.Waypoint;

   function As_RoutePlan_Message (Msg : not null RoutePlan_Any) return Route_Aggregator_Messages.RoutePlan;

   function As_RoutePlanResponse_Message (Msg : not null RoutePlanResponse_Any) return Route_Aggregator_Messages.RoutePlanResponse;

   function As_RouteRequest_Message (Msg : not null RouteRequest_Any) return Route_Aggregator_Messages.RouteRequest;

   function As_RoutePlanRequest_Message (Msg : not null RoutePlanRequest_Any) return Route_Aggregator_Messages.RoutePlanRequest;

   function As_UniqueAutomationRequest_Message (Msg : not null UniqueAutomationRequest_Any) return Route_Aggregator_Messages.UniqueAutomationRequest;

   function As_TaskPlanOption_Message (Msg : not null TaskPlanOptions_Any) return Route_Aggregator_Messages.TaskPlanOptions;

   function As_EntityState_Message (Msg : not null EntityState_Any) return Route_Aggregator_Messages.EntityState;

   function As_Object_Any (Msg : Route_Aggregator_Messages.Message_Root'Class) return AVTAS.LMCP.Object.Object_Any;

end Route_Aggregator_Message_Conversions;
