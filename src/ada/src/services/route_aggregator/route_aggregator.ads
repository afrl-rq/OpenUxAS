with SPARK.Containers.Formal.Unbounded_Hashed_Maps;
with SPARK.Containers.Formal.Unbounded_Hashed_Sets;
with SPARK.Containers.Formal.Unbounded_Ordered_Maps;
with SPARK.Containers.Functional.Maps;
with SPARK.Containers.Functional.Vectors;
with SPARK.Containers.Functional.Infinite_Sequences;
with SPARK.Containers.Types;               use SPARK.Containers.Types;
with SPARK.Big_Integers; use SPARK.Big_Integers;

with Common;                         use Common;
with LMCP_Messages;                  use LMCP_Messages;
with Route_Aggregator_Communication; use Route_Aggregator_Communication;

package Route_Aggregator with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);

   pragma Assertion_Policy (Ignore);

   --  Configuration data is separated from the service state as it is not
   --  handled by the same primitives. We use functional containers, as it is
   --  not supposed to be modified often.

   package ES_Maps is new SPARK.Containers.Functional.Maps
     (Key_Type     => Int64,
      Element_Type => EntityState);
   use ES_Maps;
   subtype EntityState_Map is ES_Maps.Map
     with Predicate =>
       (for all Id of EntityState_Map => (Id = Get (EntityState_Map, Id).Id));

   type Route_Aggregator_Configuration_Data is record
      m_entityStates     : Int64_Seq;
      m_entityStatesInfo : EntityState_Map;
      m_airVehicles      : Int64_Set;
      m_groundVehicles   : Int64_Set;
      m_surfaceVehicles  : Int64_Set;
      m_fastPlan         : Boolean;
   end record
     with Predicate =>
       (for all Id of m_entityStates => Contains (m_airVehicles, Id)
        or else Contains (m_groundVehicles, Id)
        or else Contains (m_surfaceVehicles, Id))
       and then
       (for all Id of m_entityStatesInfo =>
          (Contains (m_entityStates, Int64_Sequences.First, Last (m_entityStates), Id)));

   package Int64_Formal_Sets is new SPARK.Containers.Formal.Unbounded_Hashed_Sets
     (Element_Type => Int64,
      Hash         => Int64_Hash);
   use Int64_Formal_Sets;
   use Int64_Formal_Sets.Formal_Model;
   package Int_Set_P renames Int64_Formal_Sets.Formal_Model.P;
   package Int_Set_E renames Int64_Formal_Sets.Formal_Model.E;
   package Int_Set_M renames Int64_Formal_Sets.Formal_Model.M;

   subtype Int64_Formal_Set is Int64_Formal_Sets.Set
     (Int64_Formal_Sets.Default_Modulus (200));

   --  Use ordered maps so that we can modify the container during iteration

   package Int64_Formal_Set_Maps is new SPARK.Containers.Formal.Unbounded_Ordered_Maps
     (Key_Type     => Int64,
      Element_Type => Int64_Formal_Set);
   use Int64_Formal_Set_Maps;
   use Int64_Formal_Set_Maps.Formal_Model;
   package Int_Set_Maps_P renames Int64_Formal_Set_Maps.Formal_Model.P;
   package Int_Set_Maps_K renames Int64_Formal_Set_Maps.Formal_Model.K;
   package Int_Set_Maps_M is new SPARK.Containers.Functional.Maps
     (Int64, Int64_Set);
   use type Int_Set_Maps_M.Map;

   subtype Int64_Formal_Set_Map is Int64_Formal_Set_Maps.Map;

   function Same_Mappings
     (M : Int64_Formal_Set_Maps.Formal_Model.M.Map;
      N : Int_Set_Maps_M.Map)
      return Boolean
   with Ghost,
        Annotate => (GNATprove, Inline_For_Proof);
   --  The two structures contain the same mappings

   function Model (M : Int64_Formal_Set_Map) return Int_Set_Maps_M.Map with
     Post => Same_Mappings
       (Int64_Formal_Set_Maps.Formal_Model.Model (M), Model'Result);
   --  Redefine the model of a map of formal sets to be a map of functional
   --  sets to ease formal verification.
   --  Model cannot be ghost as it is used in a type predicate.

   package Int64_RouteResponse_Maps is new SPARK.Containers.Formal.Unbounded_Hashed_Maps
     (Key_Type     => Int64,
      Element_Type => RoutePlanResponse,
      Hash         => Int64_Hash);
   use Int64_RouteResponse_Maps;
   use Int64_RouteResponse_Maps.Formal_Model;
   package RR_Maps_M renames Int64_RouteResponse_Maps.Formal_Model.M;

   subtype Int64_RouteResponse_Map is Int64_RouteResponse_Maps.Map
     (Int64_RouteResponse_Maps.Default_Modulus (200))
   with Predicate =>
         (for all K of Int64_RouteResponse_Map =>
            Element (Int64_RouteResponse_Map, K).ResponseID = K);

   type IdPlanPair is record
      Id   : Int64;
      Plan : RoutePlan;
      Cost : Int64 := -1;
   end record;

   package Int64_IdPlanPair_Maps is new SPARK.Containers.Formal.Unbounded_Hashed_Maps
     (Key_Type     => Int64,
      Element_Type => IdPlanPair,
      Hash         => Int64_Hash);
   use Int64_IdPlanPair_Maps;
   use Int64_IdPlanPair_Maps.Formal_Model;

   subtype Int64_IdPlanPair_Map is Int64_IdPlanPair_Maps.Map
     (Int64_IdPlanPair_Maps.Default_Modulus (200))
   with Predicate =>
         (for all K of Int64_IdPlanPair_Map =>
            Element (Int64_IdPlanPair_Map, K).Plan.RouteID = K);

   --  State of the service is modified more often, data can be removed as
   --  well has added. Use formal containers for efficiency.

   function No_Overlaps (pendingRoute : Int_Set_Maps_M.Map) return Boolean;

   function No_Overlaps (pendingRoute, pendingAutoReq : Int_Set_Maps_M.Map) return Boolean;

   function All_Pending_Requests_Seen
     (pendingRequest : Int_Set_Maps_M.Map;
      routeRequestId : Int64)
      return Boolean;

   package UAR_Maps is new SPARK.Containers.Formal.Unbounded_Ordered_Maps
     (Key_Type => Int64,
      Element_Type => UniqueAutomationRequest);
   use UAR_Maps;

   subtype Int64_UniqueAutomationRequest_Map is UAR_Maps.Map;

   type TaskOptionPair is record
      vehicleId      : Int64 := 0;
      prevTaskId     : Int64 := 0;
      prevTaskOption : Int64 := 0;
      taskId         : Int64 := 0;
      taskOption     : Int64 := 0;
   end record;

   package Int64_TaskOptionPair_Maps is new SPARK.Containers.Formal.Unbounded_Hashed_Maps
     (Key_Type => Int64,
      Element_Type => TaskOptionPair,
      Hash => Int64_Hash);
   use Int64_TaskOptionPair_Maps;

   subtype Int64_TaskOptionPair_Map is Int64_TaskOptionPair_Maps.Map (Int64_TaskOptionPair_Maps.Default_Modulus (200));

   package Int64_TaskPlanOptions_Maps is new SPARK.Containers.Formal.Unbounded_Hashed_Maps
     (Key_Type        => Int64,
      Element_Type    => TaskPlanOptions,
      Hash            => Int64_Hash);
   use Int64_TaskPlanOptions_Maps;

   subtype Int64_TaskPlanOptions_Map is Int64_TaskPlanOptions_Maps.Map (Int64_TaskOptionPair_Maps.Default_Modulus (200));

   package RPReq_Sequences is new SPARK.Containers.Functional.Vectors
        (Index_Type   => Positive,
         Element_Type => RoutePlanRequest);
   type RPReq_Seq is new RPReq_Sequences.Sequence;

   type Route_Aggregator_State is record
      --  Unique ID associated to a RoutePlanRequest
      m_routeRequestId           : Int64 := 1;
      --  Unique ID associated to a RouteConstraint in a RoutePlanRequest
      m_routeId                  : Int64 := 1000000;
      --  Unique ID associated to a UniqueAutomationRequest
      m_autoRequestId            : Int64 := 1;
      --  Set of route IDs that correspond to an original RouteRequest
      m_pendingRoute             : Int64_Formal_Set_Map;
      --  Received RoutePlanResponses
      m_routePlanResponses       : Int64_RouteResponse_Map;
      --  Individual RoutePlans contained in RoutePlanResponses
      m_routePlans               : Int64_IdPlanPair_Map;
      --  Set of route IDs that correspond to an original UniqueAutomationRequest
      m_pendingAutoReq           : Int64_Formal_Set_Map;
      --  Received UniqueAutomationRequests
      m_uniqueAutomationRequests : Int64_UniqueAutomationRequest_Map;
      --  Mapping from route ID to the corresponding task/option pair
      m_routeTaskPairing         : Int64_TaskOptionPair_Map;
      --  Mapping from task ID to associated TaskplanOptions
      m_taskOptions              : Int64_TaskPlanOptions_Map;
   end record
     with Predicate =>

     --  Pending routes plan requests are associated to a seen identifier

     All_Pending_Requests_Seen (Model (m_pendingRoute), m_routeRequestId)
     and All_Pending_Requests_Seen (Model (m_pendingAutoReq), m_routeRequestId)

     --  Pending routes plan requests are associated to one route request
     --  or one automation request only

     and No_Overlaps (Model (m_pendingRoute))
     and No_Overlaps (Model (m_pendingAutoReq))
     and No_Overlaps (Model (m_pendingRoute), Model (m_pendingAutoReq))

     --  Pending automation requests are associated to a received automation request

     and (for all ReqID of m_pendingAutoReq => Contains (m_uniqueAutomationRequests, ReqID));

   function Plan_To_Route (pendingRoute : Int64_Formal_Set_Map) return Int64_Map with
     Ghost,
     Pre  => No_Overlaps (Model (pendingRoute)),

     --  Map each plan request id to the corresponding route request

     Post => (for all I of Model (pendingRoute) =>
                 (for all K of Int_Set_Maps_M.Get (Model (pendingRoute), I) =>
                      Has_Key (Plan_To_Route'Result, K)
                  and then Get (Plan_To_Route'Result, K) = I))
     and then (for all I of Plan_To_Route'Result =>
                Int_Set_Maps_M.Has_Key (Model (pendingRoute), Get (Plan_To_Route'Result, I))
               and then Contains (Int_Set_Maps_M.Get (Model (pendingRoute), Get (Plan_To_Route'Result, I)), I));

   --  Property of State

   function All_Plans_Registered
     (routePlanResponses : Int64_RouteResponse_Map;
      routePlans         : Int64_IdPlanPair_Map)
      return Boolean
   with Ghost;
   --  All plans associated to pending route plan responses are registered

   function Only_Pending_Plans
     (routePlanResponses : Int64_RouteResponse_Map;
      routePlans         : Int64_IdPlanPair_Map)
      return Boolean
   with Ghost;
   --  All plans are associated to a pending route plan response.
   --  Plans are stored in the route responses of the associated plan response.

   function Valid_Plan_Responses
     (pendingRoute       : Int64_Formal_Set_Map;
      pendingAutoReq     : Int64_Formal_Set_Map;
      routePlanResponses : Int64_RouteResponse_Map)
      return Boolean
   with Ghost,
        Pre  => No_Overlaps (Model (pendingRoute)) and then No_Overlaps (Model (pendingAutoReq));
   --  We only have route plan responses associated to pending routes or
   --  pending automation requests.

   function Is_Pending
     (pendingRoute       : Int_Set_Maps_M.Map;
      routePlanResponses : RR_Maps_M.Map;
      Request_Id         : Int64)
      return Boolean
   with Ghost,
        Pre => Int_Set_Maps_M.Has_Key (pendingRoute, Request_Id);
   --  True iff we are still waiting for some route plan response for Id

   function No_Finished_Request
     (pendingRoute, pendingAutoReq : Int64_Formal_Set_Map;
      routePlanResponses           : Int64_RouteResponse_Map)
      return Boolean
   with Ghost;
   --  We only have pending requests in m_pendingRequest

   package Message_History with
     Ghost,
     Annotate => (GNATprove, Always_Return)
   is
      type Event_Kind is
        (Receive_RouteRequest, Send_PlanRequest, Receive_PlanResponse, Send_RouteResponse);

      type Event is record
         Kind : Event_Kind;
         Id   : Int64;
      end record;

      package Event_Sequences is new SPARK.Containers.Functional.Infinite_Sequences
        (Element_Type => Event);
      type History_Type is new Event_Sequences.Sequence;

      --  At the moment, History only stores message exchanges related to
      --  a received RouteRequest. In the future, this needs to be extended
      --  to AutomationRequests.

      History : History_Type;

      function Valid_Events (routeRequestId : Int64) return Boolean;
      --  All pln ids in history are smaller than routeRequestId

      function RouteResponse_Sent (Id : Int64) return Boolean;
      --  A route response was sent for this Id

      function PlanRequest_Sent (Id : Int64) return Boolean;
      --  A plan request was sent for this Id

      function No_RouteRequest_Lost (pendingRoute : Int64_Formal_Set_Map) return Boolean;
      --  All received route requests are either pending or answered

      function No_PlanResponse_Lost
        (pendingRoute       : Int64_Formal_Set_Map;
         routePlanResponses : Int64_RouteResponse_Map)
         return Boolean
      is
        (for all E of History =>
            (if E.Kind = Receive_PlanResponse
               and Has_Key (Plan_To_Route (pendingRoute), E.Id)
             then Contains (routePlanResponses, E.Id)))
       with Pre => No_Overlaps (Model (pendingRoute));
      --  All received plan responses are stored

      function PlanRequest_Processed
        (routePlanResponses : Int64_RouteResponse_Map;
         Id                 : Int64) return Boolean
      is
        (Contains (routePlanResponses, Id)
         or else PlanRequest_Sent (Id));

      function All_Pending_Plans_Sent
        (pendingRoute       : Int64_Formal_Set_Map;
         routePlanResponses : Int64_RouteResponse_Map)
         return Boolean
      is
        (for all Id of Plan_To_Route (pendingRoute) =>
              PlanRequest_Processed (routePlanResponses, Id))
       with Pre => No_Overlaps (Model (pendingRoute));
      --  All plan requests associated to pending route request are either
      --  answered or sent (they are not all sent as some might go through
      --  fast planning).

   private

      function Valid_Events (routeRequestId : Int64) return Boolean is
        (for all E of History =>
            (if E.Kind in Send_PlanRequest | Receive_PlanResponse then
                  E.Id <= routeRequestId));

      function RouteResponse_Sent (Id : Int64) return Boolean is
        (for some E of History =>
            E.Kind = Send_RouteResponse and then E.Id = Id);

      function PlanRequest_Sent (Id : Int64) return Boolean is
        (for some E of History =>
            E.Kind = Send_PlanRequest and then E.Id = Id);

      function No_RouteRequest_Lost (pendingRoute : Int64_Formal_Set_Map) return Boolean is
        (for all E of History =>
            (if E.Kind = Receive_RouteRequest then
                  Contains (pendingRoute, E.Id)
             or else RouteResponse_Sent (E.Id)));
   end Message_History;
   use Message_History;

   --  Service functionality

   procedure Euclidean_Plan
     (Data               : Route_Aggregator_Configuration_Data;
      routePlanResponses : in out Int64_RouteResponse_Map;
      routePlans         : in out Int64_IdPlanPair_Map;
      Request            : RoutePlanRequest)
   with
--       Import,
     Global => null,
     Pre  => not Contains (routePlanResponses, Request.RequestID)
             and All_Plans_Registered (routePlanResponses, routePlans)
             and Only_Pending_Plans (routePlanResponses, routePlans),
     Post => Contains (routePlanResponses, Request.RequestID)
             and Model (routePlanResponses)'Old <= Model (routePlanResponses)
             and RR_Maps_M.Keys_Included_Except (Model (routePlanResponses),
                                                 Model (routePlanResponses)'Old,
                                                 Request.RequestID)
             and All_Plans_Registered (routePlanResponses, routePlans)
             and Only_Pending_Plans (routePlanResponses, routePlans); --  Not true in C implementation!
   --  Stub, TBD

   procedure Handle_Route_Plan_Response
     (Mailbox  : in out Route_Aggregator_Mailbox;
      State    : in out Route_Aggregator_State;
      Response : RoutePlanResponse)
   with
     Pre =>

     --  The response is expected

     (Has_Key (Plan_To_Route (State.m_pendingRoute), Response.ResponseID)
      or Has_Key (Plan_To_Route (State.m_pendingAutoReq), Response.ResponseID))

     --  The response was not already received

     and not Contains (State.m_routePlanResponses, Response.ResponseID)

     --  Plans associated to Response are new

     and (for all Pl of Response.RouteResponses =>
            not Contains (State.m_routePlans, Pl.RouteID))

     --  General invariants

     and All_Plans_Registered (State.m_routePlanResponses, State.m_routePlans)
     and Only_Pending_Plans (State.m_routePlanResponses, State.m_routePlans)
     and Valid_Plan_Responses (State.m_pendingRoute, State.m_pendingAutoReq, State.m_routePlanResponses)
     and No_Finished_Request (State.m_pendingRoute, State.m_pendingAutoReq, State.m_routePlanResponses)

     --  History invariants

     and Valid_Events (State.m_routeRequestId)
     and No_RouteRequest_Lost (State.m_pendingRoute)
     and No_PlanResponse_Lost (State.m_pendingRoute, State.m_routePlanResponses)
     and All_Pending_Plans_Sent (State.m_pendingRoute, State.m_routePlanResponses),

     --  General invariants

     Post =>

     All_Plans_Registered (State.m_routePlanResponses, State.m_routePlans)
     and Only_Pending_Plans (State.m_routePlanResponses, State.m_routePlans)
     and Valid_Plan_Responses (State.m_pendingRoute, State.m_pendingAutoReq, State.m_routePlanResponses)
     and No_Finished_Request (State.m_pendingRoute, State.m_pendingAutoReq, State.m_routePlanResponses)

     --  The response has been added to the history

     and History'Old < History
     and Get (History, Last (History)'Old + 1).Kind = Receive_PlanResponse
     and Get (History, Last (History)'Old + 1).Id = Response.ResponseID

     --  History invariants

     and Valid_Events (State.m_routeRequestId)
     and No_RouteRequest_Lost (State.m_pendingRoute)
     and No_PlanResponse_Lost (State.m_pendingRoute, State.m_routePlanResponses)
     and All_Pending_Plans_Sent (State.m_pendingRoute, State.m_routePlanResponses);

   procedure Handle_Task_Plan_Options
     (Mailbox : in out Route_Aggregator_Mailbox;
      Data  : Route_Aggregator_Configuration_Data;
      State : in out Route_Aggregator_State;
      Options : TaskPlanOptions);

   procedure Handle_Route_Request
     (Data    : Route_Aggregator_Configuration_Data;
      Mailbox : in out Route_Aggregator_Mailbox;
      State   : in out Route_Aggregator_State;
      Request : RouteRequest)
   with

   --  We have at least an entity registed in the service

     Pre  => Length (Data.m_entityStates) /= 0

   --  The request id is fresh

     and not Contains (State.m_pendingRoute, Request.RequestID)

   --  The vehicules ids of the request are registered

     and (for all V of Request.VehicleID =>
            (for some V2 of Data.m_entityStates => V = V2))

     --  General invariants

     and All_Plans_Registered (State.m_routePlanResponses, State.m_routePlans)
     and Only_Pending_Plans (State.m_routePlanResponses, State.m_routePlans)
     and Valid_Plan_Responses (State.m_pendingRoute, State.m_pendingAutoReq, State.m_routePlanResponses)
     and No_Finished_Request (State.m_pendingRoute, State.m_pendingAutoReq, State.m_routePlanResponses)

     --  History invariants

     and Valid_Events (State.m_routeRequestId)
     and No_RouteRequest_Lost (State.m_pendingRoute)
     and No_PlanResponse_Lost (State.m_pendingRoute, State.m_routePlanResponses)
     and All_Pending_Plans_Sent (State.m_pendingRoute, State.m_routePlanResponses),

     --  General invariants

     Post => All_Plans_Registered (State.m_routePlanResponses, State.m_routePlans)
     and Only_Pending_Plans (State.m_routePlanResponses, State.m_routePlans)
     and Valid_Plan_Responses (State.m_pendingRoute, State.m_pendingAutoReq, State.m_routePlanResponses)
     and No_Finished_Request (State.m_pendingRoute, State.m_pendingAutoReq, State.m_routePlanResponses)

     --  The request has been added to the history

     and History'Old < History
     and Get (History, Last (History)'Old + 1).Kind = Receive_RouteRequest
     and Get (History, Last (History)'Old + 1).Id = Request.RequestID

     --  History invariants

     and Valid_Events (State.m_routeRequestId)
     and No_RouteRequest_Lost (State.m_pendingRoute)
     and No_PlanResponse_Lost (State.m_pendingRoute, State.m_routePlanResponses)
     and All_Pending_Plans_Sent (State.m_pendingRoute, State.m_routePlanResponses);

   procedure Handle_Unique_Automation_Request
     (Data    : Route_Aggregator_Configuration_Data;
      Mailbox : in out Route_Aggregator_Mailbox;
      State   : in out Route_Aggregator_State;
      Areq    : UniqueAutomationRequest)
   with
       Pre => State.m_autoRequestId < Int64'Last
       and then not Contains (State.m_uniqueAutomationRequests, State.m_autoRequestId + 1)
       and then Length (State.m_uniqueAutomationRequests) < Count_Type'Last;

   procedure Check_All_Route_Plans
     (Mailbox : in out Route_Aggregator_Mailbox;
      State   : in out Route_Aggregator_State)
   with

     --  General invariants

     Pre  => All_Plans_Registered (State.m_routePlanResponses, State.m_routePlans)
     and Only_Pending_Plans (State.m_routePlanResponses, State.m_routePlans)
     and Valid_Plan_Responses (State.m_pendingRoute, State.m_pendingAutoReq, State.m_routePlanResponses)

     --  History invariants

     and Valid_Events (State.m_routeRequestId)
     and No_RouteRequest_Lost (State.m_pendingRoute)
     and No_PlanResponse_Lost (State.m_pendingRoute, State.m_routePlanResponses)
     and All_Pending_Plans_Sent (State.m_pendingRoute, State.m_routePlanResponses),

     --  General invariants

     Post => All_Plans_Registered (State.m_routePlanResponses, State.m_routePlans)
     and Only_Pending_Plans (State.m_routePlanResponses, State.m_routePlans)
     and Valid_Plan_Responses (State.m_pendingRoute, State.m_pendingAutoReq, State.m_routePlanResponses)
     and No_Finished_Request (State.m_pendingRoute, State.m_pendingAutoReq, State.m_routePlanResponses)

     --  History invariants

     and History'Old <= History
     and Valid_Events (State.m_routeRequestId)
     and No_RouteRequest_Lost (State.m_pendingRoute)
     and No_PlanResponse_Lost (State.m_pendingRoute, State.m_routePlanResponses)
     and All_Pending_Plans_Sent (State.m_pendingRoute, State.m_routePlanResponses);

   procedure SendRouteResponse
     (Mailbox            : in out Route_Aggregator_Mailbox;
      pendingRoute       : Int64_Formal_Set_Map;
      pendingAutoReq     : Int64_Formal_Set_Map;
      routePlanResponses : in out Int64_RouteResponse_Map;
      routePlans         : in out Int64_IdPlanPair_Map;
      routeKey           : Int64)
   with
     Pre  => Int_Set_Maps_M.Has_Key (Model (pendingRoute), routeKey)

     --  Only send route response if all plans are received

     and then (for all Id of Int_Set_Maps_M.Get (Model (pendingRoute), routeKey) =>
                  Contains (routePlanResponses, Id))

     --  General invariants

     and then No_Overlaps (Model (pendingRoute))
     and then No_Overlaps (Model (pendingAutoReq))
     and then No_Overlaps (Model (pendingRoute), Model (pendingAutoReq))
     and then All_Plans_Registered (routePlanResponses, routePlans)
     and then Only_Pending_Plans (routePlanResponses, routePlans)
     and then Valid_Plan_Responses (pendingRoute, pendingAutoReq, routePlanResponses)

     --  History invariants

     and then No_RouteRequest_Lost (pendingRoute)
     and then No_PlanResponse_Lost (pendingRoute, routePlanResponses)
     and then All_Pending_Plans_Sent (pendingRoute, routePlanResponses),

     --  Plan responses associated to routeKey have been removed

     Post =>
           (for all Id of Model (routePlanResponses) =>
                Contains (Model (routePlanResponses)'Old, Id))
     and (for all Id of Model (routePlanResponses) =>
                not Contains (Element (pendingRoute, routeKey), Id))
     and (for all Id of Model (routePlanResponses)'Old =>
            (if not Contains (Element (pendingRoute, routeKey), Id) then
                 Contains (routePlanResponses, Id)))

     --  General invariants

     and All_Plans_Registered (routePlanResponses, routePlans)
     and Only_Pending_Plans (routePlanResponses, routePlans)
     and Valid_Plan_Responses (pendingRoute, pendingAutoReq, routePlanResponses)

     --  The route response was sent

     and History'Old < History
     and Last (History) = Last (History)'Old + 1
     and Get (History, Last (History)) = (Send_RouteResponse, routeKey)

     --  History invariants

     and No_RouteRequest_Lost (pendingRoute)
     and
        (for all E of History =>
            (if E.Kind = Receive_PlanResponse
               and then Has_Key (Plan_To_Route (pendingRoute), E.Id)
               and then Get (Plan_To_Route (pendingRoute), E.Id) /= routeKey
             then Contains (routePlanResponses, E.Id)))
     and
       (for all Id of Plan_To_Route (pendingRoute) =>
          (if Get (Plan_To_Route (pendingRoute), Id) /= routeKey then
                 Contains (routePlanResponses, Id)
           or else PlanRequest_Sent (Id)));

   procedure Check_All_Task_Options_Received
     (Mailbox : in out Route_Aggregator_Mailbox;
      Data  : Route_Aggregator_Configuration_Data;
      State : in out Route_Aggregator_State);

   procedure Build_Matrix_Requests
     (Mailbox : in out Route_Aggregator_Mailbox;
      Data  : Route_Aggregator_Configuration_Data;
      State : in out Route_Aggregator_State;
      ReqId : Int64);

   procedure SendMatrix
     (Mailbox                    : in out Route_Aggregator_Mailbox;
      m_uniqueAutomationRequests : Int64_UniqueAutomationRequest_Map;
      m_pendingRoute             : Int64_Formal_Set_Map;
      m_pendingAutoReq           : Int64_Formal_Set_Map;
      m_routePlans               : in out Int64_IdPlanPair_Map;
      m_routeTaskPairing         : in out Int64_TaskOptionPair_Map;
      m_routePlanResponses       : in out Int64_RouteResponse_Map;
      m_taskOptions              : in out Int64_TaskPlanOptions_Map;
      autoKey                    : Int64)
   with
     Pre  => Int_Set_Maps_M.Has_Key (Model (m_pendingAutoReq), autoKey)

     and then Contains (m_uniqueAutomationRequests, autoKey)

     and then No_Overlaps (Model (m_pendingRoute))
     and then No_Overlaps (Model (m_pendingAutoReq))
     and then No_Overlaps (Model (m_pendingRoute), Model (m_pendingAutoReq))
     and then (for all Id of Int_Set_Maps_M.Get (Model (m_pendingAutoReq), autoKey) =>
            Contains (m_routePlanResponses, Id))

     and then All_Plans_Registered (m_routePlanResponses, m_routePlans)
     and then Only_Pending_Plans (m_routePlanResponses, m_routePlans)
     and then Valid_Plan_Responses (m_pendingRoute, m_pendingAutoReq, m_routePlanResponses)

     --  History invariants

     and then No_RouteRequest_Lost (m_pendingRoute)
     and then No_PlanResponse_Lost (m_pendingRoute, m_routePlanResponses)
     and then All_Pending_Plans_Sent (m_pendingRoute, m_routePlanResponses),

     Post => (for all Id of Model (m_routePlanResponses) =>
                Contains (Model (m_routePlanResponses)'Old, Id))
     and (for all Id of Model (m_routePlanResponses) =>
                not Contains (Element (m_pendingAutoReq, autoKey), Id))
     and (for all Id of Model (m_routePlanResponses)'Old =>
            (if not Contains (Element (m_pendingAutoReq, autoKey), Id) then
                 Contains (m_routePlanResponses, Id)))

     --  General invariants

     and All_Plans_Registered (m_routePlanResponses, m_routePlans)
     and Only_Pending_Plans (m_routePlanResponses, m_routePlans)
     and Valid_Plan_Responses (m_pendingRoute, m_pendingAutoReq, m_routePlanResponses)

     --  History invariants

     and No_PlanResponse_Lost (m_pendingRoute, m_routePlanResponses)
     and All_Pending_Plans_Sent (m_pendingRoute, m_routePlanResponses);

private

   function Same_Mappings
     (M : Int64_Formal_Set_Maps.Formal_Model.M.Map;
      N : Int_Set_Maps_M.Map)
      return Boolean
   is
     ((for all I of M => Int_Set_Maps_M.Has_Key (N, I))
      and then (for all I of N => Contains (M, I))
      and then
        (for all I of N =>
             (for all E of Int_Set_Maps_M.Get (N, I) =>
                     Contains (Element (M, I), E)))
      and then
        (for all I of N =>
             (for all E of Element (M, I) =>
                     Contains (Int_Set_Maps_M.Get (N, I), E))));

   function Disjoint (S1, S2 : Int64_Set) return Boolean is
     (for all E of S1 => not Contains (S2, E));

   function No_Overlaps (pendingRoute : Int_Set_Maps_M.Map) return Boolean is
     (for all R_1 of pendingRoute =>
        (for all R_2 of pendingRoute =>
             (if R_1 /= R_2 then
                   Disjoint (Int_Set_Maps_M.Get (pendingRoute, R_1),
                             Int_Set_Maps_M.Get (pendingRoute, R_2)))));

   function No_Overlaps (pendingRoute, pendingAutoReq : Int_Set_Maps_M.Map) return Boolean is
     (for all R_1 of pendingRoute =>
         (for all R_2 of pendingAutoReq =>
            Disjoint (Int_Set_Maps_M.Get (pendingRoute, R_1),
                      Int_Set_Maps_M.Get (pendingAutoReq, R_2))));

   function All_Pending_Requests_Seen
     (pendingRequest : Int_Set_Maps_M.Map;
      routeRequestId : Int64)
      return Boolean
   is
     (for all R_Id of pendingRequest =>
          (for all Id of Int_Set_Maps_M.Get (pendingRequest, R_Id) =>
                Id <= routeRequestId));

   function All_Plans_Registered
     (routePlanResponses : Int64_RouteResponse_Map;
      routePlans         : Int64_IdPlanPair_Map)
      return Boolean
   is
     (for all RP of Model (routePlanResponses) =>
        (for all Pl of Element (routePlanResponses, RP).RouteResponses =>
              Contains (routePlans, Pl.RouteID)
         and then Element (routePlans, Pl.RouteID).Id = RP));

   function Only_Pending_Plans
     (routePlanResponses : Int64_RouteResponse_Map;
      routePlans         : Int64_IdPlanPair_Map)
      return Boolean
   is
     (for all Pl of Model (routePlans) =>
        (Contains (routePlanResponses, Element (routePlans, Pl).Id)
           and then Contains (Element (routePlanResponses, Element (routePlans, Pl).Id).RouteResponses, Pl)));

   function Valid_Plan_Responses
     (pendingRoute       : Int64_Formal_Set_Map;
      pendingAutoReq     : Int64_Formal_Set_Map;
      routePlanResponses : Int64_RouteResponse_Map)
      return Boolean
   is
     (for all RP of Model (routePlanResponses) =>
           (Has_Key (Plan_To_Route (pendingRoute), RP)
            or else Has_Key (Plan_To_Route (pendingAutoReq), RP)));

   function Is_Pending
     (pendingRoute       : Int_Set_Maps_M.Map;
      routePlanResponses : RR_Maps_M.Map;
      Request_Id         : Int64)
      return Boolean
   is
     (for some RP of Int_Set_Maps_M.Get (pendingRoute, Request_Id) =>
           not Contains (routePlanResponses, RP));

   function No_Finished_Request
     (pendingRoute, pendingAutoReq : Int64_Formal_Set_Map;
      routePlanResponses           : Int64_RouteResponse_Map)
      return Boolean
   is
     ((for all Id of Model (pendingRoute) =>
          Is_Pending (Model (pendingRoute), Model (routePlanResponses), Id))
      and then
      (for all Id of Model (pendingAutoReq) =>
          Is_Pending (Model (pendingAutoReq), Model (routePlanResponses), Id)));
end Route_Aggregator;
