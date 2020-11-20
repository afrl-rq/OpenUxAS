with DOM.Core;

with Assignment_Tree_Branch_Bound;               use Assignment_Tree_Branch_Bound;
with Assignment_Tree_Branch_Bound_Communication; use Assignment_Tree_Branch_Bound_Communication;
with Common;                                     use Common;

with AVTAS.LMCP.Types;
with UxAS.Messages.lmcptask.UniqueAutomationRequest; use UxAS.Messages.lmcptask.UniqueAutomationRequest;
with UxAS.Messages.lmcptask.TaskPlanOptions;         use UxAS.Messages.lmcptask.TaskPlanOptions;
with UxAS.Messages.lmcptask.AssignmentCostMatrix;    use UxAS.Messages.lmcptask.AssignmentCostMatrix;
package UxAS.Comms.LMCP_Net_Client.Service.Assignment_Tree_Branch_Bounding is

   type Assignment_Tree_Branch_Bound_Service is new Service_Base with private;

   Type_Name : constant String := "AssignmentTreeBranchBoundService";

   Directory_Name : constant String := "";

   --  static const std::vector<std::string>
   --  s_registryServiceTypeNames()
   function Registry_Service_Type_Names return Service_Type_Names_List;

   --  static ServiceBase*
   --  create()
   function Create return Any_Service;

private

   type Assignment_Tree_Branch_Bound_Service is new Service_Base with record

      --  the following types are defined in SPARK code
      Config  : Assignment_Tree_Branch_Bound_Configuration_Data;
      Mailbox : Assignment_Tree_Branch_Bound_Mailbox;
      State   : Assignment_Tree_Branch_Bound_State;
   end record;

   overriding
   procedure Configure
     (This     : in out Assignment_Tree_Branch_Bound_Service;
      XML_Node : DOM.Core.Element;
      Result   : out Boolean);

   overriding
   procedure Initialize
     (This   : in out Assignment_Tree_Branch_Bound_Service;
      Result : out Boolean);

   overriding
   procedure Process_Received_LMCP_Message
     (This             : in out Assignment_Tree_Branch_Bound_Service;
      Received_Message : not null Any_LMCP_Message;
      Should_Terminate : out Boolean);

end UxAS.Comms.LMCP_Net_Client.Service.Assignment_Tree_Branch_Bounding;
