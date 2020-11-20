with Ada.Characters.Handling;
with DOM.Core.Elements;
with LMCP_Message_Conversions;

package body UxAS.Comms.LMCP_Net_Client.Service.Assignment_Tree_Branch_Bounding is

   procedure Handle_AssignmentCostMatrix_Msg
     (This : in out Assignment_Tree_Branch_Bound_Service;
      Msg  : AssignmentCostMatrix_Any);

   procedure Handle_UniqueAutomationRequest_Msg
     (This : in out Assignment_Tree_Branch_Bound_Service;
      Msg  : UniqueAutomationRequest_Any);

   procedure Handle_TaskPlanOptions_Msg
     (This : in out Assignment_Tree_Branch_Bound_Service;
      Msg  : TaskPlanOptions_Any);

   function Int64_Attribute
     (XML_Node : DOM.Core.Element;
      Name     : String;
      Default  : Common.Int64)
   return Common.Int64;

   ---------------------------------
   -- Registry_Service_Type_Names --
   ---------------------------------

   function Registry_Service_Type_Names return Service_Type_Names_List is
      (Service_Type_Names_List'(1 => Instance (Service_Type_Name_Max_Length, Content => Type_Name)));

   ------------
   -- Create --
   ------------

   function Create return Any_Service is
      Result : Any_Service;
   begin
      Result := new Assignment_Tree_Branch_Bound_Service;
      Result.Construct_Service
        (Service_Type        => Type_Name,
         Work_Directory_Name => Directory_Name);
      return Result;
   end Create;

   ---------------
   -- Configure --
   ---------------

   overriding procedure Configure
     (This     : in out Assignment_Tree_Branch_Bound_Service;
      XML_Node : DOM.Core.Element;
      Result   : out Boolean)
   is
      Unused : Boolean;
   begin
      declare
         Attr_Value : constant String := DOM.Core.Elements.Get_Attribute (XML_Node, Name => "CostFunction");
         use Ada.Characters.Handling;
      begin
         if Attr_Value = "" then
            This.Config.Cost_Function := Minmax;  -- CostFunction is an optional parameter
         elsif To_Lower (Attr_Value) = "minmax" then
            This.Config.Cost_Function := Minmax;
         elsif To_Lower (Attr_Value) = "cumulative" then
            This.Config.Cost_Function := Cumulative;
         else -- malformed value
            Result := False;
            return;
         end if;
      end;

      This.Config.Number_Nodes_Maximum :=
        Int64_Attribute (XML_Node,
                          "NumberNodesMaximum",
                          Default => This.Config.Number_Nodes_Maximum);
      This.Config.Number_Nodes_Maximum :=
        Common.Int64'Max (0, This.Config.Number_Nodes_Maximum);

      This.Add_Subscription_Address (UxAS.Messages.lmcptask.AssignmentCostMatrix.Subscription, Unused);
      This.Add_Subscription_Address (UxAS.Messages.lmcptask.TaskPlanOptions.Subscription, Unused);
      This.Add_Subscription_Address (UxAS.Messages.lmcptask.UniqueAutomationRequest.Subscription, Unused);

      Result := True;
   end Configure;

   --------------------------------
   -- Handle_AssignmentCostMatrix_Msg --
   --------------------------------

   procedure Handle_AssignmentCostMatrix_Msg
     (This : in out Assignment_Tree_Branch_Bound_Service;
      Msg  : AssignmentCostMatrix_Any)
   is
   begin
      Assignment_Tree_Branch_Bound.Handle_Assignment_Cost_Matrix
        (This.Mailbox,
         This.Config,
         This.State,
         LMCP_Message_Conversions.As_AssignmentCostMatrix_Message (Msg));
   end Handle_AssignmentCostMatrix_Msg;

   --------------------------------
   -- Handle_TaskPlanOptions_Msg --
   --------------------------------

   procedure Handle_TaskPlanOptions_Msg
     (This : in out Assignment_Tree_Branch_Bound_Service;
      Msg  : TaskPlanOptions_Any)
   is
   begin
      Assignment_Tree_Branch_Bound.Handle_Task_Plan_Options
        (This.Mailbox,
         This.Config,
         This.State,
         LMCP_Message_Conversions.As_TaskPlanOption_Message (Msg));
   end Handle_TaskPlanOptions_Msg;

   ----------------------------------------
   -- Handle_UniqueAutomationRequest_Msg --
   ----------------------------------------

   procedure Handle_UniqueAutomationRequest_Msg
     (This : in out Assignment_Tree_Branch_Bound_Service;
      Msg  : UniqueAutomationRequest_Any)
   is
   begin
      Assignment_Tree_Branch_Bound.Handle_Unique_Automation_Request
        (This.Mailbox,
         This.Config,
         This.State,
         LMCP_Message_Conversions.As_UniqueAutomationRequest_Message (Msg));
   end Handle_UniqueAutomationRequest_Msg;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize
     (This : in out Assignment_Tree_Branch_Bound_Service;
      Result : out Boolean)
   is
   begin
      Result := True; --  per the C++ version

      Assignment_Tree_Branch_Bound_Communication.Initialize
        (This.Mailbox,
         Source_Group => Value (This.Message_Source_Group),
         Unique_Id    => Common.Int64 (UxAS.Comms.LMCP_Net_Client.Unique_Entity_Send_Message_Id),
         Entity_Id    => Common.UInt32 (This.Entity_Id),
         Service_Id   => Common.UInt32 (This.Network_Id));
   end Initialize;

   -----------------------------------
   -- Process_Received_LMCP_Message --
   -----------------------------------

   overriding procedure Process_Received_LMCP_Message
     (This             : in out Assignment_Tree_Branch_Bound_Service;
      Received_Message :        not null Any_LMCP_Message;
      Should_Terminate :    out Boolean)
   is
   begin
      if Received_Message.Payload.all in UxAS.Messages.lmcptask.UniqueAutomationRequest.UniqueAutomationRequest'Class then
         This.Handle_UniqueAutomationRequest_Msg (UniqueAutomationRequest_Any (Received_Message.Payload));

      elsif Received_Message.Payload.all in TaskPlanOptions'Class then
         This.Handle_TaskPlanOptions_Msg (TaskPlanOptions_Any (Received_Message.Payload));

      elsif Received_Message.Payload.all in AssignmentCostMatrix'Class then
         This.Handle_AssignmentCostMatrix_Msg (AssignmentCostMatrix_Any (Received_Message.Payload));
      end if;

      Should_Terminate := False;
   end Process_Received_LMCP_Message;

   ---------------------
   -- Int64_Attribute --
   ---------------------

   function Int64_Attribute
     (XML_Node : DOM.Core.Element;
      Name     : String;
      Default  : Common.Int64)
   return Common.Int64
   is
      use DOM.Core;
      Attr_Value : constant DOM_String := Elements.Get_Attribute (XML_Node, Name);
   begin
      if Attr_Value /= "" and then (for all C of Attr_Value => C in '0' .. '9')
      then
         return Common.Int64'Value (Attr_Value);
      else
         return Default;
      end if;
   end Int64_Attribute;

begin
   --  All concrete service subclasses must call this procedure in their
   --  own package like this, with their own params.
   Register_Service_Creation_Function_Pointers (Registry_Service_Type_Names, Create'Access);
end UxAS.Comms.LMCP_Net_Client.Service.Assignment_Tree_Branch_Bounding;
