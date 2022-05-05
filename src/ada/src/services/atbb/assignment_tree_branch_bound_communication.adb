with AVTAS.LMCP.Types;
with LMCP_Message_Conversions;                  use LMCP_Message_Conversions;

package body Assignment_Tree_Branch_Bound_Communication is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (This         : out Assignment_Tree_Branch_Bound_Mailbox;
      Source_Group : String;
      Unique_Id    : Int64;
      Entity_Id    : UInt32;
      Service_Id   : UInt32)
   is
   begin
      --  The procedure UxAS.Comms.LMCP_Net_Client.Initialize_Network_Client()
      --  will also initialize its Message_Sender_Pipe component but will not
      --  use it for sending:
      --
      --  This.Message_Sender_Pipe.Initialize_Push
      --    (Source_Group => Value (This.Message_Source_Group),
      --     Entity_Id    => This.Entity_Id,
      --     Service_Id   => UInt32 (This.Network_Id));

      This.Message_Sender_Pipe.Initialize_Push
        (Source_Group => Source_Group,
         Entity_Id    => AVTAS.LMCP.Types.UInt32 (Entity_Id),
         Service_Id   => AVTAS.LMCP.Types.UInt32 (Service_Id));

      This.Unique_Entity_Send_Message_Id := Unique_Id;
   end Initialize;

   --------------------------
   -- sendBroadcastMessage --
   --------------------------

   --  this is sendSharedLMCPObjectBroadcastMessage(), in our code Send_Shared_LMCP_Object_Broadcast_Message

   procedure sendBroadcastMessage
     (This : in out Assignment_Tree_Branch_Bound_Mailbox;
      Msg  : Message_Root'Class)
   is
   begin
      This.Unique_Entity_Send_Message_Id := This.Unique_Entity_Send_Message_Id + 1;
      --  This.Message_Sender_Pipe.Send_Shared_Broadcast_Message (Msg);
      This.Message_Sender_Pipe.Send_Shared_Broadcast_Message (As_Object_Any (Msg));
   end sendBroadcastMessage;

   ----------------------
   -- sendErrorMessage --
   ----------------------

   procedure sendErrorMessage
     (This         : in out Assignment_Tree_Branch_Bound_Mailbox;
      Error_String : Unbounded_String)
   is
      KVP     : KeyValuePair := (Key   => To_Unbounded_String ("No UniqueAutomationResponse"),
                                 Value => Error_String);
      Message : ServiceStatus;
   begin
      Message.StatusType := Error;
      Message.Info := Add (Message.Info, KVP);
      This.Unique_Entity_Send_Message_Id := This.Unique_Entity_Send_Message_Id + 1;
      This.Message_Sender_Pipe.Send_Shared_Broadcast_Message (As_Object_Any (Message));
   end sendErrorMessage;

end Assignment_Tree_Branch_Bound_Communication;
