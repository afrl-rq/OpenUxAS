with LMCP_Messages; use LMCP_Messages;
with Common;        use Common;

private with Ada.Strings.Unbounded;
private with UxAS.Comms.LMCP_Object_Message_Sender_Pipes;

--  Package only concerned with message passing. It defines its own state,
--  named Mailbox here, which is not mixed with the state of the service.

package Assignment_Tree_Branch_Bound_Communication with SPARK_Mode is

   type Assignment_Tree_Branch_Bound_Mailbox is limited private;

   procedure Initialize
     (This         : out Assignment_Tree_Branch_Bound_Mailbox;
      Source_Group : String;
      Unique_Id    : Int64;
      Entity_Id    : UInt32;
      Service_Id   : UInt32);

   procedure sendBroadcastMessage
     (This : in out Assignment_Tree_Branch_Bound_Mailbox;
      Msg   : Message_Root'Class);

private
   pragma SPARK_Mode (Off);

   use Ada.Strings.Unbounded;

   use UxAS.Comms.LMCP_Object_Message_Sender_Pipes;

   type  Assignment_Tree_Branch_Bound_Mailbox is tagged limited record
      Message_Sender_Pipe           : LMCP_Object_Message_Sender_Pipe;
      Source_Group                  : Unbounded_String;
      Unique_Entity_Send_Message_Id : Int64;
   end record;

end Assignment_Tree_Branch_Bound_Communication;
