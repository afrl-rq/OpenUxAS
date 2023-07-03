with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Common;                use Common;
with LMCP_Messages;         use LMCP_Messages;
with AVTAS.LMCP.Object;     use AVTAS.LMCP.Object;

private with UxAS.Comms.LMCP_Object_Message_Sender_Pipes;

--  Package only concerned with message passing. It defines its own state,
--  named Mailbox here, which is not mixed with the state of the service.

package Plan_Builder_Communication with SPARK_Mode is

   type Plan_Builder_Mailbox is limited private;

   procedure Initialize
     (This         : out Plan_Builder_Mailbox;
      Source_Group : String;
      Unique_Id    : Int64;
      Entity_Id    : UInt32;
      Service_Id   : UInt32);

   procedure sendBroadcastMessage
     (This : in out Plan_Builder_Mailbox;
      Msg   : Message_Root'Class);

   procedure sendErrorMessage
     (This         : in out Plan_Builder_Mailbox;
      Error_String : Unbounded_String);

private
   pragma SPARK_Mode (Off);

   use Ada.Strings.Unbounded;

   use UxAS.Comms.LMCP_Object_Message_Sender_Pipes;

   type  Plan_Builder_Mailbox is tagged limited record
      Message_Sender_Pipe           : LMCP_Object_Message_Sender_Pipe;
      Source_Group                  : Unbounded_String;
      Unique_Entity_Send_Message_Id : Int64;
   end record;

end Plan_Builder_Communication;
