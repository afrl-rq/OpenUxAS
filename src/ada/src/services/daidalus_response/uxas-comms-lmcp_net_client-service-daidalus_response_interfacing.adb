with DOM.Core.Elements;
with Ada.Text_IO;  use Ada.Text_IO;

-- __TODO__
-- Include any additional packages used by this service.

with Common;                   use Common;
with LMCP_Messages;            -- use LMCP_Messages;
with LMCP_Message_Conversions; use LMCP_Message_Conversions;
with definitions;              -- use definitions;
with Daidalus_Response;        use Daidalus_Response;

-- __TODO__
-- Include any messages used by this service.
--
-- __Example__
with AFRL.CMASI.EntityState;           use AFRL.CMASI.EntityState;
with AFRL.CMASI.AirVehicleState;       use AFRL.CMASI.AirVehicleState;
with AFRL.CMASI.MissionCommand;        use AFRL.CMASI.MissionCommand;
with AFRL.CMASI.AutomationResponse;    use AFRL.CMASI.AutomationResponse;
with larcfm.DAIDALUS.DAIDALUSConfiguration; 
with larcfm.DAIDALUS.WellClearViolationIntervals;
use larcfm.DAIDALUS.DAIDALUSConfiguration; 
use larcfm.DAIDALUS.WellClearViolationIntervals;

package body UxAS.Comms.LMCP_Net_Client.Service.Daidalus_Response_Interfacing is

   -----------------------
   -- Local subprograms --
   -----------------------

   -- __TODO__
   -- Declare any subprograms used locally within the package. This should
   -- include procedures to handle specific types of Ada LMCP messages and may
   -- also include procedures to sanity-check values pulled from the OpenUxAS 
   -- XML configuration file. By convention, subprogram definitions are deferred 
   -- until later in the packge, and Ada LMCP message handlers are named
   -- Handle_<MessageType>_Msg, while SPARK message handlers in package
   -- <Service_Name> are named Handle_<MessageType>.
   --
   -- __Example__
   --
   -- function TurnType_Attribute
   --   (XML_Node : DOM.Core.Element;
   --    Name     : String;
   --    Default  : LMCP_Messages.TurnTypeEnum)
   --    return LMCP_Messages.TurnTypeEnum;
   --
   -- procedure Handle_AirVehicleState_Msg
   --    (This : in out <Service_Name>_Service;
   --     Msg  : Any_LMCP_Message);
   --
   -- procedure Handle_MissionCommand_Msg
   --    (This : in out <Service_Name>_Service;
   --     Msg  : Any_LMCP_Message);

   --  procedure Handle_MissionCommand_Msg
   --    (This : in out Daidalus_Response_Service;
   --     Msg : Any_LMCP_Message);
   --  
   --  Procedure Handle_AutomationResponse_Msg
   --    (This : in out Daidalus_Response_Service;
   --     Msg : Any_LMCP_Message);
   
   -- ------------------------
   -- -- TurnType_Attribute --
   -- ------------------------
   -- 
   -- function TurnType_Attribute
   --    (XML_Node : DOM.Core.Element;
   --    Name     : String;
   --    Default  : LMCP_Messages.TurnTypeEnum)
   --    return LMCP_Messages.TurnTypeEnum
   -- is
   --    use DOM.Core;
   --    Attr_Value : constant DOM_String := Elements.Get_Attribute (XML_Node, Name);
   -- begin
   --    if Attr_Value /= ""
   --    then
   --       begin
   --          return LMCP_Messages.TurnTypeEnum'Value (Attr_Value);
   --       exception
   --          when others =>
   --             Put_Line ("Could not convert " & Attr_Value &
   --                         " to TurnTypeEnum. Using default " & Default'Image);
   --             return Default;
   --       end;
   --    else
   --       return Default;
   --    end if;
   -- end TurnType_Attribute;
 
   function InterpretXMLNodeAsInt64 
     (XML_Node : DOM.Core.Element;
      Name : String;
      Default : Common.Int64) return Common.Int64;
   
   function InterpretXMLNodeAsInt64 
     (XML_Node : DOM.Core.Element;
      Name : String;
      Default : Common.Int64) return Common.Int64 is
      use DOM.Core;
      Attr_Value : constant DOM_String := Elements.Get_Attribute (XML_Node, 
                                                                  Name);
   begin
      if Attr_Value /= ""
      then
         begin
            return Common.Int64'Value (Attr_Value);
         exception
            when others =>
               Put_Line ("Could not convert " & Attr_Value &
                           " to Int64. Using default " & Default'Image);
               return Default;
         end;
      else
         return Default;
      end if;
   end InterpretXMLNodeAsInt64;
   
   function InterpretXMLNodeAsReal64 
     (XML_Node : DOM.Core.Element;
      Name : String;
      Default : Common.Real64) return Common.Real64;
   function InterpretXMLNodeAsReal64 
     (XML_Node : DOM.Core.Element;
      Name : String;
      Default : Common.Real64) return Common.Real64 is
      use DOM.Core;
      Attr_Value : constant DOM_String := Elements.Get_Attribute (XML_Node, 
                                                                  Name);
   begin
      if Attr_Value /= " "
      then 
         begin
            return Common.Real64'Value (Attr_Value);
         exception
            when others =>
               Put_Line ("Could not convert " & Attr_Value &
                           " to Real64. Using default " & Default'Image);
               return Default;
         end;
      else
         return Default;
      end if;
   end InterpretXMLNodeAsReal64;
   
   ---------------
   -- Configure --
   ---------------

   overriding
   procedure Configure
     (This     : in out Daidalus_Response_Service;
      XML_Node : DOM.Core.Element;
      Result   : out Boolean)
   is
      Unused : Boolean;
   begin
      -- __TODO__
      -- Configure service-specific parameters, generally fields of This.Config, 
      -- based on values from the OpenUxAS XML configuration file.
      --
      -- __Example__
      -- 
      -- This.Config.TurnType :=
      --   TurnType_Attribute (XML_Node, "TurnType",
      --                       Default => This.Config.TurnType);

      -- __TODO__
      -- Subscribe to messages this service needs to receive.
      --
      -- __Example__
      
      This.Config.VehicleID := InterpretXMLNodeAsInt64 (XML_Node, "VehicleID",
                                                        This.Config.VehicleID);
      This.Config.ActionTimeThreshold := InterpretXMLNodeAsReal64 (XML_Node, 
                                                     "ActionTimeThreshold", 
                                               This.Config.ActionTimeThreshold);
      This.Config.PriorityTimeThreshold := InterpretXMLNodeAsReal64 (XML_Node, 
                                                      "PriorityTimeThreshold", 
                                             This.Config.PriorityTimeThreshold);
      --Subscribe to AirVehicleState Messages ----------------------------------
      This.Add_Subscription_Address (AFRL.CMASI.AirVehicleState.Subscription,
                                     Unused);
      -- Subscribe to Mission Command Messages ---------------------------------
      This.Add_Subscription_Address (AFRL.CMASI.MissionCommand.Subscription, 
                                     Unused);
      --Subscribe to DAIDALUS Configuration Message ----------------------------
      This.Add_Subscription_Address (larcfm.DAIDALUS.DAIDALUSConfiguration.
                                       Subscription, Unused);
      --Subscrib to WellClear Violation Interval Messages ----------------------
      This.Add_Subscription_Address (larcfm.DAIDALUS.
                                       WellClearViolationIntervals.Subscription,
                                     Unused);     

      Result := True;
   end Configure;

   ------------
   -- Create --
   ------------

   function Create return Any_Service is
      Result : Daidalus_Response_Service_Ref;
   begin
      Result := new Daidalus_Response_Service;

      Result.Construct_Service
        (Service_Type        => Type_Name,
         Work_Directory_Name => Directory_Name);
      return Any_Service (Result);
   end Create;

   -- __TODO__
   -- Define subprograms to handle the types of Ada LMCP messages this service
   -- receives. By convention, these should be named `Handle_<MessageType>_Msg`.
   -- For simple logic processing, they might simply update e.g. This.State,
   -- This.Config, or This.<CustomField> directly without calling any SPARK
   -- subprograms. For complex logic processing, they might call SPARK
   -- subprograms from SPARK package `<Service_Name>` to process data extracted
   -- from the Ada LMCP message. Or they might convert the Ada LMCP message to a
   -- SPARK LMCP message by calling function `As_<MessageType>_Message` from
   -- package `LMCP_Message_Conversions` and passing the result to an analogous
   -- SPARK message handler from package `<Service_Name>`, conventionally named
   -- `Handle_<MessageType>`.
   --
   -- __Example__
   --
   -- --------------------------------
   -- -- Handle_AirVehicleState_Msg --
   -- --------------------------------
   --
   -- procedure Handle_AirVehicleState_Msg
   --   (This : in out <Service_Name>_Service;
   --    Msg  : Any_LMCP_Message)
   -- is
   --    AVS   : constant AirVehicleState_Any := AirVehicleState_Any (Msg.Payload);
   --    AV_ID : constant Common.Int64 := Common.Int64 (AVS.getID);
   --    Time  : constant Common.Int64 := Common.Int64 (AVS.getTime);
   --    WP_ID : constant Common.Int64 := Common.Int64 (AVS.getCurrentWaypoint);
   -- begin
   --    if AV_ID = This.Config.VehicleID then
   --       if This.Timer = 0 then
   --          This.Timer := Time;
   --      end if;
   --      if Time - This.Timer > This.Min_Time_Between_Commands_ms then
   --          This.Timer := Time;
   --          This.Time_Elapsed := True;
   --       end if;
   --       if WP_ID = This.State.Next_First_Id then
   --          This.State.Headed_To_First_Id := True;
   --       end if;
   --    end if;
   -- end Handle_AirVehicleState_Msg;
   -- 
    --------------------------------
    -- Handle_Mission_Command_Msg --
    --------------------------------
   
   --  procedure Handle_MissionCommand_Msg
   --    (This : in out Daidalus_Response_Service;
   --     Msg  : Any_LMCP_Message)
   --  is
   --     MC : constant MissionCommand_Acc := MissionCommand_Acc (Msg.Payload);
   --     Vehicle_ID : Common.Int64 := Common.Int64 (MC.getVehicleID);
   --  begin
   --     if Vehicle_ID = This.Config.VehicleID then
   --        Handle_MissionCommand (This.State, As_MissionCommand_Message (MC));
   --     end if;
   --  end Handle_MissionCommand_Msg;

   ----------------
   -- Initialize --
   ----------------

   overriding
   procedure Initialize
     (This   : in out Daidalus_Response_Service;
      Result : out Boolean)
   is
   begin
      Daidalus_Response_Mailboxes.Initialize
        (This.Mailbox,
         Source_Group => Value (This.Message_Source_Group),
         Unique_Id    => Common.Int64 (UxAS.Comms.LMCP_Net_Client.Unique_Entity_Send_Message_Id),
         Entity_Id    => Common.UInt32 (This.Entity_Id),
         Service_Id   => Common.UInt32 (This.Network_Id));

      Result := True;
   end Initialize;

   -----------------------------------
   -- Process_Received_LMCP_Message --
   -----------------------------------

   overriding
   procedure Process_Received_LMCP_Message
     (This             : in out Daidalus_Response_Service;
      Received_Message : not null Any_LMCP_Message;
      Should_Terminate : out Boolean)
   is
      DAIDALUSConfigurationMessage : LMCP_Messages.DAIDALUSConfiguration;
      WellClearViolationIntervalsMessage : LMCP_Messages.
        WellClearViolationIntervals;
      MissionCommandMessage : LMCP_Messages.MissionCommand;
      EntityStateMessage : LMCP_Messages.EntityState;
   begin

      if Received_Message.Payload.all in DAIDALUSConfiguration'Class then
         -- Convert from Ada message to SPARK message---------------------------
         DAIDALUSConfigurationMessage := As_DAIDALUSConfiguration_Message 
           (DAIDALUSConfiguration_Any (Received_Message.Payload));
         -- Process DAIDALUSConfiguration message to set state------------------
         Process_DAIDALUSConfiguration_Message 
           (This.State, This.Config, DAIDALUSConfigurationMessage);
      elsif Received_Message.Payload.all in MissionCommand'Class then
         -- Convert from Ada message to SPARK message---------------------------
         MissionCommandMessage := As_MissionCommand_Message
           (MissionCommand_Acc (Received_Message.Payload));
         -- Process MissionCommand message to set state-------------------------
         Process_MissionCommand_Message
           (m_DAIDALUSResponseServiceState  => This.State,
            m_DAIDALUSResponseServiceConfig => This.Config,
            MissionCommandMessage           => MissionCommandMessage); 
      elsif Received_Message.Payload.all in WellClearViolationIntervals'Class
      then
         -- Convert from Ada message to SPARK message---------------------------
         WellClearViolationIntervalsMessage := 
           As_WellClearViolationsIntervals_Message 
             (WellClearViolationIntervals_Any (Received_Message.Payload));
         -- Use DAIDALUS information in prosecution of collision avoidance -----
         Process_WellclearViolation_Message 
           (m_DAIDALUSResponseServiceState   => This.State,
            m_DAIDALUSResponseServiceConfig  => This.Config,
            m_DAIDALUSResponseServiceMailbox => This.Mailbox,
            WCV_Intervals                    => 
              WellClearViolationIntervalsMessage);
      elsif Received_Message.Payload.all in AirVehicleState'Class then 
         -- Convert from Ada message to SPARK message --------------------------
         EntityStateMessage := As_EntityState_Message 
           (EntityState_Any (Received_Message.Payload));
         -- Set State if EnityState Message is for ownship ---------------------
         if Common.Int64 (EntityStateMessage.Id) = This.Config.VehicleID then
            This.State.NextWaypoint := Common.Int64 
              (EntityStateMessage.CurrentWaypoint);
         end if;
      
      end if;

      -- __TODO__
      -- Add any additional processing that should be performed after a message
      -- is processed. For example, some services regularly check based on the
      -- status of This.State whether a message should be broadcast.

      Should_Terminate := False;

   end Process_Received_LMCP_Message;

   ---------------------------------
   -- Registry_Service_Type_Names --
   ---------------------------------

   function Registry_Service_Type_Names return Service_Type_Names_List is
      (Service_Type_Names_List'(1 => Instance (Service_Type_Name_Max_Length, Content => Type_Name)));

   -- __TODO__
   -- Provide definitions for local subprograms declared in the package
   -- specification or earlier in the package body, e.g. for sanity-checking
   -- values pulled from the OpenUxAS XML configuration file or other local
   -- helper functions.
   --
   -- __Example__
   -- 

   -----------------------------
   -- Package Executable Part --
   -----------------------------

begin

   Register_Service_Creation_Function_Pointers (Registry_Service_Type_Names, Create'Access);
end UxAS.Comms.LMCP_Net_Client.Service.Daidalus_Response_Interfacing;
