with DOM.Core.Elements;
with GNAT.Regexp; use GNAT.Regexp;
with Ada.Text_IO;  use Ada.Text_IO;

with AFRL.CMASI.AutomationResponse;    use AFRL.CMASI.AutomationResponse;
with AFRL.CMASI.AirVehicleState;       use AFRL.CMASI.AirVehicleState;
with AFRL.CMASI.MissionCommand;        use AFRL.CMASI.MissionCommand;
-- with UxAS.Messages.UxNative.IncrementWaypoint; use UxAS.Messages.UxNative.IncrementWaypoint;

with Common;                   use Common;
with LMCP_Message_Conversions; use LMCP_Message_Conversions;

package body UxAS.Comms.LMCP_Net_Client.Service.Waypoint_Plan_Management is

   ---------------------------
   -- Convenience functions --
   ---------------------------

   function UInt32_Attribute
     (XML_Node : DOM.Core.Element;
      Name     : String;
      Default  : Common.UInt32)
      return Common.UInt32;

   function Int64_Attribute
     (XML_Node : DOM.Core.Element;
      Name     : String;
      Default  : Common.Int64)
      return Common.Int64;

   function Real64_Attribute
     (XML_Node : DOM.Core.Element;
      Name     : String;
      Default  : Common.Real64)
      return Common.Real64;

   function Boolean_Attribute
     (XML_Node : DOM.Core.Element;
      Name     : String;
      Default  : Boolean)
      return Boolean;

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Handle_AirVehicleState
     (This : in out Waypoint_Plan_Manager_Service;
      Msg  : Any_LMCP_Message);

   procedure Handle_Automation_Response
     (This : in out Waypoint_Plan_Manager_Service;
      Msg  : Any_LMCP_Message);

   procedure Handle_Mission_Command
     (This : in out Waypoint_Plan_Manager_Service;
      Msg  : Any_LMCP_Message);

   ---------------
   -- Configure --
   ---------------

   overriding
   procedure Configure
     (This     : in out Waypoint_Plan_Manager_Service;
      XML_Node : DOM.Core.Element;
      Result   : out Boolean)
   is
      Unused : Boolean;
   begin

      This.VehicleID :=
        Int64_Attribute (XML_Node, "VehicleID",
                         Default => Common.Int64 (This.Entity_Id)); -- Is it This.Entity_Id or Entity_Id (This)?

      This.Config.NumberWaypointsToServe :=
        UInt32_Attribute (XML_Node, "NumberWaypointsToServe",
                          Default => This.Config.NumberWaypointsToServe);

      This.Config.NumberWaypointOverlap :=
        UInt32_Attribute (XML_Node, "NumberWaypointOverlap",
                          Default => This.Config.NumberWaypointOverlap);

      This.Config.NumberWaypointOverlap :=
        Common.UInt32'Max (This.Config.NumberWaypointOverlap, 2);

      This.LoiterRadiusDefault :=
        Real64_Attribute (XML_Node, "DefaultLoiterRadius_m",
                          Default => This.LoiterRadiusDefault);

      --  This.IsAddLoiterToEndOfSegments :=
      --    Boolean_Attribute (XML_Node, "AddLoiterToEndOfSegments",
      --                       Default => This.IsAddLoiterToEndOfSegments);

      --  This.IsAddLoiterToEndOfMission :=
      --    Boolean_Attribute (XML_Node, "AddLoiterToEndOfMission",
      --                       Default => This.IsAddLoiterToEndOfMission);

      --  This.IsLoopBackToFirstTask :=
      --    Boolean_Attribute (XML_Node, "LoopBackToFirstTask",
      --                       Default => This.IsLoopBackToFirstTask);

      --  This.IsSetLastWaypointSpeedTo0 :=
      --    Boolean_Attribute (XML_Node, "SetLastWaypointSpeedTo0",
      --                       Default => This.IsSetLastWaypointSpeedTo0);

      This.GimbalPayloadId :=
        Int64_Attribute (XML_Node, "GimbalPayloadId",
                         Default => This.GimbalPayloadId);

      This.TurnType := AFRL.CMASI.Enumerations.TurnTypeEnum (TurnShort);

      This.TimeBetweenMissionCommandsMin_ms := 1000; -- int64

      This.Add_Subscription_Address (AFRL.CMASI.AutomationResponse.Subscription, Unused);
      This.Add_Subscription_Address (AFRL.CMASI.AirVehicleState.Subscription, Unused);
      This.Add_Subscription_Address (AFRL.CMASI.MissionCommand.Subscription, Unused);
      -- This.Add_Subscription_Address (UxAS.Messages.UxNative.IncrementWaypoint.Subscription, Unused);

      Result := True;
   end Configure;

   ------------
   -- Create --
   ------------

   function Create return Any_Service is
      Result : Waypoint_Plan_Manager_Service_Ref;
   begin
      Result := new Waypoint_Plan_Manager_Service;

      Result.Construct_Service
        (Service_Type        => Type_Name,
         Work_Directory_Name => Directory_Name);
      return Any_Service (Result);
   end Create;

   ----------------------------
   -- Handle_AirVehicleState --
   ----------------------------

   procedure Handle_AirVehicleState
     (This : in out Waypoint_Plan_Manager_Service;
      Msg  : Any_LMCP_Message)
   is
      AVS   : constant AirVehicleState_Any := AirVehicleState_Any (Msg.Payload);
      AV_ID : constant Common.Int64 := Common.Int64 (AVS.getID);
      Time  : constant Common.Int64 := Common.Int64 (AVS.getTime);
      WP_ID : constant Common.Int64 := Common.Int64 (AVS.getCurrentWaypoint);
   begin
      -- If the AVS is for this vehicle, update timer and waypoint state
      if AV_ID = This.VehicleID then
         if This.WPM_Timer = 0 then
            This.WPM_Timer := Time;
         end if;
         if Time - This.WPM_Timer > This.TimeBetweenMissionCommandsMin_ms then
            This.WPM_Timer := Time;
            This.WPM_Timer_Triggered := True;
         end if;
         This.State.Current_WP_ID := Positive64 (WP_ID);
      end if;
   end Handle_AirVehicleState;

   ----------------------------
   -- Handle_Mission_Command --
   ----------------------------

   procedure Handle_Mission_Command
     (This : in out Waypoint_Plan_Manager_Service;
      Msg  : Any_LMCP_Message)
   is
      MC : constant MissionCommand_Acc := MissionCommand_Acc (Msg.Payload);
      Vehicle_ID : Common.Int64 := Common.Int64 (MC.getVehicleID);
   begin
      if Vehicle_ID = This.VehicleID then
         This.State.Original_MC := As_MissionCommand_Message (MC);
         This.Stored_MC := MC;
         This.State.New_Command := True;
      end if;
   end Handle_Mission_Command;

   --------------------------------
   -- Handle_Automation_Response --
   --------------------------------

   procedure Handle_Automation_Response
     (This : in out Waypoint_Plan_Manager_Service;
      Msg  : Any_LMCP_Message)
   is
      AR : constant AutomationResponse_Any := AutomationResponse_Any (Msg.Payload);
      Vec_MC_Acc_Acc : constant Vect_MissionCommand_Acc_Acc := AR.getMissionCommandList;
   begin
      for MC_Acc of Vec_MC_Acc_Acc.all loop
         if Common.Int64 (MC_Acc.getVehicleID) = This.VehicleID then
            This.State.Original_MC := As_MissionCommand_Message (MC_Acc);
            This.Stored_MC := MC_Acc;
            This.State.New_Command := True;
            exit;
         end if;
      end loop;
   end Handle_Automation_Response;

   ----------------
   -- Initialize --
   ----------------

   overriding
   procedure Initialize
     (This   : in out Waypoint_Plan_Manager_Service;
      Result : out Boolean)
   is
      -- since not doing the Timers
   begin
      --  the C++ version creates the timers here (but we don't, unless we implement the timers).
      Waypoint_Plan_Manager_Communication.Initialize
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
     (This             : in out Waypoint_Plan_Manager_Service;
      Received_Message : not null Any_LMCP_Message;
      Should_Terminate : out Boolean)
   is
   begin

      if Received_Message.Payload.all in AirVehicleState'Class then
         This.Handle_AirVehicleState (Received_Message);
      elsif Received_Message.Payload.all in MissionCommand'Class then
         This.Handle_Mission_Command (Received_Message);
      elsif Received_Message.Payload.all in AutomationResponse'Class then
         This.Handle_Automation_Response (Received_Message);
      end if;

      if This.WPM_Timer_Triggered then

         if This.State.New_Command then

            Process_Mission_Command (This.State);
            Update_Segment (This.State, This.Config);

            if This.State.Second_Element_Is_FirstID then
               This.Stored_MC.setFirstWaypoint
                 (AVTAS.LMCP.Types.Int64
                    (Element (This.State.Current_Segment,
                     First_Index (This.State.Current_Segment) + 1)));
            else
               This.Stored_MC.setFirstWaypoint
                 (AVTAS.LMCP.Types.Int64 (First_Element (This.State.Current_Segment)));
            end if;

            This.Stored_MC.getWaypointList.Clear;

            for E of This.State.Current_Segment loop
               This.Stored_MC.getWaypointList.Append (
            end loop;

            -- This.Message_Sender_Pipe.Send_Shared_Broadcast_Message (Object_Any (This.State.Current_Mission_Command));
            This.State.New_Command := False;
            This.WPM_Timer_Triggered := False;
         elsif This.State.Next_Seg_ID > 0 and then
                This.State.Current_WP_ID =
                  Element (This.State.Current_Segment,
                           Last_Index (This.State.Current_Segment) -
                                 WP_ID_Vectors.Extended_Index (This.Config.NumberWaypointOverlap + 2))
         then

            -- This.Message_Sender_Pipe.Send_Shared_Broadcast_Message (Object_Any (This.State.Current_Mission_Command));
            This.State.New_Command := False;
            This.WPM_Timer_Triggered := False;
         end if;

      end if;

      Should_Terminate := False;
   end Process_Received_LMCP_Message;

   ---------------------------------
   -- Registry_Service_Type_Names --
   ---------------------------------

   function Registry_Service_Type_Names return Service_Type_Names_List is
      (Service_Type_Names_List'(1 => Instance (Service_Type_Name_Max_Length, Content => Type_Name)));

   ----------------------
   -- UInt32_Attribute --
   ----------------------

   function UInt32_Attribute
     (XML_Node : DOM.Core.Element;
      Name     : String;
      Default  : Common.UInt32)
      return Common.UInt32
   is
      use DOM.Core; use GNAT.Regexp;
      Attr_Value : constant DOM_String := Elements.Get_Attribute (XML_Node, Name);
      RE : Regexp := Compile ("[ \t]*\d+[ \t]*", Case_Sensitive => False);
   begin
      if Attr_Value /= "" and then (Match (Attr_Value, RE))
      then
         return Common.UInt32'Value (Attr_Value);
      else
         return Default;
      end if;
   end UInt32_Attribute;

   function Int64_Attribute
     (XML_Node : DOM.Core.Element;
      Name     : String;
      Default  : Common.Int64)
      return Common.Int64
   is
      use DOM.Core;
      Attr_Value : constant DOM_String := Elements.Get_Attribute (XML_Node, Name);
   begin
      if Attr_Value /= ""
      then
         begin
            return Common.Int64'Value (Attr_Value);
         exception
            when others =>
               Put_Line ("Could not convert " & Attr_Value & " to Int64. Using default " & Default'Image);
               return Default;
         end;
      else
         return Default;
      end if;
   end Int64_Attribute;

   function Real64_Attribute
     (XML_Node : DOM.Core.Element;
      Name     : String;
      Default  : Common.Real64)
      return Common.Real64
   is
      use DOM.Core;
      Attr_Value : constant DOM_String := Elements.Get_Attribute (XML_Node, Name);
   begin
      if Attr_Value /= ""
      then
         begin
            return Common.Real64'Value (Attr_Value);
         exception
            when others =>
               Put_Line ("Could not convert " & Attr_Value & " to Real64. Using default " & Default'Image);
               return Default;
         end;
      else
         return Default;
      end if;
   end Real64_Attribute;

   function Boolean_Attribute
     (XML_Node : DOM.Core.Element;
      Name     : String;
      Default  : Boolean)
      return Boolean
   is
      use DOM.Core;
      Attr_Value : constant DOM_String := Elements.Get_Attribute (XML_Node, Name);
   begin
      if Attr_Value /= ""
      then
         begin
            return Boolean'Value (Attr_Value);
         exception
            when others =>
               Put_Line ("Could not convert " & Attr_Value & " to Boolean. Using default " & Default'Image);
               return Default;
         end;
      else
         return Default;
      end if;
   end Boolean_Attribute;

   -----------------------------
   -- Package Executable Part --
   -----------------------------

   --  This is the executable part for the package, invoked automatically and only once.
begin
   --  All concrete service subclasses must call this procedure in their
   --  own package like this, with their own params. The effect is the same as the
   --  following:
   --
   --    AutomationRequestValidatorService::ServiceBase::CreationRegistrar<AutomationRequestValidatorService>
   --    AutomationRequestValidatorService::s_registrar(AutomationRequestValidatorService::s_registryServiceTypeNames());
   --
   --  located at the top of the cpp file

   Register_Service_Creation_Function_Pointers (Registry_Service_Type_Names, Create'Access);
end UxAS.Comms.LMCP_Net_Client.Service.Waypoint_Plan_Management;
