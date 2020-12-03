--  see OpenUxAS\src\Services\AutomationRequestValidatorService.h

with DOM.Core;

with Automation_Request_Validator;               use Automation_Request_Validator;
with Automation_Request_Validator_Communication; use Automation_Request_Validator_Communication;

package UxAS.Comms.LMCP_Net_Client.Service.Automation_Request_Validation is

   type Automation_Request_Validator_Service is new Service_Base with private;

   type Automation_Request_Validator_Service_Ref is access all Automation_Request_Validator_Service;

   Type_Name : constant String := "AutomationRequestValidatorService";

   Directory_Name : constant String := "";

   --  static const std::vector<std::string>
   --  s_registryServiceTypeNames()
   function Registry_Service_Type_Names return Service_Type_Names_List;

   --  static ServiceBase*
   --  create()
   function Create return Any_Service;

private

   --  static
   --  ServiceBase::CreationRegistrar<AutomationRequestValidatorService> s_registrar;
   --  see the package body executable part

   type Automation_Request_Validator_Service is new Service_Base with record

      --  TODO: implement these timers, maybe using Timing_Events, but maybe using
      --  tasks because their purpose is to call send outgoing messages at the
      --  desired rate
      --
      --      this timer is used to track time for the system to respond to automation requests */
      --      uint64_t m_responseTimerId{0};
      --
      --      this timer is used to track time for the system to wait for task initialization */
      --      uint64_t m_taskInitTimerId{0};

      --      the maximum time to wait for a response (in ms)*/
      --      uint32_t m_maxResponseTime_ms = {5000}; // default: 5000 ms
      Max_Response_Time : UInt32 := 5000; -- milliseconds

      Config  : Automation_Request_Validator_Configuration_Data;
      Mailbox : Automation_Request_Validator_Mailbox;
      State   : Automation_Request_Validator_State;
   end record;

   overriding
   procedure Configure
     (This     : in out Automation_Request_Validator_Service;
      XML_Node : DOM.Core.Element;
      Result   : out Boolean);

   overriding
   procedure Initialize
     (This   : in out Automation_Request_Validator_Service;
      Result : out Boolean);

   overriding
   procedure Process_Received_LMCP_Message
     (This             : in out Automation_Request_Validator_Service;
      Received_Message : not null Any_LMCP_Message;
      Should_Terminate : out Boolean);

--  TODO: TIMER CALLBACKS
--  this function gets called when the response timer expires
--  void OnResponseTimeout();
--  this function gets called when the tasks involved have not reported initialization in time
--  void OnTasksReadyTimeout();

end UxAS.Comms.LMCP_Net_Client.Service.Automation_Request_Validation;
