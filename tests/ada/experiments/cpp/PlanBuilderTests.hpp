#include "PlanBuilderService.h"

#include "uxas/messages/task/UniqueAutomationRequest.h"
#include "uxas/messages/task/UniqueAutomationResponse.h"
#include "uxas/messages/task/TaskAssignmentSummary.h"
#include "uxas/messages/task/TaskAssignment.h"
#include "uxas/messages/task/TaskImplementationRequest.h"
#include "uxas/messages/task/TaskImplementationResponse.h"
#include "uxas/messages/task/PlanningState.h"

#include "afrl/cmasi/EntityState.h"
#include "afrl/cmasi/GimbalState.h"
#include "afrl/cmasi/GimbalAngleAction.h"
#include "afrl/cmasi/LoiterAction.h"
#include "afrl/impact/SpeedAltPair.h"
#include "afrl/impact/ImpactAutomationRequest.h"
#include "afrl/impact/ImpactAutomationResponse.h"

#include <cstdint> // int64_t
#include <deque>
#include <unordered_map>
#include <list>

namespace uxas
{
namespace service
{
namespace test
{

class PlanBuilderTests
{
    public:
    std::string PlanningState_To_String(uxas::messages::task::PlanningState* state);
    std::string PlanningState_Seq_To_String(std::vector<uxas::messages::task::PlanningState*> & states);
    std::string Int64_Seq_To_String(std::vector<int64_t>& seq);
    std::string UniqueAutomationRequest_To_String(std::shared_ptr<uxas::messages::task::UniqueAutomationRequest> request);
    void Write_String_To_File(const std::string filename, const std::string& content);
    void Write_UniqueAutomationRequest_Map(const std::string filename, std::unordered_map<int64_t, std::shared_ptr<uxas::messages::task::UniqueAutomationRequest> > map);
    std::string VehicleAction_To_String(afrl::cmasi::VehicleAction* va);
    std::string VA_Seq_To_String(std::vector<afrl::cmasi::VehicleAction*>& seq);
    std::string VehicleActionCommand_To_String(afrl::cmasi::VehicleActionCommand* command);
    std::string VehicleActionCommand_Seq_To_String(std::vector< afrl::cmasi::VehicleActionCommand* > seq);
    std::string WayPoint_To_String(afrl::cmasi::Waypoint* wp);
    std::string WP_Seq_To_String(std::vector<afrl::cmasi::Waypoint*>& seq);
    std::string MissionCommand_To_String(afrl::cmasi::MissionCommand* command);
    std::string MissionCommand_Seq_To_String(std::vector<afrl::cmasi::MissionCommand*>& seq);
    std::string KVP_Seq_To_String(std::vector<afrl::cmasi::KeyValuePair*> seq);
    std::string UniqueAutomationResponse_To_String(std::shared_ptr<uxas::messages::task::UniqueAutomationResponse> response);
    void Write_UniqueAutomationResponse_Map(std::string file, std::unordered_map<int64_t, std::shared_ptr<uxas::messages::task::UniqueAutomationResponse> > map);
    std::string TaskAssignment_To_String(std::shared_ptr<uxas::messages::task::TaskAssignment> task_assignment);
    std::string TaskAssignment_Seq_To_String(std::deque< std::shared_ptr<uxas::messages::task::TaskAssignment> > seq);
    std::string TaskAssignment_ptr_To_String(uxas::messages::task::TaskAssignment* task_assignment);
    std::string TaskAssignment_Vect_To_String(std::vector<uxas::messages::task::TaskAssignment*> seq);
    std::string TaskAssignmentSummary_To_String(std::shared_ptr<uxas::messages::task::TaskAssignmentSummary> task_assignment_summary);
    void Write_TaskAssignmentSummary_Map(std::string file, std::unordered_map<int64_t, std::shared_ptr<uxas::messages::task::TaskAssignmentSummary> > map);
    std::string ProjectedState_To_String(std::shared_ptr<uxas::service::PlanBuilderService::ProjectedState> state);
    std::string ProjectedState_Seq_To_String(std::vector< std::shared_ptr<uxas::service::PlanBuilderService::ProjectedState> >& seq);
    void Write_ProjectedState_Map(std::string file, std::unordered_map< int64_t, std::vector< std::shared_ptr<uxas::service::PlanBuilderService::ProjectedState> > > map);
    void Write_RemainingTaskAssignement_Map(std::string file, std::unordered_map< int64_t, std::deque< std::shared_ptr<uxas::messages::task::TaskAssignment> > > map);
    void Write_Int64_Map(std::string file, std::unordered_map< int64_t, int64_t > map);
    std::string EntityState_To_String(std::shared_ptr<afrl::cmasi::EntityState> entity_state);
    void Write_EntityState_Map(std::string file, std::unordered_map< int64_t, std::shared_ptr<afrl::cmasi::EntityState> > map);
    std::string SpeedAltPair_To_String(std::shared_ptr<afrl::impact::SpeedAltPair> sap);
    std::string SpeedAltPair_Seq_To_String(std::list<std::shared_ptr<afrl::impact::SpeedAltPair>>& seq);
    void Write_ReqeustIDVsOverrides_Map(std::string file, std::unordered_map< int64_t, std::list<std::shared_ptr<afrl::impact::SpeedAltPair>>> map);
    void writeStateToFile(const std::string File, uxas::service::PlanBuilderService* service);


    void Process_Task_Assignment_Summary_Test ();
    void Process_Task_Implementation_Response_Vehicle_Exists_WPList_Empty_Test();
    void Process_Task_Implementation_Response_Vehicle_Exists_WPList_NotEmpty_Test();
    void Process_Task_Implementation_Response_Vehicle_DoesNotExists_Test();
    void Check_Next_Task_Implementation_Request_Test();

};
}; //namespace test
}; //namespace service
}; //namespace uxas
