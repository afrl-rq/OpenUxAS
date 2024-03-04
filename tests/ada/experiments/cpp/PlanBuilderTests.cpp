#include "PlanBuilderTests.hpp"

#include <string>
#include <fstream>
#include <iostream>
#include <iomanip>

namespace uxas
{
namespace service
{
namespace test
{

    std::string PlanBuilderTests::PlanningState_To_String(uxas::messages::task::PlanningState* state) {
    std::ostringstream oss;
    oss.setf(std::ios_base::boolalpha | std::ios::scientific | std::ios::uppercase);
    oss.precision(14);
    oss << "PlanningState: EntityID =>   " << state->getEntityID()
        << ", Latitude =>  " << state->getPlanningPosition()->getLatitude()
        << ", Longitude =>  " << state->getPlanningPosition()->getLongitude()
        << ", Altitude =>  " << state->getPlanningPosition()->getAltitude()
        << ", AltitudeType =>  " << state->getPlanningPosition()->getAltitudeType()
        << ", Heading =>  " << state->getPlanningHeading();
    return oss.str();
    }

    std::string PlanBuilderTests::PlanningState_Seq_To_String(std::vector<uxas::messages::task::PlanningState*> & states) {
    std::ostringstream oss;
    for (auto element : states) {
        oss << PlanningState_To_String(element);
    }
    return oss.str();
    }

    std::string PlanBuilderTests::Int64_Seq_To_String(std::vector<int64_t>& seq) {
    std::ostringstream oss;
    oss.setf(std::ios_base::boolalpha | std::ios::scientific | std::ios::uppercase);
    oss.precision(14);
    for (auto element : seq) {
        oss << " " << element; // Convert each int64_t to string and append to stream
    }
    return oss.str();
    }

    std::string PlanBuilderTests::UniqueAutomationRequest_To_String(std::shared_ptr<uxas::messages::task::UniqueAutomationRequest> request) {
    std::ostringstream oss;
    oss.setf(std::ios_base::boolalpha | std::ios::scientific | std::ios::uppercase);
    oss.precision(14);
    oss << "UniqueAutomationRequest: RequestID =>  " << request->getRequestID()
        << ", OperatingRegion =>  " << request->getOriginalRequest()->getOperatingRegion()
        << ", RedoAllTasks =>  " << request->getOriginalRequest()->getRedoAllTasks()
        << ", Entity List: =>  " << Int64_Seq_To_String(request->getOriginalRequest()->getEntityList())
        << ", Task List: =>  " << Int64_Seq_To_String(request->getOriginalRequest()->getTaskList())
        << PlanningState_Seq_To_String(request->getPlanningStates())
        << ", SandboxRequest =>  " << request->getSandBoxRequest();
    return oss.str();
    }

    void PlanBuilderTests::Write_String_To_File(const std::string filename, const std::string& content) {
    std::ofstream outFile;
    outFile.setf(std::ios::scientific | std::ios::uppercase);
    outFile.open(filename, std::ios::out | std::ios::app);
    if (!outFile.is_open()) {
        std::cerr << "Error opening file: " << filename << std::endl;
        return;
    }
    outFile << std::fixed << std::setprecision(14) << content;
    outFile << "\n";
    outFile.close();
    }

    void PlanBuilderTests::Write_UniqueAutomationRequest_Map(const std::string filename, std::unordered_map<int64_t, std::shared_ptr<uxas::messages::task::UniqueAutomationRequest> > map)
    {
        for (auto& pair : map) {
            Write_String_To_File (filename, UniqueAutomationRequest_To_String(pair.second));
        }
    }

    std::string PlanBuilderTests::VehicleAction_To_String(afrl::cmasi::VehicleAction* va)
    {
        std::ostringstream oss;
        oss.setf(std::ios_base::boolalpha | std::ios::scientific | std::ios::uppercase);
        oss.precision(14);
        oss << ", VA =>  " + Int64_Seq_To_String(va->getAssociatedTaskList());
        return oss.str();
    }

    std::string PlanBuilderTests::VA_Seq_To_String(std::vector<afrl::cmasi::VehicleAction*>& seq) {
        std::ostringstream oss;
        oss.setf(std::ios_base::boolalpha | std::ios::scientific | std::ios::uppercase);
        oss.precision(14);
        for (auto element : seq) {
            oss <<  VehicleAction_To_String(element);
        }
        return oss.str();
    }

    std::string PlanBuilderTests::VehicleActionCommand_To_String(afrl::cmasi::VehicleActionCommand* command) {
        std::ostringstream oss;
        oss.setf(std::ios_base::boolalpha | std::ios::scientific | std::ios::uppercase);
        oss.precision(14);
        oss << "VehicleActionCommand: CommandId =>  " << command->getCommandID()
            << ", VehicleId =>  " << command->getVehicleID()
            << VA_Seq_To_String(command->getVehicleActionList())
            << ", Status =>  " << command->getStatus();
        return oss.str();
    }

    std::string PlanBuilderTests::VehicleActionCommand_Seq_To_String(std::vector< afrl::cmasi::VehicleActionCommand* > seq) {
        std::ostringstream oss;
        oss.setf(std::ios_base::boolalpha | std::ios::scientific | std::ios::uppercase);
        oss.precision(14);
        for (auto element : seq) {
            oss <<  VehicleActionCommand_To_String(element);
        }
        return oss.str();
    }

    std::string PlanBuilderTests::WayPoint_To_String(afrl::cmasi::Waypoint* wp) {
        std::ostringstream oss;
        oss.setf(std::ios_base::boolalpha | std::ios::scientific | std::ios::uppercase);
        oss.precision(14);
        oss << "Waypoint: Number =>  " << wp->getNumber()
            << ", Latitude =>  " << wp->getLatitude()
            << ", Longitude =>  " << wp->getLongitude()
            << ", Altitude =>  " << wp->getAltitude()
            << ", AltitudeType =>  " << wp->getAltitudeType()
            << ", NextWaypoint =>  " << wp->getNextWaypoint()
            << ", Speed =>  " << wp->getSpeed()
            << ", SpeedType =>  " << wp->getSpeedType()
            << ", ClimbRate =>  " << wp->getClimbRate()
            << ", TurnType =>  " << wp->getTurnType()
            << ", ContingencyWaypointA =>  " << wp->getContingencyWaypointA()
            << ", ContingencyWaypointB =>  " << wp->getContingencyWaypointB()
            << VA_Seq_To_String(wp->getVehicleActionList())
            << Int64_Seq_To_String(wp->getAssociatedTasks());
        return oss.str();
    }

    std::string PlanBuilderTests::WP_Seq_To_String(std::vector<afrl::cmasi::Waypoint*>& seq) {
        std::ostringstream oss;
        oss.setf(std::ios_base::boolalpha | std::ios::scientific | std::ios::uppercase);
        oss.precision(14);
        for (auto element : seq) {
            oss <<  WayPoint_To_String(element);
        }
        return oss.str();
    }

    std::string PlanBuilderTests::MissionCommand_To_String(afrl::cmasi::MissionCommand* command) {
        std::ostringstream oss;
        oss.setf(std::ios_base::boolalpha | std::ios::scientific | std::ios::uppercase);
        oss.precision(14);
        oss << "MissionCommand: CommandId =>  " << command->getCommandID()
            << ", VehicleId =>  " << command->getVehicleID()
            << VA_Seq_To_String(command->getVehicleActionList())
            << ", Status =>  " << command->getStatus()
            << WP_Seq_To_String(command->getWaypointList())
            << ", FirstWaypoint =>  " << command->getFirstWaypoint();
        return oss.str();
    }

    std::string PlanBuilderTests::MissionCommand_Seq_To_String(std::vector<afrl::cmasi::MissionCommand*>& seq) {
        std::ostringstream oss;
        oss.setf(std::ios_base::boolalpha | std::ios::scientific | std::ios::uppercase);
        oss.precision(14);
        for (auto element : seq) {
            oss <<  MissionCommand_To_String(element);
        }
        return oss.str();
    }

    std::string PlanBuilderTests::KVP_Seq_To_String(std::vector<afrl::cmasi::KeyValuePair*> seq) {
        std::ostringstream oss;
        oss.setf(std::ios_base::boolalpha | std::ios::scientific | std::ios::uppercase);
        oss.precision(14);
        for (auto& element : seq) {
            oss << element->getKey() << element->getValue();
        }
        return oss.str();
    }

    std::string PlanBuilderTests::UniqueAutomationResponse_To_String(std::shared_ptr<uxas::messages::task::UniqueAutomationResponse> response) {
        std::ostringstream oss;
        oss.setf(std::ios_base::boolalpha | std::ios::scientific | std::ios::uppercase);
        oss.precision(14);
        oss << "UniqueAutomationResponse: ResponseID =>  " << response->getResponseID()
            << MissionCommand_Seq_To_String(response->getOriginalResponse()->getMissionCommandList())
            << VehicleActionCommand_Seq_To_String(response->getOriginalResponse()->getVehicleCommandList())
            << KVP_Seq_To_String(response->getOriginalResponse()->getInfo())
            << PlanningState_Seq_To_String(response->getFinalStates());
        return oss.str();
    }

    void PlanBuilderTests::Write_UniqueAutomationResponse_Map(std::string file, std::unordered_map<int64_t, std::shared_ptr<uxas::messages::task::UniqueAutomationResponse> > map) {
        for (auto& pair : map)  {
            Write_String_To_File(file, UniqueAutomationResponse_To_String(pair.second));
        }
    }

    std::string PlanBuilderTests::TaskAssignment_To_String(std::shared_ptr<uxas::messages::task::TaskAssignment> task_assignment) {
        std::ostringstream oss;
        oss.setf(std::ios_base::boolalpha | std::ios::scientific | std::ios::uppercase);
        oss.precision(14);
        oss << "TaskAssignment: TaskID =>  " << task_assignment->getTaskID()
            << ", OptionID =>  " << task_assignment->getOptionID()
            << ", AssignedVehicle =>  " << task_assignment->getAssignedVehicle()
            << ", TimeThreshold =>  " << task_assignment->getTimeThreshold()
            << ", TimeTaskCompleted =>  " << task_assignment->getTimeTaskCompleted();
        return oss.str();
    }

    std::string PlanBuilderTests::TaskAssignment_Seq_To_String(std::deque< std::shared_ptr<uxas::messages::task::TaskAssignment> > seq) {
        std::ostringstream oss;
        oss.setf(std::ios_base::boolalpha | std::ios::scientific | std::ios::uppercase);
        oss.precision(14);
        for (auto element : seq) {
            if (element) {
                oss << TaskAssignment_To_String(element);
            }
        }
        return oss.str();
    }

    std::string PlanBuilderTests::TaskAssignment_ptr_To_String(uxas::messages::task::TaskAssignment* task_assignment) {
        std::ostringstream oss;
        oss.setf(std::ios_base::boolalpha | std::ios::scientific | std::ios::uppercase);
        oss.precision(14);
        oss << "TaskAssignment: TaskID =>  "  << task_assignment->getTaskID()
            << ", OptionID =>  " << task_assignment->getOptionID()
            << ", AssignedVehicle =>  " << task_assignment->getAssignedVehicle()
            << ", TimeThreshold =>  " << task_assignment->getTimeThreshold()
            << ", TimeTaskCompleted =>  " << task_assignment->getTimeTaskCompleted();
        return oss.str();
    }

    std::string PlanBuilderTests::TaskAssignment_Vect_To_String(std::vector<uxas::messages::task::TaskAssignment*> seq) {
        std::ostringstream oss;
        oss.setf(std::ios_base::boolalpha | std::ios::scientific | std::ios::uppercase);
        oss.precision(14);
        for (auto element : seq) {
            if (element) {
                oss << TaskAssignment_ptr_To_String(element);
            }
        }
        return oss.str();
    }

    std::string PlanBuilderTests::TaskAssignmentSummary_To_String(std::shared_ptr<uxas::messages::task::TaskAssignmentSummary> task_assignment_summary) {
        std::ostringstream oss;
        oss.setf(std::ios_base::boolalpha | std::ios::scientific | std::ios::uppercase);
        oss.precision(14);
        oss << "TaskAssignmentSummary: CorrespondingAutomationRequestID =>  " << task_assignment_summary->getCorrespondingAutomationRequestID()
            << ", OperatingRegion =>  " << task_assignment_summary->getOperatingRegion()
            << TaskAssignment_Vect_To_String(task_assignment_summary->getTaskList());
        return oss.str();
    }

    void PlanBuilderTests::Write_TaskAssignmentSummary_Map(std::string file, std::unordered_map<int64_t, std::shared_ptr<uxas::messages::task::TaskAssignmentSummary> > map) {
        for (auto& pair : map)  {
            Write_String_To_File(file, TaskAssignmentSummary_To_String(pair.second));
        }
    }

    std::string PlanBuilderTests::ProjectedState_To_String(std::shared_ptr<uxas::service::PlanBuilderService::ProjectedState> state) {
        std::ostringstream oss;
        oss.setf(std::ios_base::boolalpha | std::ios::scientific | std::ios::uppercase);
        oss.precision(14);
        oss << "ProjectedState: FinalWaypointID =>  " << state->finalWaypointID
            << ", Time => " << state->time
            << PlanningState_To_String(state->state);
        return oss.str();
    }

    std::string PlanBuilderTests::ProjectedState_Seq_To_String(std::vector< std::shared_ptr<uxas::service::PlanBuilderService::ProjectedState> >& seq) {
        std::ostringstream oss;
        oss.setf(std::ios_base::boolalpha | std::ios::scientific | std::ios::uppercase);
        oss.precision(14);
        for (auto element : seq) {
            if (element) {
                oss << ProjectedState_To_String(element);
            }
        }
        return oss.str();
    }

    void PlanBuilderTests::Write_ProjectedState_Map(std::string file, std::unordered_map< int64_t, std::vector< std::shared_ptr<uxas::service::PlanBuilderService::ProjectedState> > > map) {
        for (auto& pair : map)  {
            Write_String_To_File(file, ProjectedState_Seq_To_String(pair.second));
        }
    }

    void PlanBuilderTests::Write_RemainingTaskAssignement_Map(std::string file, std::unordered_map< int64_t, std::deque< std::shared_ptr<uxas::messages::task::TaskAssignment> > > map) {
        for (auto& pair : map)  {
            Write_String_To_File(file, TaskAssignment_Seq_To_String(pair.second));
        }
    }

    void PlanBuilderTests::Write_Int64_Map(std::string file, std::unordered_map< int64_t, int64_t > map) {
        for (auto& pair : map)  {
            Write_String_To_File(file, " " + std::to_string(pair.second));
        }
    }

    std::string PlanBuilderTests::EntityState_To_String(std::shared_ptr<afrl::cmasi::EntityState> entity_state) {
        std::ostringstream oss;
        oss.setf(std::ios_base::boolalpha | std::ios::scientific | std::ios::uppercase);
        oss.precision(14);
        oss << "EntityState: Id =>  " << entity_state->getID()
            << ", Latitude =>  " << entity_state->getLocation()->getLatitude()
            << ", Longitude =>  " << entity_state->getLocation()->getLongitude()
            << ", Altitude =>  " << entity_state->getLocation()->getAltitude()
            << ", AltitudeType =>  " << entity_state->getLocation()->getAltitudeType()
            << ", Heading =>  " << entity_state->getHeading()
            << ", Time =>  " << entity_state->getTime();
        return oss.str();
    }

    void PlanBuilderTests::Write_EntityState_Map(std::string file, std::unordered_map< int64_t, std::shared_ptr<afrl::cmasi::EntityState> > map) {
        for (auto& pair : map)  {
            Write_String_To_File(file, EntityState_To_String(pair.second));
        }
    }

    std::string PlanBuilderTests::SpeedAltPair_To_String(std::shared_ptr<afrl::impact::SpeedAltPair> sap) {
        std::ostringstream oss;
        oss.setf(std::ios_base::boolalpha | std::ios::scientific | std::ios::uppercase);
        oss.precision(14);
        oss << "SpeedAltPair: VehicleID =>  " << sap->getVehicleID()
            << ", TaskID =>  " << sap->getTaskID()
            << ", Speed =>  " << sap->getSpeed()
            << ", Altitude =>  " << sap->getAltitude()
            << ", AltitudeType =>  " << sap->getAltitudeType();
    return oss.str();
    }

    std::string PlanBuilderTests::SpeedAltPair_Seq_To_String(std::list<std::shared_ptr<afrl::impact::SpeedAltPair>>& seq) {
        std::ostringstream oss;
        oss.setf(std::ios_base::boolalpha | std::ios::scientific | std::ios::uppercase);
        oss.precision(14);
        for (auto element : seq) {
            oss << " " << SpeedAltPair_To_String(element); // Convert each int64_t to string and append to stream
        }
        return oss.str();
    }

    void PlanBuilderTests::Write_ReqeustIDVsOverrides_Map(std::string file, std::unordered_map< int64_t, std::list<std::shared_ptr<afrl::impact::SpeedAltPair>>> map) {
        for (auto& pair : map)  {
            Write_String_To_File(file, SpeedAltPair_Seq_To_String(pair.second));
        }
    }

    void PlanBuilderTests::writeStateToFile(const std::string File, uxas::service::PlanBuilderService* service)
    {
        // Write the content of m_uniqueAutomationRequests
        Write_String_To_File (File, "m_uniqueAutomationRequests:");
        Write_UniqueAutomationRequest_Map (File, service->m_uniqueAutomationRequests);

        // Write the content of UniqueAutomationResponse_Map
        Write_String_To_File (File, "UniqueAutomationResponse_Map:");
        Write_UniqueAutomationResponse_Map (File, service->m_inProgressResponse);

        // Write the content of UniqueAutomationResponse_Map
        Write_String_To_File (File, "m_assignmentSummaries:");
        Write_TaskAssignmentSummary_Map (File, service->m_assignmentSummaries);

        // Write the content of m_projectedEntityStates
        Write_String_To_File (File, "m_projectedEntityStates:");
        Write_ProjectedState_Map (File, service->m_projectedEntityStates);

        // Write the content of m_remainingAssignments
        Write_String_To_File (File, "m_remainingAssignments:");
        Write_RemainingTaskAssignement_Map (File, service->m_remainingAssignments);

        // Write the content of m_remainingAssignments
        Write_String_To_File (File, "m_expectedResponseID:");
        Write_Int64_Map (File, service->m_expectedResponseID);

        // Write the content of EntityState_Map
        Write_String_To_File (File, "EntityState_Map:");
        Write_EntityState_Map (File, service->m_currentEntityStates);

        // Write the content of EntityState_Map
        Write_String_To_File (File, "m_reqeustIDVsOverrides:");
        Write_ReqeustIDVsOverrides_Map (File, service->m_reqeustIDVsOverrides);
    }

    void PlanBuilderTests::Process_Task_Assignment_Summary_Test ()
    {
    uxas::service::PlanBuilderService* service = new uxas::service::PlanBuilderService();

    std::shared_ptr<uxas::messages::task::TaskAssignmentSummary> taskAssignmentSummary = std::make_shared<uxas::messages::task::TaskAssignmentSummary>();
    taskAssignmentSummary->setCorrespondingAutomationRequestID (1);

    uxas::messages::task::TaskAssignment* task1 = new uxas::messages::task::TaskAssignment();
    uxas::messages::task::TaskAssignment* task2 = new uxas::messages::task::TaskAssignment();

    task1->setTaskID(10LL);
    task2->setTaskID(11LL);

    std::vector<uxas::messages::task::TaskAssignment*>& taskList = taskAssignmentSummary->getTaskList();
    taskList.push_back(task1);
    taskList.push_back(task2);

    std::shared_ptr<afrl::cmasi::EntityState> entityStatePtr = std::make_shared<afrl::cmasi::EntityState>();
    entityStatePtr->setID(100);
    afrl::cmasi::Location3D* location = new afrl::cmasi::Location3D();
    location->setLatitude(1.0);
    location->setLongitude(1.0);
    location->setAltitude(1.0);
    location->setAltitudeType(afrl::cmasi::AltitudeType::MSL);
    entityStatePtr->setLocation(location);
    entityStatePtr->setHeading(0.0f);
    entityStatePtr->setGroundspeed(54.5f);
    std::vector<int64_t>& tasks = entityStatePtr->getAssociatedTasks();
    tasks.push_back(1);
    tasks.push_back(2);
    entityStatePtr->setTime(350);
    service->m_currentEntityStates[entityStatePtr->getID()] = entityStatePtr;

    std::shared_ptr<afrl::cmasi::EntityState> entityStatePtr2 = std::make_shared<afrl::cmasi::EntityState>();
    entityStatePtr2->setID(101);
    afrl::cmasi::Location3D* _location = new afrl::cmasi::Location3D();
    _location->setLatitude(1.0);
    _location->setLongitude(1.0);
    _location->setAltitude(1.0);
    _location->setAltitudeType(afrl::cmasi::AltitudeType::MSL);
    entityStatePtr2->setLocation(_location);
    entityStatePtr2->setHeading(0.0f);
    entityStatePtr2->setGroundspeed(54.5f);

    std::vector<int64_t>& tasks2 = entityStatePtr2->getAssociatedTasks();
    tasks2.push_back(1);
    tasks2.push_back(2);
    entityStatePtr2->setTime(350);

    service->m_currentEntityStates[entityStatePtr2->getID()] = entityStatePtr2;

    std::shared_ptr<uxas::messages::task::UniqueAutomationRequest> uniqueAutomationRequest = std::make_shared<uxas::messages::task::UniqueAutomationRequest>();
    service->m_uniqueAutomationRequests[taskAssignmentSummary->getCorrespondingAutomationRequestID()] = uniqueAutomationRequest;
    service->m_uniqueAutomationRequests[taskAssignmentSummary->getCorrespondingAutomationRequestID()]->setRequestID(taskAssignmentSummary->getCorrespondingAutomationRequestID());
    service->m_uniqueAutomationRequests[taskAssignmentSummary->getCorrespondingAutomationRequestID()]->getOriginalRequest()->getEntityList().push_back(100);
    service->m_uniqueAutomationRequests[taskAssignmentSummary->getCorrespondingAutomationRequestID()]->getOriginalRequest()->getEntityList().push_back(101);

    auto planState1 = new uxas::messages::task::PlanningState;
    planState1->setEntityID(100);
    afrl::cmasi::Location3D* location1 = new afrl::cmasi::Location3D();
    location1->setLatitude(1.0);
    location1->setLongitude(1.0);
    location1->setAltitude(1.0);
    location1->setAltitudeType(afrl::cmasi::AltitudeType::MSL);
    planState1->setPlanningPosition(location1);
    planState1->setPlanningHeading(0.0);
    service->m_uniqueAutomationRequests[taskAssignmentSummary->getCorrespondingAutomationRequestID()]->getPlanningStates().push_back(planState1);

    auto planState2 = new uxas::messages::task::PlanningState;
    planState2->setEntityID(100);
    afrl::cmasi::Location3D* location2 = new afrl::cmasi::Location3D();
    location2->setLatitude(2.0);
    location2->setLongitude(2.0);
    location2->setAltitude(2.0);
    location2->setAltitudeType(afrl::cmasi::AltitudeType::AGL);
    planState2->setPlanningPosition(location2);
    planState2->setPlanningHeading(1.0);
    service->m_uniqueAutomationRequests[taskAssignmentSummary->getCorrespondingAutomationRequestID()]->getPlanningStates().push_back(planState2);

    auto planState3 = new uxas::messages::task::PlanningState;
    planState3->setEntityID(101);
    afrl::cmasi::Location3D* location3 = new afrl::cmasi::Location3D();
    location3->setLatitude(3.0);
    location3->setLongitude(3.0);
    location3->setAltitude(3.0);
    location3->setAltitudeType(afrl::cmasi::AltitudeType::MSL);
    planState3->setPlanningPosition(location3);
    planState3->setPlanningHeading(0.0);
    service->m_uniqueAutomationRequests[taskAssignmentSummary->getCorrespondingAutomationRequestID()]->getPlanningStates().push_back(planState3);

    auto planState4 = new uxas::messages::task::PlanningState;
    planState4->setEntityID(102);
    afrl::cmasi::Location3D* location4 = new afrl::cmasi::Location3D();
    location4->setLatitude(4.0);
    location4->setLongitude(4.0);
    location4->setAltitude(4.0);
    location4->setAltitudeType(afrl::cmasi::AltitudeType::AGL);
    planState4->setPlanningPosition(location4);
    planState4->setPlanningHeading(1.0);
    service->m_uniqueAutomationRequests[taskAssignmentSummary->getCorrespondingAutomationRequestID()]->getPlanningStates().push_back(planState4);

    service->processTaskAssignmentSummary(taskAssignmentSummary);

    writeStateToFile("Cpp_Process_Task_Assignment_Summary_Test.txt", service);
    }

    void PlanBuilderTests::Process_Task_Implementation_Response_Vehicle_Exists_WPList_Empty_Test ()
    {
    uxas::service::PlanBuilderService* service = new uxas::service::PlanBuilderService();

    std::shared_ptr<uxas::messages::task::TaskImplementationResponse> taskImplementationResponse = std::make_shared<uxas::messages::task::TaskImplementationResponse>();
    taskImplementationResponse->setResponseID(1);
    taskImplementationResponse->setVehicleID(100);

    std::shared_ptr<uxas::messages::task::UniqueAutomationResponse> Response_In_Progress = std::make_shared<uxas::messages::task::UniqueAutomationResponse>();
    Response_In_Progress->setResponseID(1);

    service->m_expectedResponseID[1] = 1;

    afrl::cmasi::Waypoint* waypoint1 = new afrl::cmasi::Waypoint();
    afrl::cmasi::Waypoint* waypoint2 = new afrl::cmasi::Waypoint();
    waypoint1->setNumber(1);
    waypoint2->setNumber(2);
    taskImplementationResponse->getTaskWaypoints().push_back(waypoint1);
    taskImplementationResponse->getTaskWaypoints().push_back(waypoint2);

    afrl::cmasi::MissionCommand* missioncommand1 = new afrl::cmasi::MissionCommand();
    afrl::cmasi::MissionCommand* missioncommand2 = new afrl::cmasi::MissionCommand();
    missioncommand1->setVehicleID(100);
    missioncommand2->setVehicleID(101);
    Response_In_Progress->getOriginalResponse()->getMissionCommandList().push_back(missioncommand1);
    Response_In_Progress->getOriginalResponse()->getMissionCommandList().push_back(missioncommand2);

    service->m_inProgressResponse[1] = Response_In_Progress;

    service->processTaskImplementationResponse(taskImplementationResponse);

    writeStateToFile("Cpp_Process_Task_Implementation_Response_Vehicle_Exists_WPList_Empty_Test.txt", service);

    }

    void PlanBuilderTests::Process_Task_Implementation_Response_Vehicle_Exists_WPList_NotEmpty_Test ()
    {
    uxas::service::PlanBuilderService* service = new uxas::service::PlanBuilderService();

    std::shared_ptr<uxas::messages::task::TaskImplementationResponse> taskImplementationResponse = std::make_shared<uxas::messages::task::TaskImplementationResponse>();
    taskImplementationResponse->setResponseID(1);
    taskImplementationResponse->setVehicleID(100);

    std::shared_ptr<uxas::messages::task::UniqueAutomationResponse> Response_In_Progress = std::make_shared<uxas::messages::task::UniqueAutomationResponse>();
    Response_In_Progress->setResponseID(1);

    service->m_expectedResponseID[1] = 1;

    afrl::cmasi::Waypoint* waypoint1 = new afrl::cmasi::Waypoint();
    afrl::cmasi::Waypoint* waypoint2 = new afrl::cmasi::Waypoint();
    waypoint1->setNumber(1);
    waypoint2->setNumber(2);
    taskImplementationResponse->getTaskWaypoints().push_back(waypoint1);
    taskImplementationResponse->getTaskWaypoints().push_back(waypoint2);

    afrl::cmasi::MissionCommand* missioncommand1 = new afrl::cmasi::MissionCommand;
    afrl::cmasi::MissionCommand* missioncommand2 = new afrl::cmasi::MissionCommand;
    missioncommand1->setVehicleID(100);
    afrl::cmasi::Waypoint* waypoint3 = new afrl::cmasi::Waypoint();
    afrl::cmasi::Waypoint* waypoint4 = new afrl::cmasi::Waypoint();
    waypoint3->setNumber(1);
    waypoint4->setNumber(2);
    missioncommand1->getWaypointList().push_back(waypoint3);
    missioncommand1->getWaypointList().push_back(waypoint4);
    missioncommand2->setVehicleID(101);
    Response_In_Progress->getOriginalResponse()->getMissionCommandList().push_back(missioncommand1);
    Response_In_Progress->getOriginalResponse()->getMissionCommandList().push_back(missioncommand2);

    service->m_inProgressResponse[1] = Response_In_Progress;

    service->processTaskImplementationResponse(taskImplementationResponse);

    writeStateToFile("Cpp_Process_Task_Implementation_Response_Vehicle_Exists_WPList_NotEmpty_Test.txt", service);
    }

    void PlanBuilderTests::Process_Task_Implementation_Response_Vehicle_DoesNotExists_Test ()
    {
    uxas::service::PlanBuilderService* service = new uxas::service::PlanBuilderService();

    auto taskImplementationResponse = std::make_shared<uxas::messages::task::TaskImplementationResponse>();
    taskImplementationResponse->setResponseID(1);
    taskImplementationResponse->setVehicleID(102);

    std::shared_ptr<uxas::messages::task::UniqueAutomationResponse> Response_In_Progress = std::make_shared<uxas::messages::task::UniqueAutomationResponse>();
    Response_In_Progress->setResponseID(1);

    service->m_expectedResponseID[1] = 1;

    afrl::cmasi::Waypoint* waypoint1 = new afrl::cmasi::Waypoint();
    afrl::cmasi::Waypoint* waypoint2 = new afrl::cmasi::Waypoint();
    waypoint1->setNumber(1);
    waypoint2->setNumber(2);
    taskImplementationResponse->getTaskWaypoints().push_back(waypoint1);
    taskImplementationResponse->getTaskWaypoints().push_back(waypoint2);

    afrl::cmasi::MissionCommand* missioncommand1 = new afrl::cmasi::MissionCommand;
    afrl::cmasi::MissionCommand* missioncommand2 = new afrl::cmasi::MissionCommand;
    missioncommand1->setVehicleID(100);
    missioncommand1->getWaypointList().push_back(waypoint1);
    missioncommand1->getWaypointList().push_back(waypoint2);
    missioncommand2->setVehicleID(101);
    Response_In_Progress->getOriginalResponse()->getMissionCommandList().push_back(missioncommand1);
    Response_In_Progress->getOriginalResponse()->getMissionCommandList().push_back(missioncommand2);

    service->m_inProgressResponse[1] = Response_In_Progress;

    service->processTaskImplementationResponse(taskImplementationResponse);

    writeStateToFile("Cpp_Process_Task_Implementation_Response_Vehicle_DoesNotExists_Test.txt", service);
    }

    void PlanBuilderTests::Check_Next_Task_Implementation_Request_Test()
    {
    uxas::service::PlanBuilderService* service = new uxas::service::PlanBuilderService();

    std::vector< std::shared_ptr<uxas::service::PlanBuilderService::ProjectedState> > projectedStates;
    uxas::messages::task::PlanningState* state = new uxas::messages::task::PlanningState;
    afrl::cmasi::Location3D* location = new afrl::cmasi::Location3D();
    location->setLatitude(0.0);
    location->setLongitude(0.0);
    location->setAltitude(0.0);
    location->setAltitudeType(afrl::cmasi::AltitudeType::MSL);
    state->setPlanningPosition(location);
    state->setPlanningHeading(0.0);
    state->setEntityID(1);

    std::shared_ptr<uxas::service::PlanBuilderService::ProjectedState> projectedstate = std::make_shared<uxas::service::PlanBuilderService::ProjectedState>();
    projectedstate->setState(state);
    projectedStates.push_back(projectedstate);

    afrl::cmasi::Waypoint* waypoint1 = new afrl::cmasi::Waypoint();
    afrl::cmasi::Waypoint* waypoint2 = new afrl::cmasi::Waypoint();
    waypoint1->setNumber(1);
    waypoint2->setNumber(2);

    std::shared_ptr<uxas::messages::task::UniqueAutomationResponse> Response_In_Progress = std::make_shared<uxas::messages::task::UniqueAutomationResponse>();;

    afrl::cmasi::MissionCommand* missioncommand1 = new afrl::cmasi::MissionCommand;
    afrl::cmasi::MissionCommand* missioncommand2 = new afrl::cmasi::MissionCommand;
    missioncommand1->setVehicleID(100);
    missioncommand1->getWaypointList().push_back(waypoint1);
    missioncommand1->getWaypointList().push_back(waypoint2);
    missioncommand2->setVehicleID(101);
    Response_In_Progress->getOriginalResponse()->getMissionCommandList().push_back(missioncommand1);
    Response_In_Progress->getOriginalResponse()->getMissionCommandList().push_back(missioncommand2);

    service->m_inProgressResponse[1] = Response_In_Progress;

    std::shared_ptr<afrl::impact::SpeedAltPair> SpeedAltPair_1 = std::make_shared<afrl::impact::SpeedAltPair>();
    SpeedAltPair_1->setVehicleID(1);
    SpeedAltPair_1->setTaskID(0);
    SpeedAltPair_1->setSpeed(0.00000E+00);
    SpeedAltPair_1->setAltitudeType(afrl::cmasi::AltitudeType::AGL);
    std::shared_ptr<afrl::impact::SpeedAltPair> SpeedAltPair_2 = std::make_shared<afrl::impact::SpeedAltPair>();
    SpeedAltPair_2->setVehicleID(2);
    SpeedAltPair_2->setTaskID(0);
    SpeedAltPair_2->setSpeed(0.00000E+00);
    SpeedAltPair_2->setAltitudeType(afrl::cmasi::AltitudeType::AGL);
    std::list<std::shared_ptr<afrl::impact::SpeedAltPair>> list;
    list.push_back(SpeedAltPair_1);
    list.push_back(SpeedAltPair_2);
    service->m_reqeustIDVsOverrides[1] = list;

    std::shared_ptr<afrl::impact::SpeedAltPair> SpeedAltPair_3  = std::make_shared<afrl::impact::SpeedAltPair>();
    SpeedAltPair_3->setVehicleID(2);
    SpeedAltPair_3->setTaskID(0);
    SpeedAltPair_3->setSpeed(0.00000E+00);
    SpeedAltPair_3->setAltitudeType(afrl::cmasi::AltitudeType::AGL);
    std::shared_ptr<afrl::impact::SpeedAltPair> SpeedAltPair_4  = std::make_shared<afrl::impact::SpeedAltPair>();
    SpeedAltPair_4->setVehicleID(2);
    SpeedAltPair_4->setTaskID(0);
    SpeedAltPair_4->setSpeed(0.00000E+00);
    SpeedAltPair_4->setAltitudeType(afrl::cmasi::AltitudeType::AGL);
    std::list<std::shared_ptr<afrl::impact::SpeedAltPair>> list2;
    list2.push_back(SpeedAltPair_3);
    list2.push_back(SpeedAltPair_4);
    service->m_reqeustIDVsOverrides[2] = list2;

    service->checkNextTaskImplementationRequest(1);
    }

}; //namespace test
}; //namespace service
}; //namespace uxas
