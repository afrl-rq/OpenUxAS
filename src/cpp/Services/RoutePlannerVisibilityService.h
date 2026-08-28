// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

/* 
 * File:   Component_RoutePlannerVisibility.h
 * Author: steve
 *
 * Created on February 19, 2015, 4:47 PM
 */


#ifndef UXAS_SERVICE_ROUTE_PLANNER_VISIBILITY_SERVICE_H
#define UXAS_SERVICE_ROUTE_PLANNER_VISIBILITY_SERVICE_H


#include "VisibilityGraph.h"

#include "uxas/messages/route/RouteRequest.h"
#include "uxas/messages/route/RoutePlanRequest.h"
#include "uxas/messages/route/RouteResponse.h"
#include "uxas/messages/route/RoutePlanResponse.h"

#include "afrl/cmasi/EntityState.h"
#include "afrl/cmasi/OperatingRegion.h"


#include "ServiceBase.h"


// forward class declaration of friend class
// such that the friend class can reuse route geometry computation code of this service without
// exposing the code as public/generally applicable
namespace zoneAlert {
    class SimpleZoneAlertComputer;
}

namespace uxas
{
namespace service
{

/*! \class RoutePlannerVisibilityService
    \brief A component that constructs plans/costs to be used for assignments.

 * 1) Receive KeepInZones/KeepOutZones/Tasks/RoutePlanRequests
 * 2) Build/Maintain Base Visibility Graph (Euclidean) from KeepInZones/KeepOutZones
 * 3) ???Construct, and send out, a RoutePlanResponse which includes minimum
 *    path lengths from each vehicle to each task and from each task to every other task.?????
 * 4) ???Construct, and send out, a ???Response which includes minimum waypoint paths
 *    paths for each plan request.?????
 * 
 * Configuration String: 
 *  <Service Type="RoutePlannerVisibilityService" TurnRadiusOffset_m="0.0" 
  *                OsmFileName="" MinimumWaypointSeparation_m="50.0"/> 
 * 
 * Options:
 *  - TurnRadiusOffset_m
 *  - OsmFileName
 *  - MinimumWaypointSeparation_m
 *  - 
 *  - 
 * 
 * Subscribed Messages:
 *  - afrl::cmasi::KeepOutZone
 *  - afrl::cmasi::KeepInZone
 *  - afrl::cmasi::OperatingRegion
 *  - afrl::cmasi::AirVehicleConfiguration
 *  - afrl::vehicles::GroundVehicleConfiguration
 *  - afrl::vehicles::SurfaceVehicleConfiguration
 *  - uxas::messages::route::RoutePlanRequest
 *  - AircraftPathPlanner
 *  - afrl::cmasi::AirVehicleState
 *  - afrl::vehicles::GroundVehicleState
 *  - afrl::vehicles::SurfaceVehicleState
 *  - uxas::messages::route::RouteRequest
 * 
 * Sent Messages:
 *  - uxas::messages::route::RoutePlanResponse
 * 
 */




class RoutePlannerVisibilityService : public ServiceBase
{
public:

    static const std::string&
    s_typeName() {
        static std::string s_string("RoutePlannerVisibilityService");
        return (s_string);
    };

    static const std::vector<std::string>
    s_registryServiceTypeNames()
    {
        std::vector<std::string> registryServiceTypeNames = {s_typeName()};
        return (registryServiceTypeNames);
    };
    
    static const std::string&
    s_directoryName() {
        static std::string s_string("");
        return (s_string);
    };

    static ServiceBase*
    create() {
        return new RoutePlannerVisibilityService;
    };

    RoutePlannerVisibilityService();

    virtual
    ~RoutePlannerVisibilityService();

    // declare a friend class of RoutePlannerVisibilityService
    // @RATIONALE: RoutePlannerVisibilityService computes geometry of keep-in and keep-out zones
    //      and uses this geometry to compute zone-observant routes. The ZoneAlertService
    //      is intended to alert when vehicles are about to violate zone geometry. As zone
    //      geometry is converted from abstract shapes (circle, polygon, etc.) with latitutde
    //      and longitude coordinates into a planar region on the Earth, it is important for that
    //      service to utilize the exact same geometry as routing, so as to avoid conflicts of 
    //      truth resulting from differeing computation.
    //      Ideally, zone geometry computation would be factored out of services and placed in planning
    //      Allowing all services to utilize the same geometric code. However, to minimize invasive 
    //      refactoring, ZoneAlert merely accesses the non-public geometric computations of the 
    //      RoutePlannerVisibilityService by being a friend of that class.
    //  @TODO: Refactor geometric computations to be available to any services that need to have the 
    //    same worldview of zones.
    friend class zoneAlert::SimpleZoneAlertComputer;

private:

    static
    ServiceBase::CreationRegistrar<RoutePlannerVisibilityService> s_registrar;

    /** brief Copy construction not permitted */
    RoutePlannerVisibilityService(RoutePlannerVisibilityService const&) = delete;

    /** brief Copy assignment operation not permitted */
    void operator=(RoutePlannerVisibilityService const&) = delete;

    bool
    configure(const pugi::xml_node& serviceXmlNode) override;

    bool
    initialize() override;

    //bool
    //start() override;

    //bool
    //terminate() override;

    bool
    processReceivedLmcpMessage(std::unique_ptr<uxas::communications::data::LmcpMessage> receivedLmcpMessage) override;


public:



public: //virtual





public:

protected:
    bool bProcessZone(const std::shared_ptr<afrl::cmasi::AbstractZone>& abstractZone, const bool& isKeepIn);
    bool bProcessOperatingRegion(const std::shared_ptr<afrl::cmasi::OperatingRegion>& operatingRegion);
    bool bProcessRouteRequest(const std::shared_ptr<uxas::messages::route::RouteRequest>& routeRequest);
    bool bProcessRoutePlanRequest(const std::shared_ptr<uxas::messages::route::RoutePlanRequest>& routePlanRequest,
            std::shared_ptr<uxas::messages::route::RoutePlanResponse>& routePlanResponse);
    static bool bFindPointsForAbstractGeometry(afrl::cmasi::AbstractGeometry* pAbstractGeometry, n_FrameworkLib::V_POSITION_t& vposBoundaryPoints);
    bool isCalculateWaypoints(const n_FrameworkLib::PTR_VISIBILITYGRAPH_t& visibilityGraph,
            const std::shared_ptr<n_FrameworkLib::CPathInformation>& pathInformation,
            const int64_t& vehicleId, const double& startHeading_deg, const double& endHeading_deg,
            std::vector<afrl::cmasi::Waypoint*>& planWaypoints,const n_FrameworkLib::CTrajectoryParameters::enPathType_t& enpathType);
    void calculatePlannerParameters(const std::shared_ptr<afrl::cmasi::EntityConfiguration>& enityConfiguration);

public:

    struct s_PlannerParameters
    {
        double turnRadius_m = {0};
        double nominalSpeed_mps = {0};
    };


protected:

    /*! \brief  storage for vehicle configurations*/
    std::map<uint64_t, std::shared_ptr<afrl::cmasi::EntityConfiguration>> m_idVsEntityConfiguration;
    /*! \brief  this is where vehicle states are stored*/
    std::map<uint64_t, std::shared_ptr<afrl::cmasi::EntityState>> m_idVsEntityState;
    /*! \brief  this is where planner parameters for each vehicle are stored*/
    std::map<uint64_t, std::shared_ptr<s_PlannerParameters>> m_idVsPlannerParameters;

    /*! \brief  storage for the "processed" keep-in/keep-out boundaries*/
    n_FrameworkLib::M_UI64_PTR_BOUNDARY_t m_idVsBoundary;
    /*! \brief  storage for operating region visibility graphs */
    std::map<int64_t, n_FrameworkLib::PTR_VISIBILITYGRAPH_t> m_operatingIdVsBaseVisibilityGraph;
    /*! \brief  storage for an openstreetmap based visibility graph */
    n_FrameworkLib::PTR_VISIBILITYGRAPH_t m_osmBaseVisibilityGraph;

    /*! \brief  this value is added to the run radius value for all vehicles.*/
    double m_turnRadiusOffset_m{0.0};

    /*! \brief  when this is set to true, the component will act both as a RouteAggregator and a RoutePlanner.*/
    bool m_isRoutAggregator{false};

    /*! \brief  //TODO:: Still needed????// storage for path planner results, saved for each vehicle ID*/
    //std::map<uint64_t,n_FrameworkLib::PTR_M_INT_PTR_M_INT_PATHINFORMATION_t> m_vehicleIdVsPlannerResults;

    double m_minimumWaypointSeparation_m = 50; //TODO:: this need to be configurable

private:




};

}; //namespace service
}; //namespace uxas

#endif /* UXAS_SERVICE_ROUTE_PLANNER_VISIBILITY_SERVICE_H */

