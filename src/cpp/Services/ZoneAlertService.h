// ===============================================================================
// Authors: Rowanhill, Dependable Computing LLC
// Organization: Dependable Computing LLC
// 
// Copyright (c) 2024 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

/* 
 * File:   ZoneAlertService.h
 * Author: Rowanhill
 *
 * Created on February 4, 2024, 11:36 AM
 */

#ifndef UXAS_ZONE_ALERT_SERVICE_H
#define UXAS_ZONE_ALERT_SERVICE_H

#include "afrl/cmasi/AbstractZone.h"
#include "afrl/cmasi/AirVehicleConfiguration.h"
#include "afrl/cmasi/AirVehicleState.h"
#include "dcllc/zonealert/ImminentZoneViolation.h"

#include "ServiceBase.h"
#include "TypeDefs/UxAS_TypeDefs_Timer.h"

#include "ZoneAlertComputer.h"

//using namespace std;

using namespace afrl;
using namespace cmasi;


namespace uxas
{
namespace service
{

/*! @class ZoneAlertService
 *
 * @brief This is a service to alert subscribers when a vehicle's last reported instantaneous linear trajectory 
 * is predicted to violate one or more zones.
 *
 * 
 * TODO:
 * <li> include the new service header file in ServiceManager.cpp</li>
 * <li> add a dummy instance of the new service in ServiceManager.cpp, e.g.
 * {auto svc = uxas::stduxas::make_unique<uxas::service::MyNewService>();} 
 * Note: this is required to link the new service in when building UxAS</li>
 *  
 * </ul> @n
 * 
 * Configuration String: <Service Type="ZoneAlertService" OptionString="Option_01" OptionInt="36" />
 * 
 * Options:
 *  - OptionString - sample string option
 *  - OptionInt - sample integer option
 * 
 * Subscribed Messages:
 *  - afrl::cmasi::KeyValuePair
 * 
 * Sent Messages:
 *  - afrl::cmasi::KeyValuePair
 * 
 * 
 * @requirements SR-2, SR-3, SR-4 SR-5, Sr-6, SR-6-1, SR-6-2, SR-6-4, SR-7, SR-7-2
 * 
 *
 *  (these are really while the service is alive)
 * 
 */

class ZoneAlertService : public ServiceBase
{
public:

    /** \brief This string is used to identify this service in XML configuration
     * files, i.e. Service Type="ZoneAlertService". It is also entered into
     * service registry and used to create new instances of this service. */
    static const std::string& s_typeName() {
        static std::string s_string("ZoneAlertService");
        return (s_string);
    };

    static const std::vector<std::string> s_registryServiceTypeNames() {
        std::vector<std::string> registryServiceTypeNames = {s_typeName()};
        return (registryServiceTypeNames);
    };

    /** \brief If this string is not empty, it is used to create a data 
     * directory to be used by the service. The path to this directory is
     * accessed through the ServiceBase variable m_workDirectoryPath. */
    static const std::string& s_directoryName() { static std::string s_string(""); return (s_string); };

    static ServiceBase* create() {
        return new ZoneAlertService;
    };

    ZoneAlertService();

    virtual
    ~ZoneAlertService();

private:

    static
    ServiceBase::CreationRegistrar<ZoneAlertService> s_registrar;

    //---- Standard OpenUxAS Service Interface ---//

    ZoneAlertService(ZoneAlertService const&) = delete;

    /** brief Copy assignment operation not permitted */
    void operator=(ZoneAlertService const&) = delete;

    /**
     * @requirements SR-1, SR-2-1, SR-2-2-2, SR-2-2-2-1, SR-2-2-2-2
     * 
     */
    bool configure(const pugi::xml_node& serviceXmlNode) override;

    /** 
    * @requirements SR-2-2, SR-2-2-1, SR-3, SR-4-1, SR-4-2, SR-5-1, SR-5-2, SR-6-1-1,
    *               SR-7-1
    * 
    */
    bool initialize() override;

    bool start() override;

    bool terminate() override;

    /** 
    * @requirements SR-4-3, SR-5-3, SR-6-1-2, SR-6-1-3, SR-6-4, SR-6-4-1, SR-6-4-1-1, SR-6-4-1-2, SR-6-4-1-3, 
    * SR-7-3, SR-7-4
    */
    bool processReceivedLmcpMessage(std::unique_ptr<uxas::communications::data::LmcpMessage> receivedLmcpMessage) override;

protected:

    // ---- Internal Logic of the Zone Alert Service ---//

    /**
     * @brief Register a Zone announced in OpenUxAS. The service must know about any zones it will warn about.
     * 
     * @param abstractZoneObject lmcp object that is an AbstractZone to declare
     * @return true if the service successfully registers the zone
     * @return false if the service fails to register the zone
     * 
     * @requires SR-4-3-1
     */
    bool registerZone(std::shared_ptr<afrl::cmasi::AbstractZone> abstractZone, bool isKeepIn);

    /**
     * @brief Register a Vehicle announced in OpenUxAS. Important for computing on vehicle capabilities.
     * 
     * @param vehicleConfig The vehicle configuration message
     * @return true if the service successfully registers the vehicle
     * @return false if the service fails to register the vehicle
     * 
     * @requires SR-5-3-1
     */
    bool registerVehicle(std::shared_ptr<afrl::cmasi::AirVehicleConfiguration> airVehicleConfiguration);

    /**
     * @brief merge zones for zone violation detection
     * 
     * @return true if the service successfully merges the zones and publishes the merging
     *    
     * @requires SR-6-1, SR-6-1-2, SR-6-1-2-1, SR-6-4, SR-6-4-1, SR-6-4-1-1, SR-6-4-1-2, SR-6-4-1-3
     */
    bool mergeZones(); 

    /**
     * @brief Called when a vehicle reports its state. This is where the service checks for potential future zone violations.
     * 
     * @invariant If the reporting air vehicle has a previously registered configuration, then this function will return a
     * PredictedAlert for each previously registered zone for which there is a predicted imminent violation.
     * 
     * @return std::vector<PredictedViolation> 
     * 
     * @requires SR-7-3-1, SR-7-4, SR-7-4-1, SR-7-4-2, SR-7-4-3, SR-7-4-4, SR-7-4-4-1, SR-7-4-4-2, 
     *           SR-7-4-5, SR-7-4-5-1, SR-7-4-5-2, SR-7-4-5-6, SR-7-5
     * 
     */
    bool processVehicleStateReport(std::shared_ptr<afrl::cmasi::AirVehicleState> airVehicleState);

private:

    // the lookahead time to apply for zone warnings, as passed in by config param
    double lookaheadTime;

    // the computer of zone alerts. Can be different designs
    zoneAlert::ZoneAlertComputer *zoneAlertComputerPtr;

};

} //namespace service
} //namespace uxas

#endif /* UXAS_00_SERVICE_TEMPLATE_H */
