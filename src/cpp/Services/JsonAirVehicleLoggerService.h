/* 
 * File:   JsonAirVehicleLoggerService.h
 * Author: lhumphrey
 *
 * Created on Mar 12, 2021
 */

#ifndef UXAS_JSON_AIR_VEHICLE_LOGGER_SERVICE_H
#define UXAS_JSON_AIR_VEHICLE_LOGGER_SERVICE_H

#include <nlohmann/json.hpp>

#include "ServiceBase.h"
#include "CallbackTimer.h"
#include "TypeDefs/UxAS_TypeDefs_Timer.h"

#include <cstdint>
#include <fstream>

namespace uxas
{
namespace service
{
namespace data
{

/*! \class JsonAirVehicleLoggerService
    \brief This service saves fields of AirVehicleState messages in JSON format.

 * For each ID-specified air vehicle, save certain fields of corresponding
 * AirVehicleState messages depending on reporting level: 0-, 1, or 2+.
 * 
 * Example Configuration Element: 
 * <Service Type="JsonAirVehicleLoggerService" Filename="myOutput.txt">
 *   <AirVehicle ID="3" Level="0"/>
 *   <AirVehicle ID="4" Level="1"/>
 *   <AirVehicle ID="5" Level="2"/>
 *   <AirVehicle ID="6" Level="1"/>
 * </Service>
 *   
 * Options:
 *  - Filename - Name of the file in which to log results
 *  - - ID - The ID of the air vehicle
 *  - - Level - The level of information to report: "0", "1", or "2"
 * 
 * Subscribed Messages:
 *  - afrl::cmasi::AirVehicleState
 * 
 * Published Messages:
 *  - afrl::cmasi::KeyValuePair
 * 
 */

class JsonAirVehicleLoggerService : public ServiceBase
{
public:

    static const std::string&
    s_typeName()
    {
        static std::string s_string("JsonAirVehicleLoggerService");
        return (s_string);
    };

    static const std::vector<std::string>
    s_registryServiceTypeNames()
    {
        std::vector<std::string> registryServiceTypeNames = {s_typeName()};
        return (registryServiceTypeNames);
    };

    // Define name of directory in which output file will be stored
    static const std::string&
    s_directoryName() { static std::string s_string("JsonAirVehicleLogs"); return (s_string); };

    static ServiceBase*
    create()
    {
        return new JsonAirVehicleLoggerService;
    };

    JsonAirVehicleLoggerService();

    virtual
    ~JsonAirVehicleLoggerService();

private:

    static
    ServiceBase::CreationRegistrar<JsonAirVehicleLoggerService> s_registrar;

    /** brief Copy construction not permitted */
    JsonAirVehicleLoggerService(JsonAirVehicleLoggerService const&) = delete;

    /** brief Copy assignment operation not permitted */
    void operator=(JsonAirVehicleLoggerService const&) = delete;

    bool
    configure(const pugi::xml_node& serviceXmlNode) override;

    bool
    initialize() override;

    bool
    start() override;

    bool
    terminate() override;

    bool
    processReceivedLmcpMessage(std::unique_ptr<uxas::communications::data::LmcpMessage> receivedLmcpMessage) override;


private:

    // Member variables to store output filename, a map of ID numbers to report Level based
    // on sevice configuration element, and the set of ID numbers seen so far during execution
    std::string m_outputFilename = std::string("output.txt");
    std::unordered_map<int64_t, int64_t> m_idToLevel;
    std::unordered_set<int64_t> m_seenIds;
};

}; //namespace data
}; //namespace service
}; //namespace uxas

#endif /* UXAS_JSON_AIR_VEHICLE_LOGGER_SERVICE_H */
