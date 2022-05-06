/* 
 * File:   JsonAirVehicleLoggerService.cpp
 * Author: lhumphrey
 *
 */

#include "JsonAirVehicleLoggerService.h"

#include "afrl/cmasi/KeyValuePair.h"
#include "afrl/cmasi/AirVehicleState.h"

#include <iostream>
#include <fstream>
#include <string>

#define STRING_XML_FILENAME "Filename"
#define STRING_XML_VEHICLE "AirVehicle"
#define STRING_XML_ID "ID"
#define STRING_XML_LEVEL "Level"

namespace uxas
{
namespace service
{
namespace data
{

JsonAirVehicleLoggerService::ServiceBase::CreationRegistrar<JsonAirVehicleLoggerService>
JsonAirVehicleLoggerService::s_registrar(JsonAirVehicleLoggerService::s_registryServiceTypeNames());

JsonAirVehicleLoggerService::JsonAirVehicleLoggerService()
: ServiceBase(JsonAirVehicleLoggerService::s_typeName(), JsonAirVehicleLoggerService::s_directoryName()) { };

JsonAirVehicleLoggerService::~JsonAirVehicleLoggerService() { };

bool JsonAirVehicleLoggerService::configure(const pugi::xml_node& serviceXmlNode)
{
    bool isSuccess(true);

    // Get and save the Filename attribute (if specified)
    if (!serviceXmlNode.attribute(STRING_XML_FILENAME).empty())
    {
        m_outputFilename = serviceXmlNode.attribute(STRING_XML_FILENAME).value();
    }

    // Get and save the ID and Level attributes for each AirVehicle
    for (pugi::xml_node subXmlNode = serviceXmlNode.first_child(); subXmlNode; subXmlNode = subXmlNode.next_sibling())
    {
        if (std::string(STRING_XML_VEHICLE) == subXmlNode.name())
        {   
            if(!subXmlNode.attribute(STRING_XML_ID).empty() && !subXmlNode.attribute(STRING_XML_LEVEL).empty()) 
            {
                int64_t vId = subXmlNode.attribute(STRING_XML_ID).as_int();
                int64_t reportLevel = subXmlNode.attribute(STRING_XML_LEVEL).as_int();
                m_idToLevel[vId] = reportLevel;
            }
            else
            {
                UXAS_LOG_ERROR(s_typeName(), "::configure encountered ", STRING_XML_VEHICLE, 
                " element with missing attribute ", STRING_XML_ID, " or ", STRING_XML_LEVEL);
                isSuccess = false;
            }
        }
        else
        {
            UXAS_LOG_ERROR(s_typeName(), "::configure encountered unknown element ", subXmlNode.name());
            isSuccess = false;
        }
    }

    addSubscriptionAddress(afrl::cmasi::AirVehicleState::Subscription);

    return (isSuccess);
}

bool JsonAirVehicleLoggerService::initialize()
{
    std::cout << "*** INITIALIZING:: Service[" << s_typeName() << "] Service Id[" << m_serviceId 
              << "] with working directory [" << m_workDirectoryName << "] *** " << std::endl;
    
    return (true);
}

bool JsonAirVehicleLoggerService::start()
{
    std::cout << "*** STARTING:: Service[" << s_typeName() << "] Service Id[" << m_serviceId 
              << "] with working directory [" << m_workDirectoryName << "] *** " << std::endl;
    
    // Remove any existing contents of the output file on service start
    std::ofstream file(m_workDirectoryPath + m_outputFilename);
    file << "";
    file.close();

    return (true);
};

bool JsonAirVehicleLoggerService::terminate()
{
    std::cout << "*** TERMINATING:: Service[" << s_typeName() << "] Service Id[" << m_serviceId 
              << "] with working directory [" << m_workDirectoryName << "] *** " << std::endl;
    
    return (true);
}

bool JsonAirVehicleLoggerService::
processReceivedLmcpMessage(std::unique_ptr<uxas::communications::data::LmcpMessage> receivedLmcpMessage)
{
    if (afrl::cmasi::isAirVehicleState(receivedLmcpMessage->m_object))
    {
        auto avState = std::static_pointer_cast<afrl::cmasi::AirVehicleState> (receivedLmcpMessage->m_object);

        // If the ID of an AirVehicleState message corresponds to a configured ID and
        // has not been seen before, store the ID and publish a corresponding KeyValuePair
        if (m_idToLevel.count(avState->getID()) && !m_seenIds.count(avState->getID()))
        {
            m_seenIds.insert(avState->getID());
            auto kvp = std::make_shared<afrl::cmasi::KeyValuePair>();
            kvp->setKey(std::string("JsonAirVehicleLoggerVehicleID"));
            kvp->setValue(std::to_string(avState->getID()));
            sendSharedLmcpObjectBroadcastMessage(kvp);
        }

        // If the ID was listed in the configuration element, save a JSON message 
        // with content according to the configured Level for that ID
        if (m_idToLevel.count(avState->getID())) 
        {
            nlohmann::json j;
            j["ID"] = avState->getID();
            j["Time"] = avState->getTime();
            j["Latitude"] = avState->getLocation()->getLatitude();
            j["Longitude"] = avState->getLocation()->getLongitude();

            if (m_idToLevel[avState->getID()] > 0) {
                j["Heading"] = avState->getHeading();
                j["Groundspeed"] = avState->getGroundspeed();
                j["Airspeed"] = avState->getAirspeed();
                j["VerticalSpeed"] = avState->getVerticalSpeed();
            }

            if (m_idToLevel[avState->getID()] > 1) {
                j["EnergyAvailable"] = avState->getEnergyAvailable();
                j["ActualEnergyRate"] = avState->getActualEnergyRate();
            }

            std::ofstream file(m_workDirectoryPath + m_outputFilename, std::ios::app);
            file << j.dump() << std::endl;
            file.close();
        }
        
    }
    return false;
}

}; //namespace data
}; //namespace service
}; //namespace uxas

