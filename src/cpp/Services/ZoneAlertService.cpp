// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

/* 
 * File:   ZoneAlertService.cpp
 * Author: steve
 *
 * Created on March 17, 2017, 5:55 PM
 *
 * <Service Type="ZoneAlertService" OptionString="Option_01" OptionInt="36" />
 * 
 */

// include header for this service
#include "ZoneAlertService.h"
#include "SimpleZoneAlertComputer.h"

//include LMCP Messages

#include <iostream>     // std::cout, cerr, etc
#include "afrl/cmasi/KeepOutZone.h"
#include "afrl/cmasi/KeepInZone.h"
#include "uxas/messages/task/UniqueAutomationRequest.h"

#define COUT_INFO(MESSAGE) std::cout << MESSAGE << std::endl;std::cout.flush();

// convenience definitions for the option strings
#define STRING_XML_OPTION_LOOKAHEAD "Lookahead"

// namespace definitions
namespace uxas  // uxas::
{
namespace service   // uxas::service::
{

using std::string;
using std::stringstream;

// this entry registers the service in the service creation registry
ZoneAlertService::ServiceBase::CreationRegistrar<ZoneAlertService>
ZoneAlertService::s_registrar(ZoneAlertService::s_registryServiceTypeNames());

// service constructor
ZoneAlertService::ZoneAlertService()
: ServiceBase(ZoneAlertService::s_typeName(), ZoneAlertService::s_directoryName()) { };

// service destructor
ZoneAlertService::~ZoneAlertService() { };


bool ZoneAlertService::configure(const pugi::xml_node& ndComponent)
{
    bool isSuccess(true);

    // process options from the XML configuration node:
    if (!ndComponent.attribute(STRING_XML_OPTION_LOOKAHEAD).empty())
    {
        lookaheadTime = ndComponent.attribute(STRING_XML_OPTION_LOOKAHEAD).as_double();
    }

    // subscribe to messages to provide service
    addSubscriptionAddress(afrl::cmasi::KeepOutZone::Subscription);
    addSubscriptionAddress(afrl::cmasi::KeepInZone::Subscription);

    addSubscriptionAddress(afrl::cmasi::AirVehicleConfiguration::Subscription);
    addSubscriptionAddress(afrl::cmasi::AirVehicleState::Subscription);

    addSubscriptionAddress(uxas::messages::task::UniqueAutomationRequest::Subscription);

    return (isSuccess);
}

bool ZoneAlertService::initialize()
{
    // perform any required initialization before the service is started
    COUT_INFO("*** INITIALIZING: Service Id[" << m_serviceId << "] with working directory [" << m_workDirectoryName << "] *** ");
    
    // setup core data models
    zoneAlertComputerPtr = new zoneAlert::SimpleZoneAlertComputer(lookaheadTime);


    return (true);
}

bool ZoneAlertService::start()
{
    // perform any actions required at the time the service starts
    COUT_INFO("*** STARTING: Service[" << s_typeName() << "] with working directory [" << m_workDirectoryName << "] *** ");
    
    return (true);
};

bool ZoneAlertService::terminate()
{
    // perform any action required during service termination, before destructor is called.
    COUT_INFO("*** TERMINATING: Service[" << s_typeName() << "] with working directory [" << m_workDirectoryName << "] *** ");
    
    // deconstruct core data models
    delete zoneAlertComputerPtr;

    return (true);
}

bool ZoneAlertService::processReceivedLmcpMessage(std::unique_ptr<uxas::communications::data::LmcpMessage> receivedLmcpMessage)
{
    if (afrl::cmasi::isKeepOutZone(receivedLmcpMessage->m_object)) {
        auto abstractZone = std::static_pointer_cast<afrl::cmasi::KeepOutZone>(receivedLmcpMessage->m_object);    
        return !registerZone(abstractZone, false);
    }

    else if (afrl::cmasi::isKeepInZone(receivedLmcpMessage->m_object)) {
        auto abstractZone = std::static_pointer_cast<afrl::cmasi::KeepInZone>(receivedLmcpMessage->m_object);    
        return !registerZone(abstractZone, true);
    }

    else if (afrl::cmasi::isAirVehicleConfiguration(receivedLmcpMessage->m_object)) {
        auto airVehicleConfig = std::static_pointer_cast<afrl::cmasi::AirVehicleConfiguration>(receivedLmcpMessage->m_object);    
        return !registerVehicle(airVehicleConfig);                 
    }

    else if (afrl::cmasi::isAirVehicleState(receivedLmcpMessage->m_object)) {
        auto airVehicleState = std::static_pointer_cast<afrl::cmasi::AirVehicleState>(receivedLmcpMessage->m_object);    
        return !processVehicleStateReport(airVehicleState);
    }

    else if(uxas::messages::task::isUniqueAutomationRequest(receivedLmcpMessage->m_object)) {
      COUT_INFO("***** GOING TO MERGE SOME ZONES NOW *****");
        return !mergeZones();
    }

    return false;
}


bool ZoneAlertService::registerZone(std::shared_ptr<afrl::cmasi::AbstractZone> abstractZone, bool isKeepIn) {

    // Take message type and build bound zone
    //auto abstractZone = std::static_pointer_cast<afrl::cmasi::AbstractZone> (receivedLmcpMessage->m_object);
    COUT_INFO("*** RECEIVED:: Service[" << s_typeName() << "] Received a Zone with the id " 
        << abstractZone->getZoneID()
        << " *** ");

    // Register and record the zone
    auto success = zoneAlertComputerPtr->addZone(abstractZone, isKeepIn);

    // log if could not record the zone
    if (!success) {
    COUT_INFO("*** Service[" << s_typeName() << "] Failed to record Zone with id " 
        << abstractZone->getZoneID()
        << " *** ");
    }     

    return success;
}

bool ZoneAlertService::registerVehicle(std::shared_ptr<afrl::cmasi::AirVehicleConfiguration> airVehicleConfiguration) {

    COUT_INFO("*** RECEIVED:: Service[" << s_typeName() << "] Received a Vehicle Configuration with the id " 
        << airVehicleConfiguration->getID()
        << " *** ");

    // Store the aircraft configuration in the alert computer
    // @TODO Check memory safety of casting from unique_ptr to static pointer above and then to shared pointer in the method call
    
    auto success = zoneAlertComputerPtr->addVehicle(airVehicleConfiguration);

    if (!success) {
        COUT_INFO("*** Service[" << s_typeName() << "] FAILED to apply vehicle declration with id " 
            << airVehicleConfiguration->getID()
            << " *** ");
    }
    else {
        COUT_INFO("*** Service[" << s_typeName() << "] applied vehicle declaration with id " 
            << airVehicleConfiguration->getID()
            << " *** ");

    }

    return success;

}

bool ZoneAlertService::mergeZones() {
        // assuming that all zones and vehicles have been declared, and now automation
        // is being requested, perform zone merging and report merged zones

        COUT_INFO("*** RECEIVED:: Service[" << s_typeName() << "] Received automation request. Processing zones." 
            << " *** ");


        // @todo: Handle the fact that this is asynchronous and might miss out on 
        // the start of missiosn if merging takes too long (falls behind, required soft keep-up computational real-time)
        auto mergedZonesPtr = zoneAlertComputerPtr->mergeZones();

        // report each merged zone
        if (mergedZonesPtr != NULL) {

            COUT_INFO("*** RECEIVED:: Service[" << s_typeName() << "] Processing zones succesful. Emiting processed zone declarations." 
                << " *** ");

            for (int i = 0; i<mergedZonesPtr->size(); i++) {
                sendSharedLmcpObjectBroadcastMessage((*mergedZonesPtr)[i]);
            
                // debug output
                auto mergedZonePtr = (*mergedZonesPtr)[i];
                COUT_INFO("Merged Zone: id[" << mergedZonePtr->getZoneID() <<"] "
                            << "type = " << (mergedZonePtr->getKeepIn() ? "KEEP IN" : "KEEP OUT") ); 
            }

            mergedZonesPtr->clear();
            delete mergedZonesPtr;

            COUT_INFO("*** RECEIVED:: Service[" << s_typeName() << "] Finished processed zone declarations." 
                << " *** ");

            return true;
        }    
        else {
            COUT_INFO("*** RECEIVED:: Service[" << s_typeName() << "] Processing zones failed. No zone alerting will occur." 
                << " *** ");

            return false;
        }

}

bool ZoneAlertService::processVehicleStateReport(std::shared_ptr<afrl::cmasi::AirVehicleState> airVehicleState) {

//    std::cout << "*** RECEIVED:: Service[" << s_typeName() << "] Received a Vehicle State with the id "  
//        << airVehicleState->getID()
//        << " *** " << std::endl;

    // Process the aircraft state to identify impending zone violations
    // @TODO Check memory safety of casting from unique_ptr to static pointer above and then to shared pointer in the method call
    stringstream errorLog;
    auto violationsPtr = zoneAlertComputerPtr->computeZoneViolations(airVehicleState, errorLog);

    // report any errors that occured in construction
    string errors = errorLog.str();
    if (errors.length()>0) {
        COUT_INFO(errors);
        return false;
    }

    // report each detected violation
    if (violationsPtr != NULL) {
        for (int i = 0; i<violationsPtr->size(); i++) {
            sendSharedLmcpObjectBroadcastMessage((*violationsPtr)[i]);
            
            // debug output
            auto violationPtr = (*violationsPtr)[i];
            auto message = std::static_pointer_cast<avtas::lmcp::Object>(violationPtr);

            bool imminent = dcllc::zonealert::isImminentZoneViolation(message);
            if (imminent) {
                COUT_INFO("IMMINENT VIOLATION: vehicle[" <<violationPtr->getVehicleID()<<"] "
                        << "mergedZone["<< violationPtr->getZoneID() << "] at time " << violationPtr->getTimeToIntercept());
            }
            else {
                COUT_INFO("ACTIVE VIOLATION: vehicle[" <<violationPtr->getVehicleID()<<"] "
                        << "mergedZone["<< violationPtr->getZoneID() << "] at time " << violationPtr->getTimeToIntercept());
            }
        }

        violationsPtr->clear();
        delete violationsPtr;
    }

    return true;

}


}; //namespace service
}; //namespace uxas
