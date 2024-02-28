// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

/* 
 * File:   DAIDALUS_Processing.cpp
 * Author: SeanR
 *
 *
 *
 * <Service Type="DAIDALUS_Processing" OptionString="Option_01" OptionInt="36" />
 * 
 */

// include header for this service
#include "DAIDALUS_Processing.h"

#include "afrl/cmasi/AirVehicleState.h"
#include "BandsRegion.h"
#include "Detection3D.h"
#include "Interval.h"
#include "TrafficState.h"
#include "WCVTable.h"
#include "WCV_TAUMOD.h"
#include "WCV_TCPA.h"
#include "WCV_TEP.h"
#include "larcfm/DAIDALUS/DAIDALUSConfiguration.h"
#include "larcfm/DAIDALUS/WellClearViolationIntervals.h"
#include "uxas/messages/uxnative/StartupComplete.h"
#include "stdUniquePtr.h"


#include "FlatEarth.h"
#include "Util.h"
#include "stdUniquePtr.h"
#include "afrl/cmasi/AutomationResponse.h"
#include "afrl/cmasi/FlightDirectorAction.h"
#include "afrl/cmasi/CommandStatusType.h"
#include "afrl/cmasi/GoToWaypointAction.h"
#include "afrl/cmasi/MissionCommand.h"
#include "afrl/cmasi/Waypoint.h"
#include "Constants/Convert.h"


#include <iostream>     // std::cout, cerr, etc
#include <cmath>    //cmath::cos, sin, etc
#include <string>   //std::to_string etc
#include <algorithm>  //std::find 
#include <memory>   //std::make_shared std::make_unique
#include <cstdint>  //std::intmax_max std::intmax_min
#include <vector>   //std::vector
#include <iterator> //sdt::iterator


// convenience definitions for the option strings
// preprocessor directives associated with detection
#define STRING_XML_VEHICLE_ID "VehicleID"
#define STRING_XML_LOOKAHEADTIME "LookAheadTime"
#define STRING_XML_LEFTTRACK "LeftTrack"
#define STRING_XML_RIGHTTRACK "RightTrack"
#define STRING_XML_MINGROUNDSPEED "MinGroundSpeed"
#define STRING_XML_MAXGROUNDSPEED "MaxGroundSpeed"
#define STRING_XML_MINVERTICALSPEED "MinVerticalSpeed"
#define STRING_XML_MAXVERTICALSPEED "MaxVerticalSpeed"
#define STRING_XML_MINALTITUDE "MinAltitude"
#define STRING_XML_MAXALTITUDE "MaxAltitude"
#define STRING_XML_TRACKSTEP "TrackStep"
#define STRING_XML_GROUNDSPEEDSTEP "GroundSpeedStep"
#define STRING_XML_VERTICALSPEEDSTEP "VerticalSpeedStep"
#define STRING_XML_ALTITUDESTEP "AltitudeStep"
#define STRING_XML_HORIZONTALACCELERATION "HorizontalAcceleration"
#define STRING_XML_VERTICALACCELERATION "VerticalAcceleration"
#define STRING_XML_TURNRATE "TurnRate"
#define STRING_XML_BANKANGLE "BankAngle"
#define STRING_XML_VERTICALRATE "VerticalRate"
#define STRING_XML_RECOVERYSTABILITYTIME "RecoveryStabilityTime"
#define STRING_XML_MINHORIZONTALRECOVERY "MinHorizontalRecovery"
#define STRING_XML_MINVERTICALRECOVERY "MinVerticalRecovery"
#define STRING_XML_ISRECOVERYTRACK "isRecoveryTrack"
#define STRING_XML_ISRECOVERYGROUNDSPEED "isRecoveryGroundSpeed"
#define STRING_XML_ISRECOVERYVERTICALSPEED "isRecoveryVerticalSpeed"
#define STRING_XML_ISRECOVERYALTITUDE "isRecoveryAltitude"
#define STRING_XML_ISCOLLISIONAVOIDANCE "isCollisionAvoidance"
#define STRING_XML_COLLISIONAVOIDANCEFACTOR "CollisionAvoidanceFactor"
#define STRING_XML_HORIZONTALNMAC "HorizontalNMAC"
#define STRING_XML_VERTICALNMAC "VerticalNMAC"
#define STRING_XML_HORIZONTALCONTOURTHRESHOLD "HorizontalContourThreshold"
#define STRING_XML_TTHR "TTHR"
#define STRING_XML_RTCAALERTLEVELS "RTCAAlertLevels"
#define STRING_XML_ALERTTIME1 "AlertTime1"
#define STRING_XML_EARLYALERTTIME1 "EarlyAlertTime1"
#define STRING_XML_ALERTTIME2 "AlertTime2"
#define STRING_XML_EARLYALERTTIME2 "EarlyAlertTime2"
#define STRING_XML_ALERTTIME3 "AlertTime3"
#define STRING_XML_EARLYALERTTIME3 "EarlyAlertTime3"
#define STRING_XML_HORIZONTALDETECTIONTYPE "HorizontalDetectionType"
#define STRING_XML_DTHR "DTHR"
#define STRING_XML_ZTHR "ZTHR"
#define STRING_XML_PRIORITYSWITCHTIME "PrioritySwitchTime"
#define STRING_XML_VEHICLEID "VehicleID"

//preprocessor directives associated with response
#define STRING_XML_AUTOMATICRESPONSESTATUS "AutomaticResponseStatus"



// useful definitions
#define MILLISECONDTOSECOND 1.0/1000.0

// anonymous namespace for helper functions
namespace {
    // use direction cosine matrix to convert body frame velocity components into inertial frame velocity components: [DCM]*body_ref_vel_vector
    void makeVelocityXYZ(double u, double v, double w, double Phi_rad, double Theta_rad, double Psi_rad, double& velocityX, double& velocityY, 
            double& velocityZ)
    {
        velocityX = std::cos(Theta_rad)*std::cos(Psi_rad)*u + (std::sin(Phi_rad)*std::sin(Theta_rad)*std::cos(Psi_rad)- 
                std::cos(Phi_rad)*std::sin(Psi_rad))*v + (std::cos(Phi_rad)*std::sin(Theta_rad)*std::cos(Psi_rad) + 
                std::sin(Phi_rad)*std::sin(Psi_rad))*w;
        velocityY = std::cos(Theta_rad)*std::sin(Psi_rad)*u + (std::sin(Phi_rad)*std::sin(Theta_rad)*std::sin(Psi_rad)+ 
                std::cos(Phi_rad)*std::cos(Psi_rad))*v + (std::cos(Phi_rad)*std::sin(Theta_rad)*std::sin(Psi_rad)- 
                std::sin(Phi_rad)*std::cos(Psi_rad))*w;
        velocityZ = -std::sin(Theta_rad)*u + std::sin(Phi_rad)*std::cos(Theta_rad)*v + std::cos(Phi_rad)*std::cos(Theta_rad)*w;
    }
    // make a detection pointer choosing between TCPA, TEP, or TAUMOD given an string containing the typ and a table containing
    // the horizontal threshold for wellclear volume, vertical threshold for wellclear volume, time threshold for wellclear volume, and time to co altitude
    std::unique_ptr<larcfm::Detection3D> makeDetectionPtr(const std::string type, const larcfm::WCVTable table)
    {
        std::unique_ptr<larcfm::Detection3D> ptr;
        if (type == "TCPA")
        {
            ptr = uxas::stduxas::make_unique<larcfm::WCV_TCPA>(table);
        }
        else if (type == "TEP")
        {
            ptr = uxas::stduxas::make_unique<larcfm::WCV_TEP>(table);
        }
        else
        {
            ptr = uxas::stduxas::make_unique<larcfm::WCV_TAUMOD>(table);
        }
        return ptr;
    }

    bool isInRange (const double lower, const double upper, const double value)
    {
        return ((lower < value) && (value <= upper));
    }
    
    struct intervals
    {
        double lower;
        double upper;
    };
    
    bool isInRecovery(const double& divert, const std::vector<intervals>& r_bands)
    {
        bool RecoveryExists{false}, isFoundInRecovery{false};
        if (!r_bands.empty())
        {
            RecoveryExists = true;
            for (uint i = 0; i < r_bands.size(); i++)
            {
                if (isInRange(r_bands[i].lower, r_bands[i].upper, divert))
                {
                    isFoundInRecovery = true;
                    break;
                }
            }
        }
        else 
        {
            RecoveryExists = false;
        }
        
        return (RecoveryExists && isFoundInRecovery);
    }

}

// namespace definitions
namespace uxas  // uxas::
{
namespace service   // uxas::service::
{   

// this entry registers the service in the service creation registry
DAIDALUS_Processing::ServiceBase::CreationRegistrar<DAIDALUS_Processing>
DAIDALUS_Processing::s_registrar(DAIDALUS_Processing::s_registryServiceTypeNames());

// service constructor
DAIDALUS_Processing::DAIDALUS_Processing()
: ServiceBase(DAIDALUS_Processing::s_typeName(), DAIDALUS_Processing::s_directoryName()) { }; 

// service destructor
DAIDALUS_Processing::~DAIDALUS_Processing() { };

bool DAIDALUS_Processing::foundWCVHeadingResolution(const std::shared_ptr<larcfm::DAIDALUS::WellClearViolationIntervals>& DAIDALUS_bands)
{
    bool acceptable_action_flag = true;
    m_DivertState.heading_deg = m_CurrentState.heading_deg;
    m_DivertState.altitude_m = m_CurrentState.altitude_m;
    m_DivertState.horizontal_speed_mps = m_CurrentState.horizontal_speed_mps;
    m_DivertState.vertical_speed_mps = 0; // hold current altitude while diverting m_CurrentState.vertical_speed_mps;       

    std::vector<intervals> bands, r_bands;
    intervals temp;
    for (uint i = 0; i < DAIDALUS_bands->getWCVGroundHeadingIntervals().size(); i++)
    {
        temp.lower = DAIDALUS_bands->getWCVGroundHeadingIntervals()[i]->getGroundHeadings()[0];
        temp.upper = DAIDALUS_bands->getWCVGroundHeadingIntervals()[i]->getGroundHeadings()[1];
        bands.push_back(temp);
    }
    for (uint i = 0; i < DAIDALUS_bands->getRecoveryGroundHeadingIntervals().size(); i++)
    {
        temp.lower = DAIDALUS_bands->getRecoveryGroundHeadingIntervals()[i]->getRecoveryGroundHeadings()[0];
        temp.upper = DAIDALUS_bands->getRecoveryGroundHeadingIntervals()[i]->getRecoveryGroundHeadings()[1];
        r_bands.push_back(temp);
    }

    if (bands.size() == 0)
    {
        //Expected conflict bands but found none--set divert state to current state
        std::cout << "Expected bands but found none." << std::endl;
        acceptable_action_flag = false;
        return acceptable_action_flag;
    }
    uint initial_band = 0;  //band that the initial heading was in.
    bool isFound = false;  //boolean stating whether the band that the initial heading was in has been identified
    //Of the reported bad bands, find which one the current heading is in
    for (uint i = 0; i < bands.size(); i++) //loop over the bands trying to set divert action to the maximum (right hand turn) of an interval until no 
        //more consecutive intervals are found
    {
        if (isInRange(bands[i].lower, bands[i].upper, m_DivertState.heading_deg))
        {
            m_DivertState.heading_deg = bands[i].upper + m_heading_interval_buffer_deg; //set divert heading to just over the maximum of the interval that current heading was found in
            if (!isFound)
            {
                initial_band = i;
                isFound = true;
            }
        }
    }
    if (m_DivertState.heading_deg > m_heading_max_deg)  //If divert heading is greater than 360, check to see if a right turn is still possible if not turn left
    {
        // this loop checks the angle wrapped form of the divert action to see if increased turn to the right can find a heading that is not in conflict
        // process terminates immediately when a conflict interval does not contain the candidate divert heading
        for (uint i = 0; i < bands.size(); i++)
        {
            if (isInRange(bands[i].lower, bands[i].upper, std::fmod(m_DivertState.heading_deg + 360.0, 360.0)))
            {
                m_DivertState.heading_deg = bands[i].upper + m_heading_interval_buffer_deg; // if angle wrapped divert heading is found in an interval,
                //set divert heading to just over the maximum of the interval
            }
            else
            {
                break;
            }

        }
    }
    
    m_DivertState.heading_deg = std::fmod((m_DivertState.heading_deg + 360.0), 360.0);
    if (m_DivertState.heading_deg > m_heading_max_deg)
    {
        //divert heading greater than max after checking angle wrap then fallback
        //maneuver enacted
        acceptable_action_flag = false;
        m_DivertState.heading_deg = std::fmod((m_DivertState.heading_deg + 180.0), 360.0);
    }
    else
    {

        //using angle wrapped versions of the CurrentState_heading_deg and DivertState.heading_deg to determine if Divert would result in a right turn
        if ((std::fmod(m_CurrentState.heading_deg + 360.0, 360.0) > std::fmod(m_DivertState.heading_deg + 360.0, 360.0) &&
            (std::fmod(m_CurrentState.heading_deg + 360.0, 360.0) - std::fmod(m_DivertState.heading_deg + 360.0, 360.0) >= 180.0)) ||
            (std::fmod(m_DivertState.heading_deg + 360.0, 360.0) > std::fmod(m_CurrentState.heading_deg + 360.0, 360.0) &&
                std::fmod(m_DivertState.heading_deg + 360.0, 360.0) - std::fmod(m_CurrentState.heading_deg + 360.0, 360.0) <= 180.0)) 

        {
             m_DivertState.heading_deg = std::fmod(m_DivertState.heading_deg + 360.0, 360.0);
            //if recovery exits and divert heading is within, acceptable action achieved
            acceptable_action_flag = r_bands.empty()? true : isInRecovery(m_DivertState.heading_deg, r_bands);
        }
        else 
    {
        //this branch attempts to find a left turn for divert heading by looping over the intervals from last to first--already know some bands exist
        m_DivertState.heading_deg = m_CurrentState.heading_deg;
        for (int i = initial_band; i >= 0; i--)
        {
            if (isInRange(bands[i].lower, bands[i].upper, m_DivertState.heading_deg))
            {
                m_DivertState.heading_deg = bands[i].lower - m_heading_interval_buffer_deg; // set divert heading to just under the interval minimum 
                // that contains the current heading--loop over the bands from the current band to the initial band
            }
        }
        if (m_DivertState.heading_deg < m_heading_min_deg)  //check for angle wrap if candidate divert heading is less than the minimum
        {
            for (int i = bands.size()-1; i >=0; i--)
            {
                if (isInRange(bands[i].lower, bands[i].upper, std::fmod(m_DivertState.heading_deg + 360.0, 360.0)))
                {
                    m_DivertState.heading_deg = bands[i].lower - m_heading_interval_buffer_deg;
                }
                else
                {
                    break;
                }

            }

        }
        m_DivertState.heading_deg = std::fmod(m_DivertState.heading_deg + 360.0, 360.0);
        if (m_DivertState.heading_deg < m_heading_min_deg)
        {
            //still less than heading minimum after checking for angle wrap
            //enact fallback maneuver
            acceptable_action_flag = false;
            m_DivertState.heading_deg = std::fmod((m_CurrentState.heading_deg + 180.0), 360.0);
        }
        else
        {
        //using angle wrapped versions of the CurrentState_heading_deg and DivertState.heading_deg to determine if Divert would result in a left turn
            if ((std::fmod(m_CurrentState.heading_deg + 360.0, 360.0) > std::fmod(m_DivertState.heading_deg + 360.0, 360.0) &&
                (std::fmod(m_CurrentState.heading_deg + 360.0, 360.0) - std::fmod(m_DivertState.heading_deg + 360.0, 360.0) < 180.0)) ||
                (std::fmod(m_DivertState.heading_deg + 360.0, 360.0) > std::fmod(m_CurrentState.heading_deg + 360.0, 360.0) &&
                    std::fmod(m_DivertState.heading_deg + 360.0, 360.0) - std::fmod(m_CurrentState.heading_deg + 360.0, 360.0) > 180.0)) 
            {
                //if recovery exists and divert heading is within, acceptable action achieved
                acceptable_action_flag = r_bands.empty()? true : isInRecovery(m_DivertState.heading_deg, r_bands);
            }
            else if (!acceptable_action_flag && DAIDALUS_bands->getRecoveryGroundHeadingIntervals().size() >0)
        {
            acceptable_action_flag = false;
            bool isRecoveryFound = false;
            //Current method for placing in recovery does not properly handle preferences
            //for right turns over left turns
            for (uint i = 0; i < r_bands.size(); i++)
            {
                if ((r_bands[i].lower > m_CurrentState.heading_deg) && (r_bands[i].upper > m_CurrentState.heading_deg))
                {
                    m_DivertState.heading_deg = r_bands[i].lower + m_heading_interval_buffer_deg / 2.0;
                    isRecoveryFound = true;
                    acceptable_action_flag = true;
                    break;
                }
            }

            if (!isRecoveryFound)
            {
                for (int i = r_bands.size()-1; i >= 0; i--)
                    if ((r_bands[i].lower < m_CurrentState.heading_deg) && (r_bands[i].upper < m_CurrentState.heading_deg))
                    {
                        m_DivertState.heading_deg = r_bands[i].upper - m_heading_interval_buffer_deg / 2.0;
                        acceptable_action_flag = true;
                        break;
                    }
            }

            // std::cout << "No way to avoid violation of Well Clear Volume. Choosing resolution to exit violation as soon as possible." << std::endl;
            // std::cout << std::endl;
           //use right turn recovery band
        }
            else
            {
                acceptable_action_flag = false;
                m_DivertState.heading_deg = std::fmod((m_CurrentState.heading_deg + 180.0), 360.0); //fallback action: turn right 180deg
            // std::cout << "No resolution found. Divert heading is " << m_DivertState.heading_deg << std::endl;
            // std::cout << std::endl;
            }
        }
    }
    //TODO: ensure divert action does not violate keep out zones
    }
    return acceptable_action_flag;
}

bool DAIDALUS_Processing::foundWCVGroundSpeedResolution(const std::shared_ptr<larcfm::DAIDALUS::WellClearViolationIntervals>& DAIDALUS_bands)
{
    //slower speeds preferred
    bool acceptable_action_flag = true;
    m_DivertState.horizontal_speed_mps = m_CurrentState.horizontal_speed_mps;
    m_DivertState.altitude_m = m_CurrentState.altitude_m;
    m_DivertState.heading_deg = m_CurrentState.heading_deg;
    m_DivertState.vertical_speed_mps = 0;   //hold altitude while divertingm_CurrentState.vertical_speed_mps;
    std::vector<intervals> bands, r_bands;
    intervals temp;
    for (uint i = 0; i < DAIDALUS_bands->getWCVGroundSpeedIntervals().size(); i++)
    {
        temp.lower = DAIDALUS_bands->getWCVGroundSpeedIntervals()[i]->getGroundSpeeds()[0];
        temp.upper = DAIDALUS_bands->getWCVGroundSpeedIntervals()[i]->getGroundSpeeds()[1];
        bands.push_back(temp);
    }
    if (bands.size()== 0)
    {
        //unexpected result
        std::cout << "Expected conflict bands but found none." << std::endl;
        acceptable_action_flag = false;
        return acceptable_action_flag;
    }
    for (uint i = 0; i < DAIDALUS_bands->getRecoveryGroundSpeedIntervals().size(); i++)
    {
        temp.lower = DAIDALUS_bands->getRecoveryGroundSpeedIntervals()[i]->getRecoveryGroundSpeeds()[0];
        temp.upper = DAIDALUS_bands->getRecoveryGroundSpeedIntervals()[i]->getRecoveryGroundSpeeds()[1];
        r_bands.push_back(temp);
    }
    uint initial_band = 0;  //band that the initial heading was in.
    for (uint i = 0; i < bands.size(); i++)
    {
        if (isInRange(bands[i].lower, bands[i].upper, m_DivertState.horizontal_speed_mps))
        {
            initial_band = i;
            break;
        }
    }
    
    for (int i = initial_band; i >= 0; i--)
    {
        if (isInRange(bands[i].lower, bands[i].upper, m_DivertState.horizontal_speed_mps))
        {
            m_DivertState.horizontal_speed_mps = bands[i].lower - m_groundspeed_interval_buffer_mps; //if DivertState is in a bad band, alter to less than minimum
        }
    }

    if (m_DivertState.horizontal_speed_mps < m_min_gs_mps)  //If divert speed is less than min, a speed up is preferred.
    {
        m_DivertState.horizontal_speed_mps = m_CurrentState.horizontal_speed_mps; //reset divert horizontal speed to current then check higher speeds
        for (uint i = initial_band; i < bands.size(); i++)
        {
            if (isInRange(bands[i].lower, bands[i].upper, m_DivertState.horizontal_speed_mps))
            {
                m_DivertState.horizontal_speed_mps = bands[i].upper + m_groundspeed_interval_buffer_mps; //if divertstate is in a bad band, alter to more than maximum
            }
        }
    }
    else // divert speed is greater than min and not in a conflict band
    {
       //if recovery exists and divert is within, acceptable action achieved. 
        acceptable_action_flag = r_bands.empty()? true : isInRecovery(m_DivertState.horizontal_speed_mps, r_bands);
    }

    if (m_DivertState.horizontal_speed_mps > m_max_gs_mps)  //If after checking slow downs and speed up no better heading found, check recovery intervals
    {
        acceptable_action_flag = false;
        if (DAIDALUS_bands->getRecoveryGroundSpeedIntervals().size() > 0)
        {
            bool isRecoveryFound = false;
            //Form vector of recovery intervals
            for (uint i = 0; i < DAIDALUS_bands->getRecoveryGroundSpeedIntervals().size(); i++)
            {
                temp.lower = DAIDALUS_bands->getRecoveryGroundSpeedIntervals()[i]->getRecoveryGroundSpeeds()[0];
                temp.upper = DAIDALUS_bands->getRecoveryGroundSpeedIntervals()[i]->getRecoveryGroundSpeeds()[1];
                r_bands.push_back(temp);
            }

            for (int i = r_bands.size()-1; i >= 0; i--)
            {
                //Find the first recovery interval that is at a lower speed than current speed
                if ((r_bands[i].lower < m_CurrentState.horizontal_speed_mps) && (r_bands[i].upper < m_CurrentState.horizontal_speed_mps))
                {
                    m_DivertState.horizontal_speed_mps = r_bands[i].upper - m_groundspeed_interval_buffer_mps / 2.0;
                    isRecoveryFound = true;
                    acceptable_action_flag = true;
                    break;
                }
            }

            if (!isRecoveryFound)
            {
                for (uint i = 0; i < r_bands.size(); i++)
                    //If no lower recovery found, find first recovery interval with higher speeds than current
                    if ((r_bands[i].lower > m_CurrentState.horizontal_speed_mps) && (r_bands[i].upper > m_CurrentState.horizontal_speed_mps))
                    {
                        m_DivertState.horizontal_speed_mps = r_bands[i].lower + m_groundspeed_interval_buffer_mps / 2.0;
                        acceptable_action_flag = true;
                        break;
                    }
            }

        }
        else 
        {
            //If no recovery found, set default behavior to minimum speed
            acceptable_action_flag = false;
            m_DivertState.horizontal_speed_mps = m_min_gs_mps;
        }
    }
    else // divert speed is less than max and not in a conflict band
    {
        //if recovery exists and divert is within, acceptable action achieved.
        acceptable_action_flag = r_bands.empty()? true : isInRecovery(m_DivertState.horizontal_speed_mps, r_bands);
    }
    return acceptable_action_flag;    
}

bool DAIDALUS_Processing::foundWCVAltitudeResolution(const std::shared_ptr<larcfm::DAIDALUS::WellClearViolationIntervals>& DAIDALUS_bands)
{
    bool acceptable_action_flag = true;
    m_DivertState.altitude_m = m_CurrentState.altitude_m;
    m_DivertState.vertical_speed_mps = 0.0; //m_CurrentState.vertical_speed_mps;--"fix" for inconsistency in DivertState.
    m_DivertState.heading_deg = m_CurrentState.heading_deg;
    m_DivertState.horizontal_speed_mps = m_CurrentState.horizontal_speed_mps;    
    std::vector<intervals> bands, r_bands;
    intervals temp;
    for (uint i = 0; i < DAIDALUS_bands->getWCVAlitudeIntervals().size(); i++)
    {
        temp.lower = DAIDALUS_bands->getWCVAlitudeIntervals()[i]->getAltitude()[0];
        temp.upper = DAIDALUS_bands->getWCVAlitudeIntervals()[i]->getAltitude()[1];
        bands.push_back(temp);
    }
    for (uint i = 0; i < DAIDALUS_bands->getRecoveryAltitudeIntervals().size(); i++)
    {
        temp.lower = DAIDALUS_bands->getRecoveryAltitudeIntervals()[i]->getRecoveryAltitude()[0];
        temp.upper = DAIDALUS_bands->getRecoveryAltitudeIntervals()[i]->getRecoveryAltitude()[1];
        r_bands.push_back(temp);
    }
    if (bands.size() == 0)
    {
        //Expected conflict bands but found none--set divert state to current state (minus vertical speed)
        std::cout << "Expected bands but found none." << std::endl;
        return false;
    }
    uint initial_band = 0;  //band that the initial heading was in.
    bool isFound = false;  //boolean stating whether the band that the initial heading was in has been identified
    //Of the reported bad bands, find which one the current altitude is in
    for (uint i = 0; i < bands.size(); i++)
    {
        if (isInRange(bands[i].lower, bands[i].upper, m_DivertState.altitude_m))
        {
            m_DivertState.altitude_m = bands[i].upper + m_altitude_interval_buffer_m; //if DivertState is in a bad band, alter to more than maximum
            if (!isFound)
            {
                initial_band = i;
                isFound = true;
            }
        }
    }

    if (m_DivertState.altitude_m > m_max_alt_m)  //If divert altitude is greater than altitude max, a descent is preferred.
    {
        acceptable_action_flag = false;
        m_DivertState.altitude_m = m_CurrentState.altitude_m; //reset divert altitude to current altitude then check bands below current altitude
        for (int i = initial_band; i >= 0; i--)
        {
            if (isInRange(bands[i].lower, bands[i].upper, m_DivertState.altitude_m))
            {
                m_DivertState.altitude_m = bands[i].lower - m_altitude_interval_buffer_m; //if Divert altitude is in a bad band, alter to less than minimum
            }
        }
    }
    else //divert altitude is less than max and not in a conflict band
    {
        //if recovery exists and divert altitude is within, acceptable action achieved
        acceptable_action_flag = r_bands.empty()? true : isInRecovery(m_DivertState.altitude_m, r_bands);
    }

    if (m_DivertState.altitude_m < m_min_alt_m)//If after checking ascents and descents no better heading found, check recovery bands
    {
        acceptable_action_flag = false;
        if (DAIDALUS_bands->getRecoveryAltitudeIntervals().size() > 0)
        {
            //m_DivertState.altitude_m = m_CurrentState.altitude_m;
            bool isRecoveryFound = false;
            //Form a vector of recovery band intervals

            //Find the first recovery band that is at an altitude greater than the current altitude and set divert altitude to just over the minimum
            for (uint i = 0; i < r_bands.size(); i++)
            {
                if ((r_bands[i].lower > m_CurrentState.altitude_m) && (r_bands[i].upper > m_CurrentState.altitude_m))
                {
                    m_DivertState.altitude_m = r_bands[i].lower + m_altitude_interval_buffer_m / 2.0; 
                    isRecoveryFound = true;
                    acceptable_action_flag = true;
                    break;
                }
            }

            //If no recovery greater than current altitude exists, find the first recovery interval lower than current altitude and set divert to just 
            //under the maximum of that recovery interval
            if (!isRecoveryFound)
            {
                for (int i = r_bands.size()-1; i >= 0; i--)
                    if ((r_bands[i].lower < m_CurrentState.altitude_m) && (r_bands[i].upper < m_CurrentState.altitude_m))
                    {
                        m_DivertState.altitude_m = r_bands[i].upper - m_altitude_interval_buffer_m / 2.0;
                        acceptable_action_flag = true;
                        break;
                    }
            }
        }
        else
        {
            //if no solution and no recovery found default to diverting to maximum altitude
            acceptable_action_flag = false;
            m_DivertState.altitude_m = m_max_alt_m;
            // std::cout << "No way to avoid violation of Well Clear Volume" << std::endl;
            // std::cout << std::endl;

        }
    }
    else //divert altitude is greater than minimum and not in a conflict band
    {
        //if recovery exists and divert altitude is within, acceptable action achieved
        acceptable_action_flag = r_bands.empty()? true : isInRecovery(m_DivertState.altitude_m, r_bands);
    }
    return acceptable_action_flag;
}

void DAIDALUS_Processing::SetDivertState(const std::shared_ptr<larcfm::DAIDALUS::WellClearViolationIntervals>& DAIDALUS_bands)
{
    //different response hierarchy based on "priority"
    bool altitude_resolution_acceptable, heading_resolution_acceptable, groundspeed_resolution_acceptable;
    switch (m_priority)
    {
        case Standard:
        //standard priority seeks a resolution according to altitude > heading > groundspeed hierarchy. if all fail, fallback behavior described by groundspeed resolution
            altitude_resolution_acceptable = foundWCVAltitudeResolution(DAIDALUS_bands);
            if (!altitude_resolution_acceptable)
            {
                heading_resolution_acceptable = foundWCVHeadingResolution(DAIDALUS_bands);
                if (!heading_resolution_acceptable)
                {
                    groundspeed_resolution_acceptable = foundWCVGroundSpeedResolution(DAIDALUS_bands);
                }
                
            }

            break;
        case High:
        //high priority seeks a resolution accordingn to groundspeed > heading > altitude heirarchy. if all fail, fallback behavior described by altitude resolution
            groundspeed_resolution_acceptable = foundWCVGroundSpeedResolution(DAIDALUS_bands);
            if (!groundspeed_resolution_acceptable)
            {
                heading_resolution_acceptable = foundWCVHeadingResolution(DAIDALUS_bands);
                if (!heading_resolution_acceptable)
                {
                    altitude_resolution_acceptable = foundWCVAltitudeResolution(DAIDALUS_bands);
                }
                
            }

            break;
    }
    
}

bool DAIDALUS_Processing::isSafeToReturnToMission(const std::shared_ptr<larcfm::DAIDALUS::WellClearViolationIntervals>& DAIDALUS_bands)
{
    //TODO:  maybe add distance to intruders > DMD as a condition on safe to return.
    uxas::common::utilities::FlatEarth flatEarth;
    double CS_North_m, CS_East_m, WP_North_m, WP_East_m;
    double WP_latitude_deg, WP_longitude_deg ;
    if (m_MissionCommand == nullptr)
    {
        return true;
    }
    else 
    {
        bool isReturnHeadingSafe = true;
        bool isReturnAltitudeSafe = true;
        bool isReturnHorizontalSpeedSafe = true;
        bool isReturnVerticalSpeedSafe = true;
        for (auto i : m_MissionCommand->getWaypointList())
        {
            if (i->getNumber() == m_NextWaypoint)
            {
                WP_latitude_deg = i->getLatitude();
                WP_longitude_deg = i->getLongitude();
                m_ReturnState.altitude_m = i->getAltitude();
                m_ReturnState.horizontal_speed_mps = i->getSpeed();
                m_ReturnState.vertical_speed_mps = i->getClimbRate();
                break;
            }

        }

        flatEarth.ConvertLatLong_degToNorthEast_m(m_CurrentState.latitude_deg, m_CurrentState.longitude_deg, CS_North_m, CS_East_m);
        flatEarth.ConvertLatLong_degToNorthEast_m(WP_latitude_deg, WP_longitude_deg, WP_North_m, WP_East_m);
        m_ReturnState.heading_deg = std::fmod((n_Const::c_Convert::dRadiansToDegrees()*std::atan2((WP_East_m - CS_East_m), (WP_North_m - CS_North_m))) + 360.0, 360.0);
        struct intervals
        {
            double upper;
            double lower;
        };
        
        std::vector<intervals> bands;
        intervals temp;
        for (uint i = 0; i < DAIDALUS_bands->getWCVGroundHeadingIntervals().size(); i++)
        {
            temp.lower = std::fmod(DAIDALUS_bands->getWCVGroundHeadingIntervals()[i]->getGroundHeadings()[0]+360.0, 360.0);
            temp.upper = std::fmod(DAIDALUS_bands->getWCVGroundHeadingIntervals()[i]->getGroundHeadings()[1]+360.0, 360.0);
            bands.push_back(temp);
        }
        
        uint initial_band = UINT32_MAX;  //band that the Return to Mission heading was in.
        bool isFound = false;  //boolean stating whether the band that the initial heading was in has been identified
        for (uint i = 0; i < bands.size(); i++)
        {
            if (isInRange(bands[i].lower, bands[i].upper, m_ReturnState.heading_deg))
            {
                initial_band = i;
                isFound = true;
                break;
            }
        }
        
        //not safe to return if ground heading of waypoint is in a near band as stated by DAIDALUS
        if (isFound && DAIDALUS_bands->getWCVGroundHeadingRegions()[initial_band] == larcfm::DAIDALUS::BandsRegion::NEAR)
        {
            isReturnHeadingSafe = false;
        }

        bands.clear();
        for (uint i = 0; i < DAIDALUS_bands->getWCVGroundSpeedIntervals().size(); i++)
        {
            temp.lower = DAIDALUS_bands->getWCVGroundSpeedIntervals()[i]->getGroundSpeeds()[0];
            temp.upper = DAIDALUS_bands->getWCVGroundSpeedIntervals()[i]->getGroundSpeeds()[1];
            bands.push_back(temp);
        }
        
        initial_band = UINT32_MAX;  //band that the Return to Mission heading was in.
        isFound = false;  //boolean stating whether the band that the initial heading was in has been identified
        for (uint i = 0; i < bands.size(); i++)
        {
            if (isInRange(bands[i].lower, bands[i].upper, m_ReturnState.horizontal_speed_mps))
            {
                initial_band = i;
                isFound = true;
                break;
            }
        }
        
        if (isFound && DAIDALUS_bands->getWCVGroundSpeedRegions()[initial_band] == larcfm::DAIDALUS::BandsRegion::NEAR)
        {
            isReturnHorizontalSpeedSafe = false;
        }

        bands.clear();
        for (uint i = 0; i < DAIDALUS_bands->getWCVVerticalSpeedIntervals().size(); i++)
        {
            temp.lower = DAIDALUS_bands->getWCVVerticalSpeedIntervals()[i]->getVerticalSpeeds()[0];
            temp.upper = DAIDALUS_bands->getWCVVerticalSpeedIntervals()[i]->getVerticalSpeeds()[1];
            bands.push_back(temp);
        }
        
        initial_band = UINT32_MAX;  //band that the Return to Mission heading was in.
        isFound = false;  //boolean stating whether the band that the initial heading was in has been identified
        for (uint i = 0; i < bands.size(); i++)
        {
            if (isInRange(bands[i].lower, bands[i].upper, m_ReturnState.vertical_speed_mps))
            {
                initial_band = i;
                isFound = true;
                break;
            }
        }
        
        if (isFound && DAIDALUS_bands->getWCVVerticalSpeedRegions()[initial_band] == larcfm::DAIDALUS::BandsRegion::NEAR)
        {
            isReturnVerticalSpeedSafe = false;
        }
        
//        
        bands.clear();
        for (uint i = 0; i < DAIDALUS_bands->getWCVAlitudeIntervals().size(); i++)
        {
            temp.lower = DAIDALUS_bands->getWCVAlitudeIntervals()[i]->getAltitude()[0];
            temp.upper = DAIDALUS_bands->getWCVAlitudeIntervals()[i]->getAltitude()[1];
            bands.push_back(temp);
        }
        
        initial_band = UINT32_MAX;  //band that the Return to Mission heading was in.
        isFound = false;  //boolean stating whether the band that the initial heading was in has been identified
        for (uint i = 0; i < bands.size(); i++)
        {
            if (isInRange(bands[i].lower, bands[i].upper, m_ReturnState.altitude_m))
            {
                initial_band = i;
                isFound = true;
                break;
            }
        }
        
        if (isFound && DAIDALUS_bands->getWCVAltitudeRegions()[initial_band] == larcfm::DAIDALUS::BandsRegion::NEAR)
        {
            isReturnAltitudeSafe = false;
        }

        //TODO: consider removing the vertical speed safe component on this check as it is the least directly controlled parameter
        if (isReturnHeadingSafe && isReturnHorizontalSpeedSafe && isReturnVerticalSpeedSafe && isReturnAltitudeSafe)
        {
            std::cout << std::endl;
            std::cout << "Returning to mission." << std::endl;
            std::cout << std::endl;
            return true;
        }
        else
        {
            // std::cout << "NOT SAFE TO RETURN TO MISSION" << std::endl;
            // std::cout << std::endl;
            return false;
        }
    }
    //TODO: check if return state must pass through a near band to get there from current state and determine appropriate action
}


bool DAIDALUS_Processing::configure(const pugi::xml_node& ndComponent)
{
    bool isSuccess(true);
    bool useBankAngle = false;
    // process options from the XML configuration node:
    m_VehicleID = m_entityId;

    if (!ndComponent.attribute(STRING_XML_VEHICLE_ID).empty())
    {
        m_VehicleID = ndComponent.attribute(STRING_XML_VEHICLE_ID).as_int();
    }
    
    if (!ndComponent.attribute(STRING_XML_LOOKAHEADTIME).empty())
    {
       double local_lookahead_time_s = ndComponent.attribute(STRING_XML_LOOKAHEADTIME).as_double();
       if (local_lookahead_time_s > 0.0)
       {
           m_lookahead_time_s = local_lookahead_time_s;
       }
    }
    if (!ndComponent.attribute(STRING_XML_LEFTTRACK).empty())
    {
       double local_left_trk_deg = ndComponent.attribute(STRING_XML_LEFTTRACK).as_double();
       if (local_left_trk_deg > 0.0 && local_left_trk_deg <= 180.0)
       {
           m_left_trk_deg = local_left_trk_deg;
       }
    }
    if (!ndComponent.attribute(STRING_XML_RIGHTTRACK).empty())
    {
       double local_right_trk_deg = ndComponent.attribute(STRING_XML_RIGHTTRACK).as_double();
       if (local_right_trk_deg >0.0 && local_right_trk_deg <=180.0)
       {
           m_right_trk_deg = m_right_trk_deg;
       }
    }
    if (!ndComponent.attribute(STRING_XML_MAXGROUNDSPEED).empty())
    {
       double local_max_gs_mps = ndComponent.attribute(STRING_XML_MAXGROUNDSPEED).as_double();
       if (local_max_gs_mps > 0.0)
       {
           m_max_gs_mps = local_max_gs_mps;
       }
    }
        if (!ndComponent.attribute(STRING_XML_MINGROUNDSPEED).empty())
    {
       double local_min_gs_mps = ndComponent.attribute(STRING_XML_MINGROUNDSPEED).as_double();
       if (local_min_gs_mps >= 0.0 && m_min_gs_mps < m_max_gs_mps)
       {
           m_min_gs_mps = local_min_gs_mps;
       }
    }
    if (!ndComponent.attribute(STRING_XML_MAXVERTICALSPEED).empty())
    {
       double local_max_vs_mps = ndComponent.attribute(STRING_XML_MAXVERTICALSPEED).as_double();
       m_max_vs_mps = local_max_vs_mps;
    }
        if (!ndComponent.attribute(STRING_XML_MINVERTICALSPEED).empty())
    {
       double local_min_vs_mps = ndComponent.attribute(STRING_XML_MINVERTICALSPEED).as_double();
       if (local_min_vs_mps < m_max_vs_mps)
       {
           m_min_vs_mps = local_min_vs_mps;
       }
    }
    if (!ndComponent.attribute(STRING_XML_MAXALTITUDE).empty())
    {
       double local_max_alt_m = ndComponent.attribute(STRING_XML_MAXALTITUDE).as_double();
       m_max_alt_m = local_max_alt_m; 
    }
        if (!ndComponent.attribute(STRING_XML_MINALTITUDE).empty())
    {
       double local_min_alt_m = ndComponent.attribute(STRING_XML_MINALTITUDE).as_double();
       if (local_min_alt_m < m_max_alt_m)
       {
           m_min_alt_m = local_min_alt_m;
       }
    }
    if (!ndComponent.attribute(STRING_XML_TRACKSTEP).empty())
    {
       double local_trk_step_deg = ndComponent.attribute(STRING_XML_TRACKSTEP).as_double();
       if (local_trk_step_deg > 0.0)
       {
           m_trk_step_deg = local_trk_step_deg;
       }
    }
    if (!ndComponent.attribute(STRING_XML_GROUNDSPEEDSTEP).empty())
    {
       double local_gs_step_mps = ndComponent.attribute(STRING_XML_GROUNDSPEEDSTEP).as_double();
       if (local_gs_step_mps > 0.0)
       {
           m_gs_step_mps = local_gs_step_mps;
       }
    }
    if (!ndComponent.attribute(STRING_XML_VERTICALSPEEDSTEP).empty())
    {
       double local_vs_step_mps = ndComponent.attribute(STRING_XML_VERTICALSPEEDSTEP).as_double();
       if (local_vs_step_mps > 0.0)
       {
           m_vs_step_mps = local_vs_step_mps;
       }
    }
    if (!ndComponent.attribute(STRING_XML_ALTITUDESTEP).empty())
    {
       double local_alt_step_m = ndComponent.attribute(STRING_XML_ALTITUDESTEP).as_double();
       if (local_alt_step_m > 0.0)
       {
           m_alt_step_m = local_alt_step_m;
       }
    }
    if (!ndComponent.attribute(STRING_XML_HORIZONTALACCELERATION).empty())
    {
       double local_horizontal_accel_mpsps = ndComponent.attribute(STRING_XML_HORIZONTALACCELERATION).as_double();
       if (local_horizontal_accel_mpsps >= 0.0)
       {
           m_horizontal_accel_mpsps = local_horizontal_accel_mpsps;
       }
    }
    if (!ndComponent.attribute(STRING_XML_VERTICALACCELERATION).empty())
    {
       double local_vertical_accel_G = ndComponent.attribute(STRING_XML_VERTICALACCELERATION).as_double();
       if (local_vertical_accel_G >= 0.0)
       {
           m_vertical_accel_G = local_vertical_accel_G;
       }
    }
    if (!ndComponent.attribute(STRING_XML_TURNRATE).empty())
    {
       double local_turn_rate_degps = ndComponent.attribute(STRING_XML_TURNRATE).as_double();
       if (local_turn_rate_degps >= 0.0)
       {
           m_turn_rate_degps = local_turn_rate_degps;
       }
    }
    if (!ndComponent.attribute(STRING_XML_BANKANGLE).empty())
    {
       useBankAngle = true;
       double local_bank_angle_deg = ndComponent.attribute(STRING_XML_BANKANGLE).as_double();
       if (local_bank_angle_deg >= 0.0 && m_turn_rate_degps != 0.0)
       {
           m_bank_angle_deg = local_bank_angle_deg;
       }
    }
    if (!ndComponent.attribute(STRING_XML_VERTICALRATE).empty())
    {
       double local_vertical_rate_mps = ndComponent.attribute(STRING_XML_VERTICALRATE).as_double();
       if (local_vertical_rate_mps >= 0.0)
       {
           m_vertical_rate_mps = local_vertical_rate_mps;
       }
    }    
    if (!ndComponent.attribute(STRING_XML_RECOVERYSTABILITYTIME).empty())
    {
       double local_recovery_stability_time_s = ndComponent.attribute(STRING_XML_RECOVERYSTABILITYTIME).as_double();
       if (local_recovery_stability_time_s >= 0.0)
       {
           m_recovery_stability_time_s = local_recovery_stability_time_s;
       }
    }
    if (!ndComponent.attribute(STRING_XML_ISRECOVERYTRACK).empty())
    {
       bool local_recovery_trk_bool = ndComponent.attribute(STRING_XML_ISRECOVERYTRACK).as_bool();
       m_recovery_trk_bool = local_recovery_trk_bool;
    }
    if (!ndComponent.attribute(STRING_XML_ISRECOVERYGROUNDSPEED).empty())
    {
       bool local_recovery_gs_bool = ndComponent.attribute(STRING_XML_ISRECOVERYGROUNDSPEED).as_bool();
       m_recovery_gs_bool = local_recovery_gs_bool;
    }
    if (!ndComponent.attribute(STRING_XML_ISRECOVERYVERTICALSPEED).empty())
    {
       bool local_recovery_vs_bool = ndComponent.attribute(STRING_XML_ISRECOVERYVERTICALSPEED).as_bool();
       m_recovery_vs_bool = local_recovery_vs_bool;
    }
    if (!ndComponent.attribute(STRING_XML_ISRECOVERYALTITUDE).empty())
    {
       bool local_recovery_alt_bool = ndComponent.attribute(STRING_XML_ISRECOVERYALTITUDE).as_bool();
       m_recovery_alt_bool = local_recovery_alt_bool;
    }
    if (!ndComponent.attribute(STRING_XML_ISCOLLISIONAVOIDANCE).empty())
    {
       bool local_ca_bands_bool = ndComponent.attribute(STRING_XML_ISCOLLISIONAVOIDANCE).as_bool();
       m_ca_bands_bool = local_ca_bands_bool;
    }
    if (!ndComponent.attribute(STRING_XML_COLLISIONAVOIDANCEFACTOR).empty())
    {
       double local_ca_factor = ndComponent.attribute(STRING_XML_COLLISIONAVOIDANCEFACTOR).as_double();
       if (local_ca_factor > 0.0 && local_ca_factor <= 1.0)
       {
           m_ca_factor = local_ca_factor;
       }
    }
    if (!ndComponent.attribute(STRING_XML_HORIZONTALNMAC).empty())
    {
       double local_horizontal_nmac_m = ndComponent.attribute(STRING_XML_HORIZONTALNMAC).as_double();
       m_horizontal_nmac_m = local_horizontal_nmac_m;
    }
        if (!ndComponent.attribute(STRING_XML_MINHORIZONTALRECOVERY).empty())
    {
       double local_min_horizontal_recovery_m = ndComponent.attribute(STRING_XML_MINHORIZONTALRECOVERY).as_double();
       if (local_min_horizontal_recovery_m > 0.0 && local_min_horizontal_recovery_m >= m_horizontal_nmac_m)
       {
           m_min_horizontal_recovery_m = local_min_horizontal_recovery_m;
           m_DTHR_m = m_min_horizontal_recovery_m;
       }
    }
    if (!ndComponent.attribute(STRING_XML_VERTICALNMAC).empty())
    {
       double local_vertical_nmac_m = ndComponent.attribute(STRING_XML_VERTICALNMAC).as_double();
       m_vertical_nmac_m = local_vertical_nmac_m;
    }
        if (!ndComponent.attribute(STRING_XML_MINVERTICALRECOVERY).empty())
    {
       double local_min_vertical_recovery_m = ndComponent.attribute(STRING_XML_MINVERTICALRECOVERY).as_double();
       if (local_min_vertical_recovery_m > 0.0 && local_min_vertical_recovery_m >= m_vertical_nmac_m)
       {
           m_min_vertical_recovery_m = local_min_vertical_recovery_m;
           m_ZTHR_m = m_min_vertical_recovery_m;
       }
    }

    if (!ndComponent.attribute(STRING_XML_HORIZONTALCONTOURTHRESHOLD).empty())
    {
       double local_contour_thr_deg = ndComponent.attribute(STRING_XML_HORIZONTALCONTOURTHRESHOLD).as_double();
       if (local_contour_thr_deg >= 0.0 && local_contour_thr_deg <= 180.0)
           m_contour_thr_deg = local_contour_thr_deg;
    }
    if (!ndComponent.attribute(STRING_XML_RTCAALERTLEVELS).empty())
    {
        int local_alert_levels = ndComponent.attribute(STRING_XML_RTCAALERTLEVELS).as_int();
        if (local_alert_levels <=3 && local_alert_levels >0)
            m_RTCA_alert_levels = local_alert_levels;
    }
    if (!ndComponent.attribute(STRING_XML_TTHR).empty())
    {
        double local_TTHR_s = ndComponent.attribute(STRING_XML_TTHR).as_double();
        if (local_TTHR_s <= m_lookahead_time_s)
            m_TTHR_s = local_TTHR_s;
    }
    if (!ndComponent.attribute(STRING_XML_EARLYALERTTIME1).empty())
    {
        double local_early_alert_time_1_s = ndComponent.attribute(STRING_XML_EARLYALERTTIME1).as_double();
        if (local_early_alert_time_1_s >= m_lookahead_time_s)
            m_early_alert_time_1_s = local_early_alert_time_1_s;
    }
    if (!ndComponent.attribute(STRING_XML_ALERTTIME1).empty())
    {
        double local_alert_time_1_s = ndComponent.attribute(STRING_XML_ALERTTIME1).as_double();
        if (local_alert_time_1_s < m_early_alert_time_1_s)
            m_alert_time_1_s = local_alert_time_1_s;
    }
    if (!ndComponent.attribute(STRING_XML_EARLYALERTTIME2).empty())
    {
        double local_early_alert_time_2_s = ndComponent.attribute(STRING_XML_EARLYALERTTIME2).as_double();
        if (local_early_alert_time_2_s <= m_early_alert_time_1_s)
            m_early_alert_time_2_s = local_early_alert_time_2_s;
    }
    if (!ndComponent.attribute(STRING_XML_ALERTTIME2).empty())
    {
        double local_alert_time_2_s = ndComponent.attribute(STRING_XML_ALERTTIME2).as_double();
        if (local_alert_time_2_s < m_early_alert_time_2_s && local_alert_time_2_s <= m_alert_time_1_s)
            m_alert_time_2_s = local_alert_time_2_s;
    }
    if (!ndComponent.attribute(STRING_XML_EARLYALERTTIME3).empty())
    {
        double local_early_alert_time_3_s = ndComponent.attribute(STRING_XML_EARLYALERTTIME3).as_double();
        if (local_early_alert_time_3_s <= m_early_alert_time_2_s)
            m_early_alert_time_3_s = local_early_alert_time_3_s;
    }
    if (!ndComponent.attribute(STRING_XML_ALERTTIME3).empty())
    {
        double local_alert_time_3_s = ndComponent.attribute(STRING_XML_ALERTTIME3).as_double();
        if (local_alert_time_3_s < m_early_alert_time_3_s && local_alert_time_3_s <= m_alert_time_2_s)
            m_alert_time_3_s = local_alert_time_3_s;
    }
    if (!ndComponent.attribute(STRING_XML_HORIZONTALDETECTIONTYPE).empty())
    {
        std::string local_horizontal_detection_type = ndComponent.attribute(STRING_XML_HORIZONTALDETECTIONTYPE).as_string();
        if (local_horizontal_detection_type == "TAUMOD" || local_horizontal_detection_type == "TCPA" || local_horizontal_detection_type == "TEP")
        {
            m_horizontal_detection_type == local_horizontal_detection_type;
        }
    }
    if (!ndComponent.attribute(STRING_XML_DTHR).empty())
    {
        double local_DTHR_m = ndComponent.attribute(STRING_XML_DTHR).as_double();
        m_DTHR_m = local_DTHR_m;
        m_min_horizontal_recovery_m = m_DTHR_m;
    }
    if (!ndComponent.attribute(STRING_XML_ZTHR).empty())
    {
        double local_ZTHR_m = ndComponent.attribute(STRING_XML_ZTHR).as_double();
        m_ZTHR_m = local_ZTHR_m;
        m_min_vertical_recovery_m = m_ZTHR_m;
    }
    if (!ndComponent.attribute(STRING_XML_AUTOMATICRESPONSESTATUS).empty())
    {
        std::string local_automatic_response_status = ndComponent.attribute(STRING_XML_AUTOMATICRESPONSESTATUS).as_string();
        if (local_automatic_response_status == "ON" || local_automatic_response_status == "on" || local_automatic_response_status == "On")
        {
            m_AutomaticResponseStatus = "ON";
        }
    }
    if (!ndComponent.attribute(STRING_XML_PRIORITYSWITCHTIME).empty())
    {
        double local_priority_time_threshold_s = ndComponent.attribute(STRING_XML_PRIORITYSWITCHTIME).as_double();
        m_priority_time_threshold_s = local_priority_time_threshold_s;
    }
    if (!ndComponent.attribute(STRING_XML_VEHICLEID).empty())
    {
        double local_vehicle_ID = ndComponent.attribute(STRING_XML_VEHICLEID).as_double();
        m_VehicleID = local_vehicle_ID;
    }
    m_daa.parameters.setLookaheadTime(m_lookahead_time_s, "s");
    m_daa.parameters.setLeftTrack(m_left_trk_deg, "deg");
    m_daa.parameters.setRightTrack(m_right_trk_deg, "deg");
    m_daa.parameters.setMaxGroundSpeed(m_max_gs_mps, "m/s");
    m_daa.parameters.setMinGroundSpeed(m_min_gs_mps, "m/s");
    m_daa.parameters.setMaxVerticalSpeed(m_max_vs_mps, "m/s");
    m_daa.parameters.setMinVerticalSpeed(m_min_vs_mps, "m/s");
    m_daa.parameters.setMaxAltitude(m_max_alt_m, "m");
    m_daa.parameters.setMinAltitude(m_min_alt_m, "m");
    m_daa.parameters.setTrackStep(m_trk_step_deg, "deg");
    m_daa.parameters.setGroundSpeedStep(m_gs_step_mps, "m/s");
    m_daa.parameters.setVerticalSpeedStep(m_vs_step_mps, "m/s");
    m_daa.parameters.setAltitudeStep(m_alt_step_m, "m");
    m_daa.parameters.setHorizontalAcceleration(m_horizontal_accel_mpsps, "m/s^2");
    m_daa.parameters.setVerticalAcceleration(m_vertical_accel_G, "G");
    m_daa.parameters.setTurnRate(m_turn_rate_degps, "deg/s");
    if (useBankAngle==true)
    {
        m_daa.parameters.setBankAngle(m_bank_angle_deg, "deg");
    }
    m_daa.parameters.setVerticalRate(m_vertical_rate_mps, "m/s");
    m_daa.parameters.setRecoveryStabilityTime(m_recovery_stability_time_s, "s");
    m_daa.parameters.setRecoveryTrackBands(m_recovery_trk_bool);
    m_daa.parameters.setRecoveryGroundSpeedBands(m_recovery_gs_bool);
    m_daa.parameters.setRecoveryVerticalSpeedBands(m_recovery_vs_bool);
    m_daa.parameters.setRecoveryAltitudeBands(m_recovery_alt_bool);
    m_daa.parameters.setCollisionAvoidanceBands(m_ca_bands_bool);
    m_daa.parameters.setCollisionAvoidanceBandsFactor(m_ca_factor);
    m_daa.parameters.setHorizontalNMAC(m_horizontal_nmac_m, "m"); 
    m_daa.parameters.setMinHorizontalRecovery(m_min_horizontal_recovery_m, "m");
    m_daa.parameters.setVerticalNMAC(m_vertical_nmac_m, "m");
    m_daa.parameters.setMinVerticalRecovery(m_min_vertical_recovery_m, "m");
    m_daa.parameters.setHorizontalContourThreshold(m_contour_thr_deg, "deg");
    larcfm::WCVTable alert_level;
    alert_level.setDTHR(m_DTHR_m,"m");
    alert_level.setZTHR(m_ZTHR_m,"m");
    alert_level.setTTHR(m_TTHR_s,"s");
    alert_level.setTCOA(0,"s");
    std::unique_ptr<larcfm::Detection3D> cd = makeDetectionPtr(m_horizontal_detection_type,alert_level);
    larcfm::Detection3D* raw_ptr; //DAIDALUS Interface requires a raw pointer
    raw_ptr = cd.get();
    m_daa.parameters.alertor.clear();
    // Configure the alert levels and timing based on the number of alert levels desired (3 levels used for all testing...may be mis-interpretation for <3)
    if (m_RTCA_alert_levels == 1)
    {
        m_daa.parameters.alertor.setConflictAlertLevel(1);
        m_daa.parameters.alertor.addLevel(larcfm::AlertThresholds(raw_ptr, m_alert_time_1_s, m_early_alert_time_1_s, larcfm::BandsRegion::NEAR));
        
    }
    else if (m_RTCA_alert_levels == 2)
    {
        m_daa.parameters.alertor.setConflictAlertLevel(2);
        m_daa.parameters.alertor.addLevel(larcfm::AlertThresholds(raw_ptr, m_alert_time_1_s, m_early_alert_time_1_s, larcfm::BandsRegion::MID));
        m_daa.parameters.alertor.addLevel(larcfm::AlertThresholds(raw_ptr, m_alert_time_2_s, m_early_alert_time_2_s, larcfm::BandsRegion::NEAR));
    }
    else 
    {
        m_daa.parameters.alertor.setConflictAlertLevel(3);
        m_daa.parameters.alertor.addLevel(larcfm::AlertThresholds(raw_ptr, m_alert_time_1_s, m_early_alert_time_1_s, larcfm::BandsRegion::FAR));
        m_daa.parameters.alertor.addLevel(larcfm::AlertThresholds(raw_ptr, m_alert_time_2_s, m_early_alert_time_2_s, larcfm::BandsRegion::MID));
        m_daa.parameters.alertor.addLevel(larcfm::AlertThresholds(raw_ptr, m_alert_time_3_s, m_early_alert_time_3_s, larcfm::BandsRegion::NEAR));
    }
    
    raw_ptr = nullptr; //clean up raw pointer after use
    m_daa.parameters.saveToFile("testConfiguraton"); // produce a text file with the DAIDALUS configuration used
    addSubscriptionAddress(afrl::cmasi::AirVehicleState::Subscription);
    addSubscriptionAddress(uxas::messages::uxnative::StartupComplete::Subscription);
    addSubscriptionAddress(afrl::cmasi::AutomationResponse::Subscription);
    addSubscriptionAddress(afrl::cmasi::MissionCommand::Subscription);
    
    return (isSuccess);
}

bool DAIDALUS_Processing::initialize()
{
    // perform any required initialization before the service is started

    
    //std::cout << "*** INITIALIZING:: Service[" << s_typeName() << "] Service Id[" << m_serviceId << "] with working directory [" << m_workDirectoryName << "] *** " << std::endl;
    
    return (true);
}

bool DAIDALUS_Processing::start() 
{
    // perform any actions required at the time the service starts
    //std::cout << "*** STARTING:: Service[" << s_typeName() << "] Service Id[" << m_serviceId << "] with working directory [" << m_workDirectoryName << "] *** " << std::endl;
    
    return (true);
};

bool DAIDALUS_Processing::terminate()
{
    // perform any action required during service termination, before destructor is called.
    std::cout << "*** TERMINATING:: Service[" << s_typeName() << "] Service Id[" << m_serviceId << "] with working directory [" << 
            m_workDirectoryName << "] *** " << std::endl;    
    return (true);
}

bool DAIDALUS_Processing::processReceivedLmcpMessage(std::unique_ptr<uxas::communications::data::LmcpMessage> receivedLmcpMessage)
{
    if (afrl::cmasi::isAutomationResponse(receivedLmcpMessage->m_object))
    {
        std::shared_ptr<afrl::cmasi::AutomationResponse> pAutoResponse = 
                std::static_pointer_cast<afrl::cmasi::AutomationResponse>(receivedLmcpMessage->m_object);
        for (uint32_t i = 0; i < pAutoResponse->getMissionCommandList().size(); i++)
        {
            if (pAutoResponse->getMissionCommandList()[i]->getVehicleID() == m_VehicleID)
            {
                m_MissionCommand = std::make_shared<afrl::cmasi::MissionCommand>(*(pAutoResponse->getMissionCommandList()[i]->clone()));    //why doesn't this cause memory leaks from not getting cleaned up?
                m_isOnMission = true;
                break;
            }
        }
        m_isReadyToActMissionCommand = true;
    }
    else if (afrl::cmasi::isMissionCommand(receivedLmcpMessage->m_object))
    {
        //Process mission commands to retain a copy of the mission command--Mission commands for non-ownship vehicles are ignored
        //Assumption that only one mission command message expected...subsequent mission commands will lead to unintended behavior.
        std::shared_ptr<afrl::cmasi::MissionCommand> pMissionCommand = 
                std::static_pointer_cast<afrl::cmasi::MissionCommand>(receivedLmcpMessage->m_object);
        if (pMissionCommand->getVehicleID() == m_VehicleID)
        {
            m_MissionCommand = std::make_shared<afrl::cmasi::MissionCommand>(*(pMissionCommand->clone()));  //why doesn't this cause memory leaks from not getting cleaned up?
            m_isOnMission = true;
        }
        m_isReadyToActMissionCommand = true;
    }
    else if (afrl::cmasi::isAirVehicleState(receivedLmcpMessage->m_object))
    {
        static bool bFirst = true; // flag for first message received
        
        // Form DAIDLUS_Configuration on reception of the first message
        if (bFirst)
        {
            std::shared_ptr<larcfm::DAIDALUS::DAIDALUSConfiguration> DetectionConfiguration = std::make_shared<larcfm::DAIDALUS::DAIDALUSConfiguration>();
            DetectionConfiguration->setEntityId(m_VehicleID);
            DetectionConfiguration->setLookAheadTime(m_daa.parameters.getLookaheadTime("s"));
            DetectionConfiguration->setLeftTrack(m_daa.parameters.getLeftTrack("deg"));
            DetectionConfiguration->setRightTrack(m_daa.parameters.getRightTrack("deg"));
            DetectionConfiguration->setMaxGroundSpeed(m_daa.parameters.getMaxGroundSpeed("m/s"));
            DetectionConfiguration->setMinGroundSpeed(m_daa.parameters.getMinGroundSpeed("m/s"));
            DetectionConfiguration->setMaxVerticalSpeed(m_daa.parameters.getMaxVerticalSpeed("m/s"));
            DetectionConfiguration->setMinVerticalSpeed(m_daa.parameters.getMinVerticalSpeed("m/s"));
            DetectionConfiguration->setMaxAltitude(m_daa.parameters.getMaxAltitude("m"));
            DetectionConfiguration->setMinAltitude(m_daa.parameters.getMinAltitude("m"));
            DetectionConfiguration->setTrackStep(m_daa.parameters.getTrackStep("deg"));
            DetectionConfiguration->setGroundSpeedStep(m_daa.parameters.getGroundSpeedStep("m/s"));
            DetectionConfiguration->setVerticalSpeedStep(m_daa.parameters.getVerticalSpeedStep("m/s"));
            DetectionConfiguration->setAltitudeStep(m_daa.parameters.getAltitudeStep("m"));
            DetectionConfiguration->setHorizontalAcceleration(m_daa.parameters.getHorizontalAcceleration("m/s^2"));
            DetectionConfiguration->setVerticalAcceleration(m_daa.parameters.getVerticalAcceleration("G"));
            DetectionConfiguration->setTurnRate(m_daa.parameters.getTurnRate("deg/s"));
            DetectionConfiguration->setBankAngle(m_daa.parameters.getBankAngle("deg"));
            DetectionConfiguration->setVerticalRate(m_daa.parameters.getVerticalRate("m/s"));
            DetectionConfiguration->setRecoveryStabilityTime(m_daa.parameters.getRecoveryStabilityTime("s"));
            DetectionConfiguration->setIsRecoveryTrackBands(m_daa.parameters.isEnabledRecoveryTrackBands());
            DetectionConfiguration->setIsRecoveryGroundSpeedBands(m_daa.parameters.isEnabledRecoveryGroundSpeedBands());
            DetectionConfiguration->setIsRecoveryVerticalSpeedBands(m_daa.parameters.isEnabledRecoveryVerticalSpeedBands());
            DetectionConfiguration->setIsRecoveryAltitudeBands(m_daa.parameters.isEnabledRecoveryAltitudeBands());
            DetectionConfiguration->setIsCollisionAvoidanceBands(m_daa.parameters.isEnabledCollisionAvoidanceBands());
            DetectionConfiguration->setHorizontalNMAC(m_daa.parameters.getHorizontalNMAC("m"));
            DetectionConfiguration->setMinHorizontalRecovery(m_daa.parameters.getMinHorizontalRecovery("m"));
            DetectionConfiguration->setVerticalNMAC(m_daa.parameters.getVerticalNMAC("m"));
            DetectionConfiguration->setMinVerticalRecovery(m_daa.parameters.getMinVerticalRecovery("m"));
            DetectionConfiguration->setHorizontalContourThreshold(m_daa.parameters.getHorizontalContourThreshold("m"));
            DetectionConfiguration->setDTHR(m_DTHR_m);
            DetectionConfiguration->setZTHR(m_ZTHR_m);
            DetectionConfiguration->setTTHR(m_TTHR_s);
            DetectionConfiguration->setRTCAAlertLevels(m_RTCA_alert_levels);
            DetectionConfiguration->setAlertTime1(m_alert_time_1_s);
            DetectionConfiguration->setEarlyAlertTime1(m_early_alert_time_1_s);
            DetectionConfiguration->setAlertTime2(m_alert_time_2_s);
            DetectionConfiguration->setEarlyAlertTime2(m_early_alert_time_2_s);
            DetectionConfiguration->setAlertTime3(m_alert_time_3_s);
            DetectionConfiguration->setEarlyAlertTime3(m_early_alert_time_3_s);
            DetectionConfiguration->setHorizontalDetectionType(m_horizontal_detection_type);

            m_alertlevels_count = DetectionConfiguration->getRTCAAlertLevels();
            if (m_alertlevels_count == 1)
            {
                m_action_time_threshold_s = DetectionConfiguration->getAlertTime1();
            }
            else if (m_alertlevels_count ==2)
            {
                m_action_time_threshold_s = DetectionConfiguration->getAlertTime2();
            }
            else
            {
                m_action_time_threshold_s = DetectionConfiguration->getAlertTime3();
            }
            
            m_heading_interval_buffer_deg = m_trk_step_deg / 2.0;
            m_groundspeed_interval_buffer_mps = m_gs_step_mps / 2.0;
            m_verticalspeed_interval_buffer_mps = m_vs_step_mps / 2.0;
            m_altitude_interval_buffer_m = m_alt_step_m / 2.0;
            m_isReadyToActConfiguration = true;  //boolean indicating if a threshold time is set from reading a DAIDALUS configuration parameter.

            sendSharedLmcpObjectBroadcastMessage(DetectionConfiguration);
            bFirst = false; //turn off flag for first message
        }
        
        // handle message
        std::shared_ptr<afrl::cmasi::AirVehicleState> airVehicleState = std::static_pointer_cast<afrl::cmasi::AirVehicleState> (receivedLmcpMessage->m_object);
        if (airVehicleState->getID() == m_VehicleID)
        {
            m_CurrentState.altitude_m = airVehicleState->getLocation()->getAltitude();
            m_CurrentState.heading_deg = std::fmod(airVehicleState->getCourse()+360.0,360.0); //Course reported between -180 and 180 deg
            m_CurrentState.horizontal_speed_mps = airVehicleState->getGroundspeed();
            m_CurrentState.vertical_speed_mps = airVehicleState->getVerticalSpeed();
            m_CurrentState.latitude_deg = airVehicleState->getLocation()->getLatitude();
            m_CurrentState.longitude_deg = airVehicleState->getLocation()->getLongitude();
            m_CurrentState.altitude_type = airVehicleState->getLocation()->getAltitudeType();
            m_CurrentState.speed_type = afrl::cmasi::SpeedType::Groundspeed;
            m_CurrentState.total_velocity_mps = airVehicleState->getAirspeed();
            m_CurrentState.time_s = airVehicleState->getTime()*MILLISECONDTOSECOND;
            if (m_isOnMission)
            {
                m_NextWaypoint = airVehicleState->getCurrentWaypoint();
                m_isReadyToActWaypoint = true;
            }
        }

        struct violation_data // grouped violation data including time to violation and intruder alert level
        {
            double TimeToViolation;
            int IntruderAlertLevel;
        };
        std::unordered_map<int64_t, violation_data> detectedViolations;        
        //add air vehicle message state to the Daidalus Object
        MydaidalusPackage vehicleInfo;  
        vehicleInfo.m_daidalusPosition = larcfm::Position::makeLatLonAlt(airVehicleState->getLocation()->getLatitude(), "deg",
                                         airVehicleState->getLocation()->getLongitude(), "deg", airVehicleState->getLocation()->getAltitude(), "m");      
        float u_mps = airVehicleState->getU();
        float v_mps = airVehicleState->getV();
        float w_mps = airVehicleState->getW();
        float Phi_deg = airVehicleState->getRoll();
        float Theta_deg = airVehicleState->getPitch();
        float Psi_deg = airVehicleState->getHeading();  //currently does not account for wind.
        double velocityX_mps, velocityY_mps, velocityZ_mps;
        makeVelocityXYZ(u_mps, v_mps, w_mps, n_Const::c_Convert::toRadians(Phi_deg), n_Const::c_Convert::toRadians(Theta_deg), 
                n_Const::c_Convert::toRadians(Psi_deg), velocityX_mps, velocityY_mps, velocityZ_mps);
        // DAIDALUS expects ENUp reference while UxAS internally used NEDown--covert UxAS velocities to DAIDALUS velocities
        double daidalusVelocityZ_mps = -velocityZ_mps;    
        double daidalusVelocityX_mps = velocityY_mps;
        double daidalusVelocityY_mps = velocityX_mps;
        vehicleInfo.m_daidalusVelocity = larcfm::Velocity::makeVxyz(daidalusVelocityX_mps, daidalusVelocityY_mps, "m/s", daidalusVelocityZ_mps, "m/s");
        vehicleInfo.m_daidalusTime_s = airVehicleState->getTime()*MILLISECONDTOSECOND; // conversion from UxAS representation of time in milliseconds to DAIDALUS representation of time in seconds
        vehicleInfo.latitude_deg = airVehicleState->getLocation()->getLatitude();
        vehicleInfo.longitude_deg = airVehicleState->getLocation()->getLongitude();
        m_daidalusVehicleInfo[airVehicleState->getID()] = vehicleInfo;
        // Conditional check for appropriateness off a well clear violation check-- 2 known vehicle states including the ownship
        if (m_daidalusVehicleInfo.size()>1 && m_daidalusVehicleInfo.count(m_VehicleID)>0)    
        { 
            m_daa.setOwnshipState(std::to_string(m_VehicleID), m_daidalusVehicleInfo[m_VehicleID].m_daidalusPosition, 
                m_daidalusVehicleInfo[m_VehicleID].m_daidalusVelocity, m_daidalusVehicleInfo[m_VehicleID].m_daidalusTime_s); //set DAIDALUS object ownship state
            for (const auto& vehiclePackagedInfo : m_daidalusVehicleInfo)
            {
                //add intruder traffic state to DAIDALUS object
                if ((vehiclePackagedInfo.first!=m_VehicleID) && 
                        (std::abs(m_daidalusVehicleInfo[m_VehicleID].m_daidalusTime_s - vehiclePackagedInfo.second.m_daidalusTime_s) <= m_staleness_time_s))
                    {
                        m_daa.addTrafficState(std::to_string(vehiclePackagedInfo.first), vehiclePackagedInfo.second.m_daidalusPosition, 
                                vehiclePackagedInfo.second.m_daidalusVelocity, vehiclePackagedInfo.second.m_daidalusTime_s);
                    }
            }
            //Following conditional always executes if the DAIDALUS object was correctly configured with more than just the ownship
            if (m_daa.numberOfAircraft()>1) //Perform well_clear violation check if DAIDALUS object contains ownship and at least one intruder traffic state
            {
                // Determine the time to violation of the wellclear volume of the ownship by each intruder aircaft
                for (int intruderIndex = 1; intruderIndex<=m_daa.numberOfAircraft()-1; ++intruderIndex)
                {
                    double timeToViolation_s = m_daa.timeToViolation(intruderIndex);
                    int alert_level = m_daa.alerting(intruderIndex);
                    violation_data temp_data;
                    temp_data.TimeToViolation = timeToViolation_s;
                    temp_data.IntruderAlertLevel = alert_level;
                    if (timeToViolation_s != PINFINITY && timeToViolation_s != NaN)
                    { 
                        detectedViolations[std::stoi(m_daa.getAircraftState(intruderIndex).getId(),nullptr,10)] = temp_data;
                    }
                }
                //send out response
                //Create DAIDALUS bands object and compute conflict/peripheral bands
                larcfm::KinematicMultiBands m_daa_bands(m_daa.parameters);
                m_daa.kinematicMultiBands(m_daa_bands);
                std::shared_ptr<larcfm::DAIDALUS::WellClearViolationIntervals>  nogo_ptr = 
                        std::make_shared<larcfm::DAIDALUS::WellClearViolationIntervals>();  //Compose violations message
                larcfm::TrafficState daa_own = m_daa.getOwnshipState();
                for (auto itViolations = detectedViolations.cbegin(); itViolations !=detectedViolations.cend(); itViolations++)
                {
                    nogo_ptr->getEntityList().push_back(itViolations->first);
                    nogo_ptr->getTimeToViolationList().push_back(itViolations->second.TimeToViolation);
                    nogo_ptr->getAlertLevelList().push_back(itViolations->second.IntruderAlertLevel);
                }
                nogo_ptr->setEntityId(m_VehicleID);  //Ownship Id
                nogo_ptr->setCurrentHeading(daa_own.track("deg"));  //DAIDALUS current heading--0deg = TrueNorth Currently does not account for wind
                nogo_ptr->setCurrentGoundSpeed(daa_own.groundSpeed("m/s")); //DAIDALUS current ground speed--does not account for wind
                nogo_ptr->setCurrentVerticalSpeed(daa_own.verticalSpeed("m/s"));    //DAIDALUS current vertical speed--does not account for wind
                nogo_ptr->setCurrentAltitude(daa_own.altitude("m"));    //DAIDALUS current altitude
                nogo_ptr->setCurrentLatitude(m_daidalusVehicleInfo[m_VehicleID].latitude_deg);    //Current ownship latitude
                nogo_ptr->setCurrentLongitude(m_daidalusVehicleInfo[m_VehicleID].longitude_deg);  //Current ownship longitude
                nogo_ptr->setCurrentTime(m_daidalusVehicleInfo[m_VehicleID].m_daidalusTime_s);
                
                for (int ii = 0; ii < m_daa_bands.trackLength(); ii++)  //ground track bands
                {
                    std::unique_ptr<larcfm::DAIDALUS::GroundHeadingInterval> pTempPtr (new larcfm::DAIDALUS::GroundHeadingInterval);
                    larcfm::Interval iv = m_daa_bands.track(ii,"deg");
                    double lower_trk_deg = iv.low; //lower bound on interval
                    double upper_trk_deg = iv.up;   //upper bound on interval
                    larcfm::BandsRegion::Region regionType = m_daa_bands.trackRegion(ii);   //region classification for above interval
                    //Currently only considering conflict bands classified as NEAR, MID, or FAR
                    if (regionType == larcfm::BandsRegion::FAR || regionType == larcfm::BandsRegion::MID || regionType == larcfm::BandsRegion::NEAR)
                    {
                        larcfm::DAIDALUS::BandsRegion::BandsRegion temp_regionType;
                        if (regionType == larcfm::BandsRegion::FAR)
                        {
                            temp_regionType = larcfm::DAIDALUS::BandsRegion::FAR;
                        }
                        else if (regionType == larcfm::BandsRegion::MID)
                        {
                            temp_regionType = larcfm::DAIDALUS::BandsRegion::MID;
                        }
                        else
                        {
                            temp_regionType = larcfm::DAIDALUS::BandsRegion::NEAR;
                        }
                        pTempPtr->getGroundHeadings()[0] = lower_trk_deg;
                        pTempPtr->getGroundHeadings()[1] = upper_trk_deg;
                        nogo_ptr->getWCVGroundHeadingIntervals().push_back(pTempPtr.release());
                        nogo_ptr->getWCVGroundHeadingRegions().push_back(temp_regionType);
                    }
                    else if (regionType == larcfm::BandsRegion::RECOVERY)
                    {
                        std::unique_ptr<larcfm::DAIDALUS::GroundHeadingRecoveryInterval> pRecoveryPtr (new larcfm::DAIDALUS::GroundHeadingRecoveryInterval);
                        pRecoveryPtr->getRecoveryGroundHeadings()[0] = lower_trk_deg;
                        pRecoveryPtr->getRecoveryGroundHeadings()[1] = upper_trk_deg;
                        nogo_ptr->getRecoveryGroundHeadingIntervals().push_back(pRecoveryPtr.release());
                    }
                    
                }
                
                for (int ii = 0; ii < m_daa_bands.groundSpeedLength();++ii) //ground speed bands
                {
                    std::unique_ptr<larcfm::DAIDALUS::GroundSpeedInterval> pTempPtr (new larcfm::DAIDALUS::GroundSpeedInterval);
                    larcfm::Interval iv = m_daa_bands.groundSpeed(ii, "mps");
                    double lower_gs_mps = iv.low;
                    double upper_gs_mps =iv.up;
                    larcfm::BandsRegion::Region regionType = m_daa_bands.groundSpeedRegion(ii);
                    if (regionType == larcfm::BandsRegion::FAR || regionType == larcfm::BandsRegion::MID || regionType == larcfm::BandsRegion::NEAR)
                    {
                        larcfm::DAIDALUS::BandsRegion::BandsRegion temp_regionType;
                        if (regionType == larcfm::BandsRegion::FAR)
                        {
                            temp_regionType = larcfm::DAIDALUS::BandsRegion::FAR;
                        }
                        else if (regionType == larcfm::BandsRegion::MID)
                        {
                            temp_regionType = larcfm::DAIDALUS::BandsRegion::MID;
                        }
                        else
                        {
                            temp_regionType = larcfm::DAIDALUS::BandsRegion::NEAR;
                        }
                        pTempPtr->getGroundSpeeds()[0] = lower_gs_mps;
                        pTempPtr->getGroundSpeeds()[1] = upper_gs_mps;
                        nogo_ptr->getWCVGroundSpeedIntervals().push_back(pTempPtr.release());
                        nogo_ptr->getWCVGroundSpeedRegions().push_back(temp_regionType);
                    }
                    else if (regionType == larcfm::BandsRegion::RECOVERY)
                    {
                        std::unique_ptr<larcfm::DAIDALUS::GroundSpeedRecoveryInterval> pRecoveryPtr (new larcfm::DAIDALUS::GroundSpeedRecoveryInterval);
                        pRecoveryPtr->getRecoveryGroundSpeeds()[0] = lower_gs_mps;
                        pRecoveryPtr->getRecoveryGroundSpeeds()[1] = upper_gs_mps;
                        nogo_ptr->getRecoveryGroundSpeedIntervals().push_back(pRecoveryPtr.release());
                    }
                    
                }
                
                for (int ii =0; ii < m_daa_bands.verticalSpeedLength();++ii)    //vertical speed bands
                {
                    std::unique_ptr<larcfm::DAIDALUS::VerticalSpeedInterval> pTempPtr (new larcfm::DAIDALUS::VerticalSpeedInterval);
                    larcfm::Interval iv = m_daa_bands.verticalSpeed(ii, "mps");
                    double lower_vs_mps = iv.low;
                    double upper_vs_mps = iv.up;
                    larcfm::BandsRegion::Region regionType = m_daa_bands.verticalSpeedRegion(ii);
                    if (regionType == larcfm::BandsRegion::FAR || regionType == larcfm::BandsRegion::MID || regionType == larcfm::BandsRegion::NEAR)
                    {
                        larcfm::DAIDALUS::BandsRegion::BandsRegion temp_regionType;
                        if (regionType == larcfm::BandsRegion::FAR)
                        {
                            temp_regionType = larcfm::DAIDALUS::BandsRegion::FAR;
                        }
                        else if (regionType == larcfm::BandsRegion::MID)
                        {
                            temp_regionType = larcfm::DAIDALUS::BandsRegion::MID;
                        }
                        else
                        {
                            temp_regionType = larcfm::DAIDALUS::BandsRegion::NEAR;
                        }
                        pTempPtr->getVerticalSpeeds()[0] = lower_vs_mps;
                        pTempPtr->getVerticalSpeeds()[1] = upper_vs_mps;
                        nogo_ptr->getWCVVerticalSpeedIntervals().push_back(pTempPtr.release());
                        nogo_ptr->getWCVVerticalSpeedRegions().push_back(temp_regionType);
                    }
                    else if (regionType == larcfm::BandsRegion::RECOVERY)
                    {
                        std::unique_ptr<larcfm::DAIDALUS::VerticalSpeedRecoveryInterval> pRecoveryPtr (new larcfm::DAIDALUS::VerticalSpeedRecoveryInterval);
                        pRecoveryPtr->getRecoveryVerticalSpeed()[0] = lower_vs_mps;
                        pRecoveryPtr->getRecoveryVerticalSpeed()[1] = upper_vs_mps;
                        nogo_ptr->getRecoveryVerticalSpeedIntervals().push_back(pRecoveryPtr.release());
                    }
                    
                }
                
                for (int ii = 0; ii < m_daa_bands.altitudeLength(); ++ii)   //altitude bands
                {
                    std::unique_ptr<larcfm::DAIDALUS::AltitudeInterval> pTempPtr (new larcfm::DAIDALUS::AltitudeInterval);
                    larcfm::Interval iv = m_daa_bands.altitude(ii, "m");
                    double lower_alt_m = iv.low;
                    double upper_alt_m = iv.up;
                    larcfm::BandsRegion::Region regionType = m_daa_bands.altitudeRegion(ii);
                    if (regionType == larcfm::BandsRegion::FAR || regionType == larcfm::BandsRegion::MID || regionType == larcfm::BandsRegion::NEAR)
                    {
                        larcfm::DAIDALUS::BandsRegion::BandsRegion temp_regionType;
                        if (regionType == larcfm::BandsRegion::FAR)
                        {
                            temp_regionType = larcfm::DAIDALUS::BandsRegion::FAR;
                        }
                        else if (regionType == larcfm::BandsRegion::MID)
                        {
                            temp_regionType = larcfm::DAIDALUS::BandsRegion::MID;
                        }
                        else
                        {
                            temp_regionType = larcfm::DAIDALUS::BandsRegion::NEAR;
                        }
                        pTempPtr->getAltitude()[0] = lower_alt_m;
                        pTempPtr->getAltitude()[1] = upper_alt_m;
                        nogo_ptr->getWCVAlitudeIntervals().push_back(pTempPtr.release());
                        nogo_ptr->getWCVAltitudeRegions().push_back(temp_regionType);
                    }
                    else if (regionType == larcfm::BandsRegion::RECOVERY)
                    {
                        std::unique_ptr<larcfm::DAIDALUS::AltitudeRecoveryInterval> pRecoveryPtr (new larcfm::DAIDALUS::AltitudeRecoveryInterval);
                        pRecoveryPtr->getRecoveryAltitude()[0] = lower_alt_m;
                        pRecoveryPtr->getRecoveryAltitude()[1] = upper_alt_m;
                        nogo_ptr->getRecoveryAltitudeIntervals().push_back(pRecoveryPtr.release());
                    }
                    
                }
                
                sendSharedLmcpObjectBroadcastMessage(nogo_ptr);

                if (m_AutomaticResponseStatus == "ON")
                {
                    if (m_isReadyToActMissionCommand && m_isReadyToActConfiguration)
                    {
                        std::vector<int64_t> ConflictResolutionList;
                        int local_priority = 0;
                        for (size_t i = 0; i < nogo_ptr->getEntityList().size(); i++)
                        {
                            if (nogo_ptr->getTimeToViolationList()[i] <= m_action_time_threshold_s)
                            {
                                ConflictResolutionList.push_back(nogo_ptr->getEntityList()[i]);
                            }
                            if (nogo_ptr->getTimeToViolationList()[i] <= m_priority_time_threshold_s)
                            {
                                local_priority = local_priority + 1;
                            }
                        }
                        if (local_priority > 0) 
                        {
                            m_priority = High;
                        }
                        else
                        {
                            m_priority = Standard;
                        }
                        int64_t RoW;
                        if (ConflictResolutionList.size() > 0)
                        {
                            RoW = INT64_MAX;
                        }
                        else
                        {
                            RoW = INT64_MIN;
                        }
                        for (auto i : ConflictResolutionList)
                        {
                            if (i < RoW)
                            {
                                RoW = i;
                            }
                        }
                        if (ConflictResolutionList.size() > 0)
                        {
                            m_state = InConflict;
                        }
                        switch (m_state)
                        {
                            case OnMission:
                                break;
                            case InConflict:
                                if (m_VehicleID < RoW)
                                {
                                    //Ownship has the Right of Way and therefore should take no action
                                    m_state = OnHold;
                                }
                                else
                                {
                                    SetDivertState(nogo_ptr);
                                    std::unique_ptr<afrl::cmasi::FlightDirectorAction> pDivertThisWay = uxas::stduxas::make_unique<afrl::cmasi::FlightDirectorAction>();
                                    pDivertThisWay->setHeading(static_cast<float>(m_DivertState.heading_deg)); 
                                    pDivertThisWay->setAltitude(m_DivertState.altitude_m);
                                    pDivertThisWay->setSpeed(m_DivertState.horizontal_speed_mps);
                                    pDivertThisWay->setAltitudeType(m_DivertState.altitude_type);
                                    pDivertThisWay->setClimbRate(m_DivertState.vertical_speed_mps);
                                    std::shared_ptr<afrl::cmasi::VehicleActionCommand> pAvoidViolation = std::make_shared<afrl::cmasi::VehicleActionCommand>();
                                    pAvoidViolation->setCommandID(getUniqueEntitySendMessageId());
                                    pAvoidViolation->setVehicleID(m_VehicleID);
                                    pAvoidViolation->setStatus(afrl::cmasi::CommandStatusType::Approved);
                                    pAvoidViolation->getVehicleActionList().push_back(pDivertThisWay.release());
                                    m_isOnMission = false;
                                    sendSharedLmcpObjectBroadcastMessage(pAvoidViolation);
                                    std::cout << "ENTITY " << m_VehicleID << " is conducting a divert maneuver." << std::endl;  
                                    m_state = OnHold;
                                    
                                }
                                
                                break;
                            case OnHold:
                                if (m_isOnMission)
                                {
                                    m_state = OnMission;
                                }
                                else
                                {
                                    if (isSafeToReturnToMission(nogo_ptr))
                                    {
                                        //send safe to return to mission command
                                        if (m_NextWaypoint != -1)
                                        {
                                            //find pointer to the last next waypoint when still on mission. Then send MissionCommand that eliminates all points
                                            //more than 1 point prior to last known on mission waypoint due to WaypointManager's handling of Mission Commands.
                                             int cutpoint_index = 0;
                                            // for (auto waypoint_index = m_MissionCommand->getWaypointList().begin(); waypoint_index < m_MissionCommand->getWaypointList().end(); waypoint_index++)
                                            for (auto waypoint_index : m_MissionCommand->getWaypointList())
                                            {
                                                if ((waypoint_index->getNumber()) == m_NextWaypoint)
                                                {
                                                    break;
                                                }
                                                cutpoint_index+= 1;
                                            }
                                            afrl::cmasi::Waypoint* NextWaypoint = m_MissionCommand->getWaypointList()[cutpoint_index];
                                            auto iCutPoint = std::find(m_MissionCommand->getWaypointList().begin(), m_MissionCommand->getWaypointList().end(), NextWaypoint);
                                             if (iCutPoint != m_MissionCommand->getWaypointList().end() && iCutPoint != m_MissionCommand->getWaypointList().begin())
                                                {
                                                    m_MissionCommand->getWaypointList().erase(m_MissionCommand->getWaypointList().begin(), iCutPoint-1);
                                                }
                                             /*   int counter = 0;
                                            for (auto waypoint_index2 : m_MissionCommand->getWaypointList())
                                            {
                                                if (counter < cutpoint_index)
                                                {
                                                    waypoint_index2->setAltitude(NextWaypoint->getAltitude());
                                                    waypoint_index2->setAltitudeType(NextWaypoint->getAltitudeType());
                                                    waypoint_index2->setLatitude(NextWaypoint->getLatitude());
                                                    waypoint_index2->setLongitude(NextWaypoint->getLongitude());
                                                    waypoint_index2->setNumber(NextWaypoint->getNumber());
                                                    waypoint_index2->setNextWaypoint(NextWaypoint->getNextWaypoint());
                                                    waypoint_index2->setSpeed(NextWaypoint->getSpeed());
                                                    waypoint_index2->setSpeedType(NextWaypoint->getSpeedType());
                                                    waypoint_index2->setClimbRate(NextWaypoint->getClimbRate());
                                                    waypoint_index2->setTurnType(NextWaypoint->getTurnType());
                                                    waypoint_index2->setContingencyWaypointA(NextWaypoint->getContingencyWaypointA());
                                                    waypoint_index2->setContingencyWaypointB(NextWaypoint->getContingencyWaypointB());
                                                }
                                                counter +=1;
                                                // std::cout << "Current iteration is " << counter << " Cut-off for repetition is " << cutpoint_index << std::endl;
                                            } */
                                            m_MissionCommand->setFirstWaypoint(m_NextWaypoint);
                                            sendSharedLmcpObjectBroadcastMessage(m_MissionCommand);
                                            m_state = OnMission;
                                            m_isOnMission = true;
                                        }
                                    }
                                }
                                break;
                        }
                    }
                }
            }
        }

    }
    return false;
}

} //namespace service
} //namespace uxas
