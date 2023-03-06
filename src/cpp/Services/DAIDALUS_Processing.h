// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

/* 
 * File:   DAIDALUS_WCV_Detection.h
 * Author: SeanR
 *
 * Created on Oct 01 2020
 */

#ifndef UXAS_DAIDALUS_PROCESSING_H
#define UXAS_DAIDALUS_PROCESSING_H



#include "Constants/Convert.h"
#include "Daidalus.h"
#include "KinematicMultiBands.h"
#include "Position.h"
#include "ServiceBase.h"
#include "Velocity.h"
#include "afrl/cmasi/Waypoint.h"
#include "afrl/cmasi/MissionCommand.h"
#include "afrl/cmasi/AltitudeType.h"
#include "afrl/cmasi/SpeedType.h"
#include "larcfm/DAIDALUS/WellClearViolationIntervals.h"

#include <array>
#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

namespace uxas
{
namespace service
{

/*! \class DAIDALUS_Processing
    \brief This is a service that acts as an interface between UxAS and NASA's DAIDALUS.
 * 
 * 
 * Configuration String: <Service Type="DAIDALUS_Processing" LookAheadTime="180" LeftTrack="180" RightTrack="180" MinGroundSpeed="5" 
 * MaxGroundSpeed="360.11 MinVerticalSpeed="xx" MaxVerticalSpeed="xx" MinAltitude="xx" MaxAltitude="xx" TrackStep="xx" GroundSpeedStep="xx" 
 * VerticalSpeedStep="xx" AltitudeStep="xx" HorizontalAcceleration="xx" VerticalAcceleration="xx" TurnRate="xx" BankAngle="xx" VerticalRate="xx"
 * RecoveryStabilityTime"xx" MinHorizontalRecovery="xx" MinVerticalRecovery="xx" isRecoveryTack="true" isRecoveryGroundTracK="true" 
 * isRecoveryVerticalSpeed="true" isRecoveryAltitude="true" isCollisionAvoidance="false" CollisionAvoidanceFactor="xx"
 * HorizontalNMAC="xx" VerticalNMAC="xx" HorizontalContourThreshold="xx" TTHR="xx" RTCAAlertingLevels="x" AlertingTime1="xx" EarlyAlertingTime1="xx" 
 * AlertingTime2="xx" EarlyAlertingTime2="xx" AlertingTime3="xx" EarlyAlertingTime3="xx" HorizontalDetectionType="TAUMOD" />
 * 
 * Options:
 *  - LookAheadTime - time horizon for all DAIDALUS functions
 *  - LeftTrack - relative maximum horizontal direction maneuver to the left of current ownship direction 
 *  - RightTrack - relative maximum horizontal direction maneuver to the right of the current ownship direction 
 *  - MinGroundSpeed - absolute minimum horizontal speed maneuver 
 *  - MaxGroundSpeed - absolute maximum horizontal speed maneuver
 *  - MinVerticalSpeed - absolute minimum vertical speed maneuver
 *  - MaxVerticalSpeed - absolute maximum vertical speed maneuver
 *  - MinAltitude - absolute minimum altitude maneuver
 *  - MaxAltitude - absolute maximum altitude maneuver
 *  - TrackStep - granularity of horizontal direction maneuvers
 *  - GroundSpeedStep - granularity of horizontal speed maneuvers
 *  - VerticalSpeedStep - granularity of vertical speed maneuvers
 *  - AltitudeStep - granularity of altitude speed maneuvers
 *  - HorizontalAcceleration - horizontal acceleration used in the computation of horizontal speed maneuvers
 *  - VerticalAcceleration - vertical acceleration use in the computation of vertical speed maneuvers
 *  - TurnRate - turn rate used in the computation of horizontal direction maneuvers
 *  - BankAngle - bank angle used in the computation of horizontal direction maneuvers
 *  - VerticalRate - vertical rate used in the computation of altitude maneuvers
 *  - RecoveryStabilityTime - time delay to stabilize recovery maneuvers
 *  - MinHorizontalRecovery - minimum horizontal separation used in the computation of recovery maneuvers-Also sets the horizontal threshold for 
 *                            Well-ClearVolume (WCV) 
 *  - MinVerticalRecovery - minimum vertical separation used in the computation of recovery maneuvers-Also sets the vertical threshold for WCV 
 *  - isRecoveryTrack - enable computation of horizontal direction recovery maneuvers
 *  - isRecoveryGroundTrack - enable computation of horizontal speed recovery maneuvers
 *  - isRecoveryVerticalSpeed - enable computation of vertical speed recovery maneuvers
 *  - isRecoveryAltitude - enable computation of altitude recovery maneuvers
 *  - isCollisionAvoidance - enable computation of collision avoidance maneuvers
 *  - CollisionAvoidanceFactor - factor to reduce min horizontal/vertical recovery separation when computing collision avoidance maneuvers
 *  - HorizontalNMAC - horizontal NMAC
 *  - VerticalNMAC - vertical NMAC 
 *  - HorizontalContourThreshold - threshold relative to ownship horizontal direction for the computation off horizontal contours
 *  - TTHR - time threshold forWCVvolume
 *  - RTCAAlertLevels - number of classification bins to be displayed in maneuver guidance processing-max 3
 *  - AlertTime1 - the time limit to determine if a WCV violation will occur before for the FAR classification
 *  - EarlyAlerTime1 - the time limit to determine if a WCV violation will occur before for an early FAR classification 
 *  - AlertTime2 - the time limit to determine if a WCV violation will occur before for the MID classification
 *  - EarlyAlertTime2 - the time limit to determine if a WCV violation will occur before for an early MID classification
 *  - AlertTime3 - the time limit to determine if a WCV violation will occur before for the NEAR classification
 *  - EarlyAlertTime3 - the time limit to determine if a WCV violation will occur before for an early NEAR classification
 *  - HorizontalDetectionType - the type of time projection limit used in determination of WCV violation- TAUMOD, TCPA, or TEP
 *  - AutomaticResponseStatus - ON for response to detected violations causing changes to vehicle action commands. Otherwise no change to vehicle behavior
 *  - PrioritySwitchTime - time where the priority for modality of avoidance is switched
 * 
 * Design: The objective of DAIDALUS_Processing is interface with NASA's DAIDALUS code to detect projected violations of the well-clear volume for
 *         the ownship.  This service then reports any detections of projected violations to other services along with the configuration parameters 
 *         used by DAIDALUS to determine the violations.
 * 
 * Details: All AirVehicleState messages are used to populate state information in DAIDALUS for the ownship and the intruder aircraft.  Then based on 
 *          the configuration parameters well-clear violations are checked.  DAIDALUS produces the time to violation if a projected violation of the 
 *          ownship's well-clear volume.  DAIDALUS uses a projection of the ownship and the intruders for a given look-ahead time to determine 
 *          violations.  DAIDALUS also provides maneuver guidance for ground track, ground speed, vertical speed, and altitude that would lead to a
 *          well-clear violation assuming constant acceleration during the projection window.  DAIDALUS also reports a classification of the 
 *          "region" of the violation.  This service provides vehicle information to DAIDALUS to set the ownship and the intruders and provides the
 *          intervals of maneuvers that would lead to the violation if a possible violation is detected within the look-ahead window as configured.
 * Subscribed Messages:
 *  - afrl::cmasi::AirVehicleState
 *  - uxas::messages::uxnative::StartupComplete
 *  - afrl::cmasi::AutomationResponse
 *  - afrl::cmasi::MissionCommand
 * 
 * 
 * Sent Messages:
 *  - larcfm::DAIDALUS::DAIDALUSConfiguration
 *  - larcfm::DAIDALUS:::WellClearViolationIntervals
 *  - afrl::cmasi::VehicleActionCommand
 * 
 * 
 */

class DAIDALUS_Processing : public ServiceBase
{
public:

    /** \brief This string is used to identify this service in XML configuration
     * files, i.e. Service Type="DAIDALUS_Processing". It is also entered into
     * service registry and used to create new instances of this service. */
    static const std::string&
    s_typeName()
    {
        static std::string s_string("DAIDALUS_Processing");
        return (s_string);
    };

    static const std::vector<std::string>
    s_registryServiceTypeNames()
    {
        std::vector<std::string> registryServiceTypeNames = {s_typeName()};
        return (registryServiceTypeNames);
    };

    /** \brief If this string is not empty, it is used to create a data 
     * directory to be used by the service. The path to this directory is
     * accessed through the ServiceBase variable m_workDirectoryPath. */
    static const std::string&
    s_directoryName() { static std::string s_string(""); return (s_string); };

    static ServiceBase*
    create()
    {
        return new DAIDALUS_Processing;
    };

    DAIDALUS_Processing();

    virtual
    ~DAIDALUS_Processing();

private:

    static
    ServiceBase::CreationRegistrar<DAIDALUS_Processing> s_registrar;

    /** brief Copy construction not permitted */
    DAIDALUS_Processing(DAIDALUS_Processing const&) = delete;

    /** brief Copy assignment operation not permitted */
    void operator=(DAIDALUS_Processing const&) = delete;

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


private: //following are member variables associated with detection
    int64_t m_VehicleID;
    //DAIDALUS parameters   
    double m_lookahead_time_s = {180};   // seconds--Time horizon of all DAIDALUS functions (time)
    double m_left_trk_deg = {n_Const::c_Convert::toDegrees(n_Const::c_Convert::dPi())}; // degrees--relative maximum horizontal direction maneuver to the left of the current ownship direction (angle)
    double m_right_trk_deg = {n_Const::c_Convert::toDegrees(n_Const::c_Convert::dPi())};    // degrees--relative maximum horizontal direction maneuver to the right of the current ownship direction (angle)
    double m_min_gs_mps = {5.1444}; // meters per second--absolute minimum horizontal speed maneuver (speed)
    double m_max_gs_mps = {360.111};    // meters per second--absolute maximum horizontal speed maneuver (speed)
    double m_min_vs_mps = {-30.48}; // meters per second--absolute minimum vertical speed maneuver (speed)
    double m_max_vs_mps = {30.48};  // meters per second--absolute maximum vertical speed maneuver (speed)
    double m_min_alt_m = {100*n_Const::c_Convert::dFeetToMeters()};   // meters--absolute minimum altitude maneuver (altitude)
    double m_max_alt_m = {50000*n_Const::c_Convert::dFeetToMeters()}; // meters--absolute maximum altitude maneuver (altitude)
    double m_trk_step_deg = {1.0};  // degrees--granularity of horizontal direction maneuvers (angle)
    double m_gs_step_mps = {2.57222};   // meters per second--granularity of horizontal speed maneuvers (speed)
    double m_vs_step_mps = {100.0/60.0*n_Const::c_Convert::dFeetToMeters()}; // meters per second--granularity of vertical speed maneuvers (speed)
    double m_alt_step_m = {100*n_Const::c_Convert::dFeetToMeters()};  // meters--granularity of altitude maneuvers (altitude)
    double m_horizontal_accel_mpsps = {0.0};  // meters per second per second--horizontal acceleration used in computation of horizontal speed maneuvers (acceleration)
    double m_vertical_accel_G = {0.0};    // gravity--vertical acceleration used in the computation of horizontal speed maneuvers (acceleration)
    double m_turn_rate_degps = {0.0}; // degrees per second--turn rate used in the computation of horizontal direction maneuvers (angle)
    double m_bank_angle_deg = {0.0};    // degrees--bank angle used in the computation of horizontal direction maneuvers (angle)
    double m_vertical_rate_mps = {0.0}; //meters per second--vertical rate used in the computation of altitude maneuvers (speed)
    double m_recovery_stability_time_s = {0};  // seconds--time delay to stabilize recovery maneuvers 
    bool m_recovery_trk_bool = {true};   // Boolean--enable computation of horizontal direction recovery maneuvers (boolean)
    bool m_recovery_gs_bool = {true};    // Boolean--enable computation of horizontal speed recovery maneuvers
    bool m_recovery_vs_bool = {true};    // Boolean--enable computation of vertical speed recovery maneuvers
    bool m_recovery_alt_bool = {true};   // Boolean--enable computation of altitude recovery maneuvers
    bool m_ca_bands_bool = {true};  // Boolean--enable computation of collision avoidance maneuvers
    double m_ca_factor = {0.2}; //factor to reduce min horizontal/vertical recovery separation when computing collision avoidance maneuvers (scalar (0,1])
    double m_horizontal_nmac_m = {500.0*n_Const::c_Convert::dFeetToMeters()};    // meters--Horizontal Near Mid-Air Collision (distance)
    double m_vertical_nmac_m = {100.0*n_Const::c_Convert::dFeetToMeters()};   // meters--Vertical Near Mid-Air Collision (distance)
    double m_min_horizontal_recovery_m = {1222.32};   // meters--minimum horizontal separation used in the computation of recovery maneuvers (distance)
    double m_min_vertical_recovery_m = {450.0*n_Const::c_Convert::dFeetToMeters()}; // meters--minimum vertical separation used in the computation of recovery maneuvers (distance)
    double m_contour_thr_deg = {180.0}; // degrees--threshold relative to ownship horizontal direction for the computation of horizontal contours aka. blobs (angle)
    double m_DTHR_m = 1222.32; //{m_min_horizontal_recovery_m};    //meters--horizontal distance threshold for WCV volume definition
    double m_ZTHR_m = 450.0*n_Const::c_Convert::dFeetToMeters(); //{m_min_vertical_recovery_m};  //meters--vertical distance threshold for WCV volume definition
    double m_TTHR_s = {35}; //seconds--time threshold for WCV voulume definition
    double m_alert_time_1_s = {m_lookahead_time_s};  //seconds--alerting time for alert level 1
    double m_early_alert_time_1_s = {m_lookahead_time_s+20};    //seconds--early alerting time for alert level 1
    double m_alert_time_2_s = {55};  //seconds--alerting time for alert level 2
    double m_early_alert_time_2_s = {75};    //seconds--early alerting time for alert level 2
    double m_alert_time_3_s = {25};  //seconds--alerting time for alert level 3
    double m_early_alert_time_3_s = {55};    //seconds--early alerting time for alert level 3
    int m_RTCA_alert_levels = {3};   //number of alert levels reported for guidance processing--max 3
    std::string m_horizontal_detection_type = {"TAUMOD"}; //string--horizontal detection type
    double m_staleness_time_s = m_lookahead_time_s;
    
    struct MydaidalusPackage
    {
       larcfm::Position m_daidalusPosition;
        larcfm::Velocity m_daidalusVelocity;
        double m_daidalusTime_s;
        double latitude_deg;
        double longitude_deg;
    };
    
    larcfm::Daidalus m_daa;
    std::unordered_map<int64_t, MydaidalusPackage> m_daidalusVehicleInfo;

private:    //the following are member variables associated with response to DAIDALUS detections of loss of wellclear
    enum states {OnMission=1, InConflict, OnHold} m_state{OnMission};
    enum priority {Standard = 1, High = 2} m_priority{Standard};
    bool m_isConflict {false};  //boolean stating whether or not a potential WCV has been detected that requires action
    bool m_isOnMission {false};  //boolean stating whether or not UAV is executing waypoints on Mission or not (diverting)
    bool m_isReadyToAct {false};    //boolean stating whether or not service has all necessary prerequisites in order to react to an imminent collision.
    bool m_isTakenAction {false};   //boolean stating whether or not the service has issued a vehicle action command to the ownship.
    bool m_isReadyToActWaypoint {false};    //boolean stating whether or not the service has received a waypoint designating the goal location
    bool m_isReadyToActMissionCommand {false};  //boolean stating whether or not the service has received a mission command that lists all waypoints.
    bool m_isReadyToActConfiguration {false};  //boolean stating whether or not service has received configuration parameters in order to process violation messages
    bool m_isActionCompleted {false}; //boolean stating whether or not service has completed taking action for the violation under consideration.
    bool m_isCloseToDesired {false};
    double m_heading_tolerance_deg{0.5};
    double m_groundspeed_tolerance_mps{5};
    double m_verticalspeed_tolerance_mps{5};
    double m_altitude_tolerance_m{10};
    double m_tolerance_clock_s;
    double m_tolerance_threshold_time_s{5};  //time needed to stay within desired state to be considered attained--seconds
    double m_action_time_threshold_s;   // time threshold to hold when taking action
    double m_priority_time_threshold_s{-15}; //time threshold to raise priority level when taking action
    double m_action_hold_release_time_s;  //time at which an action hold must be released
    double m_heading_max_deg{360.0};  //DAIDALUS maximum heading 
    double m_heading_min_deg{0.0};   //DAIDALUS minimum heading 
    double m_heading_interval_buffer_deg{5.0};  //degrees to buffer the heading bands interval by for avoidance maneuver
    double m_groundspeed_interval_buffer_mps{10.0};   //speed to buffer the ground speed interval by for avoidance maneuver.
    double m_verticalspeed_interval_buffer_mps{5.0};  //speed to buffer the vertical speed interval by for avoidance maneuver.
    double m_altitude_interval_buffer_m{20.0};    //distance in meters to buffer the altitude interval by for avoidance maneuver.
    int64_t m_NextWaypoint{-1};// {nullptr};
    int64_t m_RoW;
    uint16_t m_alertlevels_count;
    std::shared_ptr<afrl::cmasi::MissionCommand> m_MissionCommand{nullptr};// {nullptr};
    std::string m_AutomaticResponseStatus = "OFF";
    std::vector<int64_t> m_ConflictResolutionList;

    struct State
    {
        double altitude_m;
        double horizontal_speed_mps;
        double vertical_speed_mps;
        double heading_deg;
        double latitude_deg;
        double longitude_deg;
        double time_s;
        double total_velocity_mps;
        afrl::cmasi::AltitudeType::AltitudeType altitude_type;
        afrl::cmasi::SpeedType::SpeedType speed_type;
    }m_CurrentState, m_DivertState, m_ReturnState;
    void SetDivertState(const std::shared_ptr<larcfm::DAIDALUS::WellClearViolationIntervals>& DAIDALUS_bands);
    bool isSafeToReturnToMission(const std::shared_ptr<larcfm::DAIDALUS::WellClearViolationIntervals>& DAIDALUS_band);
    bool foundWCVAltitudeResolution(const std::shared_ptr<larcfm::DAIDALUS::WellClearViolationIntervals>& DAIDALUS_bands);
    bool foundWCVGroundSpeedResolution(const std::shared_ptr<larcfm::DAIDALUS::WellClearViolationIntervals>& DAIDALUS_bands);
    bool foundWCVHeadingResolution(const std::shared_ptr<larcfm::DAIDALUS::WellClearViolationIntervals>& DAIDALUS_bands);
   
   };

} //namespace service
} //namespace uxas

#endif /* UXAS_DAIDALUS_Processing*/

