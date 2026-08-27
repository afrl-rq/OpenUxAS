/** A pure virtual defining the interface for zone alert computer.
 * @author Jonathan Rowanhill
*/

#pragma once

#include <memory>
#include <vector>
#include <array>

#include "Position.h"

#include "afrl/cmasi/AirVehicleConfiguration.h"
#include "afrl/cmasi/AirVehicleState.h"
#include "afrl/cmasi/AbstractZone.h"
#include "uxas/messages/ImminentZoneViolation.h"
#include "uxas/messages/ProcessedZone.h"


using namespace n_FrameworkLib;
using namespace uxas::messages;
using namespace afrl::cmasi;

namespace zoneAlert {

using std::vector;
using std::array;

using std::stringstream;

using std::shared_ptr;

using std::isfinite;

/**
 * @brief A device that computers alerts for imminent zone violations.
 * 
 * @details Given declarations of all relevant zones and aircraft, this alerter will compute imminent zone violations
 * for a given vehicle state report.
 */
class ZoneAlertComputer {

public:

    virtual ~ZoneAlertComputer() {};

    /**
     * @returns the lookahead time with which the zone alert computer detects potential imminent zone violations. In milliseconds
     */
    virtual int64_t getLookaheadTime() = 0;


    /**
     * @returns whether the lookahead time assigned to the Zone Alert Computer is acceptable for its use
     */
    virtual bool acceptableLookaheadTime() = 0;

    /**
     * @brief Add a declared zone to the analyzer
     * 
     * @param zone The AbstractZone that has been declared
     * @param keepIn whether the zone was sent as a KeepIn or KeepOut zones
     * @return whether the zone was added succesfully
     * 
     * @post if it returns true, then it will zone alert, otherwise it is not stored for zone alerts.
     * @post Always replaces any previously declared zone with same id, whether successful or not.
     * @post Always deletes a previously declared zone if it returns false
     * 
     * @requirements SR-4-3-2
     * @requirements SR-4-3-2-1
     * @requirements SR-4-3-2-2
     * @requirements SR-6-2-3-1
     */
    virtual bool addZone(shared_ptr<AbstractZone> zonePtr, bool keepIn) = 0;

    /**
     * @brief Add a declared vehicle to the analyzer
     * 
     * @param vehicleConfigPtr A pointer to the vehicle configuration to report
     * @return whether the vehicle was added succesfully
     * 
     * @requirements SR-5-3-2
     */
    virtual bool addVehicle(shared_ptr<AirVehicleConfiguration> vehicleConfig) = 0;

    /**
     * @brief Prepare data further for active alerting of imminent zone violations
     * 
     * @pre All zones and vehicles have been declared
     * @post The zone alert computer is ready to detect imminent zone collisions from reported vehicle states or
     * null or empty vector if none
     * 
     * @requirements  SR-6-2, SR-6-2-1, SR-6-2-2, SR-6-2-2-1, SR-6-2-2-2, SR-6-2-2-3, SR-6-2-2-4, SR-6-2-2-5, 
     *                SR-6-3, SR-6-3-1, SR-6-3-2, SR-6-3-3, SR-6-3-3-1, SR-6-3-3-2, SR-6-3-3-3,
     *                SR-9
     */
    virtual vector<shared_ptr<ProcessedZone>> * mergeZones() = 0;

    /**
     * @brief Process a vehicle state and report any predicted zone violations
     * 
     * @param vehicleState 
     * @return std::vector<PredictedViolation> a vector predicted zone violations for the vehicle or
     * null or empty vector if none
     *
     * @requires SR-7-3-2, SR-7-3-2-1, SR-7-3-2-2, SR-7-3-2-2-1,SR-7-3-2-3, SR-7-3-2-4, SR-7-3-2-5
     *           SR-7-3-2-6, SR-7-3-2-7, SR-10
     * 
     */
    virtual vector<shared_ptr<ZoneViolation>> * computeZoneViolations(
        shared_ptr<AirVehicleState> vehicleState, 
        stringstream &sstrErrorMessage) = 0;

protected:

    //----Helper Functions -----//

    /**
     * @brief Generate  instantaneous linear velocity of the vehicle in the world frame
     * 
     * @param vehicleState the vehicle state from which current linear trajectory is derived
     * @return as a standard array with linear velocity in x, y, and z world components
     */
    array<float,3> worldFrameVelocity(shared_ptr<AirVehicleState> vehicleState) {

        // compute x and y components of velocity from ground track and ground speed in m/s
        // note sin and cos are computed as doubles for accuracy and implicit precision reduction conversion occurs in the multiplication
        float xv = sind(vehicleState->getCourse()) * vehicleState->getGroundspeed();
        float yv = cosd(vehicleState->getCourse()) * vehicleState->getGroundspeed();

        // z component is vertical speed in m/s
        float zv = vehicleState->getVerticalSpeed();
        
        return std::array<float,3> {{xv, yv, zv}};
    }
    
    /**TEST(Sanity, FALSITY) {
    EXPECT_EQ(1, 0);
}

     * @brief Compute the end position of vehicle state from linear trajectory with a lookahead time
     * 
     * @details Given vehicle position, currents linear trajectory, and a future time, this function
     * computes the projected future position of the vehicle at the future time.
     * 
     * @param currentPosition the current position of the vehicle
     * @param velocity the current velocity of the vehicle in the world frame in x, y, z in that order
     * @param futureTime The future time at which to predict vehicle position where 0 is the present time
     * 
     * @return the predicted position of the vehicle at time lookahead
     */
    CPosition futurePosition(CPosition & currentPosition, array<float, 3> & velocity, double futureTime) {

        double nx = currentPosition.m_east_m + (futureTime * velocity[0]);
        double ny = currentPosition.m_north_m + (futureTime * velocity[1]);
        double nz = currentPosition.m_altitude_m + (futureTime * velocity[2]);

        return CPosition(ny, nx, nz);
    }


private:

    /**
     * @brief Convert degrees to radians
     * 
     */
    static double d2r(double d) {
        return (d / 180.0) * ((double) M_PI);
    }

    /**
     * @brief A function to produce highly accurate sin values converting radians to degrees. From StackOverflow
     * @cite https://stackoverflow.com/questions/31502120/sin-and-cos-give-unexpected-results-for-well-known-angles/31525208#31525208
     * 
     * @param x angle in degrees
     * @return double sin value
     */
    double sind(double x) {
    if (!isfinite(x)) {
        return sin(x);
    }
    if (x < 0.0) {
        return -sind(-x);
    }
    int quo;
    double x90 = remquo(fabs(x), 90.0, &quo);
    switch (quo % 4) {
        case 0:
        // Use * 1.0 to avoid -0.0
        return sin(d2r(x90)* 1.0);
        case 1:
        return cos(d2r(x90));
        case 2:
        return sin(d2r(-x90) * 1.0);
        case 3:
        return -cos(d2r(x90));
    }
    return 0.0;
    }

    /**
     * @brief An accurate function to compute the cosine of an angle in degrees
     * 
     * @param x  angle in degrees
     * @return double cosine result
     */
    double cosd(double x) {
        return sind(90 - x);
    }



};

}
