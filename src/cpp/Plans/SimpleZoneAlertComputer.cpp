#include "SimpleZoneAlertComputer.h"

#include <array>
#include <vector>
#include <memory>
#include <afrl/cmasi/Circle.h>
#include <afrl/cmasi/Polygon.h>
#include <afrl/cmasi/Rectangle.h>

#include "UnitConversions.h"
#include "RoutePlannerVisibilityService.h"

#define CIRCLE_BOUNDARY_INCREMENT (n_Const::c_Convert::dPiO10())

using namespace n_FrameworkLib;
using namespace uxas::messages;
using namespace afrl::cmasi;
using namespace dcllc::zonealert;

namespace zoneAlert {

using std::shared_ptr;
using std::make_shared;

SimpleZoneAlertComputer::SimpleZoneAlertComputer(int64_t lookahead) : ZoneAlertComputer(),
    lookaheadTime(lookahead),
    airVehicleConfigs(),
    boundaries(),
    polygons(),
    keepOutZones(),
    initialKeepInZones(),
    visibilityGraph() {};


SimpleZoneAlertComputer::~SimpleZoneAlertComputer() {
    // clear maps to let go of contained shared pointers
    airVehicleConfigs.clear();
    boundaries.clear();
    polygons.clear();
}

bool SimpleZoneAlertComputer::addZone(shared_ptr<AbstractZone> zonePtr, bool keepIn) {

    // Make a variable to build boundary points
    // And call function on RoutePlannerVisibility Service to convert zone geometry to flat earch x-y-z coordinates
    V_POSITION_t boundaryPoints; //used to store the boundary points  while we convert the,   
    
    // ovveride padding. Padding is for route oplanning, not general zone geometry
    // Fulfills requiremnt SR-6-2-3-1
    zonePtr->setPadding(0.0);

    bool isSuccess = uxas::service::RoutePlannerVisibilityService::bFindPointsForAbstractGeometry(zonePtr->getBoundary(), boundaryPoints);
    
    if (isSuccess) {
        // store the XY-plane polygonal boundary of the zone (this copies the boundaryPoints in an internal record)
        auto boundaryPtr = make_shared<CBoundary>(
            zonePtr->getZoneID(), keepIn, boundaryPoints, *zonePtr);

        // Create a Polygon and update members as needed
        auto polygonPtr = make_shared<CPolygon>(zonePtr->getZoneID());
        polygonPtr->plytypGetPolygonType().bGetKeepIn() = keepIn;
        polygonPtr->dGetPolygonExpansionDistance() = zonePtr->getPadding();        
        // polygons store indeces to an external array. Those are stored in the CBoundary object created above
        for (int index = 0; index < boundaryPtr->vposGetBoundaryPoints_m().size(); index++) {
            polygonPtr->viGetVerticies().push_back(index);
        }

        // attempt to 'finalize' the polygon 
        // and blank it out if failure
        isSuccess = (polygonPtr->errFinalizePolygon(boundaryPtr->vposGetBoundaryPoints_m()) == CPolygon::errNoError);

        // make sure the zone isn't empty
        isSuccess &= !(boundaryPtr->vposGetBoundaryPoints_m().empty());

        // if all is well, store the zone, otherwise, make sure no zone is stored (in case this is replacing an earlier declaration)
        if (isSuccess) {
            boundaries[boundaryPtr->getZoneID()] = boundaryPtr;
            polygons[polygonPtr->iGetID()] = polygonPtr;
        }
        else {
            boundaries[boundaryPtr->getZoneID()] = NULL;
            polygons[polygonPtr->iGetID()] = NULL;
        }  
    }

    return isSuccess;
}


bool SimpleZoneAlertComputer::addVehicle(shared_ptr<AirVehicleConfiguration> vehicleConfig) {
    /*
     * Unneeded info for this simple linear trajectory algorithm
     */
    return true;
}

vector<shared_ptr<ProcessedZone>> * SimpleZoneAlertComputer::mergeZones() {

    // walk all polygons and add them to the visibility graph
    // adding them to the visibility graph
    for (auto iter = boundaries.begin(); iter != boundaries.end(); iter++) {
            auto errPolygon = visibilityGraph.errAddPolygon(iter->second->getZoneID(),
                        iter->second->vposGetBoundaryPoints_m().begin(),
                        iter->second->vposGetBoundaryPoints_m().end(),
                        iter->second->bGetKeepInZone(),
                        iter->second->getPadding() );
            if (errPolygon != n_FrameworkLib::CVisibilityGraph::errNoError)
            {
                return NULL;
            }
    }

    // finalize all of the polygons in the visibility graph
    // this makes sure they are sound and grows/shrinks them by their padding
    auto errPolygon = visibilityGraph.errFinalizePolygons();

    if (errPolygon != n_FrameworkLib::CVisibilityGraph::errNoError) {
        return NULL;
    }

    // Clear out old data
    /* @todo : Check reference count logistics for any lost data
    */
    boundaries.clear();
    polygons.clear();

    vector<shared_ptr<ProcessedZone>> *processedZonesPtr = NULL;

    // now re-extract boundaries and polygons from the visibility graph
    // for easy intersection testing
    for (auto iter = visibilityGraph.vplygnGetPolygons().begin(); 
              iter != visibilityGraph.vplygnGetPolygons().end(); 
              iter++) {
        
        // copy construct the current polygon as a shared version
        polygons[iter->iGetID()] = make_shared<CPolygon>(*iter);

        auto poly = polygons[iter->iGetID()];
        
        // recreate boundary, pulling the CPositions out of visibiity graph
        // and copying them into our boundary points vector for the polygon
        V_POSITION_t boundaryPoints;   
        int startIndex = INT_MAX;
        for (auto vit = poly->viGetVerticies().begin(); vit!=poly->viGetVerticies().end(); vit++) {
            if (*vit < startIndex) {
                startIndex = *vit;
            }
            boundaryPoints.push_back(visibilityGraph.vposGetVerticiesBase()[*vit]);
        }

        // now fix the vertex indeces of the polygon to point to the right boundary points
        poly->viGetVerticies().clear();
        for (int i = 0; i< boundaryPoints.size(); i++) {
            poly->viGetVerticies().push_back(i);
        }

        // and now fix the edges of the polygon to have the right verteces for the localized boudnary points array
        for (int i = 0; i< poly->veGetPolygonEdges().size(); i++) {
            n_FrameworkLib::CEdge &edge = poly->veGetPolygonEdges()[i];
            edge.first = edge.first - startIndex;
            edge.second = edge.second - startIndex;
        }

        // note we do build the boundaries with blank abstract zones. That info was lost in use of visibilitygraph
        AbstractZone blankAbstract;
        auto boundaryPtr = make_shared<CBoundary>(
            poly->iGetID(), poly->plytypGetPolygonType().bGetKeepIn(), boundaryPoints, 
                            blankAbstract);
        boundaryPtr->setZoneID(poly->iGetID());
        boundaries[boundaryPtr->getZoneID()] = boundaryPtr;

        //  store zones by type in sets for faster iteration during violation checks
        if (poly->plytypGetPolygonType().bGetKeepIn()) {
            keepInZones.insert(poly->iGetID());
        }
        else {
            keepOutZones.insert(poly->iGetID());
        }

        // create an announcement of the zone
        shared_ptr<ProcessedZone> procZonePtr = make_shared<ProcessedZone>();
        procZonePtr->setZoneID(poly->iGetID());
        procZonePtr->setKeepIn(poly->plytypGetPolygonType().bGetKeepIn());

        for (auto vit = boundaryPtr->vposGetBoundaryPoints_m().begin(); 
                    vit != boundaryPtr->vposGetBoundaryPoints_m().end(); vit++) {
            ZoneVertex * vertexPosPtr = new ZoneVertex();
            vertexPosPtr->setEast(vit->m_east_m);
            vertexPosPtr->setNorth(vit->m_north_m);
            procZonePtr->getVertices().push_back(vertexPosPtr);            
        }

        // store the processed zone in vector to return to caller
        if (processedZonesPtr == NULL) {
            processedZonesPtr = new vector<shared_ptr<ProcessedZone>>;
        }
        processedZonesPtr->push_back(procZonePtr);
    }

    // Postcondition: assert(or NULL polygons.size() == boundaries.size());
    // Postcondition: assert(or NULL polygons.size() == visibilityGraph.vplygnGetPolygons().size())

    return processedZonesPtr;
}
        


vector<shared_ptr<ZoneViolation>> * SimpleZoneAlertComputer::computeZoneViolations(
        shared_ptr<AirVehicleState> vehicleState, 
        std::stringstream &sstrErrorMessage) {

    // extract the current position of the vehicle as a CPosition structure
    // TODO: Confirm that MSL is being sent by vehicles
    // Note: The dummy variable assures we are calling a constructor for lat, long, and altitude
    uxas::common::utilities::CUnitConversions unitConversions;
    double mNorth = 0.0;
    double mEast = 0.0;
    unitConversions.ConvertLatLong_degToNorthEast_m(
        vehicleState->getLocation()->getLatitude(), 
        vehicleState->getLocation()->getLongitude(),
        mNorth, mEast);
    CPosition startPos(mNorth, mEast,
        vehicleState->getLocation()->getAltitude());

    // get instantaneous linear velocity vector and use it to compute starting and ending points
    array<float, 3> velocity = worldFrameVelocity(vehicleState);

    // compute the final predicted position at lookahead time based on the above
    CPosition endPos = futurePosition(startPos, velocity,  lookaheadTime);

    vector<shared_ptr<ZoneViolation>> *violationsPtr = NULL;
    
    // check for keep in zone violation
    // this need only happen if the vehicle was in a merged keep in zone on its first state report
    auto initialKeepInZoneID = checkForInitialKeepInZone(vehicleState->getID(), startPos, sstrErrorMessage);    
    if (initialKeepInZoneID > 0) {

        // yes, this will check even if this is first state report iteration and guaranteed inside
        auto keepInViolation = findExistingViolationWith(initialKeepInZoneID, 
                vehicleState->getID(), startPos, vehicleState->getTime(), 
                sstrErrorMessage);

        // if not in active violation, check for impending
        if (keepInViolation == NULL ) {
            keepInViolation = findImminentViolationWith(initialKeepInZoneID,
                    vehicleState->getID(), startPos, endPos, vehicleState->getTime(),
                    velocity, sstrErrorMessage);
        }

        // if either active or impending violation with keep in zone, report
        if (keepInViolation != NULL) {
            if (violationsPtr == NULL) {
                violationsPtr = new vector<shared_ptr<ZoneViolation>>;
            }
            violationsPtr->push_back(keepInViolation);
        }
    }

    // now check keep-out zone for violations
    // SimpleZoneAlertComputer does a nieve O(n) check against n keep out zones!
    // @todo: use bounding boxes to possibly improve performance
    for (auto it = keepOutZones.begin(); it != keepOutZones.end(); it++) {

        auto keepOutZoneID = *it;

        // check for active violation with keep out zone
        auto keepOutViolation = findExistingViolationWith(keepOutZoneID, 
                vehicleState->getID(), startPos, vehicleState->getTime(), 
                sstrErrorMessage);

        // if not in active violation, check for impending
        if (keepOutViolation == NULL ) {
            keepOutViolation = findImminentViolationWith(keepOutZoneID,
                    vehicleState->getID(), startPos, endPos, vehicleState->getTime(), 
                    velocity, sstrErrorMessage);
        }

        // if either active or impending violation with keep out zone, report
        if (keepOutViolation != NULL) {
            if (violationsPtr == NULL) {
                violationsPtr = new vector<shared_ptr<ZoneViolation>>;
            }
            violationsPtr->push_back(keepOutViolation);
        }
    }
 
    return violationsPtr;
}


inline const int SimpleZoneAlertComputer::checkForInitialKeepInZone(
            const int64_t vehicleID, const CPosition &currentPos,
            std::stringstream &sstrErrorMessage) {

    // if a vehicles has a keep in zone already we are all set
    if (initialKeepInZones.count(vehicleID) > 0 && initialKeepInZones[vehicleID] > 0) {
        return initialKeepInZones[vehicleID];
    }
    // otherwise, gonna have to check if inside a keep in zone that will now own it
    else {
        
        // search all keep in zones for one that might contain this first state report
        for (auto it = keepInZones.begin(); it != keepInZones.end(); it++) {
            
            auto zoneID = *it;

            try {
                shared_ptr<CBoundary> boundaryPtr = boundaries.at(zoneID);
                shared_ptr<CPolygon> polyPtr = polygons.at(zoneID);

                bool vehicleInZone = polyPtr->InPolygon(currentPos.m_east_m, currentPos.m_north_m, 
                                currentPos.m_altitude_m, boundaryPtr->vposGetBoundaryPoints_m(), 
                                sstrErrorMessage);

                if (vehicleInZone) {
                    initialKeepInZones[vehicleID] = zoneID;
                    return zoneID;
                }

            }
            catch(const std::out_of_range &e) {
                sstrErrorMessage << "Internal zone registration error for determining point in polygon for merged zone id = '"
                    << zoneID << "'.\n";
                continue;
            }
            catch(std::exception &e) {
                sstrErrorMessage << "Standard exception occured in determining point in polygon for merged zone id = '"
                    << zoneID << "': " << e.what() << "\n";
                continue;
            }
            catch(...) {
                sstrErrorMessage << "An unknown exception occured in determining point in polygon for merged zone id = '"
                    << zoneID << "'.\n";
                continue;
            }            
        }

        // not in any keep in zone on initial vehicle state report, so record keep in 
        // for vehicle as zone 0 (meaning none)
        initialKeepInZones[vehicleID] = 0;
        return 0;

    }

}


/**
 * @REQUIREMENT_VIOLATION: Does not include being on the polygon boundary as being in the zone
 */
inline shared_ptr<ZoneViolation> SimpleZoneAlertComputer::findExistingViolationWith(
        const int64_t zoneID, const int64_t vehicleID, 
        const CPosition &startPos, const int64_t timestamp, 
        std::stringstream &sstrErrorMessage) {


    try {
        shared_ptr<CBoundary> boundaryPtr = boundaries.at(zoneID);
        shared_ptr<CPolygon> polyPtr = polygons.at(zoneID);


        // For reasons I don't understandm east is Y and north is X
        bool vehicleInZone = polyPtr->InPolygon(startPos.m_north_m, startPos.m_east_m, 
                        startPos.m_altitude_m,
                        boundaryPtr->vposGetBoundaryPoints_m(), 
                        sstrErrorMessage);

        if (vehicleInZone != boundaryPtr->bGetKeepInZone()) {
            return makeZoneViolation(zoneID, boundaryPtr->bGetKeepInZone(), vehicleID, timestamp,
                startPos.m_east_m, startPos.m_north_m, startPos.m_altitude_m, 0);
        }
        else {
            return NULL;
        } 

    }
    catch(const std::out_of_range &e) {
        sstrErrorMessage << "Internal zone registration error for determining point in polygon for merged zone id = '"
            << zoneID << "'.\n";
        return NULL;
    }
    catch(std::exception &e) {
        sstrErrorMessage << "Standard exception occured in determining point in polygon for merged zone id = '"
            << zoneID << "': " << e.what() << "\n";
        return NULL;
    }
    catch(...) {
        sstrErrorMessage << "An unknown exception occured in determining point in polygon for merged zone id = '"
            << zoneID << "'.\n";
        return NULL;
    }

}

shared_ptr<ZoneViolation> SimpleZoneAlertComputer::findImminentViolationWith(
        const int64_t zoneID, const int64_t vehicleID, 
         CPosition &startPos,  CPosition &endPos,
        const int64_t startTime, const array<float, 3> &velocity,
        std::stringstream &sstrErrorMessage) {
    
    try {

        shared_ptr<CBoundary> boundaryPtr = boundaries.at(zoneID);
        shared_ptr<CPolygon> polyPtr = polygons.at(zoneID);

        auto closestIntersectionPtr = findClosestIntersection(startPos, endPos, 
                        polyPtr, boundaryPtr);

        // if we found an intersection between the vehicle linear trajectory
        // and the edge of the zone, examine to see if it happens within lookahead limit
        // note converstion from double to int64 varies by compiler standard
        if (closestIntersectionPtr != NULL) {

            auto timeToIntercept = computeTimeToPosition(startPos, endPos, 
                                        velocity, *closestIntersectionPtr);

            if (closestIntersectionPtr != NULL && timeToIntercept <= lookaheadTime) {
                    
                    return makeZoneViolation( zoneID, boundaryPtr->bGetKeepInZone(), 
                                vehicleID, startTime,
                                closestIntersectionPtr->m_east_m, 
                                closestIntersectionPtr->m_north_m,
                                closestIntersectionPtr->m_altitude_m, timeToIntercept);

                delete closestIntersectionPtr;
            }
            else {
                return NULL;
            }
        }
        else {
            return NULL;
        }
    }
    catch(const std::out_of_range &e) {
        sstrErrorMessage << "Internal zone registration error for determining point in polygon for merged zone id = '"
            << zoneID << "'.\n";
        return NULL;
    }
    catch(std::exception &e) {
        sstrErrorMessage << "Standard exception occured in determining point in polygon for merged zone id = '"
            << zoneID << "': " << e.what() << "\n";
        return NULL;
    }
    catch(...) {
        sstrErrorMessage << "An unknown exception occured in determining point in polygon for merged zone id = '"
            << zoneID << "'.\n";
        return NULL;
    }

    return NULL;    
}


/*
 * @TODO: finding the first intersection by first finding all intersections is wasteful 
 */
CPosition * SimpleZoneAlertComputer::findClosestIntersection(
            const CPosition &startPos, const CPosition &endPos, shared_ptr<CPolygon> polygonPtr, 
            shared_ptr<CBoundary> polygonBoundaryPtr) {

    V_POSITION_t intersections;

    polygonPtr->findIntersections(polygonBoundaryPtr->vposGetBoundaryPoints_m(),startPos, 
                endPos, intersections);

    CPosition *closestPtr = NULL;

    if (intersections.size()>0) {

        double closestDist = std::numeric_limits<double>::max();

        for (auto intersection = intersections.begin(); intersection != intersections.end(); 
            intersection++) {

            double dist = startPos.relativeDistance2D_m(*intersection);
            if (dist<closestDist) {
                closestPtr = new CPosition(*intersection);
                closestDist = dist;
            }
        }
    }    

    return closestPtr;
}


inline double SimpleZoneAlertComputer::computeTimeToPosition(CPosition &startPos,  CPosition &endPos, 
                    const array<float, 3> &velocity,  CPosition &futurePosition) {

    // compute relative slope to choose whether we compute using x or y for accuracy

    // if the trajetory is mostly horizontal
    CPosition trajVec = endPos - startPos;
    CPosition diffVec = futurePosition - startPos;

    // if the future position is the start position, return the time to arrive as 0
    if (diffVec.m_east_m == 0 && diffVec.m_north_m == 0) {
        return 0;
    }

    // otherwise, compute the time to arrive based on relationship to x or y value, whichever can 
    // provide greater accuracy and precision
    if (trajVec.m_east_m != 0 && (trajVec.m_north_m / trajVec.m_east_m) < 0.5 ) {
        return diffVec.m_east_m/ velocity[0];
    }
    // if line is very vertical compute from north difference
    // note conversion from double to int64 differs by compiler
    else {        
        return diffVec.m_north_m / velocity[1];
    }

}


inline shared_ptr<ZoneViolation> SimpleZoneAlertComputer::makeZoneViolation(
                int zoneID, bool isKeepInZone, 
                int64_t vehicleID, int64_t vehicleStateReportTime,
                double east_m, double north_m, double altitude_m,
                double timeToIntercept)
{  

    // Store the position of the violation in a 3D position vector (m)
    Position2D* positionPtr = new Position2D();
    positionPtr->setEast(east_m);
    positionPtr->setNorth(north_m);

    Position2D* positionLatLongPtr = new Position2D();

    uxas::common::utilities::CUnitConversions unitConversions; // TODO: don't create on every call

    double lat_deg, long_deg;
    unitConversions.ConvertNorthEast_mToLatLong_deg(north_m, east_m, lat_deg, long_deg);
    positionLatLongPtr->setEast(long_deg);
    positionLatLongPtr->setNorth(lat_deg);

    shared_ptr<ZoneViolation> violation = NULL;

    // Make an Active or Imminent ZoneViolation depending on if it is active at the vehicles position
    // at the time of its state report, or is predicted to happen in the future of that report, respectively
    if (timeToIntercept == 0.0) {
        violation = make_shared<ActiveZoneViolation> ();
    }
    else {
        violation = make_shared<ImminentZoneViolation> ();
    }

    // fill out remaining data required for a ZoneViolation alert
    violation->setZoneID(zoneID);
    violation->setKeepIn(isKeepInZone);
    violation->setVehicleID(vehicleID);
    violation->setInterceptPosition(positionPtr);

    violation->setInterceptPositionLatLong(positionLatLongPtr);
    // time is absolute to epoch milliseconds so reports are 
    // not relative to knowing state report time
    violation->setTimeToIntercept(vehicleStateReportTime + (timeToIntercept*1000));

    return violation;

}


} // end namespace zoneAlert
