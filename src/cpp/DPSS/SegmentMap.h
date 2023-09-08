// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

#pragma once
#include "DpssDataTypes.h"

#include <vector>
class SegmentMap
{
public:
    SegmentMap();
    SegmentMap(std::vector<Dpss_Data_n::xyPoint>& uavPlan, std::vector<Dpss_Data_n::xyPoint>& road);
    ~SegmentMap();

    void Initialize(std::vector<Dpss_Data_n::xyPoint>& uavPlan, std::vector<Dpss_Data_n::xyPoint>& road);
    Dpss_Data_n::xyPoint CalculateStarePoint(Dpss_Data_n::xyPoint& uavLoc);
    static Dpss_Data_n::xyPoint CalculateStarePoint(std::vector<Dpss_Data_n::xyPoint>& uavPlan, std::vector<Dpss_Data_n::xyPoint>& road, Dpss_Data_n::xyPoint& uavLoc);
    static int SnapToSegment(std::vector<Dpss_Data_n::xyPoint>& polyline, Dpss_Data_n::xyPoint& actualLocation, Dpss_Data_n::xyPoint& snappedLocation);

private:
    std::vector<Dpss_Data_n::xyPoint> path;
    std::vector<Dpss_Data_n::xyPoint> plan;
    std::vector<double> normalizedPathLengths;
    double totalPlanLength;
    double totalPathLength;
};