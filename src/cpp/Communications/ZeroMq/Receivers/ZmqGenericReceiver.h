// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

#ifndef COMMUNICATIONS_ZMQ_GENERIC_RECEIVER_H
#define COMMUNICATIONS_ZMQ_GENERIC_RECEIVER_H

#include "ZmqReceiver.h"
#include "UxAS_Log.h"

#include <string>

namespace uxas {
namespace communications {

class ZmqGenericReceiver : public ZmqReceiver<std::string> {
public:
    ZmqGenericReceiver(std::shared_ptr<ZmqSocketBase> socket);

    ~ZmqGenericReceiver() override = default;

    // Generic message receive which will handle multi-part messages and assume that all message content is data.
    std::string receive() override;
};

}
}

#endif