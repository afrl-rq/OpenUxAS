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

/**
 * @brief Basic Zmq receiver class which will only check socket for data.  Will check for multipart messages but
 *        no additional processing beyond that.  Will block on receive() call so guard appropriately.
 */
class ZmqGenericReceiver : public ZmqReceiver<std::string> {
public:
    /**
     * @brief Constructor with underlying Zmq socket as a parameter.
     * 
     * @param socket - shared pointer to Zmq socket.
     */
    ZmqGenericReceiver(std::shared_ptr<ZmqSocketBase> socket);

    /**
     * @brief Default destructor
     */
    ~ZmqGenericReceiver() override = default;

    /**
     * @brief Generic message receive which will handle multi-part messages and assume that all message content is data. 
     * 
     * @return std::string - raw data with simple concatenation from multipart messages. 
     */
    std::string receive() override;
};

}
}

#endif