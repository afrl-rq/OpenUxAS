// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

#ifndef UXAS_ZERO_MQ_GENERIC_RECEIVER_H
#define UXAS_ZERO_MQ_GENERIC_RECEIVER_H

#include "ZeroMqReceiver.h"
#include "UxAS_Log.h"

#include <string>

namespace uxas {
namespace communications {
namespace transport {

class ZeroMqGenericReceiver : public ZeroMqReceiver<std::string> {
public:
    ZeroMqGenericReceiver(std::shared_ptr<ZeroMqSocketBase> socket) : ZeroMqReceiver{socket} {}
    ~ZeroMqGenericReceiver() override = default;

    std::string receive() override {
        // NOTE: Logic here does not poll socket for possible messages and will block!
        std::string retVal{""};
        bool firstMessagePart = true;
        int more = 1;
        while (more) {
            zmq::message_t msg;
            int more;
 
            m_socket->getSocket()->recv(&msg);
            size_t moreSize = sizeof(more);
            m_socket->getSocket()->getsockopt(ZMQ_RCVMORE, &more, &moreSize);
            retVal += std::string(msg.data<std::string>()->c_str());
        }
        return retVal;
    }
};

}
}
}

#endif