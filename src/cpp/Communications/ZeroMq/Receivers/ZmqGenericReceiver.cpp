// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

#include "ZmqGenericReceiver.h"

namespace uxas {
namespace communications {

ZmqGenericReceiver::ZmqGenericReceiver(std::shared_ptr<ZmqSocketBase> socket)
    : ZmqReceiver{socket}
    {}

std::string ZmqGenericReceiver::receive() {
    UXAS_LOG_DEBUG_VERBOSE(typeid(this).name(),"::",__func__,":TRACE");
    std::string retVal{""};
    if (!m_socket || !m_socket->getRawZmqSocket()) {
        UXAS_LOG_ERROR(__FILE__,"::",__func__," - Sockets are not valid!");
        return retVal;
    }
    // NOTE: Logic here does not poll socket for possible messages and will block!
    bool firstMessagePart = true;
    int more = 1;
    while (more) {
        zmq::message_t msg;
        m_socket->getRawZmqSocket()->recv(&msg);
        size_t moreSize = sizeof(more);
        m_socket->getRawZmqSocket()->getsockopt(ZMQ_RCVMORE, &more, &moreSize);
        // Use string concatenation for combining multipart messages from socket.
        retVal += std::string{static_cast<const char*>(msg.data()), msg.size()};
    }
    return retVal;
}

}
}