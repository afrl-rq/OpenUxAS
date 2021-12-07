// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

#ifndef UXAS_ZERO_MQ_TCP_SOCKET_H
#define UXAS_ZERO_MQ_TCP_SOCKET_H

#include "ZeroMqSocketBase.h"

namespace uxas {
namespace communications {
namespace transport {

// Add some additional destructor functionality from the base class

class ZeroMqTcpSocket : public ZeroMqSocketBase {
public:
    ZeroMqTcpSocket(InitializerPtr initializer)
    : ZeroMqSocketBase{initializer, zmq::socket_type::stream} {}

    ~ZeroMqTcpSocket() override {
        if (m_socket) {
            //TODO possibly need to add disconnection logic for clients as well?
            // Send routing ID followed by an empty message to close connection prior to
            // closing socket on this end.
            m_socket->send(m_routingId.begin(), m_routingId.end(), ZMQ_SNDMORE);
            m_socket->send(0,0,0);
        }
    };
};

}
}
}

#endif