// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

#ifndef COMMUNICATIONS_ZMQ_TCP_SOCKET_H
#define COMMUNICATIONS_ZMQ_TCP_SOCKET_H

#include "ZmqSocketBase.h"

namespace uxas {
namespace communications {

// Add some additional destructor functionality from the base class

class ZmqTcpSocket : public ZmqSocketBase {
public:
    ZmqTcpSocket(InitializerPtr initializer) : ZmqSocketBase{initializer, zmq::socket_type::stream} {}

    ~ZmqTcpSocket() override {
        if (m_socket) {
            //TODO possibly need to add disconnection logic for clients as well?
            // Send routing ID followed by an empty message to close connection prior to
            // closing socket on this end.
            m_socket->send(m_routingId.begin(), m_routingId.end(), ZMQ_SNDMORE);
            // m_socket->send(m_routingId.begin(), m_routingId.size(), ZMQ_SNDMORE);
            std::string tmp;
            m_socket->send(tmp.data(),0,0);
        }
    };
};

}
}

#endif