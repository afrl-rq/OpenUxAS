// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

#include "ZmqTcpSocket.h"
#include "UxAS_Log.h"

namespace uxas {
namespace communications {

ZmqTcpSocket::~ZmqTcpSocket() {
    UXAS_LOG_DEBUG_VERBOSE(typeid(this).name(),"::",__func__,":TRACE");
    if (m_socket) {
        //TODO possibly need to add disconnection logic for clients as well?
        // Send routing ID followed by an empty message to close connection prior to
        // closing socket on this end.
        m_socket->send(m_routingId.begin(), m_routingId.end(), ZMQ_SNDMORE);
        std::string tmp;
        m_socket->send(tmp.data(),0);
    }
}

}
}