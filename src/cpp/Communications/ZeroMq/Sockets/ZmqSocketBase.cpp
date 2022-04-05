// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

#include "ZmqSocketBase.h"
#include "UxAS_Log.h"

namespace uxas {
namespace communications {

ZmqSocketBase::ZmqSocketBase(InitializerPtr initializer, zmq::socket_type socketType) 
    : m_socketType{static_cast<int32_t>(socketType)}, m_initializer{initializer} {}

ZmqSocketBase::~ZmqSocketBase() {
    if (m_socket) {
        // Set to NO linger, socket will immediately close and discard pending messages.
        m_socket->setsockopt<uint32_t>(ZMQ_LINGER,0);
        m_socket->close();
    }
}

bool ZmqSocketBase::initialize(const std::string& address, bool isServer) {
    UXAS_LOG_DEBUG_VERBOSE(typeid(this).name(),"::",__func__,":TRACE");
    m_isServer = isServer;
    if (m_initializer->initialize(m_socket, address, m_socketType, m_isServer)) {
        uint8_t buffer[256];
        std::size_t bufferSize{256};
        m_socket->getsockopt(ZMQ_ROUTING_ID, buffer, &bufferSize);
        m_routingId = std::vector<uint8_t>{buffer, buffer + bufferSize};
        return true;
    } else {
        UXAS_LOG_ERROR("*** ZmqSocketBase - Failed to initialize socket ***");
        return false;
    }
}

std::shared_ptr<zmq::socket_t> ZmqSocketBase::getRawZmqSocket() { return m_socket; }

bool ZmqSocketBase::isServer() const { return m_isServer; }

const std::vector<uint8_t>& ZmqSocketBase::getRoutingId() const { return m_routingId; }

}
}