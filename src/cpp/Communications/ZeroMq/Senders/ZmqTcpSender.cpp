// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

#include "ZmqTcpSender.h"
#include "UxAS_Log.h"

namespace uxas {
namespace communications {

ZmqTcpSender::ZmqTcpSender(std::shared_ptr<ZmqTcpSocket> socket, 
        std::shared_ptr<IClientList<std::array<uint8_t,256>>> clients) 
        : ZmqSender{std::move(socket)}, m_clients{std::move(clients)}
        {}

void ZmqTcpSender::send(std::string& msg) {
    if (m_socket && m_socket->getSocket()) {
        if (!m_socket->isServer()) {
            m_socket->getSocket()->send(m_socket->getRoutingId().begin(), m_socket->getRoutingId().end(), 0);
            UXAS_LOG_DEBUG_VERBOSE("ZmqTcpSender::send BEFORE sending TCP stream single-part message");
            ZmqSender::send(msg);
            UXAS_LOG_DEBUG_VERBOSE("ZmqTcpSender::send AFTER sending TCP stream single-part message");
        } else {
            // Send to all clients!
            for(const auto& client : m_clients->getClients()) {
                m_socket->getSocket()->send(client.begin(), client.end(), 0);
                UXAS_LOG_DEBUG_VERBOSE("ZmqTcpSender::send BEFORE sending TCP stream single-part message");
                ZmqSender::send(msg);
                UXAS_LOG_DEBUG_VERBOSE("ZmqTcpSender::send AFTER sending TCP stream single-part message");
            }
        }
    }
}

}
}