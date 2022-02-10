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
        std::shared_ptr<IClientList<std::vector<uint8_t>>> clients) 
        : ZmqSender{std::move(socket)}, m_clients{std::move(clients)}
        {}

void ZmqTcpSender::send(std::string& msg) {
    UXAS_LOG_DEBUG_VERBOSE(typeid(this).name(),"::",__func__,":TRACE");
    if (!m_socket || !m_socket->getRawZmqSocket()) {
        UXAS_LOG_ERROR(typeid(this).name(),"::",__func__," - Invalid socket pointers!");
        return;
    }

    if (!m_socket->isServer()) {
        // Only send message to the server
        m_socket->getRawZmqSocket()->send(m_socket->getRoutingId().begin(), m_socket->getRoutingId().end(), ZMQ_SNDMORE);
        m_socket->getRawZmqSocket()->send(msg.begin(), msg.end());
    } else {
        // Send to all clients!
        //TODO - Need to test.
        for(const auto& client : m_clients->getClients()) {
            m_socket->getRawZmqSocket()->send(client.begin(), client.end(), ZMQ_SNDMORE);
            m_socket->getRawZmqSocket()->send(msg.begin(), msg.end());
        }
    }
}

}
}