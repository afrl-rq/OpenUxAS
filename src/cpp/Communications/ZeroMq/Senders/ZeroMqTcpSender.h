// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

#ifndef UXAS_ZERO_MQ_TCP_SENDER_H
#define UXAS_ZERO_MQ_TCP_SENDER_H

#include "IClientList.h"
#include "ZeroMqSender.h"
#include "ZeroMqTcpSocket.h"
#include "UxAS_Log.h"

#include <string>
#include <set>

namespace uxas {
namespace communications {
namespace transport {

class ZeroMqTcpSender : public ZeroMqSender<std::string&> {
public:
    ZeroMqTcpSender(std::shared_ptr<ZeroMqTcpSocket> socket, 
        std::shared_ptr<IClientList<std::array<uint8_t,256>>> clients) 
        : ZeroMqSender{socket}, m_clients{std::move(clients)}
        {}

    ~ZeroMqTcpSender() override = default;

    // Send message on the socket
    void send(std::string& msg) override {
        if (m_socket && m_socket->getSocket()) {
            if (!m_socket->isServer()) {
                m_socket->getSocket()->send(m_socket->getRoutingId().begin(), m_socket->getRoutingId().end(), 0);
                UXAS_LOG_DEBUG_VERBOSE("ZeroMqTcpSender::send BEFORE sending TCP stream single-part message");
                ZeroMqSender::send(msg);
                UXAS_LOG_DEBUG_VERBOSE("ZeroMqTcpSender::send AFTER sending TCP stream single-part message");
            } else {
                // Send to all clients!
                for(const auto& client : m_clients->getClients()) {
                    m_socket->getSocket()->send(client.begin(), client.end(), 0);
                    UXAS_LOG_DEBUG_VERBOSE("ZeroMqTcpSender::send BEFORE sending TCP stream single-part message");
                    ZeroMqSender::send(msg);
                    UXAS_LOG_DEBUG_VERBOSE("ZeroMqTcpSender::send AFTER sending TCP stream single-part message");
                }
            }
        }
    };

private:
    std::shared_ptr<IClientList<std::array<uint8_t,256>>> m_clients;
};

}
}
}

#endif