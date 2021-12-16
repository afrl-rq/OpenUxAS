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
    if (m_socket && m_socket->getSocket()) {
        if (!m_socket->isServer()) {
            UXAS_LOG_WARN("*** CPW: ZmqTcpSender - client sending ***");
            m_socket->getSocket()->send(m_socket->getRoutingId().begin(), m_socket->getRoutingId().end(), ZMQ_SNDMORE);
            // m_socket->getSocket()->send(m_socket->getRoutingId().begin(), m_socket->getRoutingId().end(), 0);
            // m_socket->getSocket()->send(m_socket->getRoutingId().data(), m_socket->getRoutingIdSize(), ZMQ_SNDMORE);
            //TODO - Need to fix the logic for storing the ROUTING_ID!!!
            uint8_t serverid[256] = {0};
            std::size_t serveridsize{256};
            // memset(serverid, 0, 256);

            // std::stringstream ss;
            // for (unsigned int i=0; i < serveridsize; ++i) {
            //     ss << static_cast<unsigned int>(serverid[i]) << " ";
            // }
            // std::cout << "*** CPW: ROUTING_ID = " << ss.str();

            m_socket->getSocket()->getsockopt(ZMQ_ROUTING_ID, serverid, &serveridsize);
            // m_socket->getSocket()->send(serverid, serveridsize, ZMQ_SNDMORE);
            std::stringstream ss2;
            std::stringstream mySS;
            for (unsigned int i=0; i < serveridsize; ++i) {
                ss2 << static_cast<unsigned int>(serverid[i]) << " ";
            }
            for (auto val : m_socket->getRoutingId()) {
                mySS << static_cast<unsigned int>(val) << " ";
            }
            UXAS_LOG_WARN("*** CPW: ROUTING_ID = ", ss2.str());
            UXAS_LOG_WARN("*** CPW: my ROUTING_ID = ", mySS.str());


            UXAS_LOG_WARN("*** CPW: ZmqTcpSender::send BEFORE sending TCP stream single-part message");
            m_socket->getSocket()->send(msg.begin(), msg.end());
            UXAS_LOG_WARN("*** CPW: ZmqTcpSender::send AFTER sending TCP stream single-part message");
        } else {
            // Send to all clients!
            for(const auto& client : m_clients->getClients()) {
                UXAS_LOG_WARN("*** CPW: ZmqTcpSender - server sending ***");
                // m_socket->getSocket()->send(client.begin(), client.end(), 0);
                m_socket->getSocket()->send(client.data(), client.size(), ZMQ_SNDMORE);
                UXAS_LOG_WARN("*** CPW: ZmqTcpSender::send BEFORE sending TCP stream single-part message");
                m_socket->getSocket()->send(msg.begin(), msg.end());
                UXAS_LOG_WARN("*** CPW: ZmqTcpSender::send AFTER sending TCP stream single-part message");
            }
        }
    }
}

}
}