// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

#ifndef UXAS_ZERO_MQ_TCP_RECEIVER_H
#define UXAS_ZERO_MQ_TCP_RECEIVER_H

#include "ZeroMqReceiver.h"
#include "ZeroMqTcpSocket.h"
#include "IClientList.h"
#include "UxAS_Log.h"

#include <string>
#include <set>

namespace uxas {
namespace communications {
namespace transport {

class ZeroMqTcpReceiver : public ZeroMqReceiver<std::string> {
public:
    ZeroMqTcpReceiver(std::shared_ptr<ZeroMqTcpSocket> socket,
        std::shared_ptr<IClientList<std::array<uint8_t,256>>> clients) 
        : ZeroMqReceiver{socket}, m_clients{std::move(clients)}
        {}

    ~ZeroMqTcpReceiver() override = default;

    std::string receive() override {
        // NOTE: Logic here does not poll socket for possible messages and will block!
        std::string retVal{""};
        bool firstMessagePart = true;
        int more = 1;
        std::array<uint8_t,256> routeId;
        while (more) {
            zmq::message_t msg;
            int more;

            // First part of message is the routingID 
            m_socket->getSocket()->recv(&msg);
            size_t moreSize = sizeof(more);
            m_socket->getSocket()->getsockopt(ZMQ_RCVMORE, &more, &moreSize);

            if (firstMessagePart && !more) {
                UXAS_LOG_ERROR("Unexpected message received from TCP socket!");

            } else if (firstMessagePart) {
                // Record ID in client list
                routeId = std::move(*msg.data<std::array<uint8_t,256>>());
                m_clients->addClient(std::move(routeId));

                // Done processing ID data
                firstMessagePart = false;

            } else if (!firstMessagePart && msg.size() == 0) {
                // Received a blank message which indicates this client has disconnected, erase from list!
                m_clients->removeClient(routeId);

            } else {
                // All other cases append data to string
                retVal += std::string(msg.data<std::string>()->c_str());
            }
        }
        return retVal;
    }

private:
    std::shared_ptr<IClientList<std::array<uint8_t,256>>> m_clients;  // List of client IDs received over socket.
};

}
}
}

#endif