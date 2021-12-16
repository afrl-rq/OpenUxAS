// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

#ifndef COMMUNICATIONS_ZMQ_TCP_RECEIVER_H
#define COMMUNICATIONS_ZMQ_TCP_RECEIVER_H

#include "ZmqReceiver.h"
#include "ZmqTcpSocket.h"
#include "IClientList.h"
#include "UxAS_Log.h"

#include <string>
#include <set>

namespace uxas {
namespace communications {

class ZmqTcpReceiver : public ZmqReceiver<std::string> {
public:
    ZmqTcpReceiver(std::shared_ptr<ZmqTcpSocket> socket,
        std::shared_ptr<IClientList<std::vector<uint8_t>>> clients) 
        : ZmqReceiver{socket}, m_clients{std::move(clients)}
        {}

    ~ZmqTcpReceiver() override = default;

    std::string receive() override {
        UXAS_LOG_WARN("*** CPW: ZmqTcpReceiver - receive()...");
        // NOTE: Logic here does not poll socket for possible messages and will block!
        std::string retVal{""};
        int more = 0;
        size_t moreSize = sizeof(more);
        std::vector<uint8_t> routeId;

        zmq::message_t msg;
        UXAS_LOG_WARN("*** CPW: ZmqTcpReceiver - low level socker.recv ...");
        m_socket->getSocket()->recv(&msg);
        // Expect more message contents ONLY after initial routingID data
        UXAS_LOG_WARN("*** CPW: ZmqTcpReceiver - determine if MORE data ...");
        m_socket->getSocket()->getsockopt(ZMQ_RCVMORE, &more, &moreSize);
        if (!more) {
            UXAS_LOG_ERROR("Unexpected message received from TCP socket!");
        } else {
            zmq::message_t dataMsg;
            // Record ID in client list
            UXAS_LOG_WARN("*** CPW: ZmqTcpReceiver - move data ...");
            UXAS_LOG_WARN("*** CPW: ZmqTcpReceiver - size of data = ", msg.size());
            auto dataPtr = static_cast<uint8_t*>(msg.data());
            routeId = std::vector<uint8_t>{dataPtr, dataPtr + msg.size()};
            // Get payload data
            UXAS_LOG_WARN("*** CPW: ZmqTcpReceiver - data payload low level socket.recv call...");
            m_socket->getSocket()->recv(&dataMsg);

            if (dataMsg.size() == 0) {
                // Received a blank message which indicates this client is attempting connect/disconnect, add/erase
                // from clientList.
                if (m_clients->getClients().find(routeId) != m_clients->getClients().end()) {
                    UXAS_LOG_WARN("*** CPW: ZmqTcpReceiver - client is disconnecting ...");
                    // Already part of client list so remove
                    m_clients->removeClient(routeId);
                } else {
                    // Not part of client list so now add
                    UXAS_LOG_WARN("*** CPW: ZmqTcpReceiver - client is connecting ...");
                    UXAS_LOG_WARN("*** CPW: ZmqTcpReceiver - routeId size = ", routeId.size(), " ...");
                    m_clients->addClient(routeId);
                    UXAS_LOG_WARN("*** CPW: ZmqTcpReceiver - client CONNECTED! ...");
                }
            } else {
                // Received real data, return the unprocessed data packet.
                UXAS_LOG_WARN("*** CPW: ZmqTcpReceiver - get data for return...");
                retVal = std::string{static_cast<const char*>(dataMsg.data()), dataMsg.size()};
            }

        }

        UXAS_LOG_WARN("*** CPW: ZmqTcpReceiver - return val = ", retVal, " ...");
        return retVal;
    }

private:
    std::shared_ptr<IClientList<std::vector<uint8_t>>> m_clients;  // List of client IDs received over socket.
};

}
}

#endif