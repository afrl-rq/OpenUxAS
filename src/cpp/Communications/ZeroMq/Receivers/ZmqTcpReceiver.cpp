// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

#include "ZmqTcpReceiver.h"

namespace uxas {
namespace communications {

//TODO: CPW - Is there a way to decouple the tight-coupling with the ZmqTcpSocket type here?
ZmqTcpReceiver::ZmqTcpReceiver(std::shared_ptr<ZmqTcpSocket> socket, 
    std::shared_ptr<IClientList<std::vector<uint8_t>>> clients) 
    : ZmqReceiver{socket}, m_clients{std::move(clients)}
    {}

//TODO: CPW - TCP receive here does not account for multipart ZMQ messages.  Does it need to?
std::string ZmqTcpReceiver::receive() {
    UXAS_LOG_DEBUG_VERBOSE(typeid(this).name(),"::",__func__,":TRACE");
    // NOTE: Logic here does not poll socket for possible messages and will block!
    std::string retVal{""};
    if (!m_socket || !m_socket->getRawZmqSocket()) {
        UXAS_LOG_ERROR(typeid(this).name(),"::",__func__," - Socket are not valid!");
        return retVal;
    }

    int more = 0;
    size_t moreSize = sizeof(more);
    std::vector<uint8_t> routeId;
    zmq::message_t msg;
    
    m_socket->getRawZmqSocket()->recv(&msg);
    // Expect more message contents ONLY after initial routingID data
    m_socket->getRawZmqSocket()->getsockopt(ZMQ_RCVMORE, &more, &moreSize);
    if (!more) {
        UXAS_LOG_ERROR(typeid(this).name(),"::",__func__,"Unexpected message received from TCP socket!");
    } else {
        zmq::message_t dataMsg;
        // Record ID in client list
        auto dataPtr = static_cast<uint8_t*>(msg.data());
        routeId = std::vector<uint8_t>{dataPtr, dataPtr + msg.size()};
        // Get payload data
        m_socket->getRawZmqSocket()->recv(&dataMsg);

        if (dataMsg.size() == 0) {
            // Received a blank message which indicates this client is attempting connect/disconnect, add/erase
            // from clientList.
            if (m_clients->getClients().find(routeId) != m_clients->getClients().end()) {
                // Already part of client list so remove
                m_clients->removeClient(routeId);
            } else {
                // Not part of client list so now add
                m_clients->addClient(routeId);
            }
        } else {
            // Received real data, return the unprocessed data packet.
            retVal = std::string{static_cast<const char*>(dataMsg.data()), dataMsg.size()};
        }

    }

    return retVal;
}

}
}