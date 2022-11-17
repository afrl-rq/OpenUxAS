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

/**
 * @brief Receiver class that is specific to the ZMQ_STREAM type for TCP based communication.
 */

class ZmqTcpReceiver : public ZmqReceiver<std::string> {
public:
    /**
     * @brief Construct a new Zmq Tcp Receiver object
     * 
     * @param socket - Socket type is tightly-coupled with the ZmqTcpSocket type.
     * @param clients - Shared pointer to a list of clients.
     */
    ZmqTcpReceiver(std::shared_ptr<ZmqTcpSocket> socket,
        std::shared_ptr<IClientList<std::vector<uint8_t>>> clients);

    /**
     * @brief Default constructor
     */
    ~ZmqTcpReceiver() override = default;

    /**
     * @brief Receive data from the TCP connection.  This call will block on receiving data so
     *        guard this call appropriately.
     * 
     * @return std::string - Received data placed into a std::string object.
     */
    std::string receive() override;

private:
    std::shared_ptr<IClientList<std::vector<uint8_t>>> m_clients;  // List of client IDs received over socket.
};

}
}

#endif