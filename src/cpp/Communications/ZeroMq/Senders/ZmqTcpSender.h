// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

#ifndef UXAS_ZMQ_TCP_SENDER_H
#define UXAS_ZMQ_TCP_SENDER_H

#include "IClientList.h"
#include "ZmqSender.h"
#include "ZmqTcpSocket.h"
#include "UxAS_Log.h"

#include <string>
#include <set>

namespace uxas {
namespace communications {

/**
 * @brief This class utilizes a ZeroMQ ZMQ_STREAM type socket to send <std::string> messages
 * over an external TCP connection.  
 */

class ZmqTcpSender : public ZmqSender<std::string&> {
public:
    /**
     * @brief Construct a new ZmqTcpSender object passing in the appropriate TCP socket and pointer to an
     * implementation of the IClientList interface.
     * 
     * @param socket - Pointer to the underlying ZeroMQ ZMQ_STREAM socket
     * @param clients - Pointer to 
     */
    ZmqTcpSender(std::shared_ptr<ZmqTcpSocket> socket, 
        std::shared_ptr<IClientList<std::vector<uint8_t>>> clients);

    /**
     * @brief Default destructor
     */
    ~ZmqTcpSender() override = default;

    /**
     * @brief Send <std::string> message over the TCP socket
     * 
     * @param msg - message to be sent
     */
    void send(std::string& msg) override;

private:
    std::shared_ptr<IClientList<std::vector<uint8_t>>> m_clients;
};

}
}

#endif