// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

#ifndef COMMUNICATIONS_ZMQ_PULL_RECEIVER_H
#define COMMUNICATIONS_ZMQ_PULL_RECEIVER_H

#include "ISocket.h"
#include "ZmqReceiver.h"

#include <string>

namespace uxas {
namespace communications {

/**
 * @brief Class that appropriately combines a receiver and socket type to provide client functionality for
 *        a ZMQ_PULL type socket. 
 */

class ZmqPullReceiver : public ZmqReceiver<std::string>, public ISocket<const std::string&, bool> {
public:
    /**
     * @brief Default constructor which currently serves as a "dependency injector" for the socket creation and
     *        underlying IMsgReceiver implementation.
     */
    ZmqPullReceiver();

    /**
     * @brief Default destructor
     */
    ~ZmqPullReceiver() override = default;

    /**
     * @brief Pass through to initialize the underlying socket base type.
     * 
     * @param address - Zmq socket address to connect to
     * @param isServer - Boolean indicating if this socket is a server.
     * @return Return true if initialized appropriately, false otherwise.
     */
    bool initialize(const std::string& address, bool isServer) override;

    /**
     * @brief Receive messages from underlying socket.  Currently acts as a passthrough to the underlying
     *        m_receiver type.
     * 
     * @return std::string - copy of received message.
     */
    std::string receive() override;

private:
    // Underlying receiver which allows de-coupled implementation of receive logic from the socket type specification.
    std::unique_ptr<IMsgReceiver<std::string>> m_receiver;
};

}
}

#endif