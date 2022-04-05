// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

#ifndef COMMUNICATIONS_ZMQ_TCP_SENDER_RECEIVER_H
#define COMMUNICATIONS_ZMQ_TCP_SENDER_RECEIVER_H

#include "ZmqTcpSocket.h"
#include "ZmqTcpSender.h"
#include "ZmqTcpReceiver.h"
#include "SetArrayClientList.h"
#include "ZmqSocketInitializer.h"

#include <string>
#include <set>

namespace uxas {
namespace communications {

/**
 * @brief Class that appropriately combines a sender, receiver, and socket type to provide client functionality for a
 *        ZMQ_STREAM type socket with send/receive capabilities.  This class IS NOT thread-safe and should not be
 *        shared across threads!
 */

class ZmqTcpSenderReceiver : public IMsgSender<std::string&>, public IMsgReceiver<std::string>,
    public ZmqSocketBase {
public:
    /**
     * @brief Default constructor which currently serves as a "dependency injector" for the socket creation and
     *        underlying sender/receiver implementations.
     */
    ZmqTcpSenderReceiver();

    /**
     * @brief Default destructor
     */
    ~ZmqTcpSenderReceiver() override = default;

    /**
     * @brief Pass through to initialize the underlying socket base type.
     * 
     * @param address - Zmq socket address to connect to
     * @param isServer - Boolean indicating if this socket is a server.
     * @return Return true if initialized appropriately, false otherwise.
     */
    bool initialize(const std::string& address, bool isServer);

    /**
     * @brief Send message to underlying socket.  Currently acts as a passthrough to the underlying
     *        m_sender type.
     * 
     * @param msg - reference to message to be sent.
     */
    void send(std::string& msg) override;

    /**
     * @brief Receive messages from underlying socket.  Currently acts as a passthrough to the underlying
     *        m_receiver type.
     * 
     * @return std::string - copy of received message.
     */
    std::string receive() override ;

    /**
     * @brief Get a shared pointer to the underlying Zmq socket.
     * 
     * @return std::shared_ptr<ZmqSocketBase> 
     */
    std::shared_ptr<ZmqSocketBase> getSocketBase();

private:
    std::shared_ptr<ZmqSocketBase> m_socket;
    std::shared_ptr<IClientList<std::vector<uint8_t>>> m_clients;
    std::unique_ptr<IMsgSender<std::string&>> m_sender;
    std::unique_ptr<IMsgReceiver<std::string>> m_receiver;
};

}
}

#endif