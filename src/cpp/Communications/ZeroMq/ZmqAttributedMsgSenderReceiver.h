// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

#ifndef COMMUNICATIONS_ZMQ_ATTRIBUTED_MESSAGE_SENDER_RECEIVER_H
#define COMMUNICATIONS_ZMQ_ATTRIBUTED_MESSAGE_SENDER_RECEIVER_H

#include "ZmqProxy.h"
#include "AddressedAttributedMessage.h"
#include "IMsgSenderReceiver.h"

#include <string>
#include <thread>

namespace uxas {
namespace communications {

/**
 * @brief Class that approriately combines components to provide an AddressedAttributedMessage TCP sender/receiver
 *        utilizing the Zmq infrastructure within UxAS.
 */

class ZmqAttributedMsgSenderReceiver 
    : public IMsgSenderReceiver<data::AddressedAttributedMessage, const std::string&, bool>
{
public:
    /**
     * @brief Constructor which acts as a "dependency injector" to setup the appropriate sender/receivers that
     *        directly interact with the client object on the UxAS side.
     */
    ZmqAttributedMsgSenderReceiver();

    /**
     * @brief Default destructor
     */
    ~ZmqAttributedMsgSenderReceiver() override = default;

    // Initialize and start Proxy

    bool initialize(const std::string& address, bool isServer) override;

    // Send messages to the local socket (will be forwarded to proxy)
    void send(data::AddressedAttributedMessage& msg) override;

    // Receive messages from the local socket (received from the proxy)
    data::AddressedAttributedMessage receive() override;

private:
    std::shared_ptr<ISocket<const std::string&, bool>> m_sendSocket;
    std::shared_ptr<ISocket<const std::string&, bool>> m_receiveSocket;
    std::shared_ptr<IMsgSender<std::string&>> m_sender;
    // std::shared_ptr<IMsgReceiver<data::AddressedAttributedMessage>> m_receiver;
    std::shared_ptr<IMsgReceiver<std::string>> m_receiver;
    std::shared_ptr<ZmqProxy> m_tcpProxy;
    common::SentinelSerialBuffer m_serialBuffer;

    // Keep track of how many instances of this class have been initialized
    static std::atomic<int> count;
};

}
}

#endif