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

#include "MsgSenderSentinel.h"
#include "MsgReceiverSentinel.h"
#include "ZmqPushSender.h"
#include "ZmqPullReceiver.h"
#include "ZmqProxy.h"
#include "ZmqTcpSenderReceiver.h"
#include "AddressedAttributedMessage.h"

#include <string>
#include <thread>

namespace uxas {
namespace communications {

class ZmqAttributedMsgSenderReceiver 
    : public ISocket<const std::string&, bool>,
      public IMsgSender<data::AddressedAttributedMessage&>,
      public IMsgReceiver<data::AddressedAttributedMessage> 
{
public:
    ZmqAttributedMsgSenderReceiver();

    ~ZmqAttributedMsgSenderReceiver() override = default;

    // Initialize and start Proxy
    bool initialize(const std::string& address, bool isServer) override;

    // Send messages to the local socket (will be forwarded to proxy)
    void send(data::AddressedAttributedMessage& msg) override;

    // Receive messages from the local socket (received from the proxy)
    data::AddressedAttributedMessage receive() override;

    // Set Proxy Send/Recv Addresses
    void setProxySend(std::string address) { m_proxySendAddress = std::move(address); }
    void setProxyRecv(std::string address) { m_proxyReceiveAddress = std::move(address); }

private:
    std::shared_ptr<ISocket<const std::string&, bool>> m_sendSocket;
    std::shared_ptr<ISocket<const std::string&, bool>> m_receiveSocket;
    std::shared_ptr<IMsgSender<std::string&>> m_sender;
    std::shared_ptr<IMsgReceiver<data::AddressedAttributedMessage>> m_receiver;
    std::shared_ptr<ZmqProxy> m_tcpProxy;
    common::SentinelSerialBuffer m_serialBuffer;
    std::string m_proxySendAddress;   // This is sending to the Proxy from UxAS
    std::string m_proxyReceiveAddress;  // This is receiving from the Proxy
};

}
}

#endif