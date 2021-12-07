// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

#ifndef UXAS_ZERO_MQ_ATTRIBUTED_MESSAGE_SENDER_RECEIVER_H
#define UXAS_ZERO_MQ_ATTRIBUTED_MESSAGE_SENDER_RECEIVER_H

#include "MsgSenderSentinel.h"
#include "ZeroMqPushSender.h"
#include "ZeroMqPullReceiver.h"
#include "ZeroMqProxy.h"
#include "ZeroMqTcpSenderReceiver.h"
#include "AddressedAttributedMessage.h"

#include <string>
#include <thread>
#include <deque>

namespace uxas {
namespace communications {
namespace transport {

class ZeroMqAttributedMsgSenderReceiver 
    : public ISocket<const std::string&, bool>,
      public IMsgSender<data::AddressedAttributedMessage&>,
      public IMsgReceiver<data::AddressedAttributedMessage> 
{
public:
    ZeroMqAttributedMsgSenderReceiver() {
        // Setup sender backend
        auto sendSocket = std::make_shared<ZeroMqPushSender>();
        m_sendSocket = sendSocket;
        m_sender = std::make_shared<MsgSenderSentinel>(std::move(m_sendSocket));

        // Setup receiver backend
        auto receiveSocket = std::make_shared<ZeroMqPullReceiver>();
        m_receiveSocket = receiveSocket;
        m_receiver = std::move(receiveSocket);
    }

    ~ZeroMqAttributedMsgSenderReceiver() override = default;

    // Initialize and start Proxy
    bool initialize(const std::string& address, bool isServer) override {
        std::string sendAddress{"inproc://sendToProxy"};   // For Proxy this is fromUxAS
        std::string recvAddress{"inproc://recvFromProxy"};  // For Proxy this is to UxAS

        // Initialize the sender backend
        m_sendSocket->initialize(sendAddress, false);

        // Initialze the receiver backend
        m_receiveSocket->initialize(recvAddress, true);

        // Setup TCP proxy, initialize, and begin thread
        auto receiveSocket = std::make_shared<ZeroMqPullReceiver>();
        auto sendSocket = std::make_shared<ZeroMqPushSender>();
        auto tcpSocket = std::make_shared<ZeroMqTcpSenderReceiver>();
        receiveSocket->initialize(sendAddress, true);
        sendSocket->initialize(recvAddress, false);
        tcpSocket->initialize(address, isServer);
        m_tcpProxy = std::make_shared<ZeroMqProxy>(std::move(receiveSocket), std::move(tcpSocket), 
            std::move(tcpSocket), std::move(sendSocket));
        m_tcpProxy->run();
    }

    // Send messages to the local socket (will be forwarded to proxy)
    void send(data::AddressedAttributedMessage& msg) {
        std::string msgCopy = msg.getString();
        m_sender->send(msgCopy);
    }

    // Receive messages from the local socket (received from the proxy)
    data::AddressedAttributedMessage receive() {
        data::AddressedAttributedMessage nextMsg;
        std::string receivedMsg = m_serialBuffer.getNextPayloadString( m_receiver->receive() );

        while (!receivedMsg.empty()) {
            data::AddressedAttributedMessage msg;
            if (msg.setAddressAttributesAndPayloadFromDelimitedString(std::move(receivedMsg))) {
                m_receivedMsgs.push_back( std::move(msg) );
            }
            receivedMsg = m_serialBuffer.getNextPayloadString("");
        }

        if (!m_receivedMsgs.empty()) {
            nextMsg = std::move(m_receivedMsgs[0]);
            m_receivedMsgs.pop_front();
        }
        return nextMsg;
    }

private:
    std::shared_ptr<ISocket<const std::string&, bool>> m_sendSocket;
    std::shared_ptr<ISocket<const std::string&, bool>> m_receiveSocket;
    std::shared_ptr<IMsgSender<std::string&>> m_sender;
    std::shared_ptr<IMsgReceiver<std::string>> m_receiver;
    std::shared_ptr<ZeroMqProxy> m_tcpProxy;
    common::SentinelSerialBuffer m_serialBuffer;
    std::deque<data::AddressedAttributedMessage> m_receivedMsgs;
};

}
}
}

#endif