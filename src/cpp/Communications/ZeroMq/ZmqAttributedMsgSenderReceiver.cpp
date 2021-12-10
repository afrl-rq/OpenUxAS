// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

#include "ZmqAttributedMsgSenderReceiver.h"

namespace uxas {
namespace communications {

ZmqAttributedMsgSenderReceiver::ZmqAttributedMsgSenderReceiver() {
    // Setup sender backend
    auto sendSocket = std::make_shared<ZmqPushSender>();
    m_sendSocket = sendSocket;
    m_sender = std::make_shared<MsgSenderSentinel>(std::move(sendSocket));

    // Setup receiver backend
    auto receiveSocket = std::make_shared<ZmqPullReceiver>();
    m_receiveSocket = receiveSocket;
    m_receiver = std::move(receiveSocket);
}

// Initialize and start Proxy
bool ZmqAttributedMsgSenderReceiver::initialize(const std::string& address, bool isServer) {
    std::string sendAddress{"inproc://sendToProxy"};   // For Proxy this is fromUxAS
    std::string recvAddress{"inproc://recvFromProxy"};  // For Proxy this is to UxAS

    // Initialize the sender backend
    m_sendSocket->initialize(sendAddress, false);

    // Initialze the receiver backend
    m_receiveSocket->initialize(recvAddress, true);

    // Setup TCP proxy, initialize, and begin thread
    auto receiver = std::make_shared<ZmqPullReceiver>();
    auto sender = std::make_shared<ZmqPushSender>();
    auto tcpSenderReceiver = std::make_shared<ZmqTcpSenderReceiver>();

    receiver->initialize(sendAddress, true);
    sender->initialize(recvAddress, false);
    tcpSenderReceiver->initialize(address, isServer);

    auto receiverPair = std::make_pair(receiver, receiver->getSocket());
    auto senderPair = std::make_pair(sender, sender->getSocket());
    auto tcpSenderReceiverPair = std::make_pair(tcpSenderReceiver, tcpSenderReceiver->getSocket());

    m_tcpProxy = std::make_shared<ZmqProxy>(receiverPair, tcpSenderReceiverPair, 
        tcpSenderReceiverPair, senderPair);
    return m_tcpProxy->run();
}

// Send messages to the local socket (will be forwarded to proxy)
void ZmqAttributedMsgSenderReceiver::send(data::AddressedAttributedMessage& msg) {
    std::string msgCopy = msg.getString();
    m_sender->send(msgCopy);
}

// Receive messages from the local socket (received from the proxy)
data::AddressedAttributedMessage ZmqAttributedMsgSenderReceiver::receive() {
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

}
}
