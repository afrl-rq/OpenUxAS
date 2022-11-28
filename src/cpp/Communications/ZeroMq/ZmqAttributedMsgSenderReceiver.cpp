// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

#include "MsgSenderSentinel.h"
#include "MsgReceiverSentinel.h"
#include "ZmqPullReceiver.h"
#include "ZmqPushSender.h"
#include "ZmqTcpSenderReceiver.h"
#include "ZmqAttributedMsgSenderReceiver.h"

#include "UxAS_ConfigurationManager.h"

namespace uxas {
namespace communications {

std::atomic<int> ZmqAttributedMsgSenderReceiver::count{0};

ZmqAttributedMsgSenderReceiver::ZmqAttributedMsgSenderReceiver() {
    UXAS_LOG_DEBUG_VERBOSE(typeid(this).name(),"::",__func__,":TRACE");
    // Setup sender backend
    auto sendSocket = std::make_shared<ZmqPushSender>();
    m_sendSocket = sendSocket;
    // Use decorator class "MsgSenderSentinel" to wrap the underlying sender.
    m_sender = std::make_shared<MsgSenderSentinel>(std::move(sendSocket));

    // Setup receiver backend
    auto receiveSocket = std::make_shared<ZmqPullReceiver>();
    m_receiveSocket = receiveSocket;
    // Use decorator class "MsgReceiverSentinel" to wrap the underlying receiver.
    m_receiver = std::make_shared<MsgReceiverSentinel>(std::move(receiveSocket));
}

// Initialize and start Proxy
bool ZmqAttributedMsgSenderReceiver::initialize(const std::string& address, bool isServer) {
    UXAS_LOG_DEBUG_VERBOSE(typeid(this).name(),"::",__func__,":TRACE");
    // Internal setting of proxy address
    std::string entityId = std::to_string(common::ConfigurationManager::getInstance().getEntityId());
    // Increment counter to keep track of how many sender/receivers have been initialized.
    int initializeCount = ++count;
    std::string proxySendAddress = "inproc://toZmqProxy" + entityId + "_" + std::to_string(initializeCount);
    std::string proxyReceiveAddress = "inproc://fromZmqProxy" + entityId + "_" + std::to_string(initializeCount);

    // Initialize the sender backend
    m_sendSocket->initialize(proxySendAddress, false);

    // Initialze the receiver backend
    m_receiveSocket->initialize(proxyReceiveAddress, true);

    // Setup TCP proxy, initialize, and begin thread
    auto receiver = std::make_shared<ZmqPullReceiver>();
    auto sender = std::make_shared<ZmqPushSender>();
    auto tcpSenderReceiver = std::make_shared<ZmqTcpSenderReceiver>();

    receiver->initialize(proxySendAddress, true);
    sender->initialize(proxyReceiveAddress, false);
    tcpSenderReceiver->initialize(address, isServer);

    auto receiverPair = std::make_pair(receiver, receiver->getSocketBase());
    auto senderPair = std::make_pair(sender, sender->getSocketBase());
    auto tcpSenderReceiverPair = std::make_pair(tcpSenderReceiver, tcpSenderReceiver->getSocketBase());

    m_tcpProxy = std::make_shared<ZmqProxy>(receiverPair, tcpSenderReceiverPair, 
        tcpSenderReceiverPair, senderPair);
    return m_tcpProxy->run();
}

// Send messages to the local socket (will be forwarded to proxy)
void ZmqAttributedMsgSenderReceiver::send(data::AddressedAttributedMessage& msg) {
    UXAS_LOG_DEBUG_VERBOSE(typeid(this).name(),"::",__func__,":TRACE");
    std::string msgCopy = msg.getString();
    m_sender->send(msgCopy);
}

// Receive messages from the local socket (received from the proxy)
data::AddressedAttributedMessage ZmqAttributedMsgSenderReceiver::receive() {
    UXAS_LOG_DEBUG_VERBOSE(typeid(this).name(),"::",__func__,":TRACE");
    data::AddressedAttributedMessage msg;
    std::string receivedMsg = m_receiver->receive();
    while( !msg.isValid() && !receivedMsg.empty()) {
        msg.setAddressAttributesAndPayloadFromDelimitedString(receivedMsg);
        if (!msg.isValid()) {
            receivedMsg = m_receiver->receive();
        }
    }

    return msg;
}

}
}
