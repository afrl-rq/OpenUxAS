// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

#ifndef UXAS_ZERO_MQ_TCP_SENDER_RECEIVER_H
#define UXAS_ZERO_MQ_TCP_SENDER_RECEIVER_H

#include "ZeroMqTcpSocket.h"
#include "ZeroMqTcpSender.h"
#include "ZeroMqTcpReceiver.h"
#include "SetArrayClientList.h"
#include "ZeroMqSocketInitializer.h"
#include "UxAS_Log.h"

#include <string>
#include <set>

namespace uxas {
namespace communications {
namespace transport {

// This class instantiates a TCP sender/receiver, this IS NOT thread-safe and should not be shared across threads!

class ZeroMqTcpSenderReceiver : public ZeroMqSender<std::string&>, public ZeroMqReceiver<std::string>,
    public ISocket<const std::string&, bool> {
public:
    ZeroMqTcpSenderReceiver() 
    : m_clients{std::make_shared<SetArrayClientList>()}
    {
        auto socket = std::make_shared<ZeroMqTcpSocket>(std::make_shared<ZeroMqSocketInitializer>());
        m_socket = socket;
        m_sender = stduxas::make_unique<ZeroMqTcpSender>(socket,m_clients);
        m_receiver = stduxas::make_unique<ZeroMqTcpReceiver>(socket,m_clients);
    }

    ~ZeroMqTcpSenderReceiver() override = default;

    // Initialize our socket!
    bool initialize(const std::string& address, bool isServer) override {
        return m_socket->initialize(address, isServer);
    }

    // Send message
    void send(std::string& msg) override {
        m_sender->send(msg);
    }

    // Receive message
    std::string receive() override {
        std::string retVal = m_receiver->receive();
        
        return retVal;
    }

private:
    std::shared_ptr<ISocket<const std::string&, bool>> m_socket;
    std::shared_ptr<IClientList<std::array<uint8_t,256>>> m_clients;
    std::unique_ptr<IMsgSender<std::string&>> m_sender;
    std::unique_ptr<IMsgReceiver<std::string>> m_receiver;
};

}
}
}

#endif