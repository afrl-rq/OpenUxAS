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

// This class instantiates a TCP sender/receiver, this IS NOT thread-safe and should not be shared across threads!

class ZmqTcpSenderReceiver : public IMsgSender<std::string&>, public IMsgReceiver<std::string>,
    public ZmqSocketBase {
public:
    ZmqTcpSenderReceiver();

    ~ZmqTcpSenderReceiver() override = default;

    // Initialize our socket!
    bool initialize(const std::string& address, bool isServer);

    // Send message
    void send(std::string& msg) override;

    // Receive message
    std::string receive() override ;

    // Get underlying ZeroMqSocket
    std::shared_ptr<ZmqSocketBase> getSocket();

private:
    std::shared_ptr<ZmqSocketBase> m_socket;
    std::shared_ptr<IClientList<std::vector<uint8_t>>> m_clients;
    std::unique_ptr<IMsgSender<std::string&>> m_sender;
    std::unique_ptr<IMsgReceiver<std::string>> m_receiver;
};

}
}

#endif