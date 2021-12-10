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

#include "ZmqGenericReceiver.h"
#include "ZmqSocketBase.h"
#include "ZmqSocketInitializer.h"
#include "UxAS_Log.h"

#include <string>

namespace uxas {
namespace communications {

class ZmqPullReceiver : public ZmqReceiver<std::string>, public ISocket<const std::string&, bool> {
public:
    ZmqPullReceiver()
    : ZmqReceiver{std::make_shared<ZmqSocketBase>(std::make_shared<ZmqSocketInitializer>(), zmq::socket_type::pull)},
      m_receiver{stduxas::make_unique<ZmqGenericReceiver>(m_socket)}
    {}

    ~ZmqPullReceiver() override = default;

    // Initialize our socket!
    bool initialize(const std::string& address, bool isServer) override {
        return getSocket()->initialize(address, isServer);
    }

    // Receive message!
    std::string receive() override {
        return m_receiver->receive();
    }

private:
    std::unique_ptr<IMsgReceiver<std::string>> m_receiver;
};

}
}

#endif