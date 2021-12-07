// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

#ifndef UXAS_ZERO_MQ_PULL_RECEIVER_H
#define UXAS_ZERO_MQ_PULL_RECEIVER_H

#include "ZeroMqGenericReceiver.h"
#include "ZeroMqSocketBase.h"
#include "ZeroMqSocketInitializer.h"
#include "UxAS_Log.h"

#include <string>

namespace uxas {
namespace communications {
namespace transport {

class ZeroMqPullReceiver : public ZeroMqReceiver<std::string>, public ISocket<const std::string&, bool> {
public:
    ZeroMqPullReceiver()
    : ZeroMqReceiver{std::make_shared<ZeroMqSocketBase>(std::make_shared<ZeroMqSocketInitializer>(), zmq::socket_type::pull)},
      m_receiver{stduxas::make_unique<ZeroMqGenericReceiver>(m_socket)}
    {}

    ~ZeroMqPullReceiver() override = default;

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
}

#endif