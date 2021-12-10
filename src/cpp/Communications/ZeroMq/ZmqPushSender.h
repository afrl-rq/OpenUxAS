// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

#ifndef COMMUNICATIONS_ZMQ_PUSH_SENDER_H
#define COMMUNICATIONS_ZMQ_PUSH_SENDER_H

#include "ZmqSocketBase.h"
#include "ZmqSender.h"
#include "ZmqSocketInitializer.h"
#include "UxAS_Log.h"

#include <string>

namespace uxas {
namespace communications {

class ZmqPushSender : public ZmqSender<std::string&>, public ISocket<const std::string&, bool> {
public:
    ZmqPushSender() 
    : ZmqSender{std::make_shared<ZmqSocketBase>(std::make_shared<ZmqSocketInitializer>(), zmq::socket_type::push)}
    {}

    ~ZmqPushSender() override = default;

    // Initialize our socket!
    bool initialize(const std::string& address, bool isServer) override {
        return getSocket()->initialize(address, isServer);
    }
};

}
}

#endif