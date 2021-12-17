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

#include "ISocket.h"
#include "ZmqReceiver.h"

#include <string>

namespace uxas {
namespace communications {

class ZmqPullReceiver : public ZmqReceiver<std::string>, public ISocket<const std::string&, bool> {
public:
    ZmqPullReceiver();

    ~ZmqPullReceiver() override = default;

    // Initialize our socket!
    bool initialize(const std::string& address, bool isServer) override;

    // Receive message!
    std::string receive() override;

private:
    std::unique_ptr<IMsgReceiver<std::string>> m_receiver;
};

}
}

#endif