// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

#ifndef COMMUNICATIONS_ZMQ_SOCKET_INITIALIZER_H
#define COMMUNICATIONS_ZMQ_SOCKET_INITIALIZER_H

#include "ZeroMqFabric.h"
#include "TransportBase.h"
#include "ISocket.h"

#include <memory>

namespace uxas {
namespace communications {

// This class provides a means of creating the ZeroMq socket! 

class ZmqSocketInitializer : public ISocket<std::shared_ptr<zmq::socket_t>, const std::string&, int32_t, bool> {
public:
    ~ZmqSocketInitializer() override = default;

    // Initialize the socket
    bool initialize(std::shared_ptr<zmq::socket_t> socketPtr, 
        const std::string& address, int32_t type, bool isServer) override
    {
        transport::ZeroMqSocketConfiguration config(uxas::communications::transport::NETWORK_NAME::zmqLmcpNetwork(),
            address, type, isServer, false, 10000, 10000);
        socketPtr = std::move(transport::ZeroMqFabric::getInstance().createSocket(config));
        return true;
    }
};

}
}

#endif