// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

#ifndef UXAS_ZERO_MQ_SOCKET_INITIALIZER_H
#define UXAS_ZERO_MQ_SOCKET_INITIALIZER_H

#include "ZeroMqFabric.h"
#include "TransportBase.h"
#include "ISocket.h"

#include <memory>

namespace uxas {
namespace communications {
namespace transport {

// This class provides a means of creating the ZeroMq socket! 

class ZeroMqSocketInitializer : public ISocket<std::shared_ptr<zmq::socket_t>, const std::string&, int32_t, bool> {
public:
    ZeroMqSocketInitializer();
    ~ZeroMqSocketInitializer() override;

    // Initialize the socket
    bool initialize(std::shared_ptr<zmq::socket_t> socketPtr, 
        const std::string& address, int32_t type, bool isServer) override
    {
        ZeroMqSocketConfiguration config(uxas::communications::transport::NETWORK_NAME::zmqLmcpNetwork(),
            address, type, isServer, false, 10000, 10000);
        socketPtr = std::move(ZeroMqFabric::getInstance().createSocket(config));
        return true;
    }
};

}
}
}

#endif