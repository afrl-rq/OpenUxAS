// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

#include "ZeroMqFabric.h"
#include "TransportBase.h"
#include "ZmqSocketInitializer.h"
#include "UxAS_Log.h"

namespace uxas {
namespace communications {

bool ZmqSocketInitializer::initialize(std::shared_ptr<zmq::socket_t>& socketPtr, 
    const std::string& address, int32_t type, bool isServer)
{
    UXAS_LOG_DEBUG_VERBOSE(typeid(this).name(),"::",__func__,":TRACE");
    transport::ZeroMqSocketConfiguration config(uxas::communications::transport::NETWORK_NAME::zmqLmcpNetwork(),
        address, type, isServer, false, 10000, 10000);
    socketPtr = std::move(transport::ZeroMqFabric::getInstance().createSocket(config));
    return (socketPtr) ? true : false;
}

}
}