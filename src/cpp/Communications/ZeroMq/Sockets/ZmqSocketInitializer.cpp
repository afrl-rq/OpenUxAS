// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

#include "ZmqSocketInitializer.h"
#include "ZeroMqFabric.h"
#include "TransportBase.h"
#include "UxAS_Log.h"

namespace uxas {
namespace communications {

bool ZmqSocketInitializer::initialize(std::shared_ptr<zmq::socket_t>& socketPtr, 
    const std::string& address, int32_t type, bool isServer)
{
    UXAS_LOG_DEBUG_VERBOSE(typeid(this).name(),"::",__func__,":TRACE");
    //TODO: CPW - Possibly refactor this area to remove ZeroMqFabric (singleton) dependence and the use of the
    //      zmqLmcpNetwork() call.
    // Currently this configuration sets the High-Water-Marks (values at which Zmq sockets start dropping information
    // due to buffers filling up) to 0 which essentially means infinite buffers.
    transport::ZeroMqSocketConfiguration config(uxas::communications::transport::NETWORK_NAME::zmqLmcpNetwork(),
        address, type, isServer, false, 0, 0);
    socketPtr = std::move(transport::ZeroMqFabric::getInstance().createSocket(config));
    return (socketPtr) ? true : false;
}

}
}