// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

#include "ZmqPushSender.h"
#include "ZmqSocketBase.h"
#include "ZmqSocketInitializer.h"
#include "UxAS_Log.h"

namespace uxas {
namespace communications {

ZmqPushSender::ZmqPushSender() 
    : ZmqSender{std::make_shared<ZmqSocketBase>(std::make_shared<ZmqSocketInitializer>(), zmq::socket_type::push)}
    {}

bool ZmqPushSender::initialize(const std::string& address, bool isServer) {
    UXAS_LOG_DEBUG_VERBOSE(typeid(this).name(),"::",__func__,":TRACE");
    return getSocketBase()->initialize(address, isServer);
}

}
}