// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

#include "ZmqTcpSenderReceiver.h"
#include "UxAS_Log.h"

namespace uxas {
namespace communications {

ZmqTcpSenderReceiver::ZmqTcpSenderReceiver() 
    : m_clients{std::make_shared<SetArrayClientList>()}
{
    UXAS_LOG_DEBUG_VERBOSE(typeid(this).name(),"::",__func__,":TRACE");
    auto socket = std::make_shared<ZmqTcpSocket>(std::make_shared<ZmqSocketInitializer>());
    m_socket = socket;
    m_sender = stduxas::make_unique<ZmqTcpSender>(socket,m_clients);
    m_receiver = stduxas::make_unique<ZmqTcpReceiver>(socket,m_clients);
}

bool ZmqTcpSenderReceiver::initialize(const std::string& address, bool isServer) {
    UXAS_LOG_DEBUG_VERBOSE(typeid(this).name(),"::",__func__,":TRACE");
    return m_socket->initialize(address, isServer);
}

void ZmqTcpSenderReceiver::send(std::string& msg) {
    UXAS_LOG_DEBUG_VERBOSE(typeid(this).name(),"::",__func__,":TRACE");
    m_sender->send(msg);
}

std::string ZmqTcpSenderReceiver::receive() {
    UXAS_LOG_DEBUG_VERBOSE(typeid(this).name(),"::",__func__,":TRACE");
    std::string retVal = m_receiver->receive();
    return retVal;
}

std::shared_ptr<ZmqSocketBase> ZmqTcpSenderReceiver::getSocketBase() {
    return m_socket; 
}

}
}