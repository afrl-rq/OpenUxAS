// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

#include "ZmqProxy.h"
#include "UxAS_Log.h"

#include "zmq.hpp"
#include <vector>

namespace uxas {
namespace communications {

ZmqProxy::ZmqProxy( ReceiverType inRecv, SenderType outSend, ReceiverType outRecv, SenderType inSend )
    : m_internalReceiver(std::move(inRecv)), m_externalSender(std::move(outSend)),
    m_externalReceiver(std::move(outRecv)), m_internalSender(std::move(inSend))
    {}

void ZmqProxy::executeOnThread() {
    UXAS_LOG_WARN(__FILE__, "::", __func__, ": Setup zmq poller...");
    std::vector<zmq_pollitem_t> pollItems;
    
    if (m_internalReceiver.second->getSocket()) {
        UXAS_LOG_WARN(__FILE__, "::", __func__, ": Internal Receiver socket is good!");
    }
    if (m_externalReceiver.second->getSocket()) {
        UXAS_LOG_WARN(__FILE__, "::", __func__, ": External Receiver socket is good!");
    }

    pollItems.push_back( {*m_internalReceiver.second->getSocket(), 0, ZMQ_POLLIN, 0} );
    pollItems.push_back( {*m_externalReceiver.second->getSocket(), 0, ZMQ_POLLIN, 0} );

    UXAS_LOG_WARN(__FILE__, "::", __func__, ": Starting receiver loop...");
    while (!m_shutdown) {
        // blocking call for receiving data!
        UXAS_LOG_WARN(__FILE__, "::", __func__, ": Blocking poll() call...");
        zmq::poll(pollItems);
        if (pollItems[0].revents & ZMQ_POLLIN) {
            UXAS_LOG_WARN(__FILE__, "::", __func__, ": Received something on internal Receiver...");
            std::string msg = m_internalReceiver.first->receive();
            m_externalSender.first->send(msg);
        }

        if (pollItems[1].revents & ZMQ_POLLIN) {
            UXAS_LOG_WARN(__FILE__, "::", __func__, ": Received something on external Receiver...");
            std::string msg = m_externalReceiver.first->receive();
            m_internalSender.first->send(msg);
        }
    }
}

}
}
