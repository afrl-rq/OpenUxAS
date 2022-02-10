// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

#include "MsgReceiverSentinel.h"

namespace uxas {
namespace communications {

MsgReceiverSentinel::MsgReceiverSentinel(std::shared_ptr<IMsgReceiver<std::string>> receiver)
    : m_receiver{std::move(receiver)} {}

data::AddressedAttributedMessage MsgReceiverSentinel::receive() {
    UXAS_LOG_DEBUG_VERBOSE(typeid(this).name(),"::",__func__,":TRACE");
    data::AddressedAttributedMessage retVal{};
    if (!m_receiver) {
        UXAS_LOG_ERROR(typeid(this).name(),"::",__func__," - receiver pointer is not valid!");
        return retVal;
    }

    if (!m_receivedMsgs.empty()) {
        // We already have some buffered messages, return these!
        retVal = std::move(m_receivedMsgs.front());
        m_receivedMsgs.pop();
    } else {
        // Attempt to receive new messages from the owned IMsgReceiver
        std::string receivedMsg = m_serialBuffer.getNextPayloadString( m_receiver->receive() );
        while (!receivedMsg.empty()) {
            data::AddressedAttributedMessage msg{};
            if (msg.setAddressAttributesAndPayloadFromDelimitedString(std::move(receivedMsg))) {
                m_receivedMsgs.push(std::move(msg));
            } else {
            }
            receivedMsg = m_serialBuffer.getNextPayloadString("");
        }

        if (!m_receivedMsgs.empty()) {
            retVal = std::move(m_receivedMsgs.front());
            m_receivedMsgs.pop();
        }
    }
    return retVal;
}

}
}