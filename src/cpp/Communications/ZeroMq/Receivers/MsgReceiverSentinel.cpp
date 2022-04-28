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

// data::AddressedAttributedMessage MsgReceiverSentinel::receive() {
std::string MsgReceiverSentinel::receive() {
    UXAS_LOG_DEBUG_VERBOSE(typeid(this).name(),"::",__func__,":TRACE");
    // data::AddressedAttributedMessage retVal{};
    std::string retVal{};
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
            m_receivedMsgs.push(receivedMsg);
            // Attempt to receive another message (TCP stream receive call can return multiple LMCP messages).
            receivedMsg = m_serialBuffer.getNextPayloadString("");
        }

        // Check if we received any messages successfully, and return the first message if so.
        if (!m_receivedMsgs.empty()) {
            retVal = std::move(m_receivedMsgs.front());
            m_receivedMsgs.pop();
        }
    }
    return retVal;
}

}
}