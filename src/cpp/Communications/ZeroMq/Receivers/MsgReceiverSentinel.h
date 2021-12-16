// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

#ifndef COMMUNICATIONS_MSG_SENTINEL_RECEIVE_H
#define COMMUNICATIONS_MSG_SENTINEL_RECEIVE_H

#include "IMsgReceiver.h"
#include "AddressedAttributedMessage.h"
#include "UxAS_SentinelSerialBuffer.h"

#include <queue>

namespace uxas {
namespace communications {

/**
 * @brief Class that decorates an IMsgReceiver<std::string> type with sentinels
 */

class MsgReceiverSentinel : public IMsgReceiver<data::AddressedAttributedMessage> {
public:
    MsgReceiverSentinel(std::shared_ptr<IMsgReceiver<std::string>> receiver) 
    : m_receiver{std::move(receiver)} {}

    virtual ~MsgReceiverSentinel() override = default;

    data::AddressedAttributedMessage receive() override {
        data::AddressedAttributedMessage retVal{};
        if (!m_receivedMsgs.empty()) {
            // We already have some buffered messages, return these!
            UXAS_LOG_WARN("*** CPW: MsgReceiverSentinel - Already had some queued messages to return ***");
            retVal = std::move(m_receivedMsgs.front());
            m_receivedMsgs.pop();
        } else {
            // Attempt to receive new messages from the owned IMsgReceiver
            UXAS_LOG_WARN("*** CPW: MsgReceiverSentinel - Get next payload ***");
            std::string receivedMsg = m_serialBuffer.getNextPayloadString( m_receiver->receive() );
            while (!receivedMsg.empty()) {
                data::AddressedAttributedMessage msg{};
                if (msg.setAddressAttributesAndPayloadFromDelimitedString(std::move(receivedMsg))) {
                    UXAS_LOG_WARN("*** CPW: MsgReceiverSentinel - Add new message to queue ***");
                    m_receivedMsgs.push(std::move(msg));
                } else {
                    UXAS_LOG_ERROR("*** CPW: MsgReceiverSentinel - Issue generating AddressAttributed Msg ***");
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

private:
    std::shared_ptr<IMsgReceiver<std::string>> m_receiver;
    common::SentinelSerialBuffer m_serialBuffer;
    // Possible to receive a TCP data payload with multiple messages, so need to buffer the subsequent messages
    std::queue<data::AddressedAttributedMessage> m_receivedMsgs;
};

}
}

#endif