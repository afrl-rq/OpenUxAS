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

namespace uxas {
namespace communications {

/**
 * @brief Class that decorates an IMsgReceiver<std::string> type with sentinels
 */

class MsgSentinelReceive : public IMsgReceiver<std::string> {
public:
    MsgSentinelReceive(std::unique_ptr<IMsgReceiver<std::string>> receiver) 
    : m_receiver{std::move(receiver)} {}

    virtual ~MsgSentinelReceive() override = default;

    std::string receive() {
        return m_serialBuffer.getNextPayloadString(m_receiver->receive());
    }

private:
    std::unique_ptr<IMsgReceiver<std::string>> m_receiver;
    common::SentinelSerialBuffer m_serialBuffer;
};

}
}

#endif