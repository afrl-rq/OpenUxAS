// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

#ifndef UXAS_MSG_SENDER_SENTINEL_H
#define UXAS_MSG_SENDER_SENTINEL_H

#include "IMsgSender.h"
#include "UxAS_SentinelSerialBuffer.h"

namespace uxas {
namespace communications {
namespace transport {

/**
 * @brief Class that decorates an IMsgSender<std::string> type with sentinels
 */

class MsgSenderSentinel : public IMsgSender<std::string&> {
public:
    MsgSenderSentinel(std::unique_ptr<IMsgSender<std::string&>> sender) 
    : m_sender{std::move(sender)} {}

    virtual ~MsgSenderSentinel() override = default;

    // Send message 
    void send(std::string& msg) {
        msg = common::SentinelSerialBuffer::createSentinelizedString( msg );
        m_sender->send(msg);
    }

private:
    std::unique_ptr<IMsgSender<std::string&>> m_sender;
};

}
}
}

#endif