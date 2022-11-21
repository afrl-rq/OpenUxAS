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
 * @brief Class that decorates an IMsgReceiver<std::string> type.  This class is intended to
 * received a "sentinelized" LMCP message (from TCP stream) and will extract the payload.
 */

// class MsgReceiverSentinel : public IMsgReceiver<data::AddressedAttributedMessage> {
class MsgReceiverSentinel : public IMsgReceiver<std::string> {
public:
    /**
     * @brief Constructor with decorated object passed as a parameter.
     * 
     * @param receiver - Receiver object that will be called before message processing logic of this class. 
     */
    MsgReceiverSentinel(std::shared_ptr<IMsgReceiver<std::string>> receiver);

    /**
     * @brief Default destructor
     */
    virtual ~MsgReceiverSentinel() override = default;

    /**
     * @brief Retrieve message from owned object which is then "de-sentinelized" and converted to
     * an AddressedAttributedMessage and then returned.
     * 
     * @return data::AddressedAttributedMessage 
     */
    // data::AddressedAttributedMessage receive() override;
    std::string receive() override;

private:
    std::shared_ptr<IMsgReceiver<std::string>> m_receiver;
    common::SentinelSerialBuffer m_serialBuffer;
    // Possible to receive a TCP data payload with multiple messages, so need to buffer the subsequent messages
    // std::queue<data::AddressedAttributedMessage> m_receivedMsgs;
    std::queue<std::string> m_receivedMsgs;
};

}
}

#endif