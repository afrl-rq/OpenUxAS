// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

#ifndef COMMUNICATIONS_MSG_SENDER_SENTINEL_H
#define COMMUNICATIONS_MSG_SENDER_SENTINEL_H

#include "IMsgSender.h"

#include <string>
#include <memory>

namespace uxas {
namespace communications {

/**
 * @brief Class that decorates an IMsgSender<std::string> type with LMCP message Sentinels
 * that are used within UxAS.
 */

class MsgSenderSentinel : public IMsgSender<std::string&> {
public:
    /**
     * @brief Constructor with decorated object passed as a parameter
     * 
     * @param sender - Sender object that will be called after send() method of this class.
     */
    explicit MsgSenderSentinel(std::shared_ptr<IMsgSender<std::string&>> sender);

    /**
     * @brief Default destructor
     * 
     */
    virtual ~MsgSenderSentinel() override = default;

    /**
     * @brief Modify message string by adding sentinels and then forwarding on to the 
     * owned object for sending.
     * 
     * @param msg - message to be "sentinelized".  This is intended to be an LMCP message
     * that has not been sentinelized yet for sending over a TCP stream.
     */
    void send(std::string& msg);

private:
    std::shared_ptr<IMsgSender<std::string&>> m_sender;
};

}
}

#endif