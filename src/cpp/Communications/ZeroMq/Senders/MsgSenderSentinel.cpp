// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

#include "MsgSenderSentinel.h"
#include "UxAS_SentinelSerialBuffer.h"

#include "UxAS_Log.h"

namespace uxas {
namespace communications {

MsgSenderSentinel::MsgSenderSentinel(std::shared_ptr<IMsgSender<std::string&>> sender) 
    : m_sender{std::move(sender)} 
    {}

void MsgSenderSentinel::send(std::string& msg) {
    UXAS_LOG_DEBUG_VERBOSE(typeid(this).name(),"::",__func__,":TRACE");
    if (!m_sender) {
        UXAS_LOG_ERROR(typeid(this).name(),"::",__func__," - Invalid sender pointer");
        return;
    }
    //TODO - CPW: The class that adds the sentinels could use some refactoring.  It has
    //       a lot of different responsibilities.
    std::string msgToSend = common::SentinelSerialBuffer::createSentinelizedString( msg );
    m_sender->send(msgToSend);
}

}
}