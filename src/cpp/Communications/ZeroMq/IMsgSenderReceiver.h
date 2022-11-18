// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

#ifndef COMMUNICATIONS_I_MSG_SENDER_RECEIVER_H
#define COMMUNICATIONS_I_MSG_SENDER_RECEIVER_H

#include "ISocket.h"
#include "IMsgSender.h"
#include "IMsgReceiver.h"

namespace uxas {
namespace communications {

template<typename Msg, typename... Ts>
class IMsgSenderReceiver : public IMsgSender<Msg&>, public IMsgReceiver<Msg>, public ISocket<Ts...> {};

}
}

#endif