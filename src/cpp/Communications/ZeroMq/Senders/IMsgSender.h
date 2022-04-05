// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

#ifndef COMMUNICATIONS_I_MSG_SENDER_H
#define COMMUNICATIONS_I_MSG_SENDER_H

namespace uxas {
namespace communications {

/**
 * @brief This class provides a templated interface for any class that intends to implement a send() method.
 * 
 * @tparam Msg - type of message intended to be sent
 */
template<typename Msg>
class IMsgSender {
public:
    /**
     * @brief Default destructor
     * 
     */
    virtual ~IMsgSender() = default;

    /**
     * @brief Interface method for sending a message (of type Msg)
     * 
     * @param msg - message to be send (of type Msg)
     */
    virtual void send(Msg msg) = 0;
};

}
}

#endif