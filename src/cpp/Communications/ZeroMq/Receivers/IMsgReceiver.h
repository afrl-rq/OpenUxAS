// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

#ifndef COMMUNICATION_I_MSG_RECEIVER_H
#define COMMUNICATION_I_MSG_RECEIVER_H

namespace uxas {
namespace communications {

/**
 * @brief This class provides a templated interface for and class that intends to implement
 * a receive() method
 * 
 * @tparam Msg - type of message intended to be received and returned by the virtual method.
 */

template<typename Msg>
class IMsgReceiver {
public:
    /**
     * @brief Default destructor
     * 
     */
    virtual ~IMsgReceiver() = default;

    /**
     * @brief Interface method for receiving a message.
     * 
     * @return Msg - type of message to be received.
     */
    virtual Msg receive() = 0;
};

}
}

#endif