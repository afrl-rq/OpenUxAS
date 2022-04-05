// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

#ifndef COMMUNICATIONS_ZMQ_TCP_SOCKET_H
#define COMMUNICATIONS_ZMQ_TCP_SOCKET_H

#include "ZmqSocketBase.h"

namespace uxas {
namespace communications {

// Add some additional destructor functionality from the base class
/**
 * @brief Socket class specific to ZMQ_STREAM socket type (TCP connection).  Provides additional
 *        destructor functionality that is required for ZMQ_STREAM type sockets.
 */

class ZmqTcpSocket : public ZmqSocketBase {
public:
    /**
     * @brief Constructor which specifies the socket type as ZMQ_STREAM
     * 
     * @param initializer 
     */
    ZmqTcpSocket(InitializerPtr initializer) : ZmqSocketBase{initializer, zmq::socket_type::stream} {}

    /**
     * @brief Desctructor which provides additional functionality required for closing of TCP socket.
     */
    ~ZmqTcpSocket() override;
};

}
}

#endif