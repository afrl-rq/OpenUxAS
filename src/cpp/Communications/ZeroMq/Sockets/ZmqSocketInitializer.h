// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

#ifndef COMMUNICATIONS_ZMQ_SOCKET_INITIALIZER_H
#define COMMUNICATIONS_ZMQ_SOCKET_INITIALIZER_H

#include "ISocket.h"
#include "UxAS_Log.h"

#include "zmq.hpp"

#include <memory>

namespace uxas {
namespace communications {

// This class provides a means of creating the ZeroMq socket! 
/**
 * @brief This class provides a de-coupled means of creating a ZeroMq socket.  The creation and 
 *        initialization of ZeroMq sockets require a shared "context" which is currently accomplished
 *        in UxAS via a Singleton class.  This class was created with the objective of refactoring the
 *        removal of the Singleton in the future.
 */

class ZmqSocketInitializer : public ISocket<std::shared_ptr<zmq::socket_t>&, const std::string&, int32_t, bool> {
public:
    /**
     * @brief Default destructor
     */
    ~ZmqSocketInitializer() override = default;

    /**
     * @brief Method to initialize a given socket based on given parameters.
     * 
     * @param socketPtr - Pointer to uninitialized RAW Zmq socket.
     * @param address - Address this socket will connect to.
     * @param type - Type of the socket (e.g. ZMQ_PUSH, ZMQ_PULL, ZMQ_STREAM, etc.)
     * @param isServer - Boolean indicating if this socket is a server.
     * @return Return true if initialized appropriately, false otherwise.
     */
    bool initialize(std::shared_ptr<zmq::socket_t>& socketPtr, 
        const std::string& address, int32_t type, bool isServer) override;
};

}
}

#endif