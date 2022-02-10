// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

#ifndef COMMUNICATIONS_ZMQ_SOCKET_BASE_H
#define COMMUNICATIONS_ZMQ_SOCKET_BASE_H

#include "ISocket.h"
#include "zmq.hpp"

#include <memory>

namespace uxas {
namespace communications {

// This class allows for different socket types to be created via constructor arguments.  For
// additional constructor/destructor setup should extend this class.

class ZmqSocketBase : public ISocket<const std::string&, bool> {
protected:
    // Initializer provides uncoupled method of instantiating socket.
    typedef std::shared_ptr<ISocket<std::shared_ptr<zmq::socket_t>&, const std::string&, int32_t, bool>>
        InitializerPtr;  

public:
    ZmqSocketBase() = default;
    ZmqSocketBase(InitializerPtr initializer, zmq::socket_type socketType);

    virtual ~ZmqSocketBase() override;

    // Initialize the socket
    virtual bool initialize(const std::string& address, bool isServer) override;

    // Get pointer to the socket
    std::shared_ptr<zmq::socket_t> getRawZmqSocket();

    // Return server status of this socket
    bool isServer() const;

    const std::vector<uint8_t>& getRoutingId() const;

protected: 
    bool m_isServer{false};
    std::shared_ptr<zmq::socket_t> m_socket{nullptr};
    InitializerPtr m_initializer;
    int32_t m_socketType;
    std::vector<uint8_t> m_routingId;
};

}
}

#endif