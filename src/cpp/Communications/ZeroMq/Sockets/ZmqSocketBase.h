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
#include "UxAS_Log.h"

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
    ZmqSocketBase(InitializerPtr initializer, zmq::socket_type socketType) 
    : m_socketType{static_cast<int32_t>(socketType)}, m_initializer{initializer} {}

    virtual ~ZmqSocketBase() override {
        if (m_socket) {
            m_socket->setsockopt<uint32_t>(ZMQ_LINGER,0);
            m_socket->close();
        }
    };

    // Initialize the socket
    virtual bool initialize(const std::string& address, bool isServer) override {
        m_isServer = isServer;
        if (m_initializer->initialize(m_socket, address, m_socketType, m_isServer)) {
            uint8_t buffer[256];
            std::size_t bufferSize{256};
            m_socket->getsockopt(ZMQ_ROUTING_ID, buffer, &bufferSize);
            m_routingId = std::vector<uint8_t>{buffer, buffer + bufferSize};
            UXAS_LOG_WARN("*** CPW: ZmqSocketBase - Initialized socket with address: ", address, " ***");
            return true;
        } else {
            UXAS_LOG_WARN("*** CPW: ZmqSocketBase - Failed to initialize socket ***");
            return false;
        }
    }

    // Get pointer to the socket
    std::shared_ptr<zmq::socket_t> getSocket() { return m_socket; }

    // Return server status of this socket
    bool isServer() const { return m_isServer; }

    const std::vector<uint8_t>& getRoutingId() const { return m_routingId; }

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