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

/**
 * @brief - This class allows for different socket types to be created via constructor arguments.  For 
 *          additional constructor/destructor setup, this class should be extended.
 */

class ZmqSocketBase : public ISocket<const std::string&, bool> {
protected:
    // Initializer provides uncoupled method of instantiating socket.
    typedef std::shared_ptr<ISocket<std::shared_ptr<zmq::socket_t>&, const std::string&, int32_t, bool>>
        InitializerPtr;  

public:
    /**
     * @brief Default constructor
     */
    ZmqSocketBase() = default;

    /**
     * @brief Construct a Zmq socket object from an initializer and type of socket.
     * 
     * @param initializer - pointer to a decoupled method of socket instantiation.
     * @param socketType - requested Zmq socket type to instantiate.
     */
    ZmqSocketBase(InitializerPtr initializer, zmq::socket_type socketType);

    /**
     * @brief Destructor with basic functionality to close socket upon object destruction. 
     */
    virtual ~ZmqSocketBase() override;

    // Initialize the socket
    /**
     * @brief Initialize the socket.  The separate functionality here allows for error handling outside
     *        of object instantiation and for passing address after config file has been read.
     * 
     * @param address - Zmq address for socket to connect.
     * @param isServer - Boolean indicating if this Zmq socket is a server.
     * @return boolean indicating if socket was appropriately initialized.
     */
    virtual bool initialize(const std::string& address, bool isServer) override;

    /**
     * @brief Get shared pointer to the RAW Zmq socket.
     * 
     * @return std::shared_ptr<zmq::socket_t> 
     */
    std::shared_ptr<zmq::socket_t> getRawZmqSocket();

    /**
     * @brief Query on server status of this socket.
     * 
     * @return Return true if a server, false otherwise.
     */
    bool isServer() const;

    /**
     * @brief Get routing ID for this socket (used for some socket types that require routing)
     * 
     * @return const std::vector<uint8_t>& - ensure lifetime of this object exceeds that of caller scope.
     */
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