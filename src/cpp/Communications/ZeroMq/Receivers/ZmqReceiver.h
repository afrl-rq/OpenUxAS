// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

#ifndef COMMUNICATIONS_ZMQ_RECEIVER_BASE_H
#define COMMUNICATIONS_ZMQ_RECEIVER_BASE_H

#include "IMsgReceiver.h"
#include "ZmqSocketBase.h"

#include <memory>

namespace uxas {
namespace communications {

// This abstract class adds data members to the pure interface base class
/**
 * @brief Abstract base class for all Zmq receivers which adds needed data members to the pure interface IMsgReceiver.
 * 
 * @tparam Msg - message type to return on receive() call of the interface.
 */

template<typename Msg>
class ZmqReceiver : public IMsgReceiver<Msg> {
public:
    /**
     * @brief Default constructor set socket to null pointer, requiring user to set after construction.
     */
    ZmqReceiver() : m_socket{nullptr} {}

    /**
     * @brief Construct a new Zmq Receiver object passing underlying Zmq socket as a parameter.
     * 
     * @param socket - pointer to the underlying Zmq socket for receiving data.
     */
    ZmqReceiver(std::shared_ptr<ZmqSocketBase> socket) : m_socket{socket} {}

    /**
     * @brief Default destructor
     */
    virtual ~ZmqReceiver() = default;

    /**
     * @brief Get a pointer to the underlying Zmq socket base class (this is NOT the raw Zmq socket)
     * 
     * @return std::shared_ptr<ZmqSocketBase> 
     */
    virtual std::shared_ptr<ZmqSocketBase> getSocketBase() { return m_socket; }

    /**
     * @brief Set socket based on existing Zmq socket base pointer.
     * 
     * @param socket 
     */
    void setSocket(std::shared_ptr<ZmqSocketBase> socket) { m_socket = std::move(socket); } 

protected:
    std::shared_ptr<ZmqSocketBase> m_socket;
};

}
}

#endif