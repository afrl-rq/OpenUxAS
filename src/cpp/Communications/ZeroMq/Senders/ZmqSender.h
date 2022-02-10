// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

#ifndef COMMUNICATIONS_ZMQ_SENDER_H
#define COMMUNICATIONS_ZMQ_SENDER_H

#include "IMsgSender.h"
#include "ZmqSocketBase.h"
#include "UxAS_Log.h"

#include <memory>

namespace uxas {
namespace communications {

/**
 * @brief This class provides a templated sender intended for sending over a ZeroMQ socket which is passed to
 *        the class constructor.  The owned socket object should be initialized before attempting to send().
 * 
 * @tparam Msg - type of message intended to be sent.
 */

template<typename Msg>
class ZmqSender : public IMsgSender<Msg> {
public:
    /**
     * @brief Default constructor for a new ZmqSender object
     */
    ZmqSender() : m_socket{nullptr} {}

    /**
     * @brief ZmqSender constructor passing in a pointer to intended socket which will be used to send data.
     * 
     * @param socket - Pointer to socket used for sending message.
     */
    explicit ZmqSender(std::shared_ptr<ZmqSocketBase> socket) : m_socket{socket} {}

    /**
     * @brief Default destructor
     */
    virtual ~ZmqSender() override = default;

    /**
     * @brief Send message on the socket.
     * 
     * @param msg - Message to be sent.  This type is expected to have begin() and end() methods that return iterators
     * to the underlying message container type.
     */
    virtual void send(Msg msg) override {
        if (m_socket && m_socket->getRawZmqSocket()) {
            m_socket->getRawZmqSocket()->send(msg.begin(), msg.end(), 0);
        }
    }

    /**
     * @brief Return pointer to the socket that is used by this sender
     * 
     * @return std::shared_ptr<ZmqSocketBase> 
     */
    virtual std::shared_ptr<ZmqSocketBase> getSocketBase() { return m_socket; }

    /**
     * @brief Set the socket for this sender.
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