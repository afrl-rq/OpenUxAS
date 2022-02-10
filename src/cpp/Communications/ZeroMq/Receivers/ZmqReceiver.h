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

template<typename Msg>
class ZmqReceiver : public IMsgReceiver<Msg> {
public:
    ZmqReceiver() : m_socket{nullptr} {}
    ZmqReceiver(std::shared_ptr<ZmqSocketBase> socket) : m_socket{socket} {}
    virtual ~ZmqReceiver() = default;

    virtual std::shared_ptr<ZmqSocketBase> getSocketBase() { return m_socket; }
    void setSocket(std::shared_ptr<ZmqSocketBase> socket) { m_socket = std::move(socket); } 

protected:
    std::shared_ptr<ZmqSocketBase> m_socket;
};

}
}

#endif