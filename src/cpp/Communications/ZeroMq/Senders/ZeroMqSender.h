// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

#ifndef UXAS_ZERO_MQ_SENDER_BASE_H
#define UXAS_ZERO_MQ_SENDER_BASE_H

#include "IMsgSender.h"
#include "ZeroMqSocketBase.h"
#include <memory>

namespace uxas {
namespace communications {
namespace transport {

// This class adds data members to the pure interface base class

template<typename Msg>
class ZeroMqSender : public IMsgSender<Msg> {
public:
    ZeroMqSender(std::shared_ptr<ZeroMqSocketBase> socket) : m_socket{socket} {}
    virtual ~ZeroMqSender() override = default;

    // Send messages on the socket
    void send(Msg msg) override {
        if (m_socket && m_socket->getSocket()) {
            m_socket->getSocket()->send(msg.begin(), msg.end(), 0);
        }
    }

    std::shared_ptr<ZeroMqSocketBase> getSocket() { return m_socket; }

protected:
    std::shared_ptr<ZeroMqSocketBase> m_socket;
};

}
}
}

#endif