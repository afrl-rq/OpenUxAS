// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

#ifndef UXAS_I_CLIENT_SENDER_H
#define UXAS_I_CLIENT_SENDER_H

#include <memory>

namespace uxas {
namespace communications {
namespace transport {

// Set a pointer to a client list that will be managed elsewhere.  This is simply
// to provide storage of the client list.

template<typename L>
class IClientSender {
public:
    IClientSender(std::shared_ptr<L> clientList) : m_clientList{m_clientList} {}
    virtual ~IClientSender() = default;

    // Set clients from some list type (L)
    virtual void setClients(std::shared_ptr<L>) = 0;

protected:
    std::shared_ptr<L> m_clientList;
};

}
}
}

#endif