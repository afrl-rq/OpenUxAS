// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

#ifndef COMMUNICATIONS_SET_ARRAY_CLIENT_H
#define COMMUNICATIONS_SET_ARRAY_CLIENT_H

#include "IClientList.h"
#include <array>
#include <mutex>

namespace uxas {
namespace communications {

class SetArrayClientList : public IClientList<std::array<uint8_t,256>> {
public:
    typedef std::array<uint8_t,256> Client;
    typedef std::set<Client> CList;

    // Get client list
    const CList& getClients() const override { return m_clients; };

    // Add to client list
    bool addClient(Client c) override {
        return m_clients.emplace(c).second;
    };

    // Remove a client from list
    bool removeClient(Client c) override {
        return m_clients.erase(c) > 0 ? true : false;
    };

private:
    CList m_clients;
};

}
}

#endif