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

#include "UxAS_Log.h"

namespace uxas {
namespace communications {

/**
 * @brief Concrete implementation of IClientList.  Methods are NOT thread-safe so protect accordingly.
 */

class SetArrayClientList : public IClientList<std::vector<uint8_t>> {
public:
    typedef std::vector<uint8_t> Client;
    typedef std::set<Client> CList;

    // Get client list
    const CList& getClients() const override;

    // Add to client list
    bool addClient(Client c) override;

    // Remove a client from list
    bool removeClient(Client c) override;

private:
    CList m_clients;
};

}
}

#endif