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
 * @brief Implementation of IClientList with basic add/remove/get for clients.  
 * Methods are NOT thread-safe so protect accordingly.
 */

class SetArrayClientList : public IClientList<std::vector<uint8_t>> {
public:
    typedef std::vector<uint8_t> Client;
    // std::set is the default container type of the IClientList interface
    typedef std::set<Client> CList;

    /**
     * @brief Get a reference to the client list.  Must ensure that the lifetime of this object will exceed scope
     *        in which reference is used.
     * 
     * @return const CList& - client list reference object. 
     */
    const CList& getClients() const override;

    /**
     * @brief Add to the client list.
     * 
     * @param c - client to add.
     * @return boolean indicating if the client was added successfully.
     */
    bool addClient(Client c) override;

    /**
     * @brief Remove a client from the list
     * 
     * @param c - client to remove
     * @return boolean indicating if the client was removed successfully.
     */
    bool removeClient(Client c) override;

private:
    CList m_clients;
};

}
}

#endif