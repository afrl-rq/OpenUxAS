// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

#ifndef COMMUNICATIONS_I_CLIENT_LIST_H
#define COMMUNICATIONS_I_CLIENT_LIST_H

#include <set>

namespace uxas {
namespace communications {

/**
 * @brief Maintain a list of clients of type T.  Can optinally specify container for clients.
 * 
 * @tparam T - Client type
 * @tparam Container - Client container type
 */

template<typename T, typename Container = std::set<T>>
class IClientList {
public:
    virtual ~IClientList() = default;

    // Get client list
    virtual const Container& getClients() const = 0;

    // Add to client list
    virtual bool addClient(T) = 0;

    // Remove a client from list
    virtual bool removeClient(T) = 0;
};

}
}

#endif