// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

#include "SetArrayClientList.h"
#include "UxAS_Log.h"

namespace uxas {
namespace communications {

const SetArrayClientList::CList& SetArrayClientList::getClients() const {
    UXAS_LOG_DEBUG_VERBOSE(typeid(this).name(),"::",__func__,":TRACE");
    return m_clients;
}

bool SetArrayClientList::addClient(Client c) {
    UXAS_LOG_DEBUG_VERBOSE(typeid(this).name(),"::",__func__,":TRACE");
    return m_clients.emplace(c).second;
}

bool SetArrayClientList::removeClient(Client c) {
    UXAS_LOG_DEBUG_VERBOSE(typeid(this).name(),"::",__func__,":TRACE");
    return m_clients.erase(c) > 0 ? true : false;
}

}
}