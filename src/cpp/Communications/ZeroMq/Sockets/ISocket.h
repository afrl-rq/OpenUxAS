// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

#ifndef COMMUNICATIONS_I_SOCKET_H
#define COMMUNICATIONS_I_SOCKET_H

namespace uxas {
namespace communications {

/**
 * @brief This class provides a templated interface for any socket type class that needs initialization.
 * 
 * @tparam Ts - Variadic template parameters to allow for different interface specifications.
 */

template<typename... Ts>
class ISocket {
public:
    /**
     * @brief Default destructor
     */
    virtual ~ISocket() = default;

    /**
     * @brief Interface specification for an initialize() method with variadic template spec
     * 
     * @return return if the socket was initialized appropriately.
     */
    virtual bool initialize(Ts...) = 0;
};

}
}

#endif