// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

#ifndef COMMUNICATIONS_I_THREAD_EXECUTOR_H
#define COMMUNICATIONS_I_THREAD_EXECUTOR_H

namespace uxas {
namespace communications {

/**
 * @brief This class provides a templated interface for a thread execution function.
 * 
 * @tparam Ts type parameter pack for interface specification
 */

template<typename... Ts>
class IThreadExecutor {
public:
    /**
     * @brief Default destructor
     */
    virtual ~IThreadExecutor() = default;

    /**
     * @brief Method to execute on a thread
     */
    virtual void executeOnThread(Ts...) = 0;
};

}
}

#endif