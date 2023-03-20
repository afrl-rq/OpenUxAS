// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

#ifndef COMMUNICATIONS_THREAD_RUNNER_BASE_H
#define COMMUNICATIONS_THREAD_RUNNER_BASE_H

#include "IThreadExecutor.h"
#include "stdUniquePtr.h"

#include <thread>

namespace uxas {
namespace communications {

/**
 * @brief This abstract class provides basic management of a thread execution.  Deriving from
 *        templated interface IThreadExecutor provides specification for method to execute on thread execution.
 */

class ThreadRunnerBase : public IThreadExecutor<> {
public:
    /**
     * @brief Default construction of a new ThreadRunnerBase object
     */
    ThreadRunnerBase();

    /**
     * @brief Destroy the ThreadRunnerBase object and appropriately shutdown the thread
     */
    virtual ~ThreadRunnerBase() override;

    /**
     * @brief Start thread execution.
     * @return true if thread was successfully created
     */
    bool run();

protected:
    bool m_shutdown;  // Derived classes should use this variable to identify when to shutdown the thread processing.

private:
    std::unique_ptr<std::thread> m_thread;
};

}
}

#endif