// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

#ifndef UXAS_MESSAGE_TRANSPORT_IZEROMQ_PROXY_H
#define UXAS_MESSAGE_TRANSPORT_IZEROMQ_PROXY_H

#include "IThreadExecutor.h"
#include "stdUniquePtr.h"

#include <thread>

namespace uxas {
namespace communications {
namespace transport {

class ThreadRunnerBase : public IThreadExecutor<> {
public:
    ThreadRunnerBase() : m_shutdown{false} {}

    // Shutdown proxy execution thread
    virtual ~ThreadRunnerBase() {
        if (m_thread && m_thread->joinable()) {
            m_shutdown = true;
            m_thread->join();
        }
    };

    // Execute a new thread 
    bool run() {
        m_thread = uxas::stduxas::make_unique<std::thread>(&executeOnThread, this);
    }

protected:
    bool m_shutdown;

private:
    std::unique_ptr<std::thread> m_thread;
};

}
}
}

#endif