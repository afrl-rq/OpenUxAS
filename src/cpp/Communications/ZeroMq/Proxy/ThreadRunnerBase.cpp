// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

#include "ThreadRunnerBase.h"
#include "UxAS_Log.h"

namespace uxas {
namespace communications {

ThreadRunnerBase::ThreadRunnerBase() : m_shutdown{false} {}

ThreadRunnerBase::~ThreadRunnerBase() {
    UXAS_LOG_DEBUG_VERBOSE(typeid(this).name(),"::",__func__,":TRACE");
    if (m_thread && m_thread->joinable()) {
        m_shutdown = true;
        m_thread->join();
    }
}

bool ThreadRunnerBase::run() {
    UXAS_LOG_DEBUG_VERBOSE(typeid(this).name(),"::",__func__,":TRACE");
    m_thread = uxas::stduxas::make_unique<std::thread>(&IThreadExecutor<>::executeOnThread, this);
    return true;
}

}
}