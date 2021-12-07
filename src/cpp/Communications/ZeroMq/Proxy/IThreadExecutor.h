// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

#ifndef UXAS_I_THREAD_EXECUTOR_H
#define UXAS_I_THREAD_EXECUTOR_H

namespace uxas {
namespace communications {
namespace transport {

template<typename... Ts>
class IThreadExecutor {
public:
    virtual ~IThreadExecutor() = default;

    // Receive next message 
    virtual void executeOnThread(Ts...) = 0;
};

}
}
}

#endif