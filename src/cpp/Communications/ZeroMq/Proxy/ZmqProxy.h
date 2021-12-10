// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

#ifndef COMMUNICATIONS_ZMQ_PROXY_H
#define COMMUNICATIONS_ZMQ_PROXY_H

#include "ThreadRunnerBase.h"
#include "ZmqReceiver.h"
#include "ZmqSender.h"

namespace uxas {
namespace communications {

class ZmqProxy : public ThreadRunnerBase {
private:
    typedef std::shared_ptr<ZmqReceiver<std::string>> ReceiverPtr;
    typedef std::shared_ptr<ZmqSender<std::string&>> SenderPtr;

public:
    ZmqProxy( ReceiverPtr inRecv, SenderPtr outSend, ReceiverPtr outRecv, SenderPtr inSend );
    ~ZmqProxy() override = default;

protected:
    // Implement TCP proxy execution thread
    void executeOnThread() override;

private:
    ReceiverPtr m_internalReceiver;  // Socket for (internal UxAS) -> (Proxy)
    SenderPtr m_externalSender;  // Socket for (Proxy) -> (external app)
    ReceiverPtr m_externalReceiver;  // Socket for (external app) -> (Proxy)
    SenderPtr m_internalSender;  // Socket for (Proxy) -> (internal UxAS)
};

}
}

#endif