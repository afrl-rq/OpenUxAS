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
#include "IMsgReceiver.h"
#include "IMsgSender.h"
#include "ZmqSocketBase.h"

namespace uxas {
namespace communications {

class ZmqProxy : public ThreadRunnerBase {
private:
    typedef std::shared_ptr<IMsgReceiver<std::string>> ReceiverPtr;
    typedef std::shared_ptr<IMsgSender<std::string&>> SenderPtr;
    typedef std::shared_ptr<ZmqSocketBase> SocketPtr;
    typedef std::pair<SenderPtr, SocketPtr> SenderType;
    typedef std::pair<ReceiverPtr, SocketPtr> ReceiverType;

public:
    ZmqProxy( ReceiverType inRecv, SenderType outSend, ReceiverType outRecv, SenderType inSend );
    ~ZmqProxy() override = default;

protected:
    // Implement TCP proxy execution thread
    void executeOnThread() override;

private:
    ReceiverType m_internalReceiver;  // Socket for (internal UxAS) -> (Proxy)
    SenderType m_externalSender;  // Socket for (Proxy) -> (external app)
    ReceiverType m_externalReceiver;  // Socket for (external app) -> (Proxy)
    SenderType m_internalSender;  // Socket for (Proxy) -> (internal UxAS)
};

}
}

#endif