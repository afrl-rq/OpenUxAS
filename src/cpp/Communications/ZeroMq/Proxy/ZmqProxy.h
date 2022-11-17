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

/**
 * @brief This class provides a ZeroMQ basic proxy setup with configurable internal/external
 * sender and receiver types that are specified via construction.  sender/receiver/socket types
 * should all be ZeroMQ based.
 */

class ZmqProxy : public ThreadRunnerBase {
private:
    typedef std::shared_ptr<IMsgReceiver<std::string>> ReceiverPtr;
    typedef std::shared_ptr<IMsgSender<std::string&>> SenderPtr;
    typedef std::shared_ptr<ZmqSocketBase> SocketPtr;
    typedef std::pair<SenderPtr, SocketPtr> SenderType;
    typedef std::pair<ReceiverPtr, SocketPtr> ReceiverType;

public:
    /**
     * @brief Construct a new ZmqProxy object
     * @param inRecv - receiver/socket pair for internal facing proxy communication
     * @param outSend  - sender/socket pair for external facing proxy communication 
     * @param outRecv - receiver/socket pair for external facing proxy communication
     * @param inSend  - sender/socket pair for internal facing proxy communcation
     */
    ZmqProxy( ReceiverType inRecv, SenderType outSend, ReceiverType outRecv, SenderType inSend );

    /**
     * @brief Default destructor for the Zmq Proxy object
     */
    ~ZmqProxy() override = default;

protected:
    /**
     * @brief Method for separate thread execution from proxy processing.  Should use 'm_shutdown' variable
     * from base class to determine when processing should end.
     */
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