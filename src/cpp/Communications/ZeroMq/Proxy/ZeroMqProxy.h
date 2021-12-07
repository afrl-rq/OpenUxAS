// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

#ifndef UXAS_MESSAGE_TRANSPORT_ZEROMQ_PROXY_H
#define UXAS_MESSAGE_TRANSPORT_ZEROMQ_PROXY_H

#include "ThreadRunnerBase.h"
#include "IMsgReceiver.h"
#include "IMsgSender.h"

namespace uxas {
namespace communications {
namespace transport {

class ZeroMqProxy : public ThreadRunnerBase {
private:
    typedef std::shared_ptr<ZeroMqReceiver<std::string>> ReceiverPtr;
    typedef std::shared_ptr<ZeroMqSender<std::string&>> SenderPtr;

public:
    ZeroMqProxy( ReceiverPtr inRecv, SenderPtr outSend, ReceiverPtr outRecv, SenderPtr inSend )
    : m_internalReceiver(std::move(inRecv)), m_externalSender(std::move(outSend)),
    m_externalReceiver(std::move(outRecv)), m_internalSender(std::move(inSend))
    {}

    ~ZeroMqProxy() override = default;

protected:
    // Implement TCP proxy execution thread
    void executeOnThread() override {
        std::vector<zmq_pollitem_t> pollItems;
        pollItems.push_back( {m_internalReceiver->getSocket()->getSocket().get(), 0, ZMQ_POLLIN, 0} );
        pollItems.push_back( {m_externalReceiver->getSocket()->getSocket().get(), 0, ZMQ_POLLIN, 0} );

        while (!m_shutdown) {
            // blocking call for receiving data!
            zmq::poll(pollItems);
            if (pollItems[0].revents & ZMQ_POLLIN) {
                std::string msg = m_internalReceiver->receive();
                m_externalSender->send(msg);
            }

            if (pollItems[1].revents & ZMQ_POLLIN) {
                std::string msg = m_externalReceiver->receive();
                m_internalSender->send(msg);
            }
        }
    }

private:
    ReceiverPtr m_internalReceiver;  // Socket for (internal UxAS) -> (Proxy)
    SenderPtr m_externalSender;  // Socket for (Proxy) -> (external app)
    ReceiverPtr m_externalReceiver;  // Socket for (external app) -> (Proxy)
    SenderPtr m_internalSender;  // Socket for (Proxy) -> (internal UxAS)
};

}
}
}

#endif