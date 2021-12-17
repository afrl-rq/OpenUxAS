// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

#ifndef COMMUNICATIONS_ZMQ_TCP_RECEIVER_H
#define COMMUNICATIONS_ZMQ_TCP_RECEIVER_H

#include "ZmqReceiver.h"
#include "ZmqTcpSocket.h"
#include "IClientList.h"
#include "UxAS_Log.h"

#include <string>
#include <set>

namespace uxas {
namespace communications {

class ZmqTcpReceiver : public ZmqReceiver<std::string> {
public:
    ZmqTcpReceiver(std::shared_ptr<ZmqTcpSocket> socket,
        std::shared_ptr<IClientList<std::vector<uint8_t>>> clients);

    ~ZmqTcpReceiver() override = default;

    std::string receive() override;

private:
    std::shared_ptr<IClientList<std::vector<uint8_t>>> m_clients;  // List of client IDs received over socket.
};

}
}

#endif