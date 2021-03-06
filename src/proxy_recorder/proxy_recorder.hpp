/*
   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

   Product name: redemption, a FLOSS RDP proxy
   Copyright (C) Wallix 2018
   Author(s): David Fort

   A proxy that will capture all the traffic to the target
*/

#include "proxy_recorder/nla_tee_transport.hpp"
#include "proxy_recorder/nego_client.hpp"
#include "proxy_recorder/nego_server.hpp"

#include "core/RDP/nla/nla.hpp"
#include "core/RDP/gcc.hpp"
#include "core/RDP/mcs.hpp"
#include "core/RDP/tpdu_buffer.hpp"
#include "core/listen.hpp"
#include "core/server_notifier_api.hpp"
#include "transport/recorder_transport.hpp"
#include "transport/socket_transport.hpp"
#include "utils/cli.hpp"
#include "utils/fixed_random.hpp"
#include "utils/netutils.hpp"
#include "utils/redemption_info_version.hpp"
#include "utils/utf.hpp"

#include <vector>
#include <chrono>
#include <iostream>

#include <cerrno>
#include <cstring>
#include <csignal>

#include <netinet/tcp.h>
#include <sys/select.h>
#include <openssl/ssl.h>

/** @brief a front connection with a RDP client */
class ProxyRecorder
{
    using PacketType = RecorderFile::PacketType;
public:
    ProxyRecorder(unique_fd sck, std::string host, int port, std::string const& captureFile,
        TimeObj& timeobj,std::string nla_username, std::string nla_password, bool enable_kerberos,
        uint64_t verbosity
    )
        : frontConn("front", std::move(sck), "127.0.0.1", 3389, std::chrono::milliseconds(100), to_verbose_flags(verbosity))
        , backConn("back", ip_connect(host.c_str(), port),
            host.c_str(), port, std::chrono::milliseconds(100), to_verbose_flags(verbosity))
        , outFile(timeobj, captureFile.c_str())
        , host(std::move(host))
        , nla_username(std::move(nla_username))
        , nla_password(std::move(nla_password))
        , enable_kerberos(enable_kerberos)
        , verbosity(verbosity)
    {
        if (!this->nla_password.empty()) {
            this->nla_password.push_back('\0');
        }
    }

    void front_step1();
    void front_nla();
    void front_initial_pdu_negociation();
    void back_nla_negociation();
    void back_initial_pdu_negociation();
    void run();

private:
    uint8_t select_client_protocol() const
    {
        return is_nla_client ? X224::PROTOCOL_HYBRID :
               is_tls_client ? X224::PROTOCOL_TLS
                             : X224::PROTOCOL_RDP;
    }

    enum {
        NEGOCIATING_FRONT_STEP1,
        NEGOCIATING_FRONT_NLA,
        NEGOCIATING_BACK_NLA,
        NEGOCIATING_FRONT_INITIAL_PDU,
        NEGOCIATING_BACK_INITIAL_PDU,
        FORWARD
    } pstate = NEGOCIATING_FRONT_STEP1;

    struct TraceTransport final : SocketTransport
    {
        using SocketTransport::SocketTransport;

        Transport::Read do_atomic_read(uint8_t * buffer, std::size_t len) override
        {
            LOG_IF(enable_trace, LOG_DEBUG, "%s do_atomic_read", name);
            return SocketTransport::do_atomic_read(buffer, len);
        }

        std::size_t do_partial_read(uint8_t * buffer, std::size_t len) override
        {
            LOG_IF(enable_trace, LOG_DEBUG, "%s do_partial_read", name);
            return SocketTransport::do_partial_read(buffer, len);
        }

        void do_send(const uint8_t * buffer, std::size_t len) override
        {
            LOG_IF(enable_trace, LOG_DEBUG, "%s do_send", name);
            SocketTransport::do_send(buffer, len);
        }

        bool enable_trace = false;
    };

    TraceTransport frontConn;
    TraceTransport backConn;

    RecorderFile outFile;

    TpduBuffer frontBuffer;
    TpduBuffer backBuffer;

    std::unique_ptr<NegoClient> nego_client;
    std::unique_ptr<NegoServer> nego_server;

    std::string host;
    std::string nla_username;
    std::string nla_password;
    bool enable_kerberos;
    bool is_tls_client = false;
    bool is_nla_client = false;
    uint64_t verbosity;
};

