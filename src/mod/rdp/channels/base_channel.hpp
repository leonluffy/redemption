/*
    This program is free software; you can redistribute it and/or modify it
     under the terms of the GNU General Public License as published by the
     Free Software Foundation; either version 2 of the License, or (at your
     option) any later version.

    This program is distributed in the hope that it will be useful, but
     WITHOUT ANY WARRANTY; without even the implied warranty of
     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
     Public License for more details.

    You should have received a copy of the GNU General Public License along
     with this program; if not, write to the Free Software Foundation, Inc.,
     675 Mass Ave, Cambridge, MA 02139, USA.

    Product name: redemption, a FLOSS RDP proxy
    Copyright (C) Wallix 2015
    Author(s): Christophe Grosjean, Raphael Zhou
*/


#pragma once

#include "mod/rdp/channels/virtual_channel_data_sender.hpp"
#include "utils/asynchronous_task_manager.hpp"
#include "utils/uninit_checked.hpp"
#include "mod/rdp/rdp_verbose.hpp"
#include "core/report_message_api.hpp"

#include <memory>


using data_size_type = int_fast32_t;

class BaseVirtualChannel
{
private:
    VirtualChannelDataSender* to_client_sender;
    VirtualChannelDataSender* to_server_sender;

protected:
    VirtualChannelDataSender* to_client_sender_ptr() const noexcept
    {
        return this->to_client_sender;
    }

    VirtualChannelDataSender* to_server_sender_ptr() const noexcept
    {
        return this->to_server_sender;
    }

    ReportMessageApi & report_message;
    const RDPVerbose verbose;

public:
    // TODO: move that to BaseVirtualChannelConstructor
    struct Params
    {
        ReportMessageApi & report_message;
        RDPVerbose verbose;

        explicit Params(ReportMessageApi & report_message, RDPVerbose verbose)
          : report_message(report_message)
          , verbose(verbose)
        {}
    };

protected:
    BaseVirtualChannel(
        VirtualChannelDataSender* to_client_sender_,
        VirtualChannelDataSender* to_server_sender_,
        const Params & params)
    : to_client_sender(to_client_sender_)
    , to_server_sender(to_server_sender_)
    , report_message(params.report_message)
    , verbose(params.verbose)
    {}

public:
    virtual ~BaseVirtualChannel() = default;

    void disable_to_client_sender() {
        this->to_client_sender = nullptr;
    }

    virtual const char * get_reporting_reason_exchanged_data_limit_reached()
        const = 0;

protected:
    inline bool has_valid_to_client_sender() const
    {
        return bool(this->to_client_sender);
    }

public:
    virtual void process_client_message(uint32_t total_length,
        uint32_t flags, const uint8_t* chunk_data,
        uint32_t chunk_data_length
    ) {
        (void)total_length;
        (void)flags;
        (void)chunk_data;
        (void)chunk_data_length;
    }

    virtual void process_server_message(uint32_t total_length,
        uint32_t flags, const uint8_t* chunk_data,
        uint32_t chunk_data_length,
        std::unique_ptr<AsynchronousTask> & out_asynchronous_task) = 0;

public:
    virtual inline void send_message_to_client(uint32_t total_length,
        uint32_t flags, const uint8_t* chunk_data, uint32_t chunk_data_length)
    {
        if (this->to_client_sender)
        {
            (*this->to_client_sender)(total_length, flags, chunk_data,
                chunk_data_length);
        }
    }

    virtual inline void send_message_to_server(uint32_t total_length,
        uint32_t flags, const uint8_t* chunk_data, uint32_t chunk_data_length)
    {
        if (this->to_server_sender)
        {
            (*this->to_server_sender)(total_length, flags, chunk_data,
                chunk_data_length);
        }
    }

protected:
    inline void update_exchanged_data(uint32_t /*data_length*/)
    {}
};  // class BaseVirtualChannel

