/*
*   This program is free software; you can redistribute it and/or modify
*   it under the terms of the GNU General Public License as published by
*   the Free Software Foundation; either version 2 of the License, or
*   (at your option) any later version.
*
*   This program is distributed in the hope that it will be useful,
*   but WITHOUT ANY WARRANTY; without even the implied warranty of
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*   GNU General Public License for more details.
*
*   You should have received a copy of the GNU General Public License
*   along with this program; if not, write to the Free Software
*   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*
*   Product name: redemption, a FLOSS RDP proxy
*   Copyright (C) Wallix 2010-2016
*   Author(s): Jonathan Poelen
*/

#pragma once

#include "proto/buffering2_policy.hpp"
#include "proto/buffering3_policy.hpp"
#include <cassert>

namespace detail
{
    template<class Transport>
    struct proto_transport_policy : buffering2_policy_base
    {
        void send(iovec_array iovs)
        {
            unsigned char tmpbuf[1024*64];
            unsigned char * p = tmpbuf;
            for (auto iov : iovs) {
                assert(tmpbuf + sizeof(tmpbuf) - p >= iov.iov_len);
                memcpy(p, iov.iov_base, iov.iov_len);
                p += iov.iov_len;
            }
            // TODO this->trans.write(iovs);
            this->trans.send(tmpbuf, p - tmpbuf);
        }

        proto_transport_policy(Transport & trans) : trans(trans) {}
        Transport & trans;
    };

    template<class Stream>
    struct proto_stream_policy : buffering2_policy_base
    {
        void send(array_view_u8 av)
        {
            stream = Stream(stream.get_data(), stream.get_offset() + av.size());
        }

        proto_stream_policy(Stream & stream) : stream(stream) {}
        Stream & stream;
    };
}

template<class Transport, class... Pkts>
void write_in_transport(Transport & trans, Pkts const & ... pkts)
{
    proto::apply(
        Buffering2<detail::proto_transport_policy<Transport>>{trans},
        pkts...
    );
}


template<class Stream, class... Pkts>
void write_in_stream(Stream & stream, Pkts const & ... pkts)
{
    proto::apply(
        Buffering3<detail::proto_stream_policy<Stream>>{
            stream,
            {stream.get_current(), stream.tailroom()}
        },
        pkts...
    );
}
