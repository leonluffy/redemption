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

#include "proto/iovec.hpp"
#include "utils/log.hpp"

#include <memory>

// TODO PERF this implementation is very bad
inline void hexdump_d(iovec_array iovs, std::size_t n)
{
    auto arr = std::make_unique<uint8_t[]>(n);
    auto p = arr.get();
    for (auto const & iov : iovs) {
        assert(p - arr.get() + iov.iov_len <= n);
        memcpy(p, iov.iov_base, iov.iov_len);
        p += iov.iov_len;
    }
    hexdump_d(arr.get(), n);
}

inline void hexdump_d(iovec_array iovs)
{
    std::size_t n = 0;
    for (auto const & iov : iovs) {
        n += iov.iov_len;
    }
    hexdump_d(iovs, n);
}

template<class T>
inline void hexdump_d(array_view<T> av)
{
    hexdump_d(av.data(), av.size());
}
