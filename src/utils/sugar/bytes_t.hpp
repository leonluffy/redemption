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

#include "utils/sugar/array_view.hpp"


struct bytes_t
{
    constexpr bytes_t() = default;

    constexpr bytes_t(char * data) noexcept
    : data_(data)
    {}

    constexpr bytes_t(uint8_t * data) noexcept
    : data_(data)
    {}

    template<class T, class = std::enable_if_t<std::is_void<T>::value>>
    constexpr bytes_t(T * data) noexcept
    : data_(data)
    {}

    constexpr char * to_charp() const noexcept { return static_cast<char *>(this->data_); }
    constexpr uint8_t * to_u8p() const noexcept { return static_cast<uint8_t *>(this->data_); }

    constexpr operator char * () const noexcept { return this->to_charp(); }
    constexpr operator uint8_t * () const noexcept { return this->to_u8p(); }

    constexpr explicit operator bool () const noexcept { return this->data_; }

private:
    void * data_ = nullptr;
};

struct const_bytes_t
{
    constexpr const_bytes_t() = default;

    constexpr const_bytes_t(char const * data) noexcept
    : data_(data)
    {}

    constexpr const_bytes_t(uint8_t const * data) noexcept
    : data_(data)
    {}

    constexpr const_bytes_t(bytes_t b) noexcept
    : data_(b.to_u8p())
    {}

    template<class T, class = std::enable_if_t<std::is_void<T>::value>>
    constexpr const_bytes_t(T const * data) noexcept
    : data_(data)
    {}

    constexpr char const * to_charp() const noexcept { return static_cast<char const *>(this->data_); }
    constexpr uint8_t const * to_u8p() const noexcept { return static_cast<uint8_t const *>(this->data_); }

    constexpr operator char const * () const noexcept { return this->to_charp(); }
    constexpr operator uint8_t const * () const noexcept { return this->to_u8p(); }

    constexpr explicit operator bool () const noexcept { return this->data_; }

private:
    void const * data_ = nullptr;
};


struct bytes_array : array_view<uint8_t>
{
    constexpr bytes_array() = default;
    constexpr bytes_array(bytes_array &&) = default;
    constexpr bytes_array(bytes_array const &) = default;

    using array_view<uint8_t>::array_view;

    constexpr bytes_array(array_view<uint8_t> av) noexcept
    : array_view<uint8_t>(av.data(), av.size())
    {}

    constexpr bytes_array(array_view<char> av) noexcept
    : array_view<uint8_t>(bytes_t{av.data()}.to_u8p(), av.size())
    {}

    constexpr bytes_array(array_view<void> av) noexcept
    : array_view<uint8_t>(bytes_t{av.data()}.to_u8p(), av.size())
    {}

    template<class T>
    constexpr bytes_array(T && a) noexcept
    : bytes_array(1, a)
    {}

private:
    constexpr bytes_array(int, array_view<char> av) noexcept
    : bytes_array(av)
    {}

    constexpr bytes_array(int, array_view<uint8_t> av) noexcept
    : bytes_array(av)
    {}

    constexpr bytes_array(int, array_view<void> av) noexcept
    : bytes_array(av)
    {}

public:
    bytes_array & operator = (bytes_array &&) noexcept = default;
    bytes_array & operator = (bytes_array const &) noexcept = default;

    template<class T, class = decltype(array_view<uint8_t>(std::declval<T>()))>
    bytes_array & operator = (T && a) noexcept
    { return *this = bytes_array(a); }
};

struct const_bytes_array : array_view<const uint8_t>
{
    constexpr const_bytes_array() = default;
    constexpr const_bytes_array(const_bytes_array &&) = default;
    constexpr const_bytes_array(const_bytes_array const &) = default;

    using array_view<const uint8_t>::array_view;

    constexpr const_bytes_array(array_view<const uint8_t> av) noexcept
    : array_view<const uint8_t>(av.data(), av.size())
    {}

    constexpr const_bytes_array(array_view<const char> av) noexcept
    : array_view<const uint8_t>(const_bytes_t{av.data()}.to_u8p(), av.size())
    {}

    constexpr const_bytes_array(array_view<const void> av) noexcept
    : array_view<const uint8_t>(const_bytes_t{av.data()}.to_u8p(), av.size())
    {}

    template<class T>
    constexpr const_bytes_array(T && a) noexcept
    : const_bytes_array(1, a)
    {}

private:
    constexpr const_bytes_array(int, array_view<const char> av) noexcept
    : const_bytes_array(av)
    {}

    constexpr const_bytes_array(int, array_view<const uint8_t> av) noexcept
    : const_bytes_array(av)
    {}

    constexpr const_bytes_array(int, array_view<const void> av) noexcept
    : const_bytes_array(av)
    {}

public:
    const_bytes_array & operator = (const_bytes_array &&) noexcept = default;
    const_bytes_array & operator = (const_bytes_array const &) noexcept = default;

    template<class T, class = decltype(array_view<const uint8_t>(std::declval<T>()))>
    const_bytes_array & operator = (T && a) noexcept
    { return *this = const_bytes_array(a); }
};
