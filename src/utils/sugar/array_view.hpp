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
*   Copyright (C) Wallix 2010-2015
*   Author(s): Jonathan Poelen
*/

#pragma once

#include <type_traits>

#include <cstdint>
#include <cassert>

template<class T>
struct array_view
{
    using value_type = T;

    using reference = value_type &;
    using const_reference = value_type const &;

    using iterator = value_type *;
    using const_iterator = value_type const *;

    using pointer = value_type *;
    using const_pointer = value_type const *;

    constexpr array_view() = default;
    constexpr array_view(array_view const &) = default;
    array_view & operator = (array_view const &) = default;

    constexpr array_view(std::nullptr_t) noexcept
    {}

    constexpr array_view(std::nullptr_t, std::nullptr_t) noexcept
    {}

    constexpr array_view(pointer p, std::size_t sz) noexcept
    : p(p)
    , sz(sz)
    {}

    constexpr array_view(pointer p, pointer pright) noexcept
    : p(p)
    , sz(pright - p)
    {}

    template<std::size_t N>
    constexpr array_view(value_type (&a)[N]) noexcept
    : array_view(a, N)
    {}

    template<class U, class = decltype(
        *static_cast<value_type**>(nullptr) = static_cast<U*>(nullptr)->data(),
        *static_cast<std::size_t*>(nullptr) = static_cast<U*>(nullptr)->size()
    )>
    constexpr array_view(U & x) noexcept
    : p(x.data())
    , sz(x.size())
    {}

    constexpr bool empty() const noexcept { return !this->sz; }

    constexpr std::size_t size() const noexcept { return this->sz; }

    constexpr pointer data() noexcept { return this->p; }
    constexpr const_pointer data() const noexcept { return this->p; }

    constexpr iterator begin() noexcept { return this->p; }
    constexpr iterator end() noexcept { return this->p + this->sz; }
    constexpr const_iterator begin() const noexcept { return this->p; }
    constexpr const_iterator end() const noexcept { return this->p + this->sz; }

    constexpr reference front() noexcept { assert(this->size()); return *this->p; }
    constexpr const_reference front() const noexcept { assert(this->size()); return *this->p; }

    constexpr reference back() noexcept { assert(this->size()); return *(this->p + this->sz - 1); }
    constexpr const_reference back() const noexcept { assert(this->size()); return *(this->p + this->sz - 1); }

    constexpr reference operator[](std::size_t i) noexcept { assert(i < this->size()); return this->p[i]; }
    constexpr const_reference operator[](std::size_t i) const noexcept { assert(i < this->size()); return this->p[i]; }

private:
    value_type * p = nullptr;
    std::size_t sz = 0;
};

template<>
struct array_view<void const>
{
    using value_type = void const;

    using const_iterator = uint8_t const *;
    using iterator = const_iterator;

    using pointer = value_type *;
    using const_pointer = value_type const *;

    constexpr array_view() = default;
    constexpr array_view(array_view const &) = default;
    array_view & operator = (array_view const &) = default;

    constexpr array_view(std::nullptr_t) noexcept
    {}

    constexpr array_view(std::nullptr_t, std::nullptr_t) noexcept
    {}

    template<class T, class = std::enable_if_t<std::is_void<std::remove_cv_t<T>>::value>>
    constexpr array_view(T * p, std::size_t sz) noexcept
    : p(p)
    , sz(sz)
    {}

    template<class T, class U, class = std::enable_if_t<(
        std::is_void<std::remove_cv_t<T>>::value and
        std::is_void<std::remove_cv_t<U>>::value
    )>>
    constexpr array_view(T * p, U * pright) noexcept
    : p(p)
    , sz(static_cast<const_iterator>(pright) - static_cast<const_iterator>(p))
    {}

    constexpr bool empty() const noexcept { return !this->sz; }

    constexpr std::size_t size() const noexcept { return this->sz; }

    constexpr const_pointer data() const noexcept { return this->p; }

    constexpr const_iterator begin() const noexcept { return static_cast<const_iterator>(this->p); }
    constexpr const_iterator end() const noexcept { return static_cast<const_iterator>(this->p) + this->sz; }

private:
    value_type * p = nullptr;
    std::size_t sz = 0;
};

template<>
struct array_view<void>
{
    using value_type = void;

    using iterator = uint8_t *;
    using const_iterator = uint8_t const *;

    using pointer = value_type *;
    using const_pointer = value_type const *;

    constexpr array_view() = default;
    constexpr array_view(array_view const &) = default;
    array_view & operator = (array_view const &) = default;

    constexpr array_view(std::nullptr_t) noexcept
    {}

    constexpr array_view(std::nullptr_t, std::nullptr_t) noexcept
    {}

    template<class T, class = std::enable_if_t<std::is_void<T>::value>>
    constexpr array_view(T * p, std::size_t sz) noexcept
    : p(p)
    , sz(sz)
    {}

    template<class T, class U, class = std::enable_if_t<(
        std::is_void<T>::value and
        std::is_void<U>::value
    )>>
    constexpr array_view(T * p, U * pright) noexcept
    : p(p)
    , sz(static_cast<const_iterator>(pright) - static_cast<const_iterator>(p))
    {}

    constexpr bool empty() const noexcept { return !this->sz; }

    constexpr std::size_t size() const noexcept { return this->sz; }

    constexpr pointer data() noexcept { return this->p; }
    constexpr const_pointer data() const noexcept { return this->p; }

    constexpr iterator begin() { return static_cast<iterator>(this->p); }
    constexpr iterator end() { return static_cast<iterator>(this->p) + this->sz; }
    constexpr const_iterator begin() const noexcept { return static_cast<const_iterator>(this->p); }
    constexpr const_iterator end() const noexcept { return static_cast<const_iterator>(this->p) + this->sz; }

private:
    value_type * p = nullptr;
    std::size_t sz = 0;
};


template<class T>
constexpr array_view<T> make_array_view(T * x, std::size_t n) noexcept
{ return {x, n}; }

template<class T>
constexpr array_view<T> make_array_view(T * left, T * right) noexcept
{ return {left, right}; }

template<class T>
constexpr array_view<const T> make_array_view(T const * left, T * right) noexcept
{ return {left, right}; }

template<class T>
constexpr array_view<const T> make_array_view(T * left, T const * right) noexcept
{ return {left, right}; }

template<class T, std::size_t N>
constexpr array_view<T> make_array_view(T (&arr)[N]) noexcept
{ return {arr, N}; }

template<class Cont>
constexpr auto make_array_view(Cont & cont) noexcept
-> array_view<typename std::remove_pointer<decltype(cont.data())>::type>
{ return {cont}; }

template<class T>
constexpr array_view<T const> make_const_array_view(T const * x, std::size_t n) noexcept
{ return {x, n}; }

template<class T>
constexpr array_view<const T> make_const_array_view(T const * left, T const * right) noexcept
{ return {left, right}; }

template<class T, std::size_t N>
constexpr array_view<T const> make_const_array_view(T const (&arr)[N]) noexcept
{ return {arr, N}; }


template<std::size_t N>
constexpr array_view<char const> cstr_array_view(char const (&str)[N]) noexcept
{ return {str, N-1}; }

// forbidden: array_view is for litterals
template<std::size_t N>
array_view<char> cstr_array_view(char (&str)[N]) = delete;


using array_view_u8 = array_view<uint8_t>;
using array_view_u16 = array_view<uint16_t>;
using array_view_u32 = array_view<uint32_t>;
using array_view_u64 = array_view<uint64_t>;
using array_view_const_u8 = array_view<uint8_t const>;
using array_view_const_u16 = array_view<uint16_t const>;
using array_view_const_u32 = array_view<uint32_t const>;
using array_view_const_u64 = array_view<uint64_t const>;

using array_view_s8 = array_view<int8_t>;
using array_view_s16 = array_view<int16_t>;
using array_view_s32 = array_view<int32_t>;
using array_view_s64 = array_view<int64_t>;
using array_view_const_s8 = array_view<int8_t const>;
using array_view_const_s16 = array_view<int16_t const>;
using array_view_const_s32 = array_view<int32_t const>;
using array_view_const_s64 = array_view<int64_t const>;

using array_view_char = array_view<char>;
using array_view_const_char = array_view<char const>;

