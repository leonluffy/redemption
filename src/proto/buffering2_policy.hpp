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


#include "proto/proto.hpp"
#include "proto/iovec.hpp"
#include "utils/sugar/numerics/safe_conversions.hpp"

#if defined(IN_IDE_PARSER)
# define PROTO_TRACE std::cout <<
# define PROTO_ENABLE_IF_TRACE
# define PROTO_ENABLE_IF_TRACE_PRE
#elif defined(ENABLE_PROTO_TRACE)
# include "proto/printer_policy.hpp"
# include <iostream>
# define PROTO_TRACE(...) std::cout << __VA_ARGS__
# define PROTO_ENABLE_IF_TRACE(...) __VA_ARGS__
# define PROTO_ENABLE_IF_TRACE_PRE(...) __VA_ARGS__
#else
# define PROTO_TRACE(...) void(0)
# define PROTO_ENABLE_IF_TRACE(...) void(0)
# define PROTO_ENABLE_IF_TRACE_PRE(...)
#endif

#ifdef __clang__
# include <tuple>
#endif

namespace brigand
{
    template<class L, class n> using drop = pop_front<L, n>;
    template<class L, class n> using take = pop_back<L, brigand::size_t<(brigand::size<L>::value - n::value)>>;

    template<class Lbd, class LArgs>
    using lapply = typename detail::apply<Lbd, LArgs>::type;

    template<template<class...> class Tpl, class... Ts>
    using pbind = brigand::pin<brigand::bind<Tpl, Ts...>>;

    template<template<class...> class Tpl>
    using pcall = brigand::pin<brigand::call<Tpl>>;
}

namespace proto_buffering2 {

#ifdef IN_IDE_PARSER
#define PROTO_DECLTYPE_AUTO_RETURN(expr) -> decltype(expr) { return (expr); }
#else
#define PROTO_DECLTYPE_AUTO_RETURN(...) -> decltype(__VA_ARGS__) { return (__VA_ARGS__); }
#endif

#ifndef __clang__
namespace detail {
    template<class T> struct arg_impl;
    template<class... Ts>
    struct arg_impl<brigand::list<Ts...>>
    {
        template<class T> static T * impl(Ts..., T * x, ...) { return x; }
    };
}
#endif

template<std::size_t i, class... T>
auto const & arg(T const & ... args)
{
#ifdef __clang__
    // inlining with clang
    return std::get<i>(std::forward_as_tuple(args...));
#else
    // not inlining with clang
    return *detail::arg_impl<brigand::filled_list<void const *, i>>::impl(&args...);
#endif
}

template<std::size_t i, class L>
auto const & larg(L const & l)
{ return l.apply([](auto const & ... v) PROTO_DECLTYPE_AUTO_RETURN(arg<i>(v...))); }


template<class T>
using is_buffer_delimiter = brigand::bool_<
    proto::is_dynamic_buffer<T>::value or
    proto::is_view_buffer<T>::value or
    (proto::is_limited_buffer<T>::value and proto::has_special_sz<T>::value)
>;


using proto::desc_type_t;

namespace detail {
    template<class T>
    struct is_size_impl : std::false_type
    {};

    template<std::size_t n>
    struct is_size_impl<proto::size_<n>> : std::true_type
    {};

    template<class T>
    struct is_limited_size_impl : std::false_type
    {};

    template<std::size_t n>
    struct is_limited_size_impl<proto::limited_size<n>> : std::true_type
    {};
}
template<class Sz>
using is_size_ = typename detail::is_size_impl<Sz>::type;

template<class Sz>
using is_limited_size = typename detail::is_limited_size_impl<Sz>::type;

template<class Val>
using has_pkt_sz = typename std::is_same<
    proto::get_arguments_t<desc_type_t<Val>>,
    brigand::list<proto::dsl::pkt_sz>
>::type;

template<class Val>
using has_pkt_sz_with_self = typename std::is_same<
    proto::get_arguments_t<desc_type_t<Val>>,
    brigand::list<proto::dsl::pkt_sz_with_self>
>::type;

template<class Val>
using has_pkt_sz_cat = brigand::bool_<
    has_pkt_sz<Val>{} ||
    has_pkt_sz_with_self<Val>{}
>;

template<class Val>
using has_pkt_data = proto::is_reserializer<desc_type_t<Val>>;

template<class Val>
using has_special_pkt = brigand::bool_<
    has_pkt_sz_cat<Val>{} || has_pkt_data<Val>{}
>;

template<class Info>
using keep_info_pkt_data_ptr = brigand::bool_<
    !Info::is_begin_buf && Info::enable_pkt_data && Info::is_begin_pkt
>;

template<class Val, class Sz, class Sz2>
using is_undeterministic_sizeof_special = brigand::bool_<
    !proto::is_static_buffer<desc_type_t<Val>>::value
    and (
        (has_pkt_sz_with_self<Val>::value and !is_size_<Sz>::value)
        or
        (has_pkt_sz<Val>::value and !is_size_<Sz2>::value)
        or
        (has_pkt_data<Val>::value)
    )
>;

template<class Info>
using keep_info_special_pkt_ptr = brigand::bool_<
    !Info::is_begin_buf && !Info::is_end_buf && (
        (
            has_pkt_sz_with_self<typename Info::val>::value and
            !is_size_<typename Info::sz_self>::value
        )
        or
        (
            has_pkt_sz<typename Info::val>::value and
            !is_size_<typename Info::sz>::value
        )
        or has_pkt_data<typename Info::val>::value
    )
>;

template<class Val, class PrevVal, class PrevSz, class PrevSz2>
using is_delimiter = brigand::bool_<
    proto::is_dynamic_buffer<desc_type_t<Val>>::value
    or
    proto::is_view_buffer<desc_type_t<Val>>::value
    or
    proto::is_dynamic_buffer<desc_type_t<PrevVal>>::value
    or
    proto::is_view_buffer<desc_type_t<PrevVal>>::value
    or
    is_undeterministic_sizeof_special<PrevVal, PrevSz, PrevSz2>::value
>;

template<class IsEnd, class Val, class Sz, class Sz2>
using is_pkt_sz_delimiter = brigand::bool_<
    !IsEnd::value
    and
    proto::is_limited_buffer<desc_type_t<Val>>::value
    and
    ( ( has_pkt_sz_with_self<Val>::value and !is_size_<Sz>::value )
          or
          ( has_pkt_sz<Val>::value and !is_size_<Sz2>::value )
    )
>;


template<std::size_t n>
using mk_seq = brigand::range<std::size_t, 0, n>;

template<class n>
using mk_seq2 = brigand::range<std::size_t, 0, n::value>;

template<class T, class Size>
using mk_filled_list = brigand::filled_list<T, Size::value>;

template<class IPacket, class IVar, class DescType>
struct var_info {
    using ipacket = IPacket;
    using ivar = IVar;
    using desc_type = DescType;
};

template<
    class I,
    class Val,
    class IPacket,
    class IVar,
    class IBuf,
    class DescType,
    class isBeginBuf,
    class isEndBuf,
    class isBeginPkt,
    class isEndPkt,
    class EnablePktData,
    class SzSelf,
    class Sz2
>
struct val_info
{
    using val = Val;
    using desc_type = DescType;
    static constexpr std::size_t i = I::value;
    static constexpr std::size_t ipacket = IPacket::value;
    static constexpr std::size_t ivar = IVar::value;
    static constexpr std::size_t ibuf = IBuf::value;
    static constexpr bool is_begin_buf = isBeginBuf::value;
    static constexpr bool is_end_buf = isEndBuf::value;
    static constexpr bool is_begin_pkt = isBeginPkt::value;
    static constexpr bool is_end_pkt = isEndPkt::value;
    static constexpr bool enable_pkt_data = EnablePktData::value;
    using sz_self = SzSelf;
    using sz = Sz2;
};

using cvoidp = void const *;

template<class var_info>
using var_info_is_buffer_delimiter = is_buffer_delimiter<typename var_info::desc_type>;


template<std::size_t n> struct static_size : brigand::size_t<n> {};
template<std::size_t n> struct dynamic_size : brigand::size_t<n> {};
template<std::size_t n> struct limited_size : brigand::size_t<n> {};

namespace lazy {
    template<class p, class i>
    struct add_size_impl;

    template<template<std::size_t> class Size, std::size_t n, class add>
    struct add_size_impl<Size<n>, add>
    { using type = Size<(n+add::value)>; };

    template<template<std::size_t> class Size, std::size_t n>
    struct add_size_impl<Size<n>, proto::dyn_size>
    { using type = dynamic_size<n>; };

    template<template<std::size_t> class Size, std::size_t n1, std::size_t n2>
    struct add_size_impl<Size<n1>, proto::limited_size<n2>>
    { using type = Size<n1+n2>; };

    template<std::size_t n1, std::size_t n2>
    struct add_size_impl<brigand::size_t<n1>, proto::limited_size<n2>>
    { using type = limited_size<n1+n2>; };

    template<std::size_t n1, std::size_t n2>
    struct add_size_impl<static_size<n1>, proto::limited_size<n2>>
    { using type = limited_size<n1+n2>; };

    template<std::size_t n1, std::size_t n2>
    struct add_size_impl<brigand::size_t<n1>, brigand::size_t<n2>>
    { using type = brigand::size_t<n1+n2>; };
}
template<class i1, class i2>
using add_size = typename lazy::add_size_impl<i1, i2>::type;

template<class L>
using sizeof_packet = brigand::fold<
    brigand::transform<L, brigand::call<proto::sizeof_>>,
    static_size<0>,
    brigand::call<add_size>
>;


namespace lazy {
    template<class, class>
    struct sizeof_packet_add2_impl
    { using type = proto::dyn_size; };

    template<std::size_t n1, std::size_t n2>
    struct sizeof_packet_add2_impl<proto::size_<n1>, proto::size_<n2>>
    { using type = proto::size_<n1+n2>; };

    template<std::size_t n1, std::size_t n2>
    struct sizeof_packet_add2_impl<proto::limited_size<n1>, proto::limited_size<n2>>
    { using type = proto::limited_size<n1+n2>; };

    template<std::size_t n1, std::size_t n2>
    struct sizeof_packet_add2_impl<proto::size_<n1>, proto::limited_size<n2>>
    { using type = proto::limited_size<n1+n2>; };

    template<std::size_t n1, std::size_t n2>
    struct sizeof_packet_add2_impl<proto::limited_size<n1>, proto::size_<n2>>
    { using type = proto::limited_size<n1+n2>; };
}
template<class i1, class i2>
using sizeof_packet_add2 = typename lazy::sizeof_packet_add2_impl<i1, i2>::type;

template<class L>
using sizeof_packet2 = brigand::fold<
    brigand::transform<L, brigand::call<proto::sizeof_>>,
    proto::size_<0>,
    brigand::call<sizeof_packet_add2>
>;

namespace detail {
    template<class Sz>
    struct limited_to_dyn_impl
    { using type = Sz; };

    template<std::size_t n>
    struct limited_to_dyn_impl<proto::limited_size<n>>
    { using type = proto::dyn_size; };
}

template<class Sz>
using limited_to_dyn = typename detail::limited_to_dyn_impl<Sz>::type;

namespace detail {
    template<class T>
    struct limited_size_to_dyn_size
    { using type = T; };

    template<std::size_t n>
    struct limited_size_to_dyn_size<proto::limited_size<n>>
    { using type = proto::dyn_size; };
}
template<class T>
using limited_size_to_dyn_size = typename detail::limited_size_to_dyn_size<T>::type;

template<class L>
using sizeof_packet_with_limited_size_to_dyn_size = brigand::fold<
    brigand::transform<
        brigand::transform<L, brigand::call<proto::sizeof_>>,
        brigand::call<limited_size_to_dyn_size>
    >,
    static_size<0>,
    brigand::call<add_size>
>;

namespace lazy {
    template<class L, class Add>
    struct mk_list_accu;

    template<template<class...> class L, class... Ts, class add>
    struct mk_list_accu<L<Ts...>, add>
    { using type = L<add_size<Ts, add>..., add>; };
}
template<class L, class x>
using mk_list_accu = typename lazy::mk_list_accu<L, x>::type;

template<class L>
using make_accumulate_sizeof_list = brigand::fold<L, brigand::list<>, brigand::call<mk_list_accu>>;

namespace detail {
    template<class L, class Add>
    struct mk_list_accu_impl2;

    template<template<class...> class L, class... Ts, class n>
    struct mk_list_accu_impl2<L<Ts...>, n>
    { using type = L<sizeof_packet_add2<Ts, n>..., n>; };
}
template<class L, class x>
using mk_list_accu2 = typename detail::mk_list_accu_impl2<L, x>::type;

namespace detail {
    template<class L, class Add>
    struct laccu_impl;

    template<template<class...> class L, class... Ts, class n>
    struct laccu_impl<L<Ts...>, n>
    { using type = L<brigand::size_t<Ts::value + n::value>..., n>; };
}
template<class L, class T>
using laccu = typename detail::laccu_impl<L, T>::type;

template<class L>
using accu_add_size_list = brigand::fold<L, brigand::list<>, brigand::call<mk_list_accu2>>;

template <class A, class B>
using plus = std::integral_constant<std::size_t, A::value + B::value>;

template <class A, class B>
using minus = std::integral_constant<std::size_t, A::value - B::value>;

template <class A, class B>
using not_equal_to = brigand::bool_<(A::value != B::value)>;

template<class C, class True, class False>
using eval = typename std::conditional_t<C::value, True, False>::type;

namespace detail {
    template<class T, std::size_t n> struct pkt_sz_with_size { using desc_type = T; };
}

}

namespace proto {
    template<class T, std::size_t n>
    struct sizeof_impl<proto_buffering2::detail::pkt_sz_with_size<T, n>>
    : sizeof_impl<T>
    {};
}

namespace proto_buffering2 {

namespace detail {
    template<template<class> class IsPktSz, class Pkt, class Sz>
    struct convert_pkt_sz
    { using type = Pkt; };

    template<template<class> class IsPktSz, class... Ts, std::size_t n>
    struct convert_pkt_sz<IsPktSz, brigand::list<Ts...>, proto::size_<n>>
    { using type = brigand::list<std::conditional_t<IsPktSz<Ts>{}, pkt_sz_with_size<Ts, n>, Ts>...>; };
}
template<class Pkt, class Sz, class SzNext>
using convert_pkt_sz = typename detail::convert_pkt_sz<
    proto::is_pkt_sz_with_self,
    typename detail::convert_pkt_sz<proto::is_pkt_sz, Pkt, SzNext>::type,
    Sz
>::type;


template<std::size_t i>
using i_ = std::integral_constant<std::size_t, i>;

template<class L>
using mk_sizeof_var_info_list = brigand::transform<
    brigand::transform<L, brigand::call<proto::desc_type_t>>,
    brigand::call<proto::sizeof_>
>;

namespace detail {
    template<class>
    struct sizeof_to_buffer;

    template<std::size_t n>
    struct sizeof_to_buffer<dynamic_size<n>>
    { using type = proto::dyn_size; };

    template<>
    struct sizeof_to_buffer<proto::dyn_size>
    { using type = proto::dyn_size; };

    template<std::size_t n>
    struct uninitialized_buf
    {
        uninitialized_buf() {}
        uninitialized_buf(uninitialized_buf const &) = delete;
        alignas(4) uint8_t buf [n];
    };

    template<std::size_t n>
    struct sizeof_to_buffer<static_size<n>>
    { using type = uninitialized_buf<n>; };

    template<std::size_t n>
    struct sizeof_to_buffer<limited_size<n>>
    { using type = uninitialized_buf<n>; };

    template<std::size_t n>
    struct sizeof_to_buffer<proto::size_<n>>
    { using type = uninitialized_buf<n>; };

    template<std::size_t n>
    struct sizeof_to_buffer<proto::limited_size<n>>
    { using type = uninitialized_buf<n>; };
}
template<class T>
using sizeof_to_buffer = typename detail::sizeof_to_buffer<T>::type;

template<class L>
using sizeof_var_infos = brigand::fold<
    mk_sizeof_var_info_list<L>,
    static_size<0>,
    brigand::call<add_size>
>;

template<class L>
using buffer_from_var_infos = sizeof_to_buffer<sizeof_var_infos<L>>;

template<class VarInfos>
using var_infos_is_not_dynamic = proto::is_dynamic_buffer<desc_type_t<brigand::front<VarInfos>>>;

template<class T>
using var_info_is_pkt_sz = proto::is_pkt_sz_category<desc_type_t<T>>;

template<class VarInfos>
using var_infos_has_pkt_sz = brigand::any<VarInfos, brigand::call<var_info_is_pkt_sz>>;

namespace detail {
    template<class T>
    struct is_static_size : std::false_type
    {};

    template<std::size_t n>
    struct is_static_size<static_size<n>> : std::true_type
    {};

    template<class T>
    struct is_not_static_size : std::true_type
    {};

    template<std::size_t n>
    struct is_not_static_size<static_size<n>> : std::false_type
    {};

    template<class T>
    struct is_dynamic_size : std::false_type
    {};

    template<std::size_t n>
    struct is_dynamic_size<dynamic_size<n>> : std::true_type
    {};
}
template<class T>
using is_dynamic_size = typename detail::is_dynamic_size<T>::type;
template<class T>
using is_static_size = typename detail::is_static_size<T>::type;
template<class T>
using is_not_static_size = typename detail::is_not_static_size<T>::type;

template<class T, std::size_t n>
struct static_array_view;
template<class T, std::size_t n>
struct static_array_view<T const, n>
{
    static_array_view(T const (&a)[n]) noexcept : av{a} {}
    static_array_view(std::array<T, n> & a) noexcept
      : static_array_view(reinterpret_cast<T(&)[n]>(a.front())) {}
    static_array_view(std::array<T, n> const & a) noexcept
      : static_array_view(reinterpret_cast<T const (&)[n]>(a.front())) {}

    operator array_view<T const> () const noexcept { return {av}; }

    T const * data() const noexcept { return av; }
    std::size_t size() const noexcept { return n; }

    T const * begin() const noexcept { return av; }
    T const * end() const noexcept { return av + n; }

private:
    T const (&av)[n];
};


namespace detail
{
    template<class Ints, class... Ts>
    struct tuple_buf;

    template<std::size_t, class T>
    struct tuple_element
    { T elem; };

    template<std::size_t... Ints, class... Ts>
    struct tuple_buf<std::integer_sequence<std::size_t, Ints...>, Ts...>
    : tuple_element<Ints, Ts>...
    {};
}
template<class... Ts>
using tuple_buf = detail::tuple_buf<std::index_sequence_for<Ts...>, Ts...>;

template<std::size_t... Ints, class... Ts, class F>
void each_element_with_index(
    detail::tuple_buf<std::integer_sequence<std::size_t, Ints...>, Ts...> & t,
    F && f
) {
    (void)std::initializer_list<int>{
        (f(
            static_cast<detail::tuple_element<Ints, Ts>&>(t).elem,
            std::integral_constant<std::size_t, Ints>{}
        ), 1)...
    };
}

template<std::size_t i, class Ts>
Ts const & get(detail::tuple_element<i, Ts> const & te)
{ return te.elem; }



namespace detail
{
    template<std::size_t n>
    struct Buffers
    {
        std::array<iovec, n> data {};

        template<class TupleBuf>
        Buffers(TupleBuf & t)
        {
            each_element_with_index(t, [this](auto & elem, auto i) {
                this->init_buf(i, elem);
            });
        }

        iovec & operator[](std::size_t i) noexcept
        {
            return this->data[i];
        }

        static_array_view<iovec const, n> view() const
        {
            PROTO_TRACE("view()\n");
            PROTO_ENABLE_IF_TRACE(for (auto iov : this->data)
                PROTO_TRACE("  {" << iov.iov_base << ", " << iov.iov_len << "}\n"));
            return {this->data};
        }

        template<class TupleBuf>
        void reset_ptr(TupleBuf & t)
        {
            each_element_with_index(t, [this](auto & elem, auto i) {
                this->reset_buf_ptr(i, elem);
            });
        }

    private:
        template<class I, std::size_t arr_len>
        void init_buf(I i, uninitialized_buf<arr_len> & uninit_buf) {
            PROTO_TRACE(i << " - [" << arr_len << "] " << static_cast<void*>(uninit_buf.buf) << "\n");
            this->data[i].iov_base = uninit_buf.buf;
            //this->data[i].iov_len = arr_len;
        }

        template<class I>
        void init_buf(I, proto::dyn_size) {
            PROTO_TRACE(I{} << " - dyn_size\n");
        }

        template<class I, std::size_t arr_len>
        void reset_buf_ptr(I i, uninitialized_buf<arr_len> & uninit_buf) {
            this->data[i].iov_len = static_cast<uint8_t*>(this->data[i].iov_base) - uninit_buf.buf;
            this->data[i].iov_base = uninit_buf.buf;
        }

        template<class I>
        void reset_buf_ptr(I, proto::dyn_size) {
        }
    };

    template<class>
    struct Sizes;

    template<class PktSz>
    struct Sizes<brigand::list<PktSz>>
    {
        std::size_t data[2] { PktSz::value };

        void propagate_size()
        {}
    };

    template<class... PktSz>
    struct Sizes<brigand::list<PktSz...>>
    {
        std::size_t data[sizeof...(PktSz)+1] { PktSz::value... };

        void propagate_size()
        {
            std::size_t i = sizeof...(PktSz);
            while (i-- > 0) {
                this->data[i] += this->data[i+1];
            }
        }
    };
}

namespace detail {
    template<template<class> class IsPktSz, class Pkt, class Sz>
    struct convert_pkt_sz2
    { using type = Pkt; };

    template<template<class> class IsPktSz, class... Ts, std::size_t n>
    struct convert_pkt_sz2<IsPktSz, brigand::list<Ts...>, proto::size_<n>>
    { using type = brigand::list<std::conditional_t<IsPktSz<Ts>{}, proto::types::static_value<Ts, n>, Ts>...>; };
}
// TODO convert_pkt_sz
// template<class Pkt, class Sz, class SzNext>
// using convert_pkt_sz2 = typename detail::convert_pkt_sz2<
//     proto::is_pkt_sz_with_self,
//     typename detail::convert_pkt_sz2<proto::is_pkt_sz, Pkt, SzNext>::type,
//     Sz
// >::type;
template<class Pkt, class Sz, class SzNext>
using convert_pkt_sz2 = Pkt;

namespace detail
{
    template<class Val, bool = proto::is_static_buffer<desc_type_t<Val>>::value>
    struct reserved_size_impl
    {
        static std::size_t _(Val const & val)
        { return val.desc.reserved_size(); }
    };

    template<class Val>
    struct reserved_size_impl<Val, true>
    {
        static std::size_t _(Val const &)
        { return proto::sizeof_<desc_type_t<Val>>{}; }
    };
}

template<class Val>
std::size_t reserved_size(Val const & val)
{ return detail::reserved_size_impl<Val>::_(val); }

namespace detail
{
    using namespace proto_buffering2::detail;

    template<class>
    struct to_is_pkt_first_list;

    template<class T, class...>
    using enable_type = T;

    template<class T, class... Ts>
    struct to_is_pkt_first_list<brigand::list<T, Ts...>>
    { using type = brigand::list<brigand::bool_<1>, enable_type<brigand::bool_<0>, Ts>...>; };
}

template<class L>
using to_is_pkt_first_list = typename detail::to_is_pkt_first_list<L>::type;


template<class L>
using to_is_first_list = brigand::push_front<
  brigand::filled_list<std::false_type, brigand::size<L>::value-1>,
  std::true_type
>;

template<class L>
using to_is_last_list = brigand::push_back<
  brigand::filled_list<std::false_type, brigand::size<L>::value-1>,
  std::true_type
>;

template<class T, class F>
void cifv(std::true_type, T && v, F && f)
{ f(std::forward<T>(v)); }

template<class T, class F>
void cifv(std::false_type, T &&, F &&)
{}

template<class T, class F, class FElse>
void cifv(std::true_type, T && v, F && f, FElse &&)
{ f(std::forward<T>(v)); }

template<class T, class F, class FElse>
void cifv(std::false_type, T && v, F &&, FElse && f)
{ f(std::forward<T>(v)); }

#ifdef IN_IDE_PARSER
# define PROTO_UNPACK
#else
# define PROTO_UNPACK(...) \
  (void)std::initializer_list<int>{(void((__VA_ARGS__)), 1)...}
#endif

template<class... T, class F>
void mpl_for_each(brigand::list<T...>, F && f)
{ PROTO_UNPACK(f(T{})); }

template<class... T, class... U, class F>
void mpl_for_each(brigand::list<T...>, brigand::list<U...>, F && f)
{ PROTO_UNPACK(f(T{}, U{})); }

struct special_op {};

template<class Policy>
struct Buffering2
{
    template<class... Pkts>
    struct Impl
    {
        // [ [ desc_type ... ] ... ]
        using desc_list_by_packet = brigand::list<
            brigand::transform<
                typename Pkts::type_list,
                brigand::call<desc_type_t>
            >...
        >;

        using flat_values = brigand::append<typename Pkts::type_list...>;

        // [ size_<n> | limited_size<n> | dyn_size ... ]
        using sizeof_by_packet = brigand::transform<
            desc_list_by_packet,
            brigand::call<sizeof_packet2>
        >;

        // [ size_<n> | dyn_size ... ]
        using accu_sizeof_by_packet = accu_add_size_list<
            brigand::transform<
                sizeof_by_packet,
                brigand::call<limited_to_dyn>
            >
        >;
        // [ size_<n> | dyn_size ... ]
        using accu_sizeof_by_packet2 = brigand::push_back<
            brigand::pop_front<accu_sizeof_by_packet>,
            proto::size_<0>
        >;


        // [ size<pkt> ... ]
        using packet_size_list = brigand::transform<
            desc_list_by_packet,
            brigand::call<brigand::size>
        >;


        // [ size_<n> | dyn_size ... ]
        using flat_accu_sizeof_by_packet_by_packet = brigand::join<
            brigand::transform<
                accu_sizeof_by_packet,
                packet_size_list,
                brigand::call<mk_filled_list>
            >
        >;
        // [ size_<n> | dyn_size ... ]
        using flat_accu_sizeof_by_packet_by_packet2 = brigand::join<
            brigand::transform<
                accu_sizeof_by_packet2,
                packet_size_list,
                brigand::call<mk_filled_list>
            >
        >;

        // [ [ desc_type ... ] ... ]
        using desc_list_by_buffer = brigand::transform<
            brigand::split_if<
                brigand::transform<
                    flat_values,
                    brigand::push_front<
                        brigand::pop_back<flat_values>,
                        proto::var<void, proto::types::u8>
                    >,
                    brigand::push_front<
                        brigand::pop_back<
                          flat_accu_sizeof_by_packet_by_packet
                        >,
                        proto::size_<0>
                    >,
                    flat_accu_sizeof_by_packet_by_packet,
                    brigand::call<brigand::list>
                >,
                brigand::bind<
                    brigand::lapply,
                    brigand::pcall<is_delimiter>,
                    brigand::_1
                >
            >,
            // desc_type_t<front<list<V, PV, A, A2>>>...
            brigand::bind<
                brigand::transform,
                brigand::_1,
                brigand::pbind<
                    desc_type_t,
                    brigand::call<brigand::front>
                >
            >
        >;

//         brigand::transform<
//                     flat_values,
//                     brigand::push_front<
//                         brigand::pop_back<flat_values>,
//                         proto::var<void, proto::types::u8>
//                     >,
//                     brigand::push_front<
//                         brigand::pop_back<
//                           flat_accu_sizeof_by_packet_by_packet
//                         >,
//                         proto::size_<0>
//                     >,
//                     flat_accu_sizeof_by_packet_by_packet,
//                     brigand::call<brigand::list>
//                 > a = 1;
//                 desc_list_by_buffer b = 2;

        // [ filled_list<ibuf, size<buffer>> ... ]
        // [ [ 0 ... ] [ 1 ... ] ... ]
        using ibuf_list_by_buffer = brigand::transform<
            mk_seq2<brigand::size<desc_list_by_buffer>>,
            brigand::transform<
                desc_list_by_buffer,
                brigand::call<brigand::size>
            >,
            brigand::call<mk_filled_list>
        >;

        // [ [ 1 0... ] ... ]
        using is_first_ibuf_list = brigand::transform<
            desc_list_by_buffer,
            brigand::call<to_is_first_list>
        >;

        // [ [ 0... 1 ] ... ]
        using is_last_ibuf_list = brigand::transform<
            desc_list_by_buffer,
            brigand::call<to_is_last_list>
        >;


        // [ filled_list<ipacket, size<packet>> ... ]
        // [ [ 0 ... ] [ 1 ... ] ... ]
        using ipacket_list_by_packet = brigand::transform<
            mk_seq<sizeof...(Pkts)>,
            packet_size_list,
            brigand::call<mk_filled_list>
        >;

        // [ [ 0..size<packet> ] ... ]
        using ivar_list_by_packet = brigand::transform<
            packet_size_list,
            brigand::call<mk_seq2>
        >;


        // [ [ 1 0... ] ... ]
        using is_first_var_list_by_packet = brigand::transform<
            ivar_list_by_packet,
            brigand::call<to_is_first_list>
        >;

        // [ [ 0... 1 ] ... ]
        using is_last_var_list_by_packet = brigand::transform<
            ivar_list_by_packet,
            brigand::call<to_is_last_list>
        >;

        // [ [ 1 ... ] | [ 0 ... ] ... ]
        using is_pkt_data_packet = brigand::transform<
            brigand::push_front<
                brigand::pop_back<
                    brigand::list<
                        brigand::any<
                        typename Pkts::type_list,
                        brigand::call<has_pkt_data>
                        >...
                    >
                >,
                brigand::bool_<false>
            >,
            packet_size_list,
            brigand::call<mk_filled_list>
        >;


        // [ val_info<...> ... ]
        using val_infos = brigand::transform<
            mk_seq2<brigand::size<flat_values>>,
            flat_values,
            brigand::join<ipacket_list_by_packet>,
            brigand::join<ivar_list_by_packet>,
            brigand::join<ibuf_list_by_buffer>,
            brigand::join<desc_list_by_packet>,
            brigand::join<is_first_ibuf_list>,
            brigand::join<is_last_ibuf_list>,
            brigand::join<is_first_var_list_by_packet>,
            brigand::join<is_last_var_list_by_packet>,
            brigand::join<is_pkt_data_packet>,
            flat_accu_sizeof_by_packet_by_packet,
            flat_accu_sizeof_by_packet_by_packet2,
            brigand::call<val_info>
        >;


        using special_pkt_size = brigand::count_if<
            val_infos,
            brigand::call<keep_info_special_pkt_ptr>
        >;

        using pkt_data_size = brigand::count_if<
            val_infos,
            brigand::call<keep_info_pkt_data_ptr>
        >;

        // [ size_<n> | limited_size<n> | dyn_size ... ]
        using sizeof_by_buffer = brigand::transform<
            desc_list_by_buffer,
            brigand::call<sizeof_packet2>
        >;

        // [ uninitialized_buf | dyn_size ... ]
        using buffer_list = brigand::transform<
            sizeof_by_buffer,
            brigand::call<sizeof_to_buffer>
        >;

        using special_ptr_array_t = std::array<void*, special_pkt_size{}>;
        using pkt_size_array_t = std::array<std::size_t, sizeof...(Pkts)>;
        using pkt_ptr_array_t = std::array<void*, pkt_data_size{}>;


        brigand::wrap<buffer_list, tuple_buf> buffer_tuple;
        detail::Buffers<brigand::size<buffer_list>{}> buffers{buffer_tuple};

        special_ptr_array_t special_pkt_ptrs;
        pkt_size_array_t sizes;
        pkt_ptr_array_t pkt_data_ptrs;
        Policy const & policy;

        typename special_ptr_array_t::iterator pspecial_pkt;
        typename pkt_size_array_t::iterator psize;
        typename pkt_ptr_array_t::iterator ppkt_data_ptrs;

        iovec * piov;

        Impl(Policy const & policy) noexcept
        : policy(policy)
        , pspecial_pkt(std::begin(this->special_pkt_ptrs))
        , psize(std::begin(sizes))
        , ppkt_data_ptrs(std::begin(pkt_data_ptrs))
        , piov(buffers.data.data())
        {
        }

        uint8_t * iov_base() const noexcept { return static_cast<uint8_t*>(this->piov->iov_base); }

        void impl(Pkts const & ... packets)
        {
            PROTO_TRACE("----------- pkt (" << sizeof...(Pkts) << ") -----------\n");
            PROTO_TRACE("--------- bufsz (" << brigand::size<buffer_list>{} << ") -----------\n");

            this->serialize(val_infos{}, packets...);
        }

        template<class... Info>
        void serialize(brigand::list<Info...>, Pkts const & ... pkts)
        {
            using at = brigand::index_if<
                brigand::list<desc_type_t<Info>...>,
                brigand::call<proto::is_dynamic_buffer>,
                i_<sizeof...(Info)>
            >;

            using splitted = brigand::split_at<
                brigand::list<Info...>,
                at
            >;

            this->serialize_(
                brigand::front<splitted>{},
                brigand::back<splitted>{},
                pkts...
            );
        }

        void serialize(brigand::list<>, Pkts const & ... pkts)
        {
            PROTO_TRACE("----------- special (" << special_pkt_size{} << ") -----------\n");

            PROTO_TRACE("\nsizes: ");
            PROTO_ENABLE_IF_TRACE(for (auto sz : this->sizes) PROTO_TRACE(sz << " "));

            this->buffers.reset_ptr(this->buffer_tuple);

            // propagate_size
            {
                std::size_t i = sizeof...(Pkts) - 1;
                while (i-- > 0) {
                    this->sizes[i] += this->sizes[i+1];
                }
            }

            PROTO_TRACE("\nsizes: ");
            PROTO_ENABLE_IF_TRACE(for (auto sz : this->sizes) PROTO_TRACE(sz << " "));

            PROTO_TRACE("\nbuf.iov.len: ");
            PROTO_ENABLE_IF_TRACE(for (auto iov : this->buffers.data) PROTO_TRACE(iov.iov_len << " "));
            PROTO_TRACE("\n\n");

            this->serialize_spe(brigand::reverse<val_infos>{}, pkts...);

            PROTO_TRACE("----------- send -----------\n");

            this->policy.send(this->buffers.view());
        }

        template<class... Info>
        void serialize_spe(brigand::list<Info...>, Pkts const & ... pkts)
        {
            PROTO_UNPACK(
                has_special_pkt<typename Info::val>{}
                ? this->serialize_spe_value(
                    Info{},
                    has_special_pkt<typename Info::val>{},
                    larg<Info::ivar>(arg<Info::ipacket>(pkts...))
                )
                : void()
            );
        }

        template<class Info, class Val>
        void serialize_spe_value(Info, std::false_type, Val const &)
        {}

        template<class Info>
        std::size_t next_size_or_0() const
        {
            return Info::ipacket + 1 == sizeof...(Pkts) ? 0u : this->sizes[Info::ipacket+1];
        }

        template<class Info, class Val>
        void serialize_spe_value(Info, std::true_type, Val const & val)
        {
            PROTO_TRACE("i" << Info::i << ".b" << Info::ibuf << ".p" << Info::ipacket << ". ");
            PROTO_TRACE(name(val) << " = ");

            using pkt_sz = typename Info::sz;
            using pkt_sz_self = typename Info::sz_self;

            using is_pkt_sz = brigand::bool_<has_pkt_sz<Val>{} && !is_size_<pkt_sz>{}>;
            using is_pkt_sz_self = brigand::bool_<has_pkt_sz_with_self<Val>{} && !is_size_<pkt_sz_self>{}>;

            cifv(is_pkt_sz{}, val, [this](auto const & val){
                PROTO_TRACE(" [pkt_sz: " << this->next_size_or_0<Info>() << "]");
                this->serialize_eval_sz<Info, proto::dsl::pkt_sz>([this]{
                    return this->next_size_or_0<Info>();
                }, val);
            });

            cifv(is_pkt_sz_self{}, val, [this](auto const & val){
                PROTO_TRACE(" [pkt_sz_with_self: " << this->sizes[Info::ipacket] << "]");
                this->serialize_eval_sz<Info, proto::dsl::pkt_sz_with_self>([this]{
                    return this->sizes[Info::ipacket];
                }, val);
            });

            cifv(has_pkt_data<Val>{}, val, [this](auto const & val){
                PROTO_TRACE(" [reserialize]");
                auto const & new_val = val.to_proto_value(proto::utils::make_parameters());
                PROTO_ENABLE_IF_TRACE(Printer::print(new_val, 1));
                iovec & iov = this->buffers.data[Info::ibuf];

                auto buf = static_cast<uint8_t*>(
                    keep_info_special_pkt_ptr<Info>{}
                    ? *--this->pspecial_pkt
                    : this->buffers.data[Info::ibuf].iov_base
                );
                PROTO_TRACE(" [buf: " << cvoidp(buf) << "]");

                using buf_cat = proto::buffer_category<desc_type_t<Val>>;

                cifv(
                    brigand::bool_<Info::ipacket + 1 == sizeof...(Pkts)>{},
                    brigand::size_t<Info::ipacket>{},
                    [&](auto){
                        PROTO_TRACE(" [data: {null, 0}]");
                        this->reserializer<Info>(buf_cat{}, buf, new_val.desc, array_view_u8{});
                    },
                    [&](auto ipkt){
                        using next_ipkt = brigand::size_t<
                            Info::i
                          + brigand::size<brigand::at<desc_list_by_packet, decltype(ipkt)>>::value
                          - Info::ivar
                        >;
                        using next_info = brigand::at<val_infos, next_ipkt>;
                        using keep_pkt_data = keep_info_pkt_data_ptr<next_info>;
                        PROTO_TRACE(" [ikeep_ptr: " << next_ipkt{} << "]");
                        PROTO_TRACE(" [keep_ptr: " << keep_pkt_data{} << "]");

                        uint8_t * p;
                        if (keep_pkt_data{}) {
                            assert(std::begin(pkt_data_ptrs) != this->ppkt_data_ptrs);
                            p = static_cast<uint8_t *>(*--this->ppkt_data_ptrs);
                        }

                        if (next_info::ibuf == brigand::size<buffer_list>::value - 1) {
                            auto iov_base = static_cast<uint8_t *>(iov.iov_base);
                            this->reserializer<Info>(
                                buf_cat{},
                                buf,
                                new_val.desc,
                                keep_pkt_data{}
                                ? array_view_u8{p, std::size_t(iov.iov_len - (p - iov_base))}
                                : array_view_u8{iov_base, iov.iov_len}
                            );
                        }
                        else {
                            iovec tmpiov = iov;
                            if (keep_pkt_data{}) {
                                auto iov_base = static_cast<uint8_t *>(iov.iov_base);
                                assert(iov_base <= p);
                                assert(iov.iov_len >= std::size_t(p - iov_base));
                                iov.iov_len -= p - iov_base;
                                iov.iov_base = p;
                            }

                            constexpr std::size_t iov_count = brigand::size<buffer_list>{} - next_info::ibuf;
                            PROTO_TRACE(" [iov_count: " << iov_count << "]");
                            PROTO_TRACE(" [total: " << this->sizes[next_info::ipacket] << "]");
                            this->reserializer<Info>(
                                buf_cat{},
                                buf,
                                new_val.desc,
                                //static_iovec_array<iovec, brigand::size<buffer_list>{} - next_info::ibuf>{};
                                iovec_array{&iov, iov_count},
                                this->sizes[next_info::ipacket]
                            );

                            if (keep_pkt_data{}) {
                                iov = tmpiov;
                            }
                        }
                    }
                );
            });
            PROTO_TRACE("\n");
        }

        template<class Info, class Sp, class Get, class Val>
        void serialize_eval_sz(Get get, Val const & val)
        {
            auto get_val = proto::val<Sp, Get>{get};
            auto const & new_val = val.desc.to_proto_value(proto::utils::make_parameters(get_val));
            using new_val_type = std::remove_reference_t<decltype(new_val)>;
            PROTO_ENABLE_IF_TRACE(Printer::print(new_val, 1));

            void * buf = keep_info_special_pkt_ptr<Info>{}
              ? *--this->pspecial_pkt
              : this->buffers.data[Info::ibuf].iov_base;

            this->serialize_spe_type<Info>(
                proto::buffer_category<desc_type_t<new_val_type>>{},
                reinterpret_cast<uint8_t *>(buf),
                new_val.desc
            );
        }

        template<class Info, class Desc>
        void serialize_spe_type(proto::tags::static_buffer, uint8_t * p, Desc const & d) {
            this->policy.static_serialize(p, d);
        }

        template<class Info, class Desc>
        void serialize_spe_type(proto::tags::limited_buffer, uint8_t * p, Desc const & d) {
            this->inc_size<Info>(this->policy.limited_serialize(p, d));
        }

        template<class Info, class... Args>
        auto reserializer(proto::tags::static_buffer, Args && ... args) {
            this->policy.static_reserialize(args...);
        }

        template<class Info, class... Args>
        auto reserializer(proto::tags::limited_buffer, Args && ... args) {
            this->inc_size<Info>(this->policy.limited_reserialize(args...));
        }

        template<class Info>
        void inc_size(std::size_t n)
        {
            PROTO_TRACE(" [sz inc: " << n << "]");
            this->buffers.data[Info::ibuf].iov_len += n;
            for (std::size_t i = 0; i <= Info::ipacket; ++i) {
                this->sizes[i] += n;
            }
        }

        template<class... Infos, class DInfos>
        void serialize_(
            brigand::list<Infos...>,
            DInfos,
            Pkts const & ... pkts
        ) {
            PROTO_UNPACK(this->serialize_value(
                Infos{},
                larg<Infos::ivar>(arg<Infos::ipacket>(pkts...))
            ));

            this->serialize_dyn(DInfos{}, pkts...);
        }

        template<class Info, class... Infos>
        void serialize_dyn(
            brigand::list<Info, Infos...>,
            Pkts const & ... pkts
        ) {
#define PROTO_NIL
#ifndef NDEBUG
# define PROTO_ENABLE_IF_DEBUG(...) __VA_ARGS__
#else
# define PROTO_ENABLE_IF_DEBUG(...)
#endif
            auto const & val = larg<Info::ivar>(arg<Info::ipacket>(pkts...));
            this->pre_serialize_value(Info{}, val);
            PROTO_ENABLE_IF_DEBUG(int dynamic_buf_ctxfunc_is_used = 0;)
            [this, &val](auto f){
                this->policy.dynamic_serialize(f, val.desc);
            }(
                [this, PROTO_ENABLE_IF_DEBUG(&dynamic_buf_ctxfunc_is_used, PROTO_NIL) &pkts...]
                (auto && av) {
                    PROTO_ENABLE_IF_DEBUG(++dynamic_buf_ctxfunc_is_used;)
                    this->piov->iov_base = const_cast<void*>(cvoidp(av.data()));
                    this->piov->iov_len = av.size();
                    this->post_serialize_value(Info{});
                    this->serialize(brigand::list<Infos...>{}, pkts...);
                }
            );
            assert(dynamic_buf_ctxfunc_is_used == 1);
#undef PROTO_ENABLE_IF_DEBUG
#undef PROTO_NIL
        }

        void serialize_dyn(
            brigand::list<>,
            Pkts const & ... pkts
        ) {
            this->serialize(brigand::list<>{}, pkts...);
        }


        template<class Info, class Val>
        void pre_serialize_value(Info, Val const & val)
        {
            (void)val;
            PROTO_TRACE("i" << Info::i << ".b" << Info::ibuf << ".p" << Info::ipacket << ". ");
            PROTO_TRACE(name(val));
            PROTO_ENABLE_IF_TRACE(
                cifv(brigand::bool_<!has_special_pkt<Val>{}>{}, val, [this](auto const & v) {
                    PROTO_TRACE(" = ");
                    PROTO_ENABLE_IF_TRACE(this->print(v));
                })
            );
            PROTO_ENABLE_IF_TRACE(
                cifv(has_special_pkt<Val>{}, val, [](auto const &) {
                    PROTO_TRACE(" [spe] ");
                })
            );

            if (Info::is_begin_pkt) {
                using sizeof_packet = brigand::at_c<sizeof_by_packet, Info::ipacket>;
                using is_size = is_size_<sizeof_packet>;
                assert(std::size_t(this->psize - std::begin(this->sizes)) < this->sizes.size());
                if (is_size{}) {
                    cifv(is_size{}, sizeof_packet{}, [this](auto sz){
                        PROTO_TRACE(" [*psz: " << sz << "] ");
                        *this->psize = sz;
                    });
                }
                else {
                    cifv(
                        brigand::bool_<Info::is_begin_buf>{},
                        brigand::size_t<Info::ibuf>{},
                        [this](auto){
                            PROTO_TRACE(" [*psz: 0] ");
                            *this->psize = 0;
                        },
                        [this](auto ibuf){
                            using i = decltype(ibuf);
                            PROTO_TRACE(" [*psz: -" << (this->iov_base() - get<i::value>(this->buffer_tuple).buf) << "] ");
                            *this->psize = -(this->iov_base() - get<i::value>(this->buffer_tuple).buf);
                        }
                    );
                }
            }

            if (keep_info_pkt_data_ptr<Info>{}) {
                assert(std::size_t(this->ppkt_data_ptrs - std::begin(this->pkt_data_ptrs)) < this->pkt_data_ptrs.size());
                *this->ppkt_data_ptrs = this->piov->iov_base;
                PROTO_TRACE(" [spdata: " << cvoidp(*this->ppkt_data_ptrs) << "]");
                ++this->ppkt_data_ptrs;
            }

            if (keep_info_special_pkt_ptr<Info>{}) {
                assert(std::size_t(this->pspecial_pkt - std::begin(this->special_pkt_ptrs)) < this->special_pkt_ptrs.size());
                *this->pspecial_pkt = this->piov->iov_base;
                PROTO_TRACE(" [sp: " << cvoidp(*this->pspecial_pkt) << "]");
                ++this->pspecial_pkt;
            }

            assert(std::size_t(this->piov - buffers.data.data()) < this->buffers.data.size());
        }

        template<class Info, class Val>
        void serialize_value(Info info, Val const & val)
        {
            this->pre_serialize_value(info, val);
            this->serialize_type(
                info,
                has_special_pkt<Val>{},
                proto::buffer_category<desc_type_t<Info>>{},
                val
            );
            this->post_serialize_value(info);
            PROTO_TRACE("\n");
        }

        template<class Info>
        void post_serialize_value(Info)
        {
            using is_view = proto::is_view_buffer<desc_type_t<Info>>;
            using is_dyn = proto::is_dynamic_buffer<desc_type_t<Info>>;
            if (is_view{} || is_dyn{}) {
                PROTO_TRACE(" [*psz: +" << this->piov->iov_len << "] ");
                *this->psize += this->piov->iov_len;
            }

            if (Info::is_end_pkt || Info::is_end_buf) {
                using sizeof_packet = brigand::at_c<sizeof_by_packet, Info::ipacket>;
                using is_size = is_size_<sizeof_packet>;
                using is_limited = brigand::bool_<!is_size{} && !is_view{} && !is_dyn{}>;
                cifv(is_limited{}, i_<Info::ibuf>{}, [this](auto ibuf){
                    using i = decltype(ibuf);
                    auto const inc_sz = this->iov_base() - get<i::value>(this->buffer_tuple).buf;
                    PROTO_TRACE(" [*psz: +" << inc_sz << "] ");
                    *this->psize += inc_sz;
                });
            }

            if (Info::is_end_pkt) {
                PROTO_TRACE(" [++psz] ");
                ++this->psize;
            }

            if (Info::is_end_buf) {
                ++this->piov;
            }
        }

        template<class Info, class Val>
        void serialize_type(
            Info,
            std::false_type,
            proto::tags::static_buffer,
            Val const & val
        ) {
            policy.static_serialize(this->iov_base(), val.desc);
            constexpr std::size_t sz = proto::sizeof_<desc_type_t<Info>>::value;
            PROTO_TRACE(" [slen: " << sz << "]");
            this->piov->iov_base = this->iov_base() + sz;
        }

        template<class Info, class Val>
        void serialize_type(
            Info,
            std::false_type,
            proto::tags::limited_buffer,
            Val const & val
        ) {
            std::size_t const len = policy.limited_serialize(this->iov_base(), val.desc);
            PROTO_TRACE(" [len: " << len << "]");
            assert(proto::sizeof_<desc_type_t<Info>>::value >= len);
            this->piov->iov_base = this->iov_base() + len;
        }

        template<class Info, class Val>
        void serialize_type(
            Info,
            std::false_type,
            proto::tags::view_buffer,
            Val const & val
        ) {
            auto const av = policy.get_view_buffer(val.desc);
            auto * const data = static_cast<void const *>(av.data());
            PROTO_TRACE(" [view: 0x" << data << " | len: " << av.size() << "]");
            this->piov->iov_base = const_cast<void *>(data);
            this->piov->iov_len = av.size();
        }

        template<class Info, class Val>
        void serialize_type(
            Info,
            std::true_type,
            proto::tags::view_buffer,
            Val const & val
        ) {
            this->piov->iov_len = reserved_size(val);
            PROTO_TRACE(" [reserved: " << reserved_size(val) << "]");
        }

        template<class Info, class Val, class BufCat>
        void serialize_type(
            Info,
            std::true_type,
            BufCat,
            Val const & val
        ) {
            using pkt_sz = typename Info::sz;
            using pkt_sz_self = typename Info::sz_self;

            static_assert(has_pkt_sz<Val>{} && !is_size_<pkt_sz>{} && proto::is_limited_buffer<desc_type_t<Info>>{} ? Info::is_end_buf : true, "internal error to split_if");
            static_assert(has_pkt_sz_with_self<Val>{} && !is_size_<pkt_sz_self>{} && proto::is_limited_buffer<desc_type_t<Info>>{} ? Info::is_end_buf : true, "internal error to split_if");

            using is_pkt_sz = brigand::bool_<has_pkt_sz<Val>{} && is_size_<pkt_sz>{}>;
            using is_pkt_sz_self = brigand::bool_<has_pkt_sz_with_self<Val>{} && is_size_<pkt_sz_self>{}>;

            cifv(is_pkt_sz{}, val, [this](auto & val) {
                PROTO_TRACE(" [eval_sz]");
                this->serialize_type_reval_sz(
                    Info{}, val,
                    is_pkt_sz{}, pkt_sz{},
                    proto::dsl::pkt_sz{}
                );
            });
            cifv(is_pkt_sz_self{}, val, [this](auto & val) {
                PROTO_TRACE(" [eval_sz_with_self]");
                this->serialize_type_reval_sz(
                    Info{}, val,
                    is_pkt_sz_self{}, pkt_sz_self{},
                    proto::dsl::pkt_sz_with_self{}
                );
            });

            using is_undeterministic = is_undeterministic_sizeof_special<Val, pkt_sz_self, pkt_sz>;
            cifv(is_undeterministic{}, val, [this](auto &) {
                PROTO_TRACE(" [undeterministic reserve]");
            });

            using is_reservable = brigand::bool_<
                !is_pkt_sz{} and
                !is_pkt_sz_self{} and
                !is_undeterministic{}
            >;
            cifv(is_reservable{}, val, [this](auto & val) {
                auto const sz = reserved_size(val);
                PROTO_TRACE(" [reserved: " << sz << "]");
                this->piov->iov_base = this->iov_base() + sz;
            });
        }

        template<class Info, class Val, class Sz, class Sp>
        void serialize_type_reval_sz(
            Info info, Val const & val, std::true_type, Sz, Sp
        ) {
            static_assert(!proto::is_view_buffer<desc_type_t<Info>>{}, "unimplemented view_buffer + pkt_sz_cat");
            static_assert(!proto::is_dynamic_buffer<desc_type_t<Info>>{}, "unimplemented view_buffer + pkt_sz_cat");
            auto get_sz = []{ return Sz::value; };
            auto szval = proto::val<Sp, decltype(get_sz)>{get_sz};
            this->serialize_type(
                info,
                std::false_type{},
                proto::buffer_category<desc_type_t<Info>>{},
                val.desc.to_proto_value(proto::utils::make_parameters(szval))
            );
        }

        template<class Info, class Val, class Sz, class Sp>
        void serialize_type_reval_sz(
            Info, Val const &, std::false_type, Sz, Sp
        ) {
        }


        template<class T>
        static auto name(T const &)
        {
            using deps = proto::get_dependencies<T>;
            return proto::named_dep<deps>{};
        }


        template<class Var, class T>
        static void print(proto::val<Var, T> const & x)
        {
            PROTO_ENABLE_IF_TRACE(Printer::print(x.desc, 1));
            (void)x;
        }

        template<class T, class Derived>
        static void print(proto::var<proto::types::pkt_sz<T>, Derived>)
        {
            PROTO_TRACE("[pkt_sz]");
        }

        template<class T, class Derived>
        static void print(proto::var<proto::types::pkt_sz_with_self<T>, Derived>)
        {
            PROTO_TRACE("[pkt_sz_with_self]");
        }

        static void print_buffer_type(proto::tags::static_buffer)
        {
            PROTO_TRACE("[static_buffer]");
        }
        static void print_buffer_type(proto::tags::dynamic_buffer)
        {
            PROTO_TRACE("[dyn_buffer]");
        }
        static void print_buffer_type(proto::tags::view_buffer)
        {
            PROTO_TRACE("[view_buffer]");
        }
        static void print_buffer_type(proto::tags::limited_buffer)
        {
            PROTO_TRACE("[limited_buffer]");
        }
    };

    Policy policy;

    template<class... Packets>
    void operator()(Packets const & ... packets) const
    {
        Impl<Packets...> impl{this->policy};
        impl.impl(packets...);
    }
};

struct base_policy
{
    template<class T>
    static auto static_serialize(uint8_t * p, T const & val)
    {
        return val.static_serialize(p);
    }

    template<class T, class U, class... Sz>
    static auto static_reserialize(uint8_t * p, T const & val, array_view<U> av, Sz... sz)
    {
        return val.static_reserialize(p, av, sz...);
    }

    template<class T>
    static auto get_view_buffer(T const & val)
    {
        return val.get_view_buffer();
    }

    template<class T>
    static std::size_t limited_serialize(uint8_t * p, T const & val)
    {
        return val.limited_serialize(p);
    }

    template<class T, class U, class... Sz>
    static auto limited_reserialize(uint8_t * p, T const & val, array_view<U> av, Sz... sz)
    {
        return val.limited_reserialize(p, av, sz...);
    }

    template<class F, class T>
    static void dynamic_serialize(F && f, T const & val)
    {
        val.dynamic_serialize(f);
    }

    static void send(iovec_array iovs) = delete; // please, overrided this function
};

}

using buffering2_policy_base = proto_buffering2::base_policy;
using proto_buffering2::Buffering2;
template<std::size_t n>
using static_iovec_array = proto_buffering2::static_array_view<iovec_array, n>;
