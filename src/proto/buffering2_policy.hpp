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

template<std::size_t n>
using mk_seq = brigand::range<std::size_t, 0, n>;

template<class n>
using mk_seq2 = brigand::range<std::size_t, 0, n::value>;

template<class I, class Size>
using mk_filled_list = brigand::filled_list<I, Size::value>;

template<class IPacket, class IVar, class DescType>
struct var_info {
    using ipacket = IPacket;
    using ivar = IVar;
    using desc_type = DescType;
};

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


using proto::desc_type_t;
using proto::var_type_t;

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
            PROTO_TRACE("dyn_size\n");
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

template<class T>
using var_to_desc_type = desc_type_t<proto::var_type_t<T>>;

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
        { return val.reserved_size(); }
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


struct special_op {};

template<class Policy>
struct Buffering2
{
    template<class... Pkts>
    struct Impl
    {
        // [ [ val | pkt_sz_var ... ] ... ]
        using packet_list_ = brigand::list<brigand::transform<typename Pkts::type_list, brigand::call<var_to_desc_type>>...>;

        // [ { static | dynamic | limited }_size<n> ... ]
        using sizeof_by_packet = brigand::transform<packet_list_, brigand::call<sizeof_packet>>;

        // [ { static | dynamic | limited }_size<n> ... ] == [ Xsize<0..N>, Xsize<1..N> ... ]
        using accu_sizeof_by_packet = make_accumulate_sizeof_list<sizeof_by_packet>;

        // [ [ val | pkt_sz_var | static_value ... ] ... ]
        using packet_list = brigand::transform<
            packet_list_,
            accu_sizeof_by_packet,
            brigand::push_back<brigand::pop_front<accu_sizeof_by_packet>, proto::size_<0>>,
            brigand::call<convert_pkt_sz2 /*TODO*/>
        >;

        // [ size<packet> ... ]
        using packet_count_list = brigand::transform<packet_list, brigand::call<brigand::size>>;

        // [ filled_list<ipacket, size<packet>> ... ]
        using ipacket_list_by_var = brigand::transform<mk_seq<sizeof...(Pkts)>, packet_count_list, brigand::call<mk_filled_list>>;

        // flatten<ipacket_list_by_var>
        using ipacket_list = brigand::wrap<ipacket_list_by_var, brigand::append>;

        // flatten<packet_list>
        using var_list = brigand::wrap<packet_list, brigand::append>;

        // flatten<range<0, size<packet> ... >
        using ivar_list = brigand::wrap<brigand::transform<packet_count_list, brigand::call<mk_seq2>>, brigand::append>;

        // [ var_info<ipacket, ivar, var> ... ]
        using var_info_list = brigand::transform<ipacket_list, ivar_list, var_list, brigand::call<var_info>>;


        using count_special_pkt = brigand::count_if<
            brigand::append<typename Pkts::type_list...>,
            brigand::call<proto::is_special_value>
        >;

        using pkt_ptr_is_first_list = brigand::wrap<
            brigand::transform<ipacket_list_by_var, brigand::call<to_is_pkt_first_list>>,
            brigand::append
        >;

        // [ [ var_info ... ] ... ]
        using var_info_list_by_buffer = brigand::split_if<var_info_list, brigand::call<var_info_is_buffer_delimiter>>;


        // [ uninitialized_buf | dyn_size ... ]
        using buffer_list = brigand::transform<var_info_list_by_buffer, brigand::call<buffer_from_var_infos>>;

        // [ { static | dynamic | limited }_size<n> ... ]
        //using default_buffer_size = brigand::transform<packet_list_, brigand::call<sizeof_packet_with_limited_size_to_dyn_size>>;

        // var_info_list with only pkt_sz
//         using pkt_sz_list = brigand::copy_if<
//             var_info_list,
//             brigand::bind<
//                 proto::is_pkt_sz_category,
//                 brigand::call<proto::desc_type_t>
//             >
//         >;

        brigand::wrap<buffer_list, tuple_buf> buffer_tuple;
        detail::Buffers<brigand::size<buffer_list>::value> buffers{buffer_tuple};

        using special_ptr_array_t = std::array<uint8_t *, count_special_pkt::value>;
        using pkt_size_array_t = std::array<std::size_t, sizeof...(Pkts) + 1>;
        using pkt_ptr_array_t = std::array<std::size_t, sizeof...(Pkts)>;

        special_ptr_array_t special_pkt_ptrs;
        pkt_size_array_t sizes;
        pkt_ptr_array_t pkt_ptrs;
        Policy const & policy;
        typename special_ptr_array_t::iterator special_pkt_iterator;
        typename pkt_size_array_t::iterator psize;
        typename pkt_ptr_array_t::iterator ppkt_ptrs;
        iovec * piov;

        Impl(Policy const & policy) noexcept
        : policy(policy)
        , special_pkt_iterator(std::begin(this->special_pkt_ptrs))
        , psize(std::begin(sizes))
        , ppkt_ptrs(std::begin(pkt_ptrs))
        , piov(buffers.data.data())
        {
          this->pkt_ptrs.back() = 0;
        }

        uint8_t * iov_base() const noexcept { return static_cast<uint8_t*>(this->piov->iov_base); }

        void impl(Pkts const & ... packets)
        {
            PROTO_TRACE("----------- pkt (" << sizeof...(Pkts) << ") -----------\n");
            PROTO_TRACE("--------- bufsz (" << brigand::size<buffer_list>::value << ") -----------\n");

            this->serialize(
                var_info_list_by_buffer{},
                pkt_ptr_is_first_list{},
                packets...
            );
        }

        template<class VarInfoByBuffer, class... VarInfoByBuffers, class IsFirstPkts>
        void serialize(
            brigand::list<VarInfoByBuffer, VarInfoByBuffers...>,
            IsFirstPkts,
            Pkts const & ... pkts
        ) {
            using sz = brigand::size<VarInfoByBuffer>;
            this->serialize_without_special_pkt(
                brigand::list<VarInfoByBuffers...>{},
                brigand::drop<IsFirstPkts, sz>{},
                proto::is_dynamic_buffer<desc_type_t<brigand::front<VarInfoByBuffer>>>{},
                VarInfoByBuffer{},
                brigand::take<IsFirstPkts, sz>{},
                pkts...
            );
        }

        void serialize(brigand::list<>, brigand::list<>, Pkts const & ... pkts) {
            PROTO_TRACE("----------- special (" << count_special_pkt::value << ") -----------\n");

            constexpr std::size_t ibuf = brigand::size<var_info_list_by_buffer>::value - 1;
            --this->piov;
            PROTO_TRACE(" [--piov]\n");
            PROTO_TRACE(" [ e " << this->piov->iov_base << "]");
            this->inc_size<ibuf>(proto::is_view_buffer<desc_type_t<brigand::back<var_info_list>>>{});
            *++this->psize = 0;
            PROTO_TRACE("\nsizes: ");
            PROTO_ENABLE_IF_TRACE(for (auto sz : this->sizes) PROTO_TRACE(sz << " "));
            PROTO_TRACE("\n\n");

            this->buffers.reset_ptr(this->buffer_tuple);

            // propagate_size
            // TODO to serialize_special_pkt_
            {
                std::size_t i = sizeof...(Pkts) - 1;
                while (i-- > 0) {
                    this->sizes[i] += this->sizes[i+1];
                }
            }

            this->serialize_special_pkt(
                brigand::reverse<var_info_list_by_buffer>{},
                brigand::reverse<mk_seq2<brigand::size<var_info_list_by_buffer>>>{},
                pkts...
            );

            PROTO_TRACE("----------- send -----------\n");

            this->policy.send(this->buffers.view());
        }

        template<class... ReverseVarInfosByBuffers, class... IBuf>
        void serialize_special_pkt(
            brigand::list<ReverseVarInfosByBuffers...>,
            brigand::list<IBuf...>,
            Pkts const & ... pkts
        ) {
            (void)std::initializer_list<int>{(void((
                this->serialize_special_pkt2(
                    IBuf{},
                    brigand::reverse<ReverseVarInfosByBuffers>{},
                    pkts...
                )
            )), 1)...};
            PROTO_TRACE("\n");
        }

        template<class IBuf, class... ReverseVarInfos>
        void serialize_special_pkt2(
            IBuf ibuf,
            brigand::list<ReverseVarInfos...>,
            Pkts const & ... pkts
        ) {
            (void)std::initializer_list<int>{(void((
                this->serialize_special_pkt_(
                    ibuf,
                    ReverseVarInfos{},
                    larg<ReverseVarInfos::ivar::value>(arg<ReverseVarInfos::ipacket::value>(pkts...))
                )
            )), 1)...};
            PROTO_TRACE("\n");
        }

        template<std::size_t ipacket>
        struct lazy_sz
        {
            Impl const & impl;

            std::size_t operator()() const
            {
                auto const sz = impl.sizes[ipacket+1];
                PROTO_TRACE(sz);
                return sz;
            }
        };

        template<std::size_t ipacket>
        struct lazy_sz_with_self
        {
            Impl const & impl;

            std::size_t operator()() const
            {
                auto const sz = impl.sizes[ipacket];
                PROTO_TRACE(sz);
                return sz;
            }
        };

        template<std::size_t ibuf>
        struct lazy_data
        {
            Impl const & impl;

            iovec_array operator()() const
            {
                iovec_array av{
                    &impl.buffers.data[ibuf],
                    impl.buffers.data.data() + impl.buffers.data.size()
                };
                PROTO_TRACE("{ptr, " << av.size() << "}");
                return av;
            }
        };

        template<class IBuf, class VarInfo, class Val>
        std::enable_if_t<proto::is_special_value<Val>::value>
        serialize_special_pkt_(IBuf ibuf, VarInfo, Val const & val)
        {
            PROTO_TRACE(name(val) << " = ");

            void * oldp;

            constexpr std::size_t ipacket = VarInfo::ipacket::value;
            auto l1 = proto::val<proto::dsl::pkt_sz, lazy_sz<ipacket>>{{}, {*this}};
            auto l2 = proto::val<proto::dsl::pkt_data, lazy_data<IBuf::value>>{{}, {*this}};
            auto l3 = proto::val<proto::dsl::pkt_sz_with_self, lazy_sz_with_self<ipacket>>{{}, {*this}};

            if (proto::is_reserializer<Val>::value) {
                oldp = this->buffers.data[ibuf].iov_base;
                this->buffers.data[ibuf].iov_base
                  = reinterpret_cast<uint8_t *>(this->buffers.data[ibuf].iov_base) + this->pkt_ptrs[ipacket];
            }

            --this->special_pkt_iterator;
            PROTO_TRACE("[" << static_cast<void const *>(*this->special_pkt_iterator) << "] ");

            this->serialize_type2(
                proto::buffer_category<desc_type_t<VarInfo>>{},
                proto::is_reserializer<desc_type_t<VarInfo>>{},
                *this->special_pkt_iterator,
                val.to_proto_value(proto::utils::make_parameters(l1, l2, l3))
            );
            if (proto::is_reserializer<Val>::value) {
                this->buffers.data[ibuf].iov_base = oldp;
            }
        }

        template<class IBuf, class VarInfo, class Val>
        std::enable_if_t<!proto::is_special_value<Val>::value>
        serialize_special_pkt_(IBuf, VarInfo, Val const &)
        {}

        template<class T>
        void serialize_type2(proto::tags::static_buffer, std::false_type, unsigned char * buf, T const & x)
        {
            policy.static_serialize(buf, x);
            PROTO_TRACE(" [slen: " << proto::sizeof_<T>::value << "]\n");
        }

        template<class T>
        void serialize_type2(proto::tags::static_buffer, std::true_type, unsigned char * buf, T const & x)
        {
            policy.static_reserialize(buf, x, array_view_u8{buf, proto::sizeof_<T>{}});
            PROTO_TRACE(" [slen: " << proto::sizeof_<T>::value << "]\n");
        }

        template<class T>
        void serialize_type2(proto::tags::limited_buffer, std::false_type, unsigned char * buf, T const & x)
        {
            std::size_t len = policy.limited_serialize(buf, x);
            PROTO_TRACE(" [len: " << len << "]\n");
        }

        template<class T>
        void serialize_type2(proto::tags::limited_buffer, std::true_type, unsigned char * buf, T const & x)
        {
            // TODO
            //std::size_t len = policy.limited_reserialize(buf, x, array_view_u8{*this->special_pkt_iterator, proto::sizeof_<T>::value});
            //PROTO_TRACE(" [len: " << len << "]\n");
        }

        template<class T>
        void serialize_type2(proto::tags::dynamic_buffer, std::false_type, unsigned char * buf, T const & x) = delete;


        template<class VarInfoByBuffers, class IsFirstPkts2, class... VarInfos, class... IsFirstPkts>
        void serialize_without_special_pkt(
            VarInfoByBuffers var_info_by_buffers,
            IsFirstPkts2 is_first_pkt2,
            std::false_type,
            brigand::list<VarInfos...>,
            brigand::list<IsFirstPkts...>,
            Pkts const & ... pkts
        ) {
            constexpr std::size_t ibuf = brigand::size<var_info_list_by_buffer>::value - brigand::size<VarInfoByBuffers>::value - 1;
            using first_var_info = brigand::front<brigand::list<VarInfos...>>;
            (void)std::initializer_list<int>{(void((
                this->serialize_not_dynamic<ibuf>(
                    IsFirstPkts{},
                    brigand::bool_<first_var_info::ipacket::value == VarInfos::ipacket::value>{},
                    VarInfos{},
                    larg<VarInfos::ivar::value>(arg<VarInfos::ipacket::value>(pkts...))
                )
            )), 1)...};

            PROTO_TRACE(" [++piov]\n");
            ++this->piov;
            this->serialize(var_info_by_buffers, is_first_pkt2, pkts...);
        }

        template<std::size_t i>
        void inc_size(std::true_type)
        {
            PROTO_TRACE(" [v *psize+=" << this->piov->iov_len << "]");
            *this->psize += this->piov->iov_len;
        }

        template<std::size_t i>
        void inc_size(std::false_type)
        {
            PROTO_TRACE(" [ b " << static_cast<void const *>(get<i>(this->buffer_tuple).buf) << "]");
            PROTO_TRACE(" [ e " << this->piov->iov_base << "]");
            PROTO_TRACE(" [r *psize+=" << std::size_t(this->iov_base() - get<i>(this->buffer_tuple).buf) << "]");
            *this->psize += this->iov_base() - get<i>(this->buffer_tuple).buf;
        }

        template<std::size_t ibuf, class IsFirstPkt, class IsSamePkt, class VarInfo, class Val>
        void serialize_not_dynamic(IsFirstPkt is_first_pkt, IsSamePkt is_same_pkt, VarInfo, Val const & val)
        {
            using is_special_value = proto::is_special_value<Val>;
            print_if_not_special(is_special_value{}, val);
            if (is_first_pkt) {
                if (VarInfo::ipacket::value) {
                    this->inc_size<ibuf>(proto::is_view_buffer<desc_type_t<VarInfo>>{});
                    PROTO_TRACE(" [++psize]");
                    *this->ppkt_ptrs = *this->psize;
                    ++this->ppkt_ptrs;
                    ++this->psize;
                }

                if (is_same_pkt || proto::is_view_buffer<var_to_desc_type<Val>>::value) {
                    PROTO_TRACE(" [*psize=0]");
                    *this->psize = 0u;
                }
                else {
                    *this->psize = -*(this->psize - 1);
                    PROTO_TRACE(" [*psize=" << *this->psize << "]");
                }
            }
            // TODO check overflow (assert)
            this->serialize_type(
                typename std::conditional<
                    is_special_value::value,
                    special_op, proto::buffer_category<desc_type_t<VarInfo>>
                >::type{},
                val
            );
        }

        template<class Val>
        static void print_if_not_special(std::false_type, Val const & val)
        {
            PROTO_TRACE(name(val) << " = ");
            PROTO_ENABLE_IF_TRACE(print(val));
            (void)val;
        }

        template<class Val>
        static void print_if_not_special(std::true_type, Val const &)
        {}

        template<class Val>
        void serialize_type(special_op, Val const & val)
        {
            static_assert(!proto::is_dynamic_buffer<var_to_desc_type<Val>>{}, "unimplemented");
            static_assert(!proto::is_view_buffer<var_to_desc_type<Val>>{}, "unimplemented");

            // TODO is_pkt_sz + static_size -> serialize

            *this->special_pkt_iterator = this->iov_base();
            PROTO_TRACE(" [spep=" << static_cast<void const *>(*this->special_pkt_iterator) << "]");
            ++this->special_pkt_iterator;

            PROTO_TRACE(name(val) << " [reserved: " << reserved_size(val) << "]\n");
            if (proto::is_static_buffer<var_to_desc_type<Val>>{}
             || proto::is_limited_buffer<var_to_desc_type<Val>>{}
            ) {
                this->piov->iov_base = this->iov_base() + reserved_size(val);
            }
            else {
                // TODO only if pkt_sz before
                this->piov->iov_len = reserved_size(val);
            }
        }

        template<class Val>
        void serialize_type(proto::tags::static_buffer, Val const & val)
        {
            PROTO_ENABLE_IF_TRACE_PRE(std::size_t rsz = ) policy.static_serialize(this->iov_base(), val.x);
            constexpr std::size_t sz = proto::sizeof_<desc_type_t<var_type_t<Val>>>::value;
            PROTO_TRACE(" [slen: " << sz << "]\n");
            PROTO_ENABLE_IF_TRACE(assert(rsz == sz));
            this->piov->iov_base = this->iov_base() + sz;
        }

        template<class Val>
        void serialize_type(proto::tags::limited_buffer, Val const & val)
        {
            std::size_t len = policy.limited_serialize(this->iov_base(), val.x);
            PROTO_TRACE(" [len: " << len << "]\n");
            assert(proto::sizeof_<desc_type_t<var_type_t<Val>>>::value >= len);
            this->piov->iov_base = this->iov_base() + len;
        }

        template<class Val>
        void serialize_type(proto::tags::view_buffer, Val const & val)
        {
            auto av = policy.get_view_buffer(val.x);
            PROTO_TRACE(" [view: 0x" << static_cast<void const *>(av.data()) << " | len: " << av.size() << "]\n");
            this->piov->iov_base = const_cast<uint8_t *>(av.data());
            this->piov->iov_len = av.size();
        }

        template<class VarInfoByBuffers, class IsFirstPkts2, class VarInfo, class IsFirstPkt>
        void serialize_without_special_pkt(
            VarInfoByBuffers,
            IsFirstPkts2,
            std::true_type,
            brigand::list<VarInfo>,
            brigand::list<IsFirstPkt>,
            Pkts const & ... pkts
        ) {
#define PROTO_NIL
#ifndef NDEBUG
# define PROTO_ENABLE_IF_DEBUG(...) __VA_ARGS__
#else
# define PROTO_ENABLE_IF_DEBUG(...)
#endif
            using var_info = brigand::front<VarInfo>;
            PROTO_ENABLE_IF_DEBUG(int dynamic_buf_ctxfunc_is_used = 0;)
            [this](auto & val, auto f){
                PROTO_TRACE(name(val) << " = ");
                PROTO_ENABLE_IF_TRACE(print(val));
                this->policy.dynamic_serialize(f, val.x);
            }(
                larg<var_info::ivar::value>(arg<var_info::ipacket::value>(pkts...)),
                [this, PROTO_ENABLE_IF_DEBUG(&dynamic_buf_ctxfunc_is_used, PROTO_NIL) &pkts...]
                (array_view_const_u8 av) {
                    PROTO_ENABLE_IF_DEBUG(++dynamic_buf_ctxfunc_is_used;)
                    this->piov->iov_base = const_cast<uint8_t *>(av.data());
                    this->piov->iov_len = av.size();
                    if (IsFirstPkt{}) {
                        if (VarInfo::ipacket::value) {
                            PROTO_TRACE(" [++psize]");
                            ++this->psize;
                        }
                        PROTO_TRACE(" [*psize=0]");
                        *this->psize = 0;
                    }
                    PROTO_TRACE(" [size: " << av.size() << "]");
                    PROTO_TRACE(" [*psize+=this->piov->iov_len]");
                    PROTO_TRACE("\n");

                    *this->psize += this->piov->iov_len;
                    ++this->piov;
                    this->serialize(VarInfoByBuffers{}, IsFirstPkts2{}, pkts...);
                }
            );
            assert(dynamic_buf_ctxfunc_is_used == 1);
#undef PROTO_ENABLE_IF_DEBUG
#undef PROTO_NIL
        }

        template<class Var, class T>
        static auto name(proto::val<Var, T> const & val)
        { return val.var.name(); }

        template<class T, class Derived>
        static char const * name(proto::var<proto::types::pkt_sz<T>, Derived>)
        { return Derived::name(); }

        template<class T, class Derived>
        static char const * name(proto::var<proto::types::pkt_sz_with_self<T>, Derived>)
        { return Derived::name(); }

        template<class T>
        static auto name(T const & val)
        { return val.name(); }


        template<class Var, class T>
        static void print(proto::val<Var, T> const & x)
        {
            PROTO_ENABLE_IF_TRACE(Printer::print(x.x, 1));
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

    template<class T>
    static auto static_reserialize(uint8_t * p, T const & val, array_view_u8 av)
    {
        return val.static_reserialize(p, av);
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

    template<class T>
    static auto limited_reserialize(uint8_t * p, T const & val, array_view_u8 av)
    {
        return val.limited_reserialize(p, av);
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
