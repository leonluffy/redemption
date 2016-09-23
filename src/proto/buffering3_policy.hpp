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

namespace proto_buffering3 {

using namespace proto_buffering2;

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

namespace detail {
    template<template<class> class IsPktSz, class Pkt, class Sz>
    struct convert_pkt_sz2
    { using type = Pkt; };

    template<template<class> class IsPktSz, class... Ts, std::size_t n>
    struct convert_pkt_sz2<IsPktSz, brigand::list<Ts...>, proto::size_<n>>
    { using type = brigand::list<std::conditional_t<IsPktSz<Ts>{}, proto::types::static_value<Ts, n>, Ts>...>; };
}
// TODO
template<class Pkt, class Sz, class SzNext>
using convert_pkt_sz2 = typename detail::convert_pkt_sz<
    proto::is_pkt_sz_with_self,
    typename detail::convert_pkt_sz<proto::is_pkt_sz, Pkt, SzNext>::type,
    Sz
>::type;

template<class L>
using to_is_pkt_first_list = typename detail::to_is_pkt_first_list<L>::type;

struct special_op {};

template<class Policy>
struct Buffering3
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

        // [ [ val | pkt_sz_var | pkt_sz_with_size ... ] ... ]
        using packet_list = brigand::transform<
            packet_list_,
            accu_sizeof_by_packet,
            brigand::push_back<brigand::pop_front<accu_sizeof_by_packet>, proto::size_<0>>,
            brigand::call<convert_pkt_sz2>
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

        using special_ptr_array_t = std::array<uint8_t *, count_special_pkt::value>;
        std::array<uint8_t *, brigand::size<packet_list>::value> pkt_ptrs;
        special_ptr_array_t special_pkt_ptrs;
        Policy const & policy;
        array_view_u8 av;
        uint8_t * buf;
        typename special_ptr_array_t::iterator special_pkt_iterator;

        Impl(Policy const & policy, array_view_u8 av) noexcept
        : policy(policy)
        , av(av)
        , buf(av.data())
        , special_pkt_iterator(std::begin(this->special_pkt_ptrs))
        {}

        void impl(Pkts const & ... packets)
        {
            PROTO_TRACE("special_pkt_ptrs.size: " << this->special_pkt_ptrs.size() << "\n\n");

            this->serialize_(
                var_info_list{},
                brigand::reverse<var_info_list>{},
                pkt_ptr_is_first_list{},
                packets...
            );
        }

        template<class... VarInfos, class... ReverseVarInfos, class... IsFirstPkt>
        void serialize_(
            brigand::list<VarInfos...>,
            brigand::list<ReverseVarInfos...>,
            brigand::list<IsFirstPkt...>,
            Pkts const & ... pkts
        ) {
            (void)std::initializer_list<int>{(void((
                this->serialize_without_special_pkt(
                    IsFirstPkt{},
                    VarInfos{},
                    larg<VarInfos::ivar::value>(arg<VarInfos::ipacket::value>(pkts...))
                )
            )), 1)...};

            PROTO_TRACE("----------- special (" << count_special_pkt::value << ") -----------\n");

            (void)std::initializer_list<int>{(void((
                this->serialize_special_pkt(
                    ReverseVarInfos{},
                    larg<ReverseVarInfos::ivar::value>(arg<ReverseVarInfos::ipacket::value>(pkts...))
                )
            )), 1)...};

            PROTO_TRACE("----------- send -----------\n");

            this->policy.send(array_view_u8{this->av.data(), std::size_t(this->buf-this->av.data())});
        }

        template<std::size_t ipacket>
        struct lazy_sz
        {
            Impl const & impl;

            std::size_t operator()() const
            {
                auto const sz = impl.buf - impl.pkt_ptrs[ipacket+1];
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
                auto const sz = impl.buf - impl.pkt_ptrs[ipacket];
                PROTO_TRACE(sz);
                return sz;
            }
        };

        template<std::size_t ipacket>
        struct lazy_data
        {
            Impl const & impl;

            array_view_u8 operator()() const
            {
                PROTO_TRACE("{ptr, " << impl.buf - impl.pkt_ptrs[ipacket+1] << "}");
                return array_view_u8{impl.pkt_ptrs[ipacket+1], impl.buf};
            }
        };

        template<class VarInfo, class Val>
        std::enable_if_t<proto::is_special_value<Val>::value>
        serialize_special_pkt(VarInfo, Val const & val)
        {
            PROTO_TRACE(name(val) << " = ");

            constexpr std::size_t ipacket = VarInfo::ipacket::value;
            auto l1 = proto::val<proto::dsl::pkt_sz, lazy_sz<ipacket>>{{}, {{*this}}};
//             proto::val<proto::dsl::pkt_data, lazy_data<ipacket>> l2{{}, {*this}};
//             proto::val<proto::dsl::pkt_sz_with_self, lazy_sz_with_self<ipacket>> l3{{}, {*this}};

            this->serialize_type2(
                proto::buffer_category<desc_type_t<VarInfo>>{},
                *--this->special_pkt_iterator,
                val.to_proto_value(proto::utils::make_parameters(l1/*, l2, l3*/))
            );
        }

        template<class VarInfo, class Val>
        std::enable_if_t<!proto::is_special_value<Val>::value>
        serialize_special_pkt(VarInfo, Val const &)
        {}

        template<class T>
        void serialize_type2(proto::tags::static_buffer, unsigned char * buf, T const & x)
        {
            policy.static_serialize(buf, x);
            PROTO_TRACE(" [slen: " << proto::sizeof_<T>::value << "]\n");
        }

        template<class T>
        void serialize_type2(proto::tags::limited_buffer, unsigned char * buf, T const & x)
        {
            std::size_t len = policy.limited_serialize(buf, x);
            PROTO_TRACE(" [len: " << len << "]\n");
        }

        template<class T>
        void serialize_type2(proto::tags::view_buffer, unsigned char * buf, T const & x)
        {
            auto av = policy.get_view_buffer(x);
            memcpy(buf, av.data(), av.size());
            PROTO_TRACE(" [view: 0x" << static_cast<void const *>(buf) << " | len: " << av.size() << "]\n");
        }

# define PROTO_NIL
#ifndef NDEBUG
# define PROTO_ENABLE_IF_DEBUG(...) __VA_ARGS__
#else
# define PROTO_ENABLE_IF_DEBUG(...)
#endif
        template<class T>
        void serialize_type2(proto::tags::dynamic_buffer, unsigned char * buf, T const & x)
        {
            PROTO_ENABLE_IF_DEBUG(bool dynamic_is_used = false;)
            // PERFORMANCE or limited_serialize (policy rule)
            this->policy.dynamic_serialize(
                [buf, this PROTO_ENABLE_IF_DEBUG(PROTO_NIL, &dynamic_is_used)]
                (array_view_const_u8 av) {
                    PROTO_ENABLE_IF_DEBUG(dynamic_is_used = true;)
                    memcpy(buf, av.data(), av.size());
                    PROTO_TRACE(" [size: " << av.size() << "]");
                    PROTO_TRACE("\n");
                },
                x
            );
            assert(dynamic_is_used);
        }
#undef PROTO_ENABLE_IF_DEBUG
#undef PROTO_NIL

        template<class IsFirstPkt, class VarInfo, class Val>
        void serialize_without_special_pkt(IsFirstPkt is_first_pkt, VarInfo var_info, Val const & val)
        {
            constexpr bool is_special_value = proto::is_special_value<Val>::value;
            if (!is_special_value) {
                PROTO_TRACE(name(val) << " = ");
                PROTO_ENABLE_IF_TRACE(this->print(val));
            }
            if (is_first_pkt) {
                this->pkt_ptrs[VarInfo::ipacket::value] = this->buf;
            }
            // TODO check overflow (assert)
            this->serialize_type(
                typename std::conditional<
                    is_special_value,
                    special_op, proto::buffer_category<desc_type_t<VarInfo>>
                >::type{},
                var_info,
                val
            );
        }

        template<class VarInfo, class Val>
        void serialize_type(special_op, VarInfo, Val const &)
        {
            *this->special_pkt_iterator = this->buf;
            ++this->special_pkt_iterator;
            // TODO policy.reserved_size(val.x);
            this->buf += proto::sizeof_<desc_type_t<VarInfo>>{};
        }

        template<class VarInfo, class Val>
        void serialize_type(proto::tags::static_buffer, VarInfo, Val const & val)
        {
            policy.static_serialize(this->buf, val.x);
            constexpr std::size_t sz = proto::sizeof_<desc_type_t<VarInfo>>::value;
            PROTO_TRACE(" [slen: " << sz << "]\n");
            this->buf += sz;
        }

        template<class VarInfo, class Val>
        void serialize_type(proto::tags::limited_buffer, VarInfo, Val const & val)
        {
            std::size_t len = policy.limited_serialize(this->buf, val.x);
            PROTO_TRACE(" [len: " << len << "]\n");
            this->buf += len;
        }

        template<class VarInfo, class Val>
        void serialize_type(proto::tags::view_buffer, VarInfo, Val const & val)
        {
            auto av = policy.get_view_buffer(val.x);
            memcpy(this->buf, av.data(), av.size());
            PROTO_TRACE(" [view: 0x" << static_cast<void const *>(this->buf) << " | len: " << av.size() << "]\n");
            this->buf += av.size();
        }

# define PROTO_NIL
#ifndef NDEBUG
# define PROTO_ENABLE_IF_DEBUG(...) __VA_ARGS__
#else
# define PROTO_ENABLE_IF_DEBUG(...)
#endif
        template<class VarInfo, class Val>
        void serialize_type(proto::tags::dynamic_buffer, VarInfo, Val const & val)
        {
            PROTO_ENABLE_IF_DEBUG(bool dynamic_is_used = false;)
            // PERFORMANCE or limited_serialize (policy rule)
            this->policy.dynamic_serialize(
                [this PROTO_ENABLE_IF_DEBUG(PROTO_NIL, &dynamic_is_used)]
                (array_view_const_u8 av) {
                    PROTO_ENABLE_IF_DEBUG(dynamic_is_used = true;)
                    memcpy(this->buf, av.data(), av.size());
                    this->buf += av.size();
                    PROTO_TRACE(" [size: " << av.size() << "]");
                    PROTO_TRACE("\n");
                },
                val.x
            );
            assert(dynamic_is_used);
        }
#undef PROTO_ENABLE_IF_DEBUG
#undef PROTO_NIL


        template<class Var, class T>
        auto name(proto::val<Var, T> const & val)
        { return val.var.name(); }

        template<class T, class Derived>
        static char const * name(proto::var<proto::types::pkt_sz<T>, Derived>)
        { return Derived::name(); }

        template<class T, class Derived>
        static char const * name(proto::var<proto::types::pkt_sz_with_self<T>, Derived>)
        { return Derived::name(); }

        template<class T>
        auto name(T const & val)
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

        template<class T>
        static void print(T const &)
        {
            PROTO_TRACE("[special]");
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
    array_view_u8 av;

    template<class... Packets>
    void operator()(Packets const & ... packets) const
    {
        Impl<Packets...> impl{this->policy, this->av};
        impl.impl(packets...);
    }
};

}

using proto_buffering3::Buffering3;
