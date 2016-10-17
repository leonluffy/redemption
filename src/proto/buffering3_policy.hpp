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

template<class Policy>
struct Buffering3
{
    template<class... Pkts>
    struct Impl
    {
        // [ [ val | pkt_sz_var ... ] ... ]
        using packet_list_ = brigand::list<brigand::transform<typename Pkts::type_list, brigand::call<desc_type_t>>...>;

        // [ { static | dynamic | limited }_size<n> ... ]
        using sizeof_by_packet = brigand::transform<packet_list_, brigand::call<sizeof_packet>>;

        // [ { static | dynamic | limited }_size<n> ... ] == [ Xsize<0..N>, Xsize<1..N> ... ]
        using accu_sizeof_by_packet = make_accumulate_sizeof_list<sizeof_by_packet>;

        // [ [ val | pkt_sz_var ... ] ... ]
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
            brigand::call<has_special_pkt>
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
            PROTO_TRACE("av: {" << static_cast<void const*>(this->av.data()) << ", " << this->av.size() << "}\n");
            PROTO_TRACE("special_pkt_ptrs.size: " << this->special_pkt_ptrs.size() << "\n\n");

            this->serialize_(
                var_info_list{},
                brigand::reverse<var_info_list>{},
                pkt_ptr_is_first_list{},
                packets...
            );
        }

        template<class... VarInfos, class... ReverseVarInfos, class... IsFirstPkts>
        void serialize_(
            brigand::list<VarInfos...>,
            brigand::list<ReverseVarInfos...>,
            brigand::list<IsFirstPkts...>,
            Pkts const & ... pkts
        ) {
            (void)std::initializer_list<int>{(void((
                this->serialize_without_special_pkt(
                    IsFirstPkts{},
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

        template<class Info>
        uint8_t * next_pkt_ptr_or_end()
        {
            return Info::ipacket::value + 1 == sizeof...(Pkts)
              ? this->buf
              : this->pkt_ptrs[Info::ipacket::value + 1];
        }

        template<class VarInfo, class Val>
        std::enable_if_t<has_special_pkt<Val>::value>
        serialize_special_pkt(VarInfo, Val const & val)
        {
            PROTO_TRACE(name(val) << " = ");

            --this->special_pkt_iterator;
            PROTO_TRACE("[" << static_cast<void const *>(*this->special_pkt_iterator) << "] ");

            cifv(has_pkt_sz<Val>{}, val, [this](auto const & val){
                this->serialize_eval_sz<VarInfo, proto::dsl::next_pkts_sz>([this]{
                    auto const sz = this->buf - this->next_pkt_ptr_or_end<VarInfo>();
                    PROTO_TRACE(sz);
                    return sz;
                }, val);
            });

            cifv(has_pkt_sz_with_self<Val>{}, val, [this](auto const & val){
                constexpr std::size_t ipacket = VarInfo::ipacket::value;
                this->serialize_eval_sz<VarInfo, proto::dsl::current_pkts_sz>([this]{
                    auto const sz = this->buf - this->pkt_ptrs[ipacket];
                    PROTO_TRACE(sz);
                    return sz;
                }, val);
            });

            cifv(has_pkt_data<Val>{}, val, [this](auto const & val){
                PROTO_TRACE("{ptr, " << this->buf - this->next_pkt_ptr_or_end<VarInfo>() << "}");
                this->reserializer(
                    proto::buffer_category<desc_type_t<VarInfo>>{},
                    *this->special_pkt_iterator,
                    val.desc,
                    array_view_u8{this->next_pkt_ptr_or_end<VarInfo>(), this->buf}
                );
            });
        }

        template<class VarInfo, class Sp, class Get, class Val>
        void serialize_eval_sz(Get get, Val const & val)
        {
            auto get_val = proto::val<Sp, decltype(get)>{get};
            this->serialize_type2(
                proto::buffer_category<desc_type_t<VarInfo>>{},
                *this->special_pkt_iterator,
                val.desc.to_proto_value(proto::utils::make_parameters(get_val)).desc
            );
        }

        template<class VarInfo, class Val>
        std::enable_if_t<!has_special_pkt<Val>::value>
        serialize_special_pkt(VarInfo, Val const &)
        {}

        template<class T>
        void reserializer(proto::tags::static_buffer, uint8_t * buf, T const & x, array_view_u8 av)
        {
            policy.static_reserialize(buf, x, av);
            PROTO_TRACE(" [slen: " << proto::sizeof_<T>::value << "]\n");
        }

        template<class T>
        void reserializer(proto::tags::limited_buffer, uint8_t * buf, T const & x, array_view_u8 av)
        {
            PROTO_ENABLE_IF_TRACE_PRE(std::size_t len = )
            policy.limited_reserialize(buf, x, av);
            PROTO_TRACE(" [len: " << len << "]\n");
        }

        template<class T>
        void serialize_type2(proto::tags::static_buffer, uint8_t * buf, T const & x)
        {
            policy.static_serialize(buf, x);
            PROTO_TRACE(" [slen: " << proto::sizeof_<T>::value << "]\n");
        }

        template<class T>
        void serialize_type2(proto::tags::limited_buffer, uint8_t * buf, T const & x)
        {
            PROTO_ENABLE_IF_TRACE_PRE(std::size_t len = )
            policy.limited_serialize(buf, x);
            PROTO_TRACE(" [len: " << len << "]\n");
        }

        template<class T>
        void serialize_type2(proto::tags::view_buffer, uint8_t * buf, T const & x)
        {
            auto av = policy.get_view_buffer(x);
            memcpy(buf, av.data(), av.size());
            PROTO_TRACE(" [view: 0x" << static_cast<void const *>(buf) << " | len: " << av.size() << "]\n");
        }

        template<class IsFirstPkt, class VarInfo, class Val>
        void serialize_without_special_pkt(IsFirstPkt is_first_pkt, VarInfo, Val const & val)
        {
            using is_special_value = has_special_pkt<Val>;
            print_if_not_special(is_special_value{}, val);
            if (is_first_pkt) {
                this->pkt_ptrs[VarInfo::ipacket::value] = this->buf;
            }
            // TODO check overflow (assert)
            this->serialize_type(
                std::conditional_t<
                    is_special_value::value,
                    special_op, proto::buffer_category<desc_type_t<VarInfo>>
                >{},
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
            *this->special_pkt_iterator = this->buf;
            ++this->special_pkt_iterator;
            this->buf += reserved_size(val);
        }

        template<class Val>
        void serialize_type(proto::tags::static_buffer, Val const & val)
        {
            policy.static_serialize(this->buf, val.desc);
            constexpr std::size_t sz = proto::sizeof_<desc_type_t<Val>>::value;
            PROTO_TRACE(" [slen: " << sz << "]\n");
            this->buf += sz;
        }

        template<class Val>
        void serialize_type(proto::tags::limited_buffer, Val const & val)
        {
            std::size_t len = policy.limited_serialize(this->buf, val.desc);
            PROTO_TRACE(" [len: " << len << "]\n");
            this->buf += len;
        }

        template<class Val>
        void serialize_type(proto::tags::view_buffer, Val const & val)
        {
            auto av = policy.get_view_buffer(val.desc);
            memcpy(this->buf, av.data(), av.size());
            PROTO_TRACE(" [view: 0x" << static_cast<void const *>(this->buf) << " | len: " << av.size() << "]\n");
            this->buf += av.size();
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

        template<class T>
        static void print(T const &)
        {
            PROTO_TRACE("[special]");
        }

        static void print_buffer_type(proto::tags::static_buffer)
        {
            PROTO_TRACE("[static_buffer]");
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