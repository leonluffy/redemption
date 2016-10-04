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

#include "proto/printer_policy.hpp"

namespace proto_buffering {

template<class T>
using is_buffer_delimiter = brigand::bool_<
    proto::is_dynamic_buffer<T>::value or
    proto::is_view_buffer<T>::value
>;

template<class idx_var>
using pair_is_buffer_delimiter = is_buffer_delimiter<typename idx_var::second_type>;

namespace detail {
    template<class T> struct arg_impl;
    template<class... Ts>
    struct arg_impl<brigand::list<Ts...>>
    {
        template<class T> static T * impl(Ts..., T * x, ...) { return x; }
    };
}

template<std::size_t i, class... T>
auto & arg(T & ... args)
{ return *detail::arg_impl<brigand::filled_list<void const *, i>>::impl(&args...); }

using proto::t_;

struct Buffering
{
    template<class... Val>
    void operator()(Val ... val) const {
        using iseq = brigand::range<size_t, 0, sizeof...(val)>;
        using list = brigand::list<proto::desc_type_t<Val>...>;
        using list_pair = brigand::transform<iseq, list, brigand::call<brigand::pair>>;
        using list_by_buffer = brigand::split_if<list_pair, brigand::call<pair_is_buffer_delimiter>>;

        brigand::for_each<list_by_buffer>([&val...](auto g) {
            std::cout << '[';
            brigand::for_each<t_<decltype(g)>>([&val...](auto v) {
                using pair_type = t_<decltype(v)>;
                auto & value = arg<pair_type::first_type::value>(val...);
                // TODO std::cout << value.name() << " = ";
                using deps = proto::get_dependencies<std::decay_t<decltype(value)>>;
                std::cout << proto::named_dep<deps>{} << " = ";
                print(value);
                std::cout << ", ";
            });
            std::cout << "]\n";
        });
    }

    template<class Var, class T>
    static void print(proto::val<Var, T> const & x)
    { Printer{}.print(x.desc, 1); }

    template<class T>
    static void print(T const &)
    // TODO { std::cout << x.name(); }
    { std::cout << "<name>"; }
};

}

using proto_buffering::Buffering;
