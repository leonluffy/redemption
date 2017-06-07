// Copyright (c) 2015 Jonathan Poelen
// Distributed under the MIT License.
// (See accompanying file LICENSE.md)

#ifndef FALCON_PARSE_NUMBER_PARSE_NUMBER_HPP
#define FALCON_PARSE_NUMBER_PARSE_NUMBER_HPP

#include "digit.hpp"

namespace falcon {
namespace parse_number {

namespace detail_ {
  template<unsigned long long Val, unsigned Base, char Dig, char... Digs>
  struct parse_number_impl;
}

/// \brief Base is 2, 8, 10 or 16
template<unsigned Base, char... Digs>
using parse_number = typename detail_::parse_number_impl<0u, Base, Digs...>::type;


namespace detail_ {
  template<unsigned long long Val, unsigned Base, char Dig, char... Digs>
  struct parse_number_impl
  {
    using digit_ = digit<Base, Dig>;
    static constexpr unsigned power = digit_::valid::value ? Base : 1u;
    static_assert(Val * power / power == Val,
                  "integer literal does not fit in unsigned long long");
    using type = typename parse_number_impl<
      Val * power + digit_::value, Base, Digs...
    >::type;
  };

  template<unsigned long long Val, unsigned Base, char Dig>
  struct parse_number_impl<Val, Base, Dig>
  {
    using digit_ = digit<Base, Dig>;
    static constexpr unsigned power = digit_::valid::value ? Base : 1u;
    static_assert(Val * power / power == Val,
                  "integer literal does not fit in unsigned long long");
    using type = std::integral_constant<unsigned long long, Val * power + digit_::value>;
  };
} // namespace detail_

} // namespace parse_number
} // namespace falcon

#endif
