// Copyright (c) 2015 Jonathan Poelen
// Distributed under the MIT License.
// (See accompanying file LICENSE.md)

#ifndef FALCON_PARSE_NUMBER_PARSE_INT_HPP
#define FALCON_PARSE_NUMBER_PARSE_INT_HPP

#include "parse_number.hpp"


namespace falcon {
namespace parse_number {

namespace detail_ {
  template<char... Digs>
  struct parse_int_impl;
}

/// \brief  Deducts the base and uses \ref parse_number
template<char... Digs>
using parse_int = typename detail_::parse_int_impl<Digs...>::type;


namespace detail_ {
  template<char... Digs>
  struct parse_int_impl
  : parse_number_impl<0u, 10, Digs...>
  {};

  template<char... Digs>
  struct parse_int_impl<'0', 'b', Digs...>
  : parse_number_impl<0u, 2, Digs...>::type
  {};

  template<char... Digs>
  struct parse_int_impl<'0', 'B', Digs...>
  : parse_number_impl<0u, 2, Digs...>::type
  {};

  template<char... Digs>
  struct parse_int_impl<'0', 'x', Digs...>
  : parse_number_impl<0u, 16, Digs...>::type
  {};

  template<char... Digs>
  struct parse_int_impl<'0', 'X', Digs...>
  : parse_number_impl<0u, 16, Digs...>::type
  {};

  template<char... Digs>
  struct parse_int_impl<'0', Digs...>
  : parse_number_impl<0u, 8, Digs...>::type
  {};

  template<>
  struct parse_int_impl<'0'>
  : std::integral_constant<unsigned long long, 0u>
  {};
} // namespace detail_

} // namespace parse_number
} // namespace falcon

#endif
