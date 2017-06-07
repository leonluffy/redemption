// Copyright (c) 2015 Jonathan Poelen
// Distributed under the MIT License.
// (See accompanying file LICENSE.md)

#ifndef FALCON_PARSE_NUMBER_SELECT_INT_HPP
#define FALCON_PARSE_NUMBER_SELECT_INT_HPP

#include "parse_int.hpp"

#include <limits>


namespace falcon {
namespace parse_number {

namespace detail_ {
  template<unsigned long long Val, class... Ints>
  struct select_int_impl;
}

///\brief  A std::integral_constant with the first sufficiently large integer type
template<unsigned long long Val, class... Ints>
using select_int = typename detail_::select_int_impl<Val, Ints...>::type;

///\brief  A std::integral_constant with the smallest unsigned integer type
template<char... Digs>
using smallest_parse_int = typename detail_::select_int_impl<
  parse_int<Digs...>::value
, unsigned char
, unsigned short
, unsigned int
, unsigned long
, unsigned long long
>::type;


namespace detail_{
  template<unsigned long long Val, class IntType, class... Ints>
  struct select_int_impl<Val, IntType, Ints...>
  : std::conditional<
    (Val <= std::numeric_limits<IntType>::max())
  , std::integral_constant<IntType, IntType(Val)>
  , select_int_impl<Val, Ints...>
  >::type
  {};

  template<unsigned long long Val>
  struct select_int_impl<Val>
  { static_assert(!Val&&Val, "norrowing conversion"); };
} // namespace detail_

} // namespace parse_number
} // namespace falcon

#endif
