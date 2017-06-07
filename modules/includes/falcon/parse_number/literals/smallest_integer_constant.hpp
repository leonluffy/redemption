// Copyright (c) 2015 Jonathan Poelen
// Distributed under the MIT License.
// (See accompanying file LICENSE.md)

#ifndef FALCON_PARSE_NUMBER_LITERALS_SMALLEST_INTEGER_CONSTANT_HPP
#define FALCON_PARSE_NUMBER_LITERALS_SMALLEST_INTEGER_CONSTANT_HPP

#include "../select_int.hpp"


namespace falcon {
inline namespace literals {
inline namespace smallest_integer_constant_literals {

  ///\brief  Literal operators for maked a std::integral_constant with the smallest signed integer type
  template<char... c> constexpr parse_number::select_int<
    parse_number::parse_int<c...>::value
  , signed char
  , short
  , int
  , long
  , long long
  > operator "" _small() { return {}; }

  ///\brief  Literal operators for maked a std::integral_constant with the smallest unsigned integer type
  template<char... c> constexpr parse_number::smallest_parse_int<c...>
  operator "" _usmall() { return {}; }

} // inline namespace smallest_integer_constant_literals
} // inline namespace literals

namespace parse_number {
  using namespace literals::smallest_integer_constant_literals;
} // namespace parse_number

} // namespace falcon

#endif
