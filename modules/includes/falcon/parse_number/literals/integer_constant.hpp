// Copyright (c) 2015 Jonathan Poelen
// Distributed under the MIT License.
// (See accompanying file LICENSE.md)

#ifndef FALCON_PARSE_NUMBER_LITERALS_INTEGERRAL_CONSTANT_HPP
#define FALCON_PARSE_NUMBER_LITERALS_INTEGERRAL_CONSTANT_HPP

#include "../select_int.hpp"

namespace falcon {
inline namespace literals {
///\brief Literal operators for maked std::integral_constant with fundamental types
inline namespace integer_constant_literals
{

  template<char... c> constexpr parse_number::select_int<parse_number::parse_int<c...>::value, int, long, long long>
  operator "" _c() { return {}; }

  template<char... c> constexpr parse_number::select_int<parse_number::parse_int<c...>::value, unsigned int, unsigned long, unsigned long long>
  operator "" _uc() { return {}; }

  template<char... c> constexpr parse_number::select_int<parse_number::parse_int<c...>::value, long>
  operator "" _lc() { return {}; }

  template<char... c> constexpr parse_number::select_int<parse_number::parse_int<c...>::value, unsigned long>
  operator "" _ulc() { return {}; }

  template<char... c> constexpr parse_number::select_int<parse_number::parse_int<c...>::value, long long>
  operator "" _llc() { return {}; }

  template<char... c> constexpr parse_number::parse_int<c...>
  operator "" _ullc() { return {}; }

} // inline namespace integer_constant_literals
} // inline namespace literals

namespace parse_number {
  using namespace literals::integer_constant_literals;
} // namespace parse_number

} // namespace falcon

#endif
