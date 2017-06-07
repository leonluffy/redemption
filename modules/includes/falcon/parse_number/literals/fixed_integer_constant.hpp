// Copyright (c) 2015 Jonathan Poelen
// Distributed under the MIT License.
// (See accompanying file LICENSE.md)

#ifndef FALCON_PARSE_NUMBER_LITERALS_FIXED_INTEGER_CONSTANT_HPP
#define FALCON_PARSE_NUMBER_LITERALS_FIXED_INTEGER_CONSTANT_HPP

#include "../select_int.hpp"

#include <cstdint>


namespace falcon {
inline namespace literals {
///\brief  Literal operators for maked std::integral_constant with fixed width integer types
inline namespace fixed_integer_constant_literals {

  template<char... c> constexpr parse_number::select_int<parse_number::parse_int<c...>::value, int8_t>
  operator "" _s8() { return {}; }

  template<char... c> constexpr parse_number::select_int<parse_number::parse_int<c...>::value, uint8_t>
  operator "" _u8() { return {}; }

  template<char... c> constexpr parse_number::select_int<parse_number::parse_int<c...>::value, int_fast8_t>
  operator "" _fast8() { return {}; }

  template<char... c> constexpr parse_number::select_int<parse_number::parse_int<c...>::value, int_fast8_t>
  operator "" _sfast8() { return {}; }

  template<char... c> constexpr parse_number::select_int<parse_number::parse_int<c...>::value, uint_fast8_t>
  operator "" _ufast8() { return {}; }

  template<char... c> constexpr parse_number::select_int<parse_number::parse_int<c...>::value, int_least8_t>
  operator "" _least8() { return {}; }

  template<char... c> constexpr parse_number::select_int<parse_number::parse_int<c...>::value, uint_least8_t>
  operator "" _uleast8() { return {}; }

  template<char... c> constexpr parse_number::select_int<parse_number::parse_int<c...>::value, uint_least8_t>
  operator "" _suleast8() { return {}; }


  template<char... c> constexpr parse_number::select_int<parse_number::parse_int<c...>::value, int16_t>
  operator "" _s16() { return {}; }

  template<char... c> constexpr parse_number::select_int<parse_number::parse_int<c...>::value, uint16_t>
  operator "" _u16() { return {}; }

  template<char... c> constexpr parse_number::select_int<parse_number::parse_int<c...>::value, int_fast16_t>
  operator "" _fast16() { return {}; }

  template<char... c> constexpr parse_number::select_int<parse_number::parse_int<c...>::value, int_fast16_t>
  operator "" _sfast16() { return {}; }

  template<char... c> constexpr parse_number::select_int<parse_number::parse_int<c...>::value, uint_fast16_t>
  operator "" _ufast16() { return {}; }

  template<char... c> constexpr parse_number::select_int<parse_number::parse_int<c...>::value, int_least16_t>
  operator "" _least16() { return {}; }

  template<char... c> constexpr parse_number::select_int<parse_number::parse_int<c...>::value, uint_least16_t>
  operator "" _uleast16() { return {}; }

  template<char... c> constexpr parse_number::select_int<parse_number::parse_int<c...>::value, uint_least16_t>
  operator "" _suleast16() { return {}; }


  template<char... c> constexpr parse_number::select_int<parse_number::parse_int<c...>::value, int32_t>
  operator "" _s32() { return {}; }

  template<char... c> constexpr parse_number::select_int<parse_number::parse_int<c...>::value, uint32_t>
  operator "" _u32() { return {}; }

  template<char... c> constexpr parse_number::select_int<parse_number::parse_int<c...>::value, int_fast32_t>
  operator "" _fast32() { return {}; }

  template<char... c> constexpr parse_number::select_int<parse_number::parse_int<c...>::value, int_fast32_t>
  operator "" _sfast32() { return {}; }

  template<char... c> constexpr parse_number::select_int<parse_number::parse_int<c...>::value, uint_fast32_t>
  operator "" _ufast32() { return {}; }

  template<char... c> constexpr parse_number::select_int<parse_number::parse_int<c...>::value, int_least32_t>
  operator "" _least32() { return {}; }

  template<char... c> constexpr parse_number::select_int<parse_number::parse_int<c...>::value, uint_least32_t>
  operator "" _uleast32() { return {}; }

  template<char... c> constexpr parse_number::select_int<parse_number::parse_int<c...>::value, uint_least32_t>
  operator "" _suleast32() { return {}; }


  template<char... c> constexpr parse_number::select_int<parse_number::parse_int<c...>::value, int64_t>
  operator "" _s64() { return {}; }

  template<char... c> constexpr parse_number::select_int<parse_number::parse_int<c...>::value, uint64_t>
  operator "" _u64() { return {}; }

  template<char... c> constexpr parse_number::select_int<parse_number::parse_int<c...>::value, int_fast64_t>
  operator "" _fast64() { return {}; }

  template<char... c> constexpr parse_number::select_int<parse_number::parse_int<c...>::value, int_fast64_t>
  operator "" _sfast64() { return {}; }

  template<char... c> constexpr parse_number::select_int<parse_number::parse_int<c...>::value, uint_fast64_t>
  operator "" _ufast64() { return {}; }

  template<char... c> constexpr parse_number::select_int<parse_number::parse_int<c...>::value, int_least64_t>
  operator "" _least64() { return {}; }

  template<char... c> constexpr parse_number::select_int<parse_number::parse_int<c...>::value, uint_least64_t>
  operator "" _uleast64() { return {}; }

  template<char... c> constexpr parse_number::select_int<parse_number::parse_int<c...>::value, uint_least64_t>
  operator "" _suleast64() { return {}; }


  template<char... c> constexpr parse_number::select_int<parse_number::parse_int<c...>::value, intmax_t>
  operator "" _smax() { return {}; }

  template<char... c> constexpr parse_number::select_int<parse_number::parse_int<c...>::value, intptr_t>
  operator "" _sptr() { return {}; }

  template<char... c> constexpr parse_number::select_int<parse_number::parse_int<c...>::value, uintmax_t>
  operator "" _umax() { return {}; }

  template<char... c> constexpr parse_number::select_int<parse_number::parse_int<c...>::value, uintptr_t>
  operator "" _uptr() { return {}; }

} // inline namespace fixed_integer_constant_literals
} // inline namespace literals

namespace parse_number {
  using namespace literals::fixed_integer_constant_literals;
} // namespace parse_number

} // namespace falcon

#endif
