// Copyright (c) 2015 Jonathan Poelen
// Distributed under the MIT License.
// (See accompanying file LICENSE.md)

#ifndef FALCON_PARSE_NUMBER_DIGIT_HPP
#define FALCON_PARSE_NUMBER_DIGIT_HPP

#include <type_traits>


namespace falcon {
namespace parse_number {

template<unsigned Base, char Dig>
struct digit;

template<unsigned Base>
struct digit<Base, '0'> : std::integral_constant<unsigned, 0>
{ using valid = std::true_type; };

template<unsigned Base>
struct digit<Base, '1'> : std::integral_constant<unsigned, 1>
{ using valid = std::true_type; };

namespace detail_ {
  template<unsigned Base, unsigned Val>
  struct digit_impl : std::integral_constant<unsigned, Val>
  {
    static_assert(Base > Val, "invalid digit");
    using valid = std::true_type;
  };
}

template<unsigned Base> struct digit<Base, '2'> : detail_::digit_impl<Base, 2> {};
template<unsigned Base> struct digit<Base, '3'> : detail_::digit_impl<Base, 3> {};
template<unsigned Base> struct digit<Base, '4'> : detail_::digit_impl<Base, 4> {};
template<unsigned Base> struct digit<Base, '5'> : detail_::digit_impl<Base, 5> {};
template<unsigned Base> struct digit<Base, '6'> : detail_::digit_impl<Base, 6> {};
template<unsigned Base> struct digit<Base, '7'> : detail_::digit_impl<Base, 7> {};
template<unsigned Base> struct digit<Base, '8'> : detail_::digit_impl<Base, 8> {};
template<unsigned Base> struct digit<Base, '9'> : detail_::digit_impl<Base, 9> {};
template<unsigned Base> struct digit<Base, 'a'> : detail_::digit_impl<Base, 0xa> {};
template<unsigned Base> struct digit<Base, 'A'> : detail_::digit_impl<Base, 0xa> {};
template<unsigned Base> struct digit<Base, 'b'> : detail_::digit_impl<Base, 0xb> {};
template<unsigned Base> struct digit<Base, 'B'> : detail_::digit_impl<Base, 0xb> {};
template<unsigned Base> struct digit<Base, 'c'> : detail_::digit_impl<Base, 0xc> {};
template<unsigned Base> struct digit<Base, 'C'> : detail_::digit_impl<Base, 0xc> {};
template<unsigned Base> struct digit<Base, 'd'> : detail_::digit_impl<Base, 0xd> {};
template<unsigned Base> struct digit<Base, 'D'> : detail_::digit_impl<Base, 0xd> {};
template<unsigned Base> struct digit<Base, 'e'> : detail_::digit_impl<Base, 0xe> {};
template<unsigned Base> struct digit<Base, 'E'> : detail_::digit_impl<Base, 0xe> {};
template<unsigned Base> struct digit<Base, 'f'> : detail_::digit_impl<Base, 0xf> {};
template<unsigned Base> struct digit<Base, 'F'> : detail_::digit_impl<Base, 0xf> {};

// Digit separator
template<unsigned Base> struct digit<Base, '\''> : std::integral_constant<unsigned, 0>
{ using valid = std::false_type; };

} // namespace parse_number
} // namespace falcon

#endif
