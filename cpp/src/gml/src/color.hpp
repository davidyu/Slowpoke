#pragma once

#include "vec.hpp"

typedef Vec<4> Color;

const Color white             = Vec4( 1, 1, 1, 1 );
const Color black             = Vec4( 0, 0, 0, 1 );
const Color transparent_black = Vec4( 0, 0, 0, 0 );

template <int n> Vec<n> degamma( Vec<n> in );

template <int n>
Vec<n> degamma( Vec<n> in ) {
    static_assert( n == 3 || n == 4, "degamma only defined for Vec<3>s and Vec<4>s!" );
    Vec<n> out = in;
    out ^= ( 1.0 / 2.2 );
    return out;
}
