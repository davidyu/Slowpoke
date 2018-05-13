#pragma once

// include me if you want to debug print vectors using cout

#include "vec.hpp"
#include <iostream>

template<int n>
std::ostream& operator<<( std::ostream& os, const Vec<n>& v ) {
    os << "(";
    for ( int i = 0; i < n-1; i++ ) {
        os << v.v[i] << ", ";
    }
    os << v.v[n-1] << ")";
    return os;
}
