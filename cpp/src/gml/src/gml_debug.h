#pragma once

// simple debug helper methods
// such as printing to stdout

#include "vector.h"
#include "matrix.h"
#include <iostream>

template<int n>
std::ostream& operator<<( std::ostream& os, const gml::Vec<n>& v ) {
    os << "(";
    for ( int i = 0; i < n-1; i++ ) {
        os << v.v[i] << ", ";
    }
    os << v.v[n-1] << ")";
    return os;
}

template<int rows, int cols>
std::ostream& operator<<( std::ostream& os, const gml::Mat<rows, cols>& m ) {
    for ( int r = 0; r < rows; r++ ) {
        for ( int c = 0; c < cols-1; c++ ) {
            os << m.m[r][c] << ", ";
        }
        os << m.m[r][cols-1] << "\n";
    }
    return os;
}
