#include "color.h"
#include "math_extensions.h"
#include "lib/catch.hpp"

static const float EPSILON = 1e-6;

using namespace gml;

TEST_CASE( "base color accessors", "" ) {
    Color c = { 1, 2, 3, 4 };

    REQUIRE( c.r == 1 );
    REQUIRE( c.g == 2 );
    REQUIRE( c.b == 3 );
    REQUIRE( c.a == 4 );

    REQUIRE( c[0] == 1 );
    REQUIRE( c[1] == 2 );
    REQUIRE( c[2] == 3 );
    REQUIRE( c[3] == 4 );
}

TEST_CASE( "basic color existence", "" ) {
    Color c;
    c = white;
    c = black;
    c = transparent_black;
    c = red;
    c = blue;
    c = green;
}

TEST_CASE( "color gamma ops", "" ) {
    for ( int i = 0; i < 50; i++ ) {
        Color c = { random_float(), random_float(), random_float(), 1.0 };
        REQUIRE( fabs( length( degamma( engamma( c ) ) - c ) ) < EPSILON );

        Color c_gamma = engamma( c );

        REQUIRE( fabs( pow( c.r, 2.2 ) - c_gamma.r ) < EPSILON );
        REQUIRE( fabs( pow( c.g, 2.2 ) - c_gamma.g ) < EPSILON );
        REQUIRE( fabs( pow( c.b, 2.2 ) - c_gamma.b ) < EPSILON );
    }
}
