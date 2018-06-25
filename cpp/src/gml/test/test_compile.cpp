// make sure everything compiles

#include "vector.h"
#include "matrix.h"
#include "math_extensions.h"
#include "color.h"
#include "primitives.h"
#include "collision.h"
#include "frustum.h"
#include "gml_debug.h"

#include "lib/catch.hpp"

TEST_CASE( "make sure everything compiles", "" ) {
    REQUIRE( 1 == 1 );
}
