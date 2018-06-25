#include "vector.h"
#include "lib/catch.hpp"

#if defined( _WIN64 )
    #include <time.h> // for time( NULL )
#endif

static const float EPSILON = 1e-6;

using namespace gml;

TEST_CASE( "base vector accessors", "" ) {
    Vec<1> x = { 5 };
    REQUIRE( x[0] == 5 );
    x[0] = 9;
    REQUIRE( x[0] == 9 );

    Vec<2> xy = { 1, 2 };
    REQUIRE( xy[0] == 1 );
    REQUIRE( xy[1] == 2 );

    Vec<100> big;
    for ( int i = 0; i < 100; i++ ) big[i] = i;
    for ( int i = 0; i < 100; i++ ) REQUIRE( big[i] == i );
}

TEST_CASE( "structured vector accessors", "" ) {
    Vec2 xy = { 1, 2 };
    REQUIRE( xy.x == 1 );
    REQUIRE( xy.y == 2 );

    xy.x = 3;
    xy.y = 4;
    REQUIRE( xy.x == 3 );
    REQUIRE( xy.y == 4 );

    Vec3 xyz = { 5, 6, 7 };
    REQUIRE( xyz.x == 5 );
    REQUIRE( xyz.y == 6 );
    REQUIRE( xyz.z == 7 );
    REQUIRE( xyz.r == 5 );
    REQUIRE( xyz.g == 6 );
    REQUIRE( xyz.b == 7 );
    REQUIRE( xyz.xy.x == 5 );
    REQUIRE( xyz.xy.y == 6 );

    Vec4 xyzw = { 9, 10, 11, 12 };
    REQUIRE( xyzw.x == 9 );
    REQUIRE( xyzw.y == 10 );
    REQUIRE( xyzw.z == 11 );
    REQUIRE( xyzw.w == 12 );
    REQUIRE( xyzw.r == 9 );
    REQUIRE( xyzw.g == 10 );
    REQUIRE( xyzw.b == 11 );
    REQUIRE( xyzw.a == 12 );
    REQUIRE( xyzw.xy.x == 9 );
    REQUIRE( xyzw.xy.y == 10 );
    REQUIRE( xyzw.xyz.x == 9 );
    REQUIRE( xyzw.xyz.y == 10 );
    REQUIRE( xyzw.xyz.z == 11 );
}

TEST_CASE( "vector operations", "" ) {
    Vec2 a = { 1, 2 };
    Vec2 b = { 3, 4 };
    Vec2 c = a + b;

    REQUIRE( c.x == 4 );
    REQUIRE( c.y == 6 );

    Vec3 d = { 5, 6, 7 };
    Vec3 e = { 8, 9, 10 };
    Vec3 f = d * e;

    REQUIRE( f.x == 40 );
    REQUIRE( f.y == 54 );
    REQUIRE( f.z == 70 );

    Vec4 g = { 15, 16, 17, 18 };
    Vec4 h = { 11, 12, 13, 14 };
    Vec4 i = g - h;

    REQUIRE( i.x == 4 );
    REQUIRE( i.y == 4 );
    REQUIRE( i.z == 4 );
    REQUIRE( i.w == 4 );

    Vec<5> j = { 19 * 2, 20 * 2, 21 * 2, 22 * 2, 23 * 2 };
    Vec<5> k = { 19, 20, 21, 22, 23 };
    Vec<5> l = j / k;

    REQUIRE( l[0] == 2 );
    REQUIRE( l[1] == 2 );
    REQUIRE( l[2] == 2 );
    REQUIRE( l[3] == 2 );
    REQUIRE( l[4] == 2 );

    Vec<6> m = { 19 * 2, 20 * 2, 21 * 2, 22 * 2, 23 * 2, 24 * 2 };
    float n = 2;
    Vec<6> o = m / n;

    REQUIRE( o[0] == 19 );
    REQUIRE( o[1] == 20 );
    REQUIRE( o[2] == 21 );
    REQUIRE( o[3] == 22 );
    REQUIRE( o[4] == 23 );
    REQUIRE( o[5] == 24 );

    Vec<6> p = { 19, 20, 21, 22, 23, 24 };
    float q = 2;
    Vec<6> r = p * q;

    REQUIRE( r[0] == 38 );
    REQUIRE( r[1] == 40 );
    REQUIRE( r[2] == 42 );
    REQUIRE( r[3] == 44 );
    REQUIRE( r[4] == 46 );
    REQUIRE( r[5] == 48 );

    c = a ^ b;

    REQUIRE( c.x == 1 );
    REQUIRE( c.y == 16 );

    c = a ^ 2;

    REQUIRE( c.x == 1 );
    REQUIRE( c.y == 4 );

    c = -a;
    REQUIRE( c.x == -1 );
    REQUIRE( c.y == -2 );
}

TEST_CASE( "vector in-place modifier operations", "" ) {
    Vec2 a = { 1, 2 };
    Vec2 b = { 3, 4 };
    a += b;

    REQUIRE( a.x == 4 );
    REQUIRE( a.y == 6 );

    Vec3 d = { 5, 6, 7 };
    Vec3 e = { 8, 9, 10 };
    d *= e;

    REQUIRE( d.x == 40 );
    REQUIRE( d.y == 54 );
    REQUIRE( d.z == 70 );

    Vec4 g = { 15, 16, 17, 18 };
    Vec4 h = { 11, 12, 13, 14 };
    g -= h;

    REQUIRE( g.x == 4 );
    REQUIRE( g.y == 4 );
    REQUIRE( g.z == 4 );
    REQUIRE( g.w == 4 );

    Vec<5> j = { 19 * 2, 20 * 2, 21 * 2, 22 * 2, 23 * 2 };
    Vec<5> k = { 19, 20, 21, 22, 23 };
    j /= k;

    REQUIRE( j[0] == 2 );
    REQUIRE( j[1] == 2 );
    REQUIRE( j[2] == 2 );
    REQUIRE( j[3] == 2 );
    REQUIRE( j[4] == 2 );

    Vec<6> m = { 19 * 2, 20 * 2, 21 * 2, 22 * 2, 23 * 2, 24 * 2 };
    float n = 2;
    m /= n;

    REQUIRE( m[0] == 19 );
    REQUIRE( m[1] == 20 );
    REQUIRE( m[2] == 21 );
    REQUIRE( m[3] == 22 );
    REQUIRE( m[4] == 23 );
    REQUIRE( m[5] == 24 );

    Vec<6> p = { 19, 20, 21, 22, 23, 24 };
    float q = 2;
    p *= q;

    REQUIRE( p[0] == 38 );
    REQUIRE( p[1] == 40 );
    REQUIRE( p[2] == 42 );
    REQUIRE( p[3] == 44 );
    REQUIRE( p[4] == 46 );
    REQUIRE( p[5] == 48 );

    a = { 1, 2 };
    a ^= b;

    REQUIRE( a.x == 1 );
    REQUIRE( a.y == 16 );

    a = { 1, 2 };
    a ^= 2;

    REQUIRE( a.x == 1 );
    REQUIRE( a.y == 4 );

    a = b;
    REQUIRE( a.x == b.x );
    REQUIRE( a.y == b.y );

    d = e;
    REQUIRE( d.x == e.x );
    REQUIRE( d.y == e.y );
    REQUIRE( d.z == e.z );

    g = h;
    REQUIRE( g.x == h.x );
    REQUIRE( g.y == h.y );
    REQUIRE( g.z == h.z );
    REQUIRE( g.w == h.w );
}

TEST_CASE( "vector methods", "" ) {
    Vec2 a = { 2, 3 };
    
    REQUIRE( fabs( length( a ) - sqrt( 2*2 + 3*3 ) ) < EPSILON );
    REQUIRE( lengthsq( a ) == 2*2 + 3*3 );
    REQUIRE( length( normalize( a ) ) - 1 < EPSILON );

    Vec3 b = { 4, 5, 6 };

    REQUIRE( fabs( length( b ) - sqrt( 4*4 + 5*5 + 6*6 ) ) < EPSILON );
    REQUIRE( lengthsq( b ) == 4*4 + 5*5 + 6*6 );
    REQUIRE( length( normalize( b ) ) - 1 < EPSILON );

    Vec4 c = { 7, 8, 9, 10 };

    REQUIRE( fabs( length( c ) - sqrt( 7*7 + 8*8 + 9*9 + 10*10 ) ) < EPSILON );
    REQUIRE( lengthsq( c ) == 7*7 + 8*8 + 9*9 + 10*10 );
    REQUIRE( length( normalize( c ) ) - 1 < EPSILON );

    Vec2 a2 = { 4, 5 };
    REQUIRE( dot( a, a2 ) == 2*4 + 3*5 );

    Vec3 b2 = { 7, 8, 9 };
    REQUIRE( dot( b, b2 ) == 4*7 + 5*8 + 6*9 );

    Vec4 c2 = { 11, 12, 13, 14 };
    REQUIRE( dot( c, c2 ) == 7*11 + 8*12 + 9*13 + 10*14 );

    REQUIRE( dot( cross( b, b2 ), b ) == 0 );
}

TEST_CASE( "random constructors", "" ) {
    srand( time( NULL ) );

    Vec2 a = randomOnSphere<2>();
    Vec3 b = randomOnSphere<3>();
    Vec4 c = randomOnSphere<4>();

    REQUIRE( fabs( length( a ) - 1 ) < EPSILON );
    REQUIRE( fabs( length( b ) - 1 ) < EPSILON );
    REQUIRE( fabs( length( c ) - 1 ) < EPSILON );

    Vec2 d = randomOnSphere<2>( 2 );
    Vec3 e = randomOnSphere<3>( 2 );
    Vec4 f = randomOnSphere<4>( 2 );

    REQUIRE( fabs( length( d ) - 2 ) < EPSILON );
    REQUIRE( fabs( length( e ) - 2 ) < EPSILON );
    REQUIRE( fabs( length( f ) - 2 ) < EPSILON );

    zero( a );
    zero( b );
    zero( c );
    zero( d );
    zero( e );
    zero( f );

    randomOnSphere( a );
    randomOnSphere( b );
    randomOnSphere( c );

    REQUIRE( fabs( length( a ) - 1 ) < EPSILON );
    REQUIRE( fabs( length( b ) - 1 ) < EPSILON );
    REQUIRE( fabs( length( c ) - 1 ) < EPSILON );

    randomOnSphere( d, 2 );
    randomOnSphere( e, 2 );
    randomOnSphere( f, 2 );

    REQUIRE( fabs( length( d ) - 2 ) < EPSILON );
    REQUIRE( fabs( length( e ) - 2 ) < EPSILON );
    REQUIRE( fabs( length( f ) - 2 ) < EPSILON );

    zero( a );
    zero( b );
    zero( c );
    zero( d );
    zero( e );
    zero( f );

    a = randomInSphere<2>();
    b = randomInSphere<3>();
    c = randomInSphere<4>();

    REQUIRE( length( a ) <= 1.0 + EPSILON );
    REQUIRE( length( b ) <= 1.0 + EPSILON );
    REQUIRE( length( c ) <= 1.0 + EPSILON );

    a = randomInSphere<2>( 0.01 );
    b = randomInSphere<3>( 0.01 );
    c = randomInSphere<4>( 0.01 );

    REQUIRE( length( a ) <= 0.01 + EPSILON );
    REQUIRE( length( b ) <= 0.01 + EPSILON );
    REQUIRE( length( c ) <= 0.01 + EPSILON );

    d = randomOnSphere<2>( 2 );
    e = randomOnSphere<3>( 2 );
    f = randomOnSphere<4>( 2 );

    REQUIRE( fabs( length( d ) - 2 ) < EPSILON );
    REQUIRE( fabs( length( e ) - 2 ) < EPSILON );
    REQUIRE( fabs( length( f ) - 2 ) < EPSILON );

    zero( a );
    zero( b );
    zero( c );
    zero( d );
    zero( e );
    zero( f );

    randomOnSphere( a );
    randomOnSphere( b );
    randomOnSphere( c );

    REQUIRE( fabs( length( a ) - 1 ) < EPSILON );
    REQUIRE( fabs( length( b ) - 1 ) < EPSILON );
    REQUIRE( fabs( length( c ) - 1 ) < EPSILON );

    randomOnSphere( d, 2 );
    randomOnSphere( e, 2 );
    randomOnSphere( f, 2 );

    REQUIRE( fabs( length( d ) - 2 ) < EPSILON );
    REQUIRE( fabs( length( e ) - 2 ) < EPSILON );
    REQUIRE( fabs( length( f ) - 2 ) < EPSILON );
}
