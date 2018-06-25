#pragma once

#include "vector.h"

namespace gml {
    struct Ray {
        Vec3 origin;
        Vec3 direction;

        Ray()
            : origin( Vec3( 0, 0, 0 ) )
            , direction( Vec3( 0, 0, -1 ) )
        {}

        Ray( Vec3 o, Vec3 d )
            : origin( o )
            , direction( normalize( d ) )
        {}

        inline Vec3 at( float t ) const { return origin + t * direction; }
    };

    struct Sphere {
        Vec3 center;
        float radius;

        Sphere()
            : center( Vec3( 0, 0, 0 ) )
            , radius( 1 )
        {}

        Sphere( Vec3 c, float r )
            : center( c )
            , radius( r )
        {}
    };

    struct Plane {
       // (normal) dot (point on plane) = d
       Vec3 normal;
       float d;

       // p = { A, B, C, D } where Ax + By + Cz + D = 0
       // so we have normal = { A, B, C } and d = -D
       Plane( Vec4 p )
           : normal( normalize( Vec3( p.x, p.y, p.z ) ) )
           , d( -p.w )
       {}

       Plane( Vec3 n, float _d )
           : normal( normalize( n ) )
           , d( _d )
       {}

       Plane( Vec3 n, Vec3 pt )
           : normal( normalize( n ) )
           , d( dot( n, pt ) )
       {}
    };
}
