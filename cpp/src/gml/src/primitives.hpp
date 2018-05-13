#pragma once

#include "vec.hpp"

namespace gml {
    struct Ray {
        Vec3 origin;
        Vec3 direction;

        Ray()
            : origin( Vec3( 0, 0, 0 ) )
            , direction( Vec3( 0, 0, -1 ) )
        {}

        Ray( Vec3 o, Vec3 d, bool normalizeDir = true )
            : origin( o )
            , direction( d )
        {
            if ( normalizeDir )
                direction = normalize( direction );
        }

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
       Vec3 normal;
       float d;

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
