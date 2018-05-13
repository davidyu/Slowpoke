#include "collision.hpp"

static const float EPSILON = 0.001;

bool gml::Collision::IntersectRaySphere( const gml::Ray    r
                                       , const gml::Sphere o
                                       , float&            t
                                       , Vec3&             pt
                                       , Vec3&             normal ) {
    // calculations assume r.direction (d) is normalized, which it (generally) is
    Vec3 d = r.direction;
    Vec3 p = r.origin - o.center;

    float b = dot( p, d );
    float c = dot( p, p ) - o.radius * o.radius;

    // early exit; r starts outside o, but r points away from o
    if ( c > 0 && b > 0 ) return false;

    float discr = b*b - c;

    // r misses o
    if ( discr < 0 ) return false;

    t = -b - sqrt( discr );

    // r starts inside o
    if ( t < EPSILON ) {
        t = -b + sqrt( discr );
        if ( t < EPSILON ) return false;
    }

    pt = r.at( t );
    normal = normalize( pt - o.center );

    return true;
}

bool gml::Collision::IntersectRayPlane( const gml::Ray   r
                                      , const gml::Plane pl
                                      , float&           t
                                      , Vec3&            pt
                                      , Vec3&            normal ) {
    t = ( pl.d - dot( pl.normal, r.origin ) ) / dot( pl.normal, r.direction );

    if ( t >= EPSILON ) {
        pt = r.at( t );
        normal = pl.normal;
        return true;
    } else {
        return false;
    }
}
