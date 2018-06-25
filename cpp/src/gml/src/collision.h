#pragma once

#include "math_extensions.h"
#include "primitives.h"

namespace gml {
    // NOTE: if the ray starts inside the sphere, it will be considered not to intersect with the sphere.
    bool IntersectRaySphere( const Ray    r
                           , const Sphere o
                           , float&       t
                           , Vec3&        pt
                           , Vec3&        normal );

    bool IntersectRayPlane( const Ray   r
                          , const Plane pl
                          , float&      t
                          , Vec3&       pt
                          , Vec3&       normal );
} // namespace gml
