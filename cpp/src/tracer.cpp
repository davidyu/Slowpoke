#include "tracer.h"
#include "primitives.hpp"
#include <cstdlib>
#include <cmath>

#if defined( DEBUG )
#include "gml_debug.hpp"
#endif

struct IntersectionResult {
    Object * obj    = NULL;
    float    t      = INT_MAX;
    Vec3     pt     = zero<3>();
    Vec3     normal = zero<3>();
};

Color Tracer::envmap( gml::Ray ray ) {
    // hardcoded gradient sky
    float h = 0.5 * ( normalize( ray.direction ).y + 1.0 ); // transform range of y from -1 to 1 to 0 to 1
    return ( 1.0 - h ) * white + h * Color( 0.5, 0.7, 1.0, 1.0 );
}

Color Tracer::trace( gml::Ray ray, uint8 bouncesRemaining ) {
    if ( bouncesRemaining == 0 ) {
        return white;
    }

    VirtualTracerContext ctx;
    ctx.nBouncesRemaining = bouncesRemaining - 1;

    IntersectionResult ir;

    for ( auto it = scene.objects.begin(); it != scene.objects.end(); it++ ) {
        float t;
        Vec3  pt;
        Vec3  normal;

        if ( (*it)->intersects( ray, t, pt, normal ) && t < ir.t ) {
            ir.obj = *it;
            ir.t = t;
            ir.normal = normal;
            ir.pt = pt;
        }
    }

    if ( ir.obj != NULL ) {
        return ir.obj->mat->shade( &scene, ray, ir.pt, ir.normal, this, ctx );
    } else {
        return envmap( ray );
    }
}

Color Tracer::trace( const Vec2& uv, const Vec2& pixelSize, uint8 bouncesRemaining ) {
    if ( bouncesRemaining == 0 ) {
        return white;
    }

    // generate ray going through image plane at x, y
    Vec3 origin = camera.pos;

    // viewport parameters
    float planeWidth = tan( camera.FOV / 2 ) / camera.focalLength;
    float planeHeight = planeWidth / camera.aspectRatio;

    // focus plane parameters
    float focusPlaneWidth = planeWidth * ( camera.focalLength + camera.depthOfFocus ) / camera.focalLength;
    float focusPlaneHeight = focusPlaneWidth / camera.aspectRatio;

    // center of plane = camera.pos + camera.aim * ( camera.focalLength + camera.depthOfFocus )
    // point on plane = center of plane + planeWidth * ( ( x - 0.5 ) * camera.right ) + planeHeight * ( ( y - 0.5 ) * camera.up )
    // direction = point on plane - camera.pos
    Vec3 target = origin
                + camera.right * focusPlaneWidth  * ( uv.u - 0.5 )
                + camera.up    * focusPlaneHeight * ( uv.v - 0.5 )
                + camera.aim   * ( camera.focalLength + camera.depthOfFocus );

    // check for intersections with each object in scene
    uint nSamples = params.jitterSamples ? params.nSamples : 1;
    Color acc = black;

    gml::Ray ray( origin, target - origin );
    acc += trace( ray, bouncesRemaining );

    for ( uint i = 1; i < nSamples; i++ ) {
        // sub-pixel jitter for anti-aliasing
        auto jitter = camera.right * pixelSize.x * planeWidth  * ( drand48() - 0.5 )
                    + camera.up    * pixelSize.y * planeHeight * ( drand48() - 0.5 );

        // lens fuzzing for depth of field blurring
        auto lens_fuzz = randomInDisk( camera.aperture );

        ray.origin = camera.pos + lens_fuzz.x * camera.right + lens_fuzz.y * camera.up;
        ray.direction = normalize( target + jitter - ray.origin );
        acc += trace( ray, bouncesRemaining );
    }
    acc /= nSamples;

    return acc;
}
