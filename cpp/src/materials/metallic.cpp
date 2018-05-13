#include "metallic.h"
#include "scene.h"

Color Metallic::shade( Scene * scene, gml::Ray ray, Vec3 pt, Vec3 normal, VirtualTracer * vt, VirtualTracerContext ctx ) {
    Vec3 rdir = ray.direction - 2 * dot( ray.direction, normal ) * normal;
    Color reflection = transparent_black;
    if ( ctx.nBouncesRemaining > 0 ) {
        for ( auto i = 0; i < ctx.nGISamples; i++ ) {
            // add perturbation proportional to roughness parameter
            reflection += vt->trace( gml::Ray( pt, rdir + randomInSphere<3>( roughness ) ), ctx.nBouncesRemaining );
        }
    }

    reflection /= ctx.nGISamples;

    return albedo * reflection;
}
