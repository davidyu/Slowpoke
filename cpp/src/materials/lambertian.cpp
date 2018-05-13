#include "lambertian.h"
#include "scene.h"

Color Lambertian::shade( Scene * scene, gml::Ray ray, Vec3 pt, Vec3 normal, VirtualTracer * vt, VirtualTracerContext ctx ) {
    Color GI = transparent_black;
    for ( auto i = 0; i < ctx.nGISamples; i++ ) {
        if ( ctx.nBouncesRemaining > 0 ) {
            GI += 0.5 * vt->trace( gml::Ray( pt, normal + randomInSphere<3>() ), ctx.nBouncesRemaining );
        }
    }

    GI /= ctx.nGISamples;

    return albedo * GI;
}
