#include "dielectric.h"
#include "scene.h"

using namespace gml;

Color Dielectric::shade( Scene * scene, gml::Ray ray, Vec3 pt, Vec3 normal, VirtualTracer * vt, VirtualTracerContext ctx ) {
    Vec3 outwardNormal;
    float n1, n2;
    float cosine;
    if ( dot( ray.direction, normal ) > 0 ) { // ray originated from inside the dielectric
        outwardNormal = -normal;
        n1 = snell;
        n2 = 1.0;
        cosine = snell * dot( ray.direction, normal ) / length( ray.direction );
    } else {
        outwardNormal = normal;
        n1 = 1.0;
        n2 = snell;
        cosine = -dot( ray.direction, normal ) / length( ray.direction );
    }

    Color result = transparent_black;
    if ( ctx.nBouncesRemaining > 0 ) {
        Vec3 refractDir;
        float reflectProbability;
        Vec3 reflectDir = ray.direction - 2 * dot( ray.direction, normal ) * normal;
        if ( refract( normalize( ray.direction ), outwardNormal, n1, n2, refractDir ) ) {
            reflectProbability = shlick( cosine, n1, n2 );
            result += ( 1 - reflectProbability ) * vt->trace( gml::Ray( pt, refractDir ), ctx.nBouncesRemaining );
            result += reflectProbability * vt->trace( gml::Ray( pt, reflectDir ), ctx.nBouncesRemaining );
        } else {
            result += vt->trace( gml::Ray( pt, reflectDir ), ctx.nBouncesRemaining );
        }
    }

    return result;
}

// NOTE: incident must be normalized
bool Dielectric::refract( Vec3 incident, Vec3 normal, float n1, float n2, Vec3& out_refract ) {
    float n1_n2 = n1 / n2;
    float cos_theta1 = dot( incident, -normal ); // == cos(theta1)
    float discr = 1 - n1_n2 * n1_n2 * ( 1 - cos_theta1 * cos_theta1 );

    if ( discr > 0 ) {
        out_refract = n1_n2 * incident + ( n1_n2 * cos_theta1 - sqrt( discr ) ) * normal;
        return true;
    } else {
        return false;
    }
}

float Dielectric::shlick( float cosine, float n1, float n2 ) {
    float n1_n2 = n1 / n2;
    float r0 = ( 1 - n1_n2 ) / ( 1 + n1_n2 );
    r0 *= r0;
    return r0 + ( 1 - r0 ) * pow( ( 1 - cosine ), 5 );
}
