#pragma once

#include "material.h"

class Dielectric: public Material {
public:
    Dielectric( float _snell )
        : Material( transparent_black )
        , snell( _snell )
    {}

    Color shade( Scene * scene, gml::Ray ray, Vec3 pt, Vec3 normal, VirtualTracer * tracer, VirtualTracerContext ctx ) override;

    float snell; // refractive index

private:
    static bool  refract( Vec3 incident, Vec3 normal, float n1, float n2, Vec3& out_refract ); // NOTE: incident must be normalized
    static float shlick ( float cosine, float n1, float n2 );
};
