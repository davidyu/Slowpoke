#pragma once

#include "material.h"

class Dielectric: public Material {
public:
    Dielectric( float _snell )
        : Material( gml::transparent_black )
        , snell( _snell )
    {}

    gml::Color shade( Scene * scene, gml::Ray ray, gml::Vec3 pt, gml::Vec3 normal, VirtualTracer * tracer, VirtualTracerContext ctx ) override;

    float snell; // refractive index

private:
    static bool  refract( gml::Vec3 incident, gml::Vec3 normal, float n1, float n2, gml::Vec3& out_refract ); // NOTE: incident must be normalized
    static float shlick ( float cosine, float n1, float n2 );
};
