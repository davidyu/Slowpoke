#pragma once

#include "material.h"

class Metallic: public Material {
public:
    Metallic( Color _albedo, float _roughness )
        : Material( _albedo )
        , roughness( fmin( fmax( _roughness, 0 ), 1 ) )
    {}

    Color shade( Scene * scene, gml::Ray ray, Vec3 pt, Vec3 normal, VirtualTracer * tracer, VirtualTracerContext ctx ) override;

    float roughness;
};
