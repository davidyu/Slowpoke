#pragma once

#include "material.h"

class Metallic: public Material {
public:
    Metallic( gml::Color _albedo, float _roughness )
        : Material( _albedo )
        , roughness( fmin( fmax( _roughness, 0 ), 1 ) )
    {}

    gml::Color shade( Scene * scene, gml::Ray ray, gml::Vec3 pt, gml::Vec3 normal, VirtualTracer * tracer, VirtualTracerContext ctx ) override;

    float roughness;
};
