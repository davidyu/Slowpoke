#pragma once

#include "material.h"

class Lambertian: public Material {
public:
    Lambertian( gml::Color _albedo )
        : Material( _albedo )
    {}

    gml::Color shade( Scene * scene, gml::Ray ray, gml::Vec3 pt, gml::Vec3 normal, VirtualTracer * tracer, VirtualTracerContext ctx ) override;
};
