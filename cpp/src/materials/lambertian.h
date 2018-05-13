#pragma once

#include "material.h"

class Lambertian: public Material {
public:
    Lambertian( Color _albedo )
        : Material( _albedo )
    {}

    Color shade( Scene * scene, gml::Ray ray, Vec3 pt, Vec3 normal, VirtualTracer * tracer, VirtualTracerContext ctx ) override;
};
