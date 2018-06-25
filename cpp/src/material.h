#pragma once

#include "color.h"
#include "primitives.h"
#include "vector.h"
#include "virtualtracer.h"

class Scene;

class Material {
public:
    Material( gml::Color _albedo ): albedo( _albedo )
    {}

    virtual ~Material() {}
    virtual gml::Color shade( Scene * scene, gml::Ray ray, gml::Vec3 pt, gml::Vec3 normal, VirtualTracer * tracer, VirtualTracerContext ctx ) = 0;
protected:
    gml::Color albedo;
};
