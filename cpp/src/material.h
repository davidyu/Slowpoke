#pragma once

#include "color.hpp"
#include "primitives.hpp"
#include "vec.hpp"
#include "virtualtracer.h"

class Scene;

class Material {
public:
    Material( Color _albedo )
        : albedo( _albedo )
    {}

    virtual ~Material() {}
    virtual Color shade( Scene * scene, gml::Ray ray, Vec3 pt, Vec3 normal, VirtualTracer * tracer, VirtualTracerContext ctx ) = 0;
protected:
    Color albedo;
};
