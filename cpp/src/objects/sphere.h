#pragma once

#include "object.h"
#include "collision.hpp"

namespace rt {
    class Sphere : public Object {
    private:
        float radius;

    public:
        Sphere( Vec3 center, float _radius, Material * mat )
            : Object( center, mat )
            , radius( _radius )
        {}

        ~Sphere() {}

        inline gml::Sphere toGMLSphere() {
            return gml::Sphere( Vec3( transform.tx, transform.ty, transform.tz ), radius );
        }

        inline bool intersects( gml::Ray r, float& t, Vec3& pt, Vec3& normal ) override {
            return gml::Collision::IntersectRaySphere( r, toGMLSphere(), t, pt, normal );
        }
    };
}
