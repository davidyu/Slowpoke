#pragma once

#include "object.h"
#include "collision.h"

namespace rt {
    class Sphere : public Object {
    private:
        float radius;

    public:
        Sphere( gml::Vec3 center, float _radius, Material * mat )
            : Object( center, mat )
            , radius( _radius )
        {}

        ~Sphere() {}

        inline gml::Sphere toGMLSphere() {
            return gml::Sphere( gml::Vec3( transform.tx, transform.ty, transform.tz ), radius );
        }

        inline bool intersects( gml::Ray r, float& t, gml::Vec3& pt, gml::Vec3& normal ) override {
            return gml::IntersectRaySphere( r, toGMLSphere(), t, pt, normal );
        }
    };
}
