#pragma once

#include "object.h"
#include "collision.hpp"

namespace rt {
    class Plane : public Object {
    private:
        Vec3 normal;

    public:
        Plane( Vec3 point, Vec3 _normal, Material * mat )
             : Object( point, mat )
             , normal( _normal )
        {}

        ~Plane() {}

        inline gml::Plane toGMLPlane() {
            return gml::Plane( normal, Vec3( transform.tx, transform.ty, transform.tz ) );
        }

        inline bool intersects( gml::Ray r, float& t, Vec3& pt, Vec3& normal ) override {
            return gml::Collision::IntersectRayPlane( r, toGMLPlane(), t, pt, normal );
        }
    };
}
