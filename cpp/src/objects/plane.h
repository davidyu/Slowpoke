#pragma once

#include "object.h"
#include "collision.h"

namespace rt {
    class Plane : public Object {
    private:
        gml::Vec3 normal;

    public:
        Plane( gml::Vec3 point, gml::Vec3 _normal, Material * mat )
             : Object( point, mat )
             , normal( _normal )
        {}

        ~Plane() {}

        inline gml::Plane toGMLPlane() {
            return gml::Plane( normal, gml::Vec3( transform.tx, transform.ty, transform.tz ) );
        }

        inline bool intersects( gml::Ray r, float& t, gml::Vec3& pt, gml::Vec3& normal ) override {
            return gml::IntersectRayPlane( r, toGMLPlane(), t, pt, normal );
        }
    };
}
