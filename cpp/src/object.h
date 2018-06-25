#pragma once

#include "matrix.h"
#include "primitives.h"

#include "material.h"

class Object {
public:
    gml::Mat4 transform;
    Material * mat;

    Object( gml::Vec3 pos, Material * _mat )
        : mat( _mat )
        , transform( gml::identity<4>() )
    {
        transform.tx = pos.x;
        transform.ty = pos.y;
        transform.tz = pos.z;
    }

    virtual ~Object() {
        if ( mat != NULL ) {
            delete mat;
        }
    }

    virtual bool intersects( gml::Ray r, float& t, gml::Vec3& pt, gml::Vec3& normal ) = 0;
};
