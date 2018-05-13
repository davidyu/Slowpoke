#pragma once

#include "Mat.hpp"
#include "primitives.hpp"

#include "material.h"

class Object {
public:
    Mat4 transform;
    Material * mat;

    Object( Vec3 pos, Material * _mat )
        : mat( _mat )
        , transform( identity<4>() )
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

    virtual bool intersects( gml::Ray r, float& t, Vec3& pt, Vec3& normal ) = 0;
};
