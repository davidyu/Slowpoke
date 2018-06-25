#pragma once

#include "vector.h"
#include "matrix.h"
#include "primitives.h"

namespace gml {
    Plane getFrustumNearPlane  ( const Mat4& projection );
    Plane getFrustumFarPlane   ( const Mat4& projection );
    Plane getFrustumLeftPlane  ( const Mat4& projection );
    Plane getFrustumRightPlane ( const Mat4& projection );
    Plane getFrustumTopPlane   ( const Mat4& projection );
    Plane getFrustumBottomPlane( const Mat4& projection );

    Plane getFrustumNearPlane  ( const Mat4& projection ) {
        return Plane( getRow3( projection ) + getRow2( projection ) );
    }

    Plane getFrustumFarPlane   ( const Mat4& projection ) {
        return Plane( getRow3( projection ) - getRow2( projection ) );
    }

    Plane getFrustumLeftPlane  ( const Mat4& projection ) {
        return Plane( getRow3( projection ) + getRow0( projection ) );
    }

    Plane getFrustumRightPlane ( const Mat4& projection ) {
        return Plane( getRow3( projection ) - getRow0( projection ) );
    }

    Plane getFrustumTopPlane   ( const Mat4& projection ) {
        return Plane( getRow3( projection ) - getRow1( projection ) );
    }

    Plane getFrustumBottomPlane( const Mat4& projection ) {
        return Plane( getRow3( projection ) + getRow1( projection ) );
    }
}
