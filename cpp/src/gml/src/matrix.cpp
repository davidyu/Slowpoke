#include "matrix.h"

namespace gml {
    Mat3 constructMat3FromRows( Vec3 r0, Vec3 r1, Vec3 r2 ) {
        return { r0.v[0], r0.v[1], r0.v[2]
               , r1.v[0], r1.v[1], r1.v[2]
               , r2.v[0], r2.v[1], r2.v[2] };
    }

    Mat3 constructMat3FromCols( Vec3 c0, Vec3 c1, Vec3 c2 ) {
        return { c0.v[0], c1.v[0], c2.v[0]
               , c0.v[1], c1.v[1], c2.v[1]
               , c0.v[2], c1.v[2], c2.v[2] };
    }

    Mat4 constructMat4FromRows( Vec4 r0, Vec4 r1, Vec4 r2, Vec4 r3 ) {
        return { r0.v[0], r0.v[1], r0.v[2], r0.v[3]
               , r1.v[0], r1.v[1], r1.v[2], r1.v[3]
               , r2.v[0], r2.v[1], r2.v[2], r2.v[3]
               , r3.v[0], r3.v[1], r3.v[2], r3.v[3] };
    }

    Mat4 constructMat4FromCols( Vec4 c0, Vec4 c1, Vec4 c2, Vec4 c3 ) {
        return { c0.v[0], c1.v[0], c2.v[0], c3.v[0]
               , c0.v[1], c1.v[1], c2.v[1], c3.v[1]
               , c0.v[2], c1.v[2], c2.v[2], c3.v[2]
               , c0.v[3], c1.v[3], c2.v[3], c3.v[3] };
    }

    Vec3 getRow0( const Mat3& m ) {
        return { m.m00, m.m01, m.m02 };
    }

    Vec3 getRow1( const Mat3& m ) {
        return { m.m10, m.m11, m.m12 };
    }

    Vec3 getRow2( const Mat3& m ) {
        return { m.m20, m.m21, m.m22 };
    }

    Vec3 getCol0( const Mat3& m ) {
        return { m.m00, m.m10, m.m20 };
    }

    Vec3 getCol1( const Mat3& m ) {
        return { m.m01, m.m11, m.m21 };
    }

    Vec3 getCol2( const Mat3& m ) {
        return { m.m02, m.m12, m.m22 };
    }

    Vec4 getRow0( const Mat4& m ) {
        return { m.m00, m.m01, m.m02, m.m03 };
    }

    Vec4 getRow1( const Mat4& m ) {
        return { m.m10, m.m11, m.m12, m.m13 };
    }

    Vec4 getRow2( const Mat4& m ) {
        return { m.m20, m.m21, m.m22, m.m23 };
    }

    Vec4 getRow3( const Mat4& m ) {
        return { m.m30, m.m31, m.m32, m.m33 };
    }

    Vec4 getCol0( const Mat4& m ) {
        return { m.m00, m.m10, m.m20, m.m30 };
    }

    Vec4 getCol1( const Mat4& m ) {
        return { m.m01, m.m11, m.m21, m.m31 };
    }

    Vec4 getCol2( const Mat4& m ) {
        return { m.m02, m.m12, m.m22, m.m32 };
    }

    Vec4 getCol3( const Mat4& m ) {
        return { m.m03, m.m13, m.m23, m.m33 };
    }

    Mat4 makePerspective( float fov, float aspectRatio, float near, float far ) {
        float t = near * tan( fov / 2 );
        float r = t * aspectRatio;
        float l = -r;
        float b = -t;
        float n = near;
        float f = far;

        return { ( n * 2 ) / ( r - l ) , 0                     , ( r + l ) / ( r - l )      , 0
               , 0                     , ( n * 2 ) / ( t - b ) , ( t + b ) / ( t - b )      , 0
               , 0                     , 0                     , -( f + n ) / ( f - n )     , -( f * n * 2 ) / ( f - n )
               , 0                     , 0                     , -1                         , 0 };
    }

    Mat3 invert( const Mat3& m ) {
        float det = m.m00 * ( m.m11 * m.m22 - m.m12 * m.m21 ) - m.m01 * ( m.m10 * m.m22 - m.m12 * m.m20 ) + m.m02 * ( m.m10 * m.m21 - m.m11 * m.m20 );
        if ( det == 0 ) return identity<3>(); // fail
        float f = 1 / det;

        return { f * ( m.m11 * m.m22 - m.m12 * m.m21 )
               , f * ( m.m02 * m.m21 - m.m01 * m.m22 )
               , f * ( m.m02 * m.m12 - m.m02 * m.m11 )
               
               , f * ( m.m12 * m.m20 - m.m10 * m.m22 )
               , f * ( m.m00 * m.m22 - m.m02 * m.m20 )
               , f * ( m.m02 * m.m10 - m.m00 * m.m12 )

               , f * ( m.m10 * m.m21 - m.m11 * m.m20 )
               , f * ( m.m01 * m.m20 - m.m00 * m.m21 )
               , f * ( m.m00 * m.m11 - m.m01 * m.m10 ) };
    }

    float determinant( const Mat3& m ) {

       /* use Sarrus's rule and factor out common terms to arrive at:
        *
        * m00 * ( m11 * m22 - m12 * m21 ) - m01 * ( m10 * m22 - m12 * m20 ) + m02 * ( m10 * m21 - m11 * m20 )
        */

        return m.m00 * ( m.m11 * m.m22 - m.m12 * m.m21 ) - m.m01 * ( m.m10 * m.m22 - m.m12 * m.m20 ) + m.m02 * ( m.m10 * m.m21 - m.m11 * m.m20 );
    }

    Mat4 invert( const Mat4& m ) {
        float a00 = m.m03 * m.m11 - m.m01 * m.m13;
        float a01 = m.m20 * m.m32 - m.m22 * m.m30;
        float a02 = m.m00 * m.m13 - m.m03 * m.m10;
        float a03 = m.m21 * m.m32 - m.m22 * m.m31;
        float a04 = m.m02 * m.m10 - m.m00 * m.m12;
        float a05 = m.m21 * m.m33 - m.m23 * m.m31;
        float a06 = m.m00 * m.m11 - m.m01 * m.m10;
        float a07 = m.m22 * m.m33 - m.m23 * m.m32;
        float a08 = m.m02 * m.m11 - m.m01 * m.m12;
        float a09 = m.m23 * m.m30 - m.m20 * m.m33;
        float a10 = m.m03 * m.m12 - m.m02 * m.m13;
        float a11 = m.m21 * m.m30 - m.m20 * m.m31;

        float det = a00 * a01 + a02 * a03 + a04 * a05 + a06 * a07 + a08 * a09 + a10 * a11;
        if ( det == 0 ) return identity<4>(); // fail
        float f = 1 / det;

       /* given the expanded form of the 4x4 inverse:
        *
        *    ( f * ( -m13 * m22 * m31 + m12 * m23 * m31 + m13 * m21 * m32 - m11 * m23 * m32 - m12 * m21 * m33 + m11 * m22 * m33 )
        *    , f * (  m03 * m22 * m31 - m02 * m23 * m31 - m03 * m21 * m32 + m01 * m23 * m32 + m02 * m21 * m33 - m01 * m22 * m33 )
        *    , f * ( -m03 * m12 * m31 + m02 * m13 * m31 + m03 * m11 * m32 - m01 * m13 * m32 - m02 * m11 * m33 + m01 * m12 * m33 )
        *    , f * (  m03 * m12 * m21 - m02 * m13 * m21 - m03 * m11 * m22 + m01 * m13 * m22 + m02 * m11 * m23 - m01 * m12 * m23 )
        *
        *    , f * (  m13 * m22 * m30 - m12 * m23 * m30 - m13 * m20 * m32 + m10 * m23 * m32 + m12 * m20 * m33 - m10 * m22 * m33 )
        *    , f * ( -m03 * m22 * m30 + m02 * m23 * m30 + m03 * m20 * m32 - m00 * m23 * m32 - m02 * m20 * m33 + m00 * m22 * m33 )
        *    , f * (  m03 * m12 * m30 - m02 * m13 * m30 - m03 * m10 * m32 + m00 * m13 * m32 + m02 * m10 * m33 - m00 * m12 * m33 )
        *    , f * ( -m03 * m12 * m20 + m02 * m13 * m20 + m03 * m10 * m22 - m00 * m13 * m22 - m02 * m10 * m23 + m00 * m12 * m23 )
        *
        *    , f * ( -m13 * m21 * m30 + m11 * m23 * m30 + m13 * m20 * m31 - m10 * m23 * m31 - m11 * m20 * m33 + m10 * m21 * m33 )
        *    , f * (  m03 * m21 * m30 - m01 * m23 * m30 - m03 * m20 * m31 + m00 * m23 * m31 + m01 * m20 * m33 - m00 * m21 * m33 )
        *    , f * ( -m03 * m11 * m30 + m01 * m13 * m30 + m03 * m10 * m31 - m00 * m13 * m31 - m01 * m10 * m33 + m00 * m11 * m33 )
        *    , f * (  m03 * m11 * m20 - m01 * m13 * m20 - m03 * m10 * m21 + m00 * m13 * m21 + m01 * m10 * m23 - m00 * m11 * m23 )
        *
        *    , f * (  m12 * m21 * m30 - m11 * m22 * m30 - m12 * m20 * m31 + m10 * m22 * m31 + m11 * m20 * m32 - m10 * m21 * m32 )
        *    , f * ( -m02 * m21 * m30 + m01 * m22 * m30 + m02 * m20 * m31 - m00 * m22 * m31 - m01 * m20 * m32 + m00 * m21 * m32 )
        *    , f * (  m02 * m11 * m30 - m01 * m12 * m30 - m02 * m10 * m31 + m00 * m12 * m31 + m01 * m10 * m32 - m00 * m11 * m32 )
        *    , f * ( -m02 * m11 * m20 + m01 * m12 * m20 + m02 * m10 * m21 - m00 * m12 * m21 - m01 * m10 * m22 + m00 * m11 * m22 ) )
        *
        * rearrange to match terms a00-a11 from determinant computation:
        *
        *    ( f * (  m13 * m21 * m32 - m13 * m22 * m31 - m12 * m21 * m33 + m12 * m23 * m31 + m11 * m22 * m33 - m11 * m23 * m32 )
        *    , f * ( -m03 * m21 * m32 + m03 * m22 * m31 + m02 * m21 * m33 - m02 * m23 * m31 - m01 * m22 * m33 + m01 * m23 * m32 )
        *    , f * ( -m31 * m03 * m12 + m31 * m02 * m13 + m32 * m03 * m11 - m32 * m01 * m13 - m33 * m02 * m11 + m33 * m01 * m12 )
        *    , f * (  m21 * m03 * m12 - m21 * m02 * m13 - m22 * m03 * m11 + m22 * m01 * m13 + m23 * m02 * m11 - m23 * m01 * m12 )
        *
        *    , f * ( -m13 * m20 * m32 + m13 * m22 * m30 - m12 * m23 * m30 + m12 * m20 * m33 + m10 * m23 * m32 - m10 * m22 * m33 )
        *    , f * (  m03 * m20 * m32 - m03 * m22 * m30 + m02 * m23 * m30 - m02 * m20 * m33 - m00 * m23 * m32 + m00 * m22 * m33 )
        *    , f * (  m30 * m03 * m12 - m30 * m02 * m13 + m32 * m00 * m13 - m32 * m03 * m10 + m33 * m02 * m10 - m33 * m00 * m12 )
        *    , f * ( -m20 * m03 * m12 + m20 * m02 * m13 - m22 * m00 * m13 + m22 * m03 * m10 - m23 * m02 * m10 + m23 * m00 * m12 )
        *
        *    , f * ( -m13 * m21 * m30 + m13 * m20 * m31 + m11 * m23 * m30 - m11 * m20 * m33 + m10 * m21 * m33 - m10 * m23 * m31 )
        *    , f * (  m03 * m21 * m30 - m03 * m20 * m31 - m01 * m23 * m30 + m01 * m20 * m33 - m00 * m21 * m33 + m00 * m23 * m31 )
        *    , f * ( -m30 * m03 * m11 + m30 * m01 * m13 - m31 * m00 * m13 + m31 * m03 * m10 + m33 * m00 * m11 - m33 * m01 * m10 )
        *    , f * (  m20 * m03 * m11 - m20 * m01 * m13 + m21 * m00 * m13 - m21 * m03 * m10 - m23 * m00 * m11 + m23 * m01 * m10 )
        *
        *    , f * (  m12 * m21 * m30 - m12 * m20 * m31 + m11 * m20 * m32 - m11 * m22 * m30 - m10 * m21 * m32 + m10 * m22 * m31 )
        *    , f * ( -m02 * m21 * m30 + m02 * m20 * m31 - m01 * m20 * m32 + m01 * m22 * m30 + m00 * m21 * m32 - m00 * m22 * m31 )
        *    , f * (  m30 * m02 * m11 - m30 * m01 * m12 - m31 * m02 * m10 + m31 * m00 * m12 - m32 * m00 * m11 + m32 * m01 * m10 )
        *    , f * ( -m20 * m02 * m11 + m20 * m01 * m12 + m21 * m02 * m10 - m21 * m00 * m12 + m22 * m00 * m11 - m22 * m01 * m10 ) );
        *
        * then factor:
        *
        *    ( f * (  m13 * ( m21 * m32 - m22 * m31 ) - m12 * ( m21 * m33 - m23 * m31 ) + m11 * ( m22 * m33 - m23 * m32 ) )
        *    , f * ( -m03 * ( m21 * m32 - m22 * m31 ) + m02 * ( m21 * m33 - m23 * m31 ) - m01 * ( m22 * m33 - m23 * m32 ) )
        *    , f * ( -m31 * ( m03 * m12 - m02 * m13 ) + m32 * ( m03 * m11 - m01 * m13 ) - m33 * ( m02 * m11 - m01 * m12 ) )
        *    , f * (  m21 * ( m03 * m12 - m02 * m13 ) - m22 * ( m03 * m11 - m01 * m13 ) + m23 * ( m02 * m11 - m01 * m12 ) )
        *
        *    , f * ( -m13 * ( m20 * m32 - m22 * m30 ) - m12 * ( m23 * m30 - m20 * m33 ) + m10 * ( m23 * m32 - m22 * m33 ) )
        *    , f * (  m03 * ( m20 * m32 - m22 * m30 ) + m02 * ( m23 * m30 - m20 * m33 ) - m00 * ( m23 * m32 - m22 * m33 ) )
        *    , f * (  m30 * ( m03 * m12 - m02 * m13 ) + m32 * ( m00 * m13 - m03 * m10 ) + m33 * ( m02 * m10 - m00 * m12 ) )
        *    , f * ( -m20 * ( m03 * m12 - m02 * m13 ) - m22 * ( m00 * m13 - m03 * m10 ) - m23 * ( m02 * m10 - m00 * m12 ) )
        *
        *    , f * ( -m13 * ( m21 * m30 - m20 * m31 ) + m11 * ( m23 * m30 - m20 * m33 ) + m10 * ( m21 * m33 - m23 * m31 ) )
        *    , f * (  m03 * ( m21 * m30 - m20 * m31 ) - m01 * ( m23 * m30 - m20 * m33 ) - m00 * ( m21 * m33 - m23 * m31 ) )
        *    , f * ( -m30 * ( m03 * m11 - m01 * m13 ) - m31 * ( m00 * m13 - m03 * m10 ) + m33 * ( m00 * m11 - m01 * m10 ) )
        *    , f * (  m20 * ( m03 * m11 - m01 * m13 ) + m21 * ( m00 * m13 - m03 * m10 ) - m23 * ( m00 * m11 - m01 * m10 ) )
        *
        *    , f * (  m12 * ( m21 * m30 - m20 * m31 ) + m11 * ( m20 * m32 - m22 * m30 ) - m10 * ( m21 * m32 - m22 * m31 ) )
        *    , f * ( -m02 * ( m21 * m30 - m20 * m31 ) - m01 * ( m20 * m32 - m22 * m30 ) + m00 * ( m21 * m32 - m22 * m31 ) )
        *    , f * (  m30 * ( m02 * m11 - m01 * m12 ) - m31 * ( m02 * m10 - m00 * m12 ) - m32 * ( m00 * m11 - m01 * m10 ) )
        *    , f * ( -m20 * ( m02 * m11 - m01 * m12 ) + m21 * ( m02 * m10 - m00 * m12 ) + m22 * ( m00 * m11 - m01 * m10 ) ) );
        *
        * and finally, substitute terms to arrive at the result:
        */

        return { f * (  m.m13 * a03 - m.m12 * a05 + m.m11 * a07 )
               , f * ( -m.m03 * a03 + m.m02 * a05 - m.m01 * a07 )
               , f * ( -m.m31 * a10 + m.m32 * a00 - m.m33 * a08 )
               , f * (  m.m21 * a10 - m.m22 * a00 + m.m23 * a08 )

               , f * ( -m.m13 * a01 - m.m12 * a09 - m.m10 * a07 )
               , f * (  m.m03 * a01 + m.m02 * a09 + m.m00 * a07 )
               , f * (  m.m30 * a10 + m.m32 * a02 + m.m33 * a04 )
               , f * ( -m.m20 * a10 - m.m22 * a02 - m.m23 * a04 )

               , f * ( -m.m13 * a11 + m.m11 * a09 + m.m10 * a05 )
               , f * (  m.m03 * a11 - m.m01 * a09 - m.m00 * a05 )
               , f * ( -m.m30 * a00 - m.m31 * a02 + m.m33 * a06 )
               , f * (  m.m20 * a00 + m.m21 * a02 - m.m23 * a06 )

               , f * (  m.m12 * a11 + m.m11 * a01 - m.m10 * a03 )
               , f * ( -m.m02 * a11 - m.m01 * a01 + m.m00 * a03 )
               , f * (  m.m30 * a08 - m.m31 * a04 - m.m32 * a06 )
               , f * ( -m.m20 * a08 + m.m21 * a04 + m.m22 * a06 ) };
    }

    float determinant( const Mat4& m ) {

       /* to derive: 
        * expand the Leibniz formula (or Sarrus's rule) to arrive at the following closed form:
        *
        *     m03 * m12 * m21 * m30 - m02 * m13 * m21 * m30 - m03 * m11 * m22 * m30 + m01 * m13 * m22 * m30 +
        *     m02 * m11 * m23 * m30 - m01 * m12 * m23 * m30 - m03 * m12 * m20 * m31 + m02 * m13 * m20 * m31 +
        *     m03 * m10 * m22 * m31 - m00 * m13 * m22 * m31 - m02 * m10 * m23 * m31 + m00 * m12 * m23 * m31 +
        *     m03 * m11 * m20 * m32 - m01 * m13 * m20 * m32 - m03 * m10 * m21 * m32 + m00 * m13 * m21 * m32 +
        *     m01 * m10 * m23 * m32 - m00 * m11 * m23 * m32 - m02 * m11 * m20 * m33 + m01 * m12 * m20 * m33 +
        *     m02 * m10 * m21 * m33 - m00 * m12 * m21 * m33 - m01 * m10 * m22 * m33 + m00 * m11 * m22 * m33;
        *
        * then factor out common pairs:
        *
        *      m03 * m11 * ( m20 * m32 - m22 * m30 )
        *      m01 * m13 * ( m22 * m30 - m20 * m32 )
        *      m00 * m13 * ( m21 * m32 - m22 * m31 )
        *      m03 * m10 * ( m22 * m31 - m21 * m32 )
        *      m02 * m10 * ( m21 * m33 - m23 * m31 )
        *      m00 * m12 * ( m23 * m31 - m21 * m33 )
        *      m00 * m11 * ( m22 * m33 - m23 * m32 )
        *      m02 * m11 * ( m23 * m30 - m20 * m33 )
        *      m01 * m12 * ( m20 * m33 - m23 * m30 )
        *      m03 * m12 * ( m21 * m30 - m20 * m31 )
        *      m02 * m13 * ( m20 * m31 - m21 * m30 )
        *      m01 * m10 * ( m23 * m32 - m22 * m33 )
        *
        * one more time, and voila:
        *
        *     ( m03 * m11 - m01 * m13 ) * ( m20 * m32 - m22 * m30 )
        *     ( m00 * m13 - m03 * m10 ) * ( m21 * m32 - m22 * m31 )
        *     ( m02 * m10 - m00 * m12 ) * ( m21 * m33 - m23 * m31 )
        *     ( m00 * m11 - m01 * m10 ) * ( m22 * m33 - m23 * m32 )
        *     ( m02 * m11 - m01 * m12 ) * ( m23 * m30 - m20 * m33 )
        *     ( m03 * m12 - m02 * m13 ) * ( m21 * m30 - m20 * m31 )
        */

        float a00 = m.m03 * m.m11 - m.m01 * m.m13;
        float a01 = m.m20 * m.m32 - m.m22 * m.m30;
        float a02 = m.m00 * m.m13 - m.m03 * m.m10;
        float a03 = m.m21 * m.m32 - m.m22 * m.m31;
        float a04 = m.m02 * m.m10 - m.m00 * m.m12;
        float a05 = m.m21 * m.m33 - m.m23 * m.m31;
        float a06 = m.m00 * m.m11 - m.m01 * m.m10;
        float a07 = m.m22 * m.m33 - m.m23 * m.m32;
        float a08 = m.m02 * m.m11 - m.m01 * m.m12;
        float a09 = m.m23 * m.m30 - m.m20 * m.m33;
        float a10 = m.m03 * m.m12 - m.m02 * m.m13;
        float a11 = m.m21 * m.m30 - m.m20 * m.m31;
        return a00 * a01 + a02 * a03 + a04 * a05 + a06 * a07 + a08 * a09 + a10 * a11;
    }

}
