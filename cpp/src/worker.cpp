#include "worker.h"

#include "color.h"
#include "dielectric.h"
#include "plane.h"
#include "lambertian.h"
#include "metallic.h"
#include "sphere.h"
#include "vector.h"

#include <cstdio>

using namespace gml;

Worker::Worker( struct WorkParams params )
              : params_( params )
              , bmp_( NULL )
              , finished_( false )
{
    // no scene description implemented, hardcode scene in worker constructor >_<
    rt_.camera.focalLength = 1.0;
    rt_.camera.depthOfFocus = 40.0;
    rt_.camera.aperture = 1.0;
    rt_.camera.pos.y = -2; // raise the camera up slightly
    rt_.camera.aspectRatio = static_cast<float>( params_.w ) / static_cast<float>( params_.h );

    rt::Sphere * s1 = new rt::Sphere( Vec3( -9, -13, -44 ), 7, new Dielectric( 1.3 ) );
    rt::Sphere * s2 = new rt::Sphere( Vec3( -2, -13, -88 ), 7, new Metallic( Vec4( 0.7, 0.7, 0.7, 1 ), 0.5 ) );
    rt::Sphere * s3 = new rt::Sphere( Vec3( 10, -13, -66 ), 7, new Lambertian( Vec4( 0.7, 0.7, 0.7, 1 ) ) );

    // cornell box faces
    // top face
    rt::Plane * ptop = new rt::Plane( Vec3(  0,  20, 0 ), Vec3( 0, -1, 0 ), new Lambertian( Vec4( 0.9, 0.9, 0.9, 1 ) ) );

    // bottom face
    rt::Plane * pbot = new rt::Plane( Vec3(  0, -20, 0 ), Vec3( 0, 1, 0 ), new Lambertian( Vec4( 0.8, 0.8, 0.8, 1 ) ) );

    // back face
    rt::Plane * pbak = new rt::Plane( Vec3(  0, 0, -65 ), Vec3( 0, 0, 1 ), new Lambertian( Vec4( 0.9, 0.9, 0.9, 1 ) ) );

    // side faces
    rt::Plane * pr = new rt::Plane( Vec3(  20, 0, 0 ), Vec3( -1, 0, 0 ), new Lambertian( Vec4( 0.8, 0.3, 0.3, 1 ) ) );
    rt::Plane * pl = new rt::Plane( Vec3( -20, 0, 0 ), Vec3(  1, 0, 0 ), new Lambertian( Vec4( 0.3, 0.8, 0.3, 1 ) ) );

    rt_.scene.objects.push_back( s1 );
    rt_.scene.objects.push_back( s2 );
    rt_.scene.objects.push_back( s3 );

    // rt_.scene.objects.push_back( &ptop );
    rt_.scene.objects.push_back( pbot );
    // rt_.scene.objects.push_back( &pbak );
    // rt_.scene.objects.push_back( &pl );
    // rt_.scene.objects.push_back( &pr );
}

// single-threaded base implementation; just do all the work in this implementation of
// StartWork()
void Worker::StartWork() {
    finished_ = false;

    bmp_ = new BMP( params_.w, params_.h );

    Vec2 uv;
    const Vec2 pixel_size = { 1.f / params_.w, 1.f / params_.h };
    for ( int y = 0; y < params_.h; y++ ) {
        for ( int x = 0; x < params_.w; x++ ) {
            uv.x = (float) x / params_.w;
            uv.y = (float) y / params_.h;
            bmp_->SetPixel( x, y, degamma( rt_.trace( uv, pixel_size, params_.bounces ) ) );
        }
    }

    for ( auto it = rt_.scene.objects.begin(); it != rt_.scene.objects.end(); it++ ) {
        delete (*it);
    }

    finished_ = true;
}

void Worker::WaitUntilFinished() {
    // do nothing, work is done after StartWork() returns
}
