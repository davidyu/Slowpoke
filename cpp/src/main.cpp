#include "bmp.h"
#include <cstdio>
#include <cstdint>
#include <cstdlib>
#include <pthread.h> 
#include <queue>
#include "tracer.h"
#include "vector.h"
#include "color.h"
#include "sphere.h"
#include "plane.h"

#include "lambertian.h"
#include "metallic.h"
#include "dielectric.h"

#include "worker.h"

using namespace gml;

const int NUM_THREADS = 4;

struct AppParams {
    Tracer               rt;
    int                  numBounces = 4;
    BMP *                bmp = NULL;
    int                  w = 0;
    int                  h = 0;
    bool                 singleThreaded = true;
    bool                 useJobs = true;
    // for jobs use only
    bool                 allJobsQueued = false;
    std::queue<int>      jobQueue;
    pthread_mutex_t      jobQueue_mutex;
    pthread_cond_t       jobQueueAvailable;
};

// static
AppParams params;

void * renderTile( void * t ) {
    // do work
    int threadSize = params.h / NUM_THREADS;
    uint64_t threadId = (uint64_t) t;

    Vec2 uv;
    const Vec2 pixelSize = { 1.f / params.w, 1.f / params.h };

    for ( int y = threadId * threadSize; y < ( threadId + 1 ) * threadSize; y++ ) {
        for ( int x = 0; x < params.w; x++ ) {
            uv.x = (float) x / params.w;
            uv.y = (float) y / params.h;
            params.bmp->SetPixel( x, y, degamma( params.rt.trace( uv, pixelSize, params.numBounces ) ) );
        }
    }

    pthread_exit( t );
}

void * startWorker( void * t ) {
    while ( !params.allJobsQueued || params.jobQueue.size() > 0 ) {
        pthread_mutex_lock( &params.jobQueue_mutex );

        while ( params.jobQueue.size() == 0 && !params.allJobsQueued ) {
            pthread_cond_wait( &params.jobQueueAvailable, &params.jobQueue_mutex );
        }

        if ( params.jobQueue.size() == 0 ) { // ==> params.allJobsQueued is true, so we've exhausted our supply of jobs
            pthread_mutex_unlock( &params.jobQueue_mutex );
            break; // we're done
        }

        int y = params.jobQueue.front();
        params.jobQueue.pop();
        pthread_mutex_unlock( &params.jobQueue_mutex );

        Vec2 uv;
        const Vec2 pixelSize = { 1.f / params.w, 1.f / params.h };
        for ( int x = 0; x < params.w; x++ ) {
            uv.x = (float) x / params.w;
            uv.y = (float) y / params.h;
            params.bmp->SetPixel( x, y, degamma( params.rt.trace( uv, pixelSize, params.numBounces ) ) );
        }
    }
    pthread_exit( NULL );
}

int main( int argc, char **argv ) {
    if ( argc > 1 ) {
        char * mode = argv[1];
        if ( strcmp( mode, "server" ) == 0 ) {
            // 
            printf( "STUB server mode.\n" );
        } else if ( strcmp( mode, "client" ) == 0 ) {
            printf( "STUB client mode.\n" );
        } else if ( strcmp( mode, "local" ) == 0 ) {
            if ( argc >= 4 ) {
                int w = atoi( argv[2] );
                int h = atoi( argv[3] );

                // parameter setup
                params.bmp = new BMP( w, h );

                params.w = w;
                params.h = h;

                params.rt.camera.focalLength = 1.0;
                params.rt.camera.depthOfFocus = 40.0;
                params.rt.camera.aperture = 1.0;
                params.rt.camera.pos.y = -2; // raise the camera up slightly
                params.rt.camera.aspectRatio = static_cast<float>( w ) / static_cast<float>( h );

                rt::Sphere s1( Vec3( -9, -13, -44 ), 7, new Dielectric( 1.3 ) );
                rt::Sphere s2( Vec3( -2, -13, -88 ), 7, new Metallic( Vec4( 0.7, 0.7, 0.7, 1 ), 0.5 ) );
                rt::Sphere s3( Vec3( 10, -13, -66 ), 7, new Lambertian( Vec4( 0.7, 0.7, 0.7, 1 ) ) );

                // cornell box faces
                // top face
                rt::Plane  ptop( Vec3(  0,  20, 0 ), Vec3( 0, -1, 0 ), new Lambertian( Vec4( 0.9, 0.9, 0.9, 1 ) ) );

                // bottom face
                rt::Plane  pbot( Vec3(  0, -20, 0 ), Vec3( 0, 1, 0 ), new Lambertian( Vec4( 0.8, 0.8, 0.8, 1 ) ) );

                // back face
                rt::Plane  pbak( Vec3(  0, 0, -65 ), Vec3( 0, 0, 1 ), new Lambertian( Vec4( 0.9, 0.9, 0.9, 1 ) ) );

                // side faces
                rt::Plane  pr( Vec3(  20, 0, 0 ), Vec3( -1, 0, 0 ), new Lambertian( Vec4( 0.8, 0.3, 0.3, 1 ) ) );
                rt::Plane  pl( Vec3( -20, 0, 0 ), Vec3(  1, 0, 0 ), new Lambertian( Vec4( 0.3, 0.8, 0.3, 1 ) ) );

                params.rt.scene.objects.push_back( &s1 );
                params.rt.scene.objects.push_back( &s2 );
                params.rt.scene.objects.push_back( &s3 );

                // params.rt.scene.objects.push_back( &ptop );
                params.rt.scene.objects.push_back( &pbot );
                // params.rt.scene.objects.push_back( &pbak );
                // params.rt.scene.objects.push_back( &pl );
                // params.rt.scene.objects.push_back( &pr );

                Worker * worker;

                struct WorkParams work_params;
                
                work_params.bounces = 4;
                work_params.w = w;
                work_params.h = h;

                if ( params.singleThreaded ) {
                    worker = new Worker( work_params );
                    worker->StartWork();
                    worker->WaitUntilFinished();
                    params.bmp = worker->GetOutput();
                } else { // multithreaded
                    pthread_t threads[ NUM_THREADS ];

                    pthread_attr_t attr;
                    pthread_attr_init( &attr );
                    pthread_attr_setdetachstate( &attr, PTHREAD_CREATE_JOINABLE );

                    if ( params.useJobs ) {
                        pthread_mutex_init( &params.jobQueue_mutex, NULL );
                        pthread_cond_init( &params.jobQueueAvailable, NULL );

                        for ( int i = 0; i < NUM_THREADS; i++ ) {
                            uint64_t t = i;
                            int rc = pthread_create( &threads[i], &attr, startWorker, (void *) t );
                        }

                        // add jobs to the queue
                        for ( int y = 0; y < h; y++ ) {
                            pthread_mutex_lock( &params.jobQueue_mutex );
                            params.jobQueue.push( y );
                            pthread_cond_signal( &params.jobQueueAvailable );
                            pthread_mutex_unlock( &params.jobQueue_mutex );
                        }

                        // we're done, wake up all waiting threads
                        pthread_mutex_lock( &params.jobQueue_mutex );
                        params.allJobsQueued = true;
                        pthread_cond_broadcast( &params.jobQueueAvailable );
                        pthread_mutex_unlock( &params.jobQueue_mutex );
                    } else { // simple thread-index based work division
                        for ( int i = 0; i < NUM_THREADS; i++ ) {
                            uint64_t t = i;
                            int rc = pthread_create( &threads[i], &attr, renderTile, (void *) t );
                        }
                    }

                    void * status;
                    for ( int i = 0; i < NUM_THREADS; i++ ) {
                        pthread_join( threads[i], &status );
                    }
                }

                params.bmp->Write( "hello.bmp" );
                // worker will be automatically reclaimed by the OS when we exit, so no need to manually call delete
                // params.bmp will be automatically reclaimed by the OS when we exit, so no need to delete
            } else {
                printf( "usage: ray local width height\n" );
            }
        } else {
            printf( "usage: ray [server|client|local] args\n" );
        }
    } else {
        printf( "usage: ray [server|client|local] args\n" );
    }

    return 0;
}
