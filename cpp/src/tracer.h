#pragma once

#include "camera.h"
#include "scene.h"
#include "vec.hpp"
#include "virtualtracer.h"

struct TracerParams {
    bool jitterSamples = true;
    uint nSamples      = 6;
};

class Tracer: public VirtualTracer {
private:
public:
    Camera camera;
    Scene scene;
    TracerParams params;

    Tracer() {}

    Tracer( TracerParams _params )
        : params( _params ) {}

    ~Tracer() {}

    Color trace( gml::Ray ray, uint8 bouncesRemaining ) override;
    Color trace( const Vec2& uv, const Vec2& pixelSize, uint8 bouncesRemaining );

    Color envmap( gml::Ray ray );
};
