#pragma once

#include "camera.h"
#include "scene.h"
#include "vector.h"
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

    gml::Color trace( gml::Ray ray, uint8 bouncesRemaining ) override;
    gml::Color trace( const gml::Vec2& uv, const gml::Vec2& pixelSize, uint8 bouncesRemaining );

    gml::Color envmap( gml::Ray ray );
};
