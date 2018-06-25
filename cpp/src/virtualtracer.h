#pragma once

#include "primitives.h"

struct VirtualTracerContext {
    uint nBouncesRemaining = 0;
    uint nGISamples = 10;
};

class VirtualTracer {
public:
    virtual gml::Color trace( gml::Ray ray, uint8 bouncesRemaining ) = 0;
};
