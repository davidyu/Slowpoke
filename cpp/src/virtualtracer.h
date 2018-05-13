#pragma once

#include "primitives.hpp"

struct VirtualTracerContext {
    uint nBouncesRemaining = 0;
    uint nGISamples = 10;
};

class VirtualTracer {
public:
    virtual Color trace( gml::Ray ray, uint8 bouncesRemaining ) = 0;
};
