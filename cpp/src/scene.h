#pragma once

#include <vector>
#include "object.h"

class Scene {
public:
    std::vector<Object *> objects;

    Scene() {}
    ~Scene() {}
};
