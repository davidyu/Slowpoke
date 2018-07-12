#include "bmp.h"
#include "tracer.h"

struct WorkParams {
    int bounces = 4;
    int w = 0;
    int h = 0;
};

class Worker {
public:
    Worker( struct WorkParams params );

    virtual void StartWork();
    virtual void WaitUntilFinished();

    virtual ~Worker() {}

    BMP * GetOutput() { return bmp_; }
    bool  IsDone()    { return finished_; }

protected:
    Tracer rt_; // local raytracer instance
    WorkParams params_;
    BMP * bmp_;
    bool finished_;
};
