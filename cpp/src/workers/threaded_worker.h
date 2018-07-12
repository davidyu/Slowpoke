#include "worker.h"

class ThreadedWorker: public Worker {
public:
    ThreadedWorker( struct WorkParams params, int numthreads = 4 );
    virtual ~ThreadedWorker() {}

    virtual void StartWork() override;
    virtual void DoWork() override;
    virtual void WaitUntilFinished() override;

protected:
    int numthreads_;
}
