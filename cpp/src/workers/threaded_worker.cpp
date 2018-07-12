#include "threaded_worker.h"

ThreadedWorker::ThreadedWorker( struct WorkParams params, int numthreads = 4 )
    : Worker( params )
    , numthreads_( numthreads )
{
}

void ThreadedWorker::StartWork()
{
    for ( int i = 0; i < NUM_THREADS; i++ ) {
        uint64_t t = i;
        int rc = pthread_create( &threads[i], &attr, renderTile, (void *) t );
    }
}

void ThreadedWorker::DoWork()
{

}

void ThreadedWorker::WaitUntilFinished()
{
    void * status;
    for ( int i = 0; i < NUM_THREADS; i++ ) {
        pthread_join( threads[i], &status );
    }

    /*
    while ( !IsDone() ) {
        // wait for threads to coalesce
        DoWork();
    }
    */
}
