/* This file defines stub pthread functions to link against the ABC sources.  This way,
 * we can compile and link against ABC even when it unconditionally depends on pthreads.
 * We simply will abort with an error message if we ever call an ABC function that
 * requires threading.
 *
 * Yes, this is a bit of a hack.
 */

#ifndef ABC_USE_PTHREADS

#include <stdlib.h>
#include <stdio.h>

#ifdef _WIN32
#include "../abc-build/lib/pthread.h"
#else
#include <unistd.h>
#include <pthread.h>
#endif

int pthread_create(pthread_t * thread, const pthread_attr_t* attr, void *(*start_routine)(void *), void* arg ) { 
  fprintf(stderr,"ABC tried to use pthreads on a system not configured with a pthread library... sorry.");
  exit(1);
}

pthread_t global_self;

void pthread_exit( void* value ) { exit(0); }
int pthread_equal(pthread_t t1, pthread_t t2) { return 1; }
int pthread_detach(pthread_t t) { return 0; }
int pthread_join(pthread_t t, void** v) { return 0; }
int pthread_cancel(pthread_t t) { return 0; }
pthread_t pthread_self(void) { return global_self; }

int pthread_mutex_lock( pthread_mutex_t *m ) { return 0; }
int pthread_mutex_unlock( pthread_mutex_t *m ) { return 0; }
int pthread_mutex_trylock( pthread_mutex_t *m ) { return 0; }
int pthread_mutex_destroy( pthread_mutex_t *m ) { return 0; }
int pthread_mutex_init( pthread_mutex_t *m, const pthread_mutexattr_t *attr ) { return 0; }
int pthread_mutextattr_init( pthread_mutexattr_t *attr ) { return 0; }
int pthread_mutextattr_destroy( pthread_mutexattr_t *attr ) { return 0; }

#endif
