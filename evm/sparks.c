#include "sparks.h"

void sparkThread(VAL thunk) {
    spark* s = (spark*)(thunk->info);

    pthread_mutex_lock(s->lock);
    SETTY(thunk, RUNNING);
    pthread_mutex_unlock(s->lock);

    VAL ans = DO_EVAL(s->thunk, 1);

    pthread_mutex_lock(s->lock);
    UPDATE(thunk, ans);
//    pthread_cond_broadcast(s->cond);
    pthread_mutex_unlock(s->lock);
}

VAL addSpark(VAL thunk) {
    VAL c = EMALLOC(sizeof(Closure)+sizeof(spark));
    spark* s = (spark*)(c+1);

    pthread_mutex_t m;
    pthread_mutex_init(&m, NULL);

//    pthread_cond_t cond;
//    pthread_cond_init(&cond, NULL);

    s->thunk = thunk;
    s->lock = &m;
    s->cond = NULL; // &cond;

    c->info = (void*)s;
    SETTY(c, RUNNING);

    return c;
}
