#include <libpq-fe.h>
#include <stdio.h>
#include <stdlib.h>
#include "hdbc-postgresql-helper.h"

/* Things can't finalize more than once.  
We'd like to let people call them from the app.
Yet we'd also like to be able to have a ForeignPtr finalize them.

So, here's a little wrapper for things. */

void PQfinish_conditional_finalizer(finalizeonce *conn);

finalizeonce *wrapobjpg(void *obj, finalizeonce *parentobj) {
  finalizeonce *newobj;
  newobj = malloc(sizeof(finalizeonce));
  if (newobj == NULL) {
    fprintf(stderr, "HDBC: could not allocate wrapper!\n");
    return NULL;
  }
  newobj->isfinalized = 0;
  newobj->refcount = 1;
  newobj->encapobj = obj;
  newobj->parent = parentobj;
  if (parentobj != NULL) 
    parentobj->refcount++;
  return newobj;
}
  
void PQfinish_app(finalizeonce *conn) {
  if (conn->isfinalized)
    return;
  PQfinish((PGconn *) (conn->encapobj));
  conn->isfinalized = 1;
}

void PQfinish_finalizer(finalizeonce *conn) {
  (conn->refcount)--;
  PQfinish_conditional_finalizer(conn);
}

void PQfinish_conditional_finalizer(finalizeonce *conn) {
  if (conn->refcount < 1) {
    PQfinish_app(conn);
    free(conn);
  }
}

void PQclear_app(finalizeonce *res) {
  if (res->isfinalized)
    return;
  PQclear((PGresult *) (res->encapobj));
  res->isfinalized = 1;
}

void PQclear_finalizer(finalizeonce *res) {
  PQclear_app(res);
  (res->refcount)--;            /* Not really important since this is never a 
                                   parent */
  (res->parent->refcount)--;
  PQfinish_conditional_finalizer(res->parent);
  free(res);
}

