#!/bin/sh

# mod_fastcgi doesn't export many environment variables
# so PATH must be explicitly exported.
export PATH=/usr/local/bin:/usr/bin:/bin

cd ..
dist/build/turbinado/turbinado -f
