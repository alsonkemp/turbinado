Welcome to HDBC, Haskell Database Connectivity.

This package provides a database backend driver for PostgreSQL.

Please see HDBC itself for documentation on use.  If you don't already
have it, you can browse this documentation at
http://darcs.complete.org/hdbc/doc/index.html.

This package provides one function in module Database.HDBC.PostgreSQL:

{- | Connect to a PostgreSQL server.

See <http://www.postgresql.org/docs/8.1/static/libpq.html#LIBPQ-CONNECT> for the meaning
of the connection string. -}
connectPostgreSQL :: String -> IO Connection

An example would be:
dbh <- connectPostgreSQL "host=localhost dbname=testdb user=foo"

DIFFERENCES FROM HDBC STANDARD
------------------------------

None known at this time.

PREREQUISITES
-------------

Before installing this package, you'll need to have HDBC 0.99.0 or
above installed.  You can download HDBC from
http://quux.org/devel/hdbc.

You'll need either GHC 6.4.1 or above, or Hugs 2005xx or above.  If
you're using Hugs, you'll also need cpphs (see
http://www.cs.york.ac.uk/fp/cpphs/ if you don't already have it).

INSTALLATION
------------

The steps to install are:

1) Examine HDBC-postgresql.cabal and edit the include-dirs
   line to point to your local PostgreSQL installation.  If necessary,
   uncomment and edit the extra-lib-dirs line to point to your
   local PostgreSQL installation.

2) ghc --make -o setup Setup.lhs

3) ./setup configure

4) ./setup build

5) ./setup install   (as root)

If you're on Windows, you can omit the leading "./".

You may need to add your version of /usr/local/pgsql/include or lib to
LD_LIBRARY_PATH.

USAGE
-----

To use with hugs, you'll want to use hugs -98.

To use with GHC, you'll want to use:

 -package HDBC -package HDBC-postgresql

Or, with Cabal, use:

  Build-Depends: HDBC>=0.99.0, HDBC-postgresql

-- John Goerzen
   December 2005
