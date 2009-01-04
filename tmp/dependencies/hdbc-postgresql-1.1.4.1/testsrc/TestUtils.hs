module TestUtils(connectDB, sqlTestCase, dbTestCase, printDBInfo) where
import Database.HDBC
import Test.HUnit
import Control.Exception
import SpecificDB(connectDB)

sqlTestCase a = 
    TestCase (handleSqlError a)

dbTestCase a =
    TestCase (do dbh <- connectDB
                 finally (handleSqlError (a dbh))
                         (handleSqlError (disconnect dbh))
             )

printDBInfo = handleSqlError $
    do dbh <- connectDB
       putStrLn "+-------------------------------------------------------------------------"
       putStrLn $ "| Testing HDBC database module: " ++ hdbcDriverName dbh ++
                ", bound to client: " ++ hdbcClientVer dbh
       putStrLn $ "| Proxied driver: " ++ proxiedClientName dbh ++
                ", bound to version: " ++ proxiedClientVer dbh
       putStrLn $ "| Connected to server version: " ++ dbServerVer dbh
       putStrLn "+-------------------------------------------------------------------------\n"
       disconnect dbh
