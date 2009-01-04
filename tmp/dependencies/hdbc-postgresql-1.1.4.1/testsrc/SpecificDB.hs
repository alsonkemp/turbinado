module SpecificDB where
import Database.HDBC
import Database.HDBC.PostgreSQL
import Database.HDBC.PostgreSQL.Parser(convertSQL)
import Test.HUnit

connectDB = 
    handleSqlError (connectPostgreSQL "")
