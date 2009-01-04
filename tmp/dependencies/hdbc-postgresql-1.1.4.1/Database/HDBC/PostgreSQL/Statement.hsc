-- -*- mode: haskell; -*-
{-# CFILES hdbc-postgresql-helper.c #-}
-- Above line for hugs
{-
Copyright (C) 2005 John Goerzen <jgoerzen@complete.org>

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
-}
module Database.HDBC.PostgreSQL.Statement where
import Database.HDBC.Types
import Database.HDBC
import Database.HDBC.PostgreSQL.Types
import Database.HDBC.PostgreSQL.Utils
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import Control.Concurrent.MVar
import Foreign.C.String
import Foreign.Marshal
import Foreign.Storable
import Control.Monad
import Data.List
import Data.Word
import Data.Maybe
import Data.Ratio
import Data.ByteString(packCString, ByteString)
import Data.Encoding
import Data.Encoding.UTF8
import Data.Encoding.ASCII
import Control.Exception
import System.IO
import System.Time
import Database.HDBC.PostgreSQL.Parser(convertSQL)
import Database.HDBC.DriverUtils
import Database.HDBC.PostgreSQL.PTypeConv

l _ = return ()
--l m = hPutStrLn stderr ("\n" ++ m)

#include <libpq-fe.h>

data SState = 
    SState { stomv :: MVar (Maybe Stmt),
             nextrowmv :: MVar (CInt), -- -1 for no next row (empty); otherwise, next row to read.
             dbo :: Conn,
             squery :: String,
             coldefmv :: MVar [(String, SqlColDesc)]}

-- FIXME: we currently do no prepare optimization whatsoever.

newSth :: Conn -> ChildList -> String -> IO Statement               
newSth indbo mchildren query = 
    do l "in newSth"
       newstomv <- newMVar Nothing
       newnextrowmv <- newMVar (-1)
       newcoldefmv <- newMVar []
       usequery <- case convertSQL query of
                      Left errstr -> throwDyn $ SqlError
                                      {seState = "",
                                       seNativeError = (-1),
                                       seErrorMsg = "hdbc prepare: " ++ 
                                                    show errstr}
                      Right converted -> return converted
       let sstate = SState {stomv = newstomv, nextrowmv = newnextrowmv,
                            dbo = indbo, squery = usequery,
                            coldefmv = newcoldefmv}
       let retval = 
                Statement {execute = fexecute sstate,
                           executeMany = fexecutemany sstate,
                           finish = public_ffinish sstate,
                           fetchRow = ffetchrow sstate,
                           originalQuery = query,
                           getColumnNames = fgetColumnNames sstate,
                           describeResult = fdescribeResult sstate}
       addChild mchildren retval
       return retval

fgetColumnNames sstate = 
    do c <- readMVar (coldefmv sstate)
       return (map fst c)

fdescribeResult sstate = 
    readMVar (coldefmv sstate)

{- For now, we try to just  handle things as simply as possible.
FIXME lots of room for improvement here (types, etc). -}
fexecute sstate args = withConn (dbo sstate) $ \cconn ->
                       withCString (squery sstate) $ \cquery ->
                       withCStringArr0 args $ \cargs ->
    do l "in fexecute"
       public_ffinish sstate    -- Sets nextrowmv to -1
       resptr <- pqexecParams cconn cquery
                 (genericLength args) nullPtr cargs nullPtr nullPtr 0
       status <- pqresultStatus resptr
       case status of
         #{const PGRES_EMPTY_QUERY} ->
             do l $ "PGRES_EMPTY_QUERY: " ++ squery sstate
                pqclear_raw resptr
                swapMVar (coldefmv sstate) []
                return 0
         #{const PGRES_COMMAND_OK} ->
             do l $ "PGRES_COMMAND_OK: " ++ squery sstate
                rowscs <- pqcmdTuples resptr
                rows <- peekCString rowscs
                pqclear_raw resptr
                swapMVar (coldefmv sstate) []
                return $ case rows of
                                   "" -> 0
                                   x -> read x
         #{const PGRES_TUPLES_OK} -> 
             do l $ "PGRES_TUPLES_OK: " ++ squery sstate
                fgetcoldef resptr >>= swapMVar (coldefmv sstate) 
                numrows <- pqntuples resptr
                if numrows < 1
                   then do pqclear_raw resptr
                           return 0
                   else do 
                        wrappedptr <- withRawConn (dbo sstate) 
                                      (\rawconn -> wrapstmt resptr rawconn)
                        fresptr <- newForeignPtr pqclearptr wrappedptr
                        swapMVar (nextrowmv sstate) 0
                        swapMVar (stomv sstate) (Just fresptr)
                        return 0
         _ -> do l $ "PGRES ERROR: " ++ squery sstate
                 csstatusmsg <- pqresStatus status
                 cserrormsg <- pqresultErrorMessage resptr
                 statusmsg <- peekCString csstatusmsg
                 errormsg <- peekCString cserrormsg
                 pqclear_raw resptr
                 throwDyn $ 
                          SqlError {seState = "",
                                    seNativeError = fromIntegral status,
                                    seErrorMsg = "execute: " ++ statusmsg ++
                                                 ": " ++ errormsg}
{- General algorithm: find out how many columns we have, check the type
of each to see if it's NULL.  If it's not, fetch it as text and return that.
-}

ffetchrow :: SState -> IO (Maybe [SqlValue])
ffetchrow sstate = modifyMVar (nextrowmv sstate) dofetchrow
    where dofetchrow (-1) = l "ffr -1" >> return ((-1), Nothing)
          dofetchrow nextrow = modifyMVar (stomv sstate) $ \stmt -> 
             case stmt of
               Nothing -> l "ffr nos" >> return (stmt, ((-1), Nothing))
               Just cmstmt -> withStmt cmstmt $ \cstmt ->
                 do l $ "ffetchrow: " ++ show nextrow
                    numrows <- pqntuples cstmt
                    l $ "numrows: " ++ show numrows
                    if nextrow >= numrows
                       then do l "no more rows"
                               -- Don't use public_ffinish here
                               ffinish cmstmt
                               return (Nothing, ((-1), Nothing))
                       else do l "getting stuff"
                               ncols <- pqnfields cstmt
                               res <- mapM (getCol cstmt nextrow) 
                                      [0..(ncols - 1)]
                               return (stmt, (nextrow + 1, Just res))
          getCol p row icol = 
             do isnull <- pqgetisnull p row icol
                if isnull /= 0
                   then return SqlNull
                   else do text <- pqgetvalue p row icol
                           coltype <- liftM oidToColType $ pqftype p icol
                           s <- packCString text
                           makeSqlValue coltype s

fgetcoldef cstmt =
    do ncols <- pqnfields cstmt
       mapM desccol [0..(ncols - 1)]
    where desccol i =
              do colname <- (pqfname cstmt i >>= peekCString)
                 coltype <- pqftype cstmt i
                 --coloctets <- pqfsize
                 let coldef = oidToColDef coltype
                 return (colname, coldef)

-- FIXME: needs a faster algorithm.
fexecutemany :: SState -> [[SqlValue]] -> IO ()
fexecutemany sstate arglist =
    mapM_ (fexecute sstate) arglist >> return ()

-- Finish and change state
public_ffinish sstate = 
    do l "public_ffinish"
       swapMVar (nextrowmv sstate) (-1)
       modifyMVar_ (stomv sstate) worker
    where worker Nothing = return Nothing
          worker (Just sth) = ffinish sth >> return Nothing

ffinish :: Stmt -> IO ()
ffinish p = withRawStmt p $ pqclear

foreign import ccall unsafe "libpq-fe.h PQresultStatus"
  pqresultStatus :: (Ptr CStmt) -> IO #{type ExecStatusType}

foreign import ccall unsafe "libpq-fe.h PQexecParams"
  pqexecParams :: (Ptr CConn) -> CString -> CInt ->
                  (Ptr #{type Oid}) ->
                  (Ptr CString) ->
                  (Ptr CInt) ->
                  (Ptr CInt) ->
                  CInt ->
                  IO (Ptr CStmt)

foreign import ccall unsafe "hdbc-postgresql-helper.h PQclear_app"
  pqclear :: Ptr WrappedCStmt -> IO ()

foreign import ccall unsafe "hdbc-postgresql-helper.h &PQclear_finalizer"
  pqclearptr :: FunPtr (Ptr WrappedCStmt -> IO ())

foreign import ccall unsafe "libpq-fe.h PQclear"
  pqclear_raw :: Ptr CStmt -> IO ()

foreign import ccall unsafe "hdbc-postgresql-helper.h wrapobjpg"
  wrapstmt :: Ptr CStmt -> Ptr WrappedCConn -> IO (Ptr WrappedCStmt)

foreign import ccall unsafe "libpq-fe.h PQcmdTuples"
  pqcmdTuples :: Ptr CStmt -> IO CString
foreign import ccall unsafe "libpq-fe.h PQresStatus"
  pqresStatus :: #{type ExecStatusType} -> IO CString

foreign import ccall unsafe "libpq-fe.h PQresultErrorMessage"
  pqresultErrorMessage :: (Ptr CStmt) -> IO CString

foreign import ccall unsafe "libpq-fe.h PQntuples"
  pqntuples :: Ptr CStmt -> IO CInt

foreign import ccall unsafe "libpq-fe.h PQnfields"
  pqnfields :: Ptr CStmt -> IO CInt

foreign import ccall unsafe "libpq-fe.h PQgetisnull"
  pqgetisnull :: Ptr CStmt -> CInt -> CInt -> IO CInt

foreign import ccall unsafe "libpq-fe.h PQgetvalue"
  pqgetvalue :: Ptr CStmt -> CInt -> CInt -> IO CString

foreign import ccall unsafe "libpq-fe.h PQfname"
  pqfname :: Ptr CStmt -> CInt -> IO CString

foreign import ccall unsafe "libpq-fe.h PQftype"
  pqftype :: Ptr CStmt -> CInt -> IO #{type Oid}



-- SqlValue construction function and helpers

-- Make a SqlValue for the passed column type and string value, where it is assumed that the value represented is not the Sql null value.
-- The IO Monad is required only to obtain the local timezone for interpreting date/time values without an explicit timezone.
makeSqlValue :: SqlTypeId -> ByteString -> IO SqlValue
makeSqlValue sqltypeid strval =

    case sqltypeid of 

      tid | tid == SqlCharT        ||
            tid == SqlVarCharT     ||
            tid == SqlLongVarCharT ||
            tid == SqlWCharT       ||
            tid == SqlWVarCharT    ||
            tid == SqlWLongVarCharT  -> return $ SqlString $ decode UTF8 strval

      tid | tid == SqlDecimalT ||
            tid == SqlNumericT   -> return $ SqlRational (makeRationalFromDecimal $ decode ASCII strval)

      tid | tid == SqlSmallIntT ||
            tid == SqlTinyIntT  ||
            tid == SqlIntegerT     -> return $ SqlInt32 (read $ decode ASCII strval)

      SqlBigIntT -> return $ SqlInteger (read $ decode ASCII strval)

      tid | tid == SqlRealT   ||
            tid == SqlFloatT  ||
            tid == SqlDoubleT   -> return $ SqlDouble (read $ decode ASCII strval)
      
      SqlBitT -> return $ case (decode ASCII strval) of
                   't':_ -> SqlBool True
                   'f':_ -> SqlBool False
                   'T':_ -> SqlBool True -- the rest of these are here "just in case", since they are legal as input
                   'y':_ -> SqlBool True
                   'Y':_ -> SqlBool True
                   "1"   -> SqlBool True
                   _     -> SqlBool False
      
      -- Dates and Date/Times
      tid | tid == SqlDateT        ||
            tid == SqlTimestampT   ||
            tid == SqlUTCDateTimeT   ->
                do
                  clockTime <- clockTimeFromISODateAndMaybeTime $ decode ASCII strval
                             
                  case clockTime of TOD epochSecs picos -> return $ SqlEpochTime epochSecs


      -- Times without dates
      tid | tid == SqlTimeT    || 
            tid == SqlUTCTimeT   -> return $ SqlTimeDiff $ secsTimeDiffFromISOTime $ decode ASCII strval
      
      -- TODO: There's no proper way to map intervals as understood by postgres currently so we resort to SqlString. 
      -- E.g. a "1 month" interval is not a specific span of time that could be converted to a SqlTimeDiff.
      -- A new SqlValue constructor would be needed (wrapping System.Time.TimeDiff) to really handle intervals properly.
      SqlIntervalT si -> return $ SqlString $ decode ASCII strval
      
      -- TODO: For now we just map the binary types to SqlStrings. New SqlValue constructors are needed to handle these.
      tid | tid == SqlBinaryT        ||
            tid == SqlVarBinaryT     || 
            tid == SqlLongVarBinaryT    -> return $ SqlByteString strval

      SqlGUIDT -> return $ SqlString $ decode ASCII strval

      SqlUnknownT s -> return $ SqlByteString strval


-- Make a rational number from a decimal string representation of the number.
makeRationalFromDecimal :: String -> Rational
makeRationalFromDecimal s = 
    case elemIndex '.' s of
      Nothing -> toRational ((read s)::Integer)
      Just dotix -> 
        let (nstr,'.':dstr) = splitAt dotix s
            num = (read $ nstr ++ dstr)::Integer
            den = 10^(genericLength dstr) :: Integer
        in
          num % den


-- Creates a ClockTime from an ISO-8601 representation of a date or date/time with optional numeric timezone, as output by Postgres.
-- The IO monad is required because local timezone information may need to be fetched if not provided in the input string.
clockTimeFromISODateAndMaybeTime :: String -> IO ClockTime
clockTimeFromISODateAndMaybeTime datestr =
    let
        (y, '-':month_etc) = head $ reads datestr
        (mo, '-':day_etc) = head $ reads month_etc
        (d, maybeTime) = head $ reads day_etc
        hourParses = reads maybeTime
        (h, min_etc) = if not (null hourParses) then head hourParses else (0,"")
        (min, sec_etc) = if not (null $ drop 1 min_etc) then head $ reads (drop 1 min_etc) else (0,"")
        (sec, maybeTZ) = if not (null $ drop 1 sec_etc) then head $ reads (drop 1 sec_etc) else (0,"")
        tzParses = reads maybeTZ
    in
      do
        tzoff <-  if not $ null tzParses then return $ 3600 * (fst $ head $ tzParses) else getLocalTimeZoneOffsetSecsForDateTime y mo d h min sec

        if null hourParses 
          then 
            return $ toClockTime $ makeCalendarTimeForDate y mo d tzoff
          else
            return $ toClockTime $ makeCalendarTimeForDateTime y mo d h min sec tzoff

    where
      makeCalendarTimeForDate :: Int -> Int -> Int -> Int -> CalendarTime
      makeCalendarTimeForDate year mon day tzoff =
          CalendarTime { ctYear = year, ctMonth = makeMonth mon, ctDay = day, 
                         ctHour = 0, ctMin = 0, ctSec = 0, ctPicosec = 0, 
                         ctWDay = Sunday, ctYDay = 0, -- bogus but ignored when converting to ClockTime according to the docs in System.Time
                         ctTZName = "",
                         ctTZ = tzoff,
                         ctIsDST = False -- bogus but ignored when converting to ClockTime
                       }

      makeCalendarTimeForDateTime :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> CalendarTime
      makeCalendarTimeForDateTime year mon day hour min sec tzoff =
          CalendarTime { ctYear = year, ctMonth = makeMonth mon, ctDay = day, ctHour = hour, ctMin = min, ctSec = sec,
                         ctPicosec = 0, 
                         ctWDay = Sunday, ctYDay = 0, -- bogus but ignored when converting to ClockTime
                         ctTZName = "",
                         ctTZ = tzoff,
                         ctIsDST = False -- bogus but ignored when converting to ClockTime
                       }

      -- Convert 1->Jan, 2->Feb etc as commonly done on planet Earth (what's with the Month enum instance in System.Time ?)
      makeMonth :: Int -> Month
      makeMonth monNum = toEnum (monNum - 1)


      getLocalTimeZoneOffsetSecsForDateTime :: Int -> Int -> Int -> Int -> Int -> Int -> IO Int 
      getLocalTimeZoneOffsetSecsForDateTime y mo d h min s =
          do
            -- Convert nominal day and time at GMT to our location to get first guess of our tz offset at required date and time
            approxLocalCalTime <- toLocalCalTime (makeCalendarTimeForDateTime y mo d h min s 0)
      
            -- First guess of the proper timezon offset for this date and time in our location.
            let firstGuess = makeCalendarTimeForDateTime y mo d h min s (ctTZ approxLocalCalTime)
      
            -- Allow up to 6 hours for date/time dependent timezone offset adjustments (Usually should be 0,1, or -1 depending on daylight savings time).
            let adjustments = map (3600*) $ [0,-1,1] ++ [2..6] ++  [-2,-3..(-6)]

            let adjustedCalTimes = map (\adj -> firstGuess { ctTZ = ctTZ firstGuess + adj }) adjustments
      
            successList <- mapM isFixedPointUnderConversionToLocalTime adjustedCalTimes

            case elemIndex True successList of
              Nothing -> error $ "Could not find proper timezone for date: " ++ 
                                 show y ++ "-" ++ show mo ++ "-" ++ show d ++ " " ++ show h ++ ":" ++ show min ++ ":" ++ show s
              Just ix -> return (ctTZ $ adjustedCalTimes!!ix)
          where
            toLocalCalTime :: CalendarTime -> IO CalendarTime
            toLocalCalTime calTime = toCalendarTime $ toClockTime calTime
      
            isFixedPointUnderConversionToLocalTime :: CalendarTime -> IO Bool
            isFixedPointUnderConversionToLocalTime calTime =
                do
                  calTime' <- toLocalCalTime calTime
                  return $ eqParts calTime calTime'

            eqParts :: CalendarTime -> CalendarTime -> Bool
            eqParts calTime1 calTime2 = ctHour calTime1 == ctHour calTime2 &&
                                        ctMin calTime1 == ctMin calTime2 &&
                                        ctSec calTime1 == ctSec calTime2 &&
                                        ctDay calTime1 == ctDay calTime2 &&
                                        ctMonth calTime1 == ctMonth calTime2 &&
                                        ctYear calTime1 == ctYear calTime2



-- Time values (without dates) are represented as a seconds count
secsTimeDiffFromISOTime :: String -> Integer
secsTimeDiffFromISOTime timestr =
    let
        (h, min_etc) = head $ reads timestr
        (min, sec_etc) = if null min_etc || min_etc == ":" then (0,":0") else head $ reads (tail  min_etc)
        (sec, _) = if null sec_etc || sec_etc == ":" then (0,"") else head $ reads (tail sec_etc)
    in
      h * 3600 + 60 * min + sec
