/*
 * Copyright 1999,2004 The Apache Software Foundation.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.haskell.hslogger4j;

import java.util.ArrayList;
import java.util.List;

import org.apache.log4j.Level;

/**
 *  An extension of the Level class that provides support for java.util.logging 
 * Levels.
 *
 *  @author Scott Deboy <sdeboy@apache.org>
 *
 */

public class HsloggerLevel extends org.apache.log4j.Level {

  public final static int EMERGENCY_INT = 51000; // FATAL
  public final static int ALERT_INT     = 50000;
  public final static int CRITICAL_INT  = 41000;
  public final static int ERROR_INT     = 40000; // ERROR
  public final static int WARNING_INT   = 30000; // WARN
  public final static int NOTICE_INT    = 21000;
  public static final int INFO_INT      = 20000; // INFO
  public static final int DEBUG_INT     = 10000; // DEBUG
  
  public static final HsloggerLevel EMERGENCY = new HsloggerLevel(EMERGENCY_INT, "EMERGENCY", 0);
  public static final HsloggerLevel ALERT     = new HsloggerLevel(ALERT_INT    , "ALERT"    , 1);      
  public static final HsloggerLevel CRITICAL  = new HsloggerLevel(CRITICAL_INT , "CRITICAL" , 2);      
  public static final HsloggerLevel ERROR     = new HsloggerLevel(ERROR_INT    , "ERROR"    , 3);
  public static final HsloggerLevel WARNING   = new HsloggerLevel(WARNING_INT  , "WARNING"  , 4);
  public static final HsloggerLevel NOTICE    = new HsloggerLevel(NOTICE_INT   , "NOTICE"   , 5);
  public static final HsloggerLevel INFO      = new HsloggerLevel(INFO_INT     , "INFO"     , 6);
  public static final HsloggerLevel DEBUG     = new HsloggerLevel(DEBUG_INT    , "DEBUG"    , 7);

  protected HsloggerLevel(int level, String levelStr, int syslogEquivalent) {
    super(level, levelStr, syslogEquivalent);
  }

  /**
    Convert an integer passed as argument to a level. If the
    conversion fails, then this method returns the specified default.
  */
  public static HsloggerLevel toLevel(int val, HsloggerLevel defaultLevel) {
    switch (val) {
    case EMERGENCY_INT:
      return EMERGENCY;

    case ALERT_INT:
      return ALERT;

    case CRITICAL_INT:
      return CRITICAL;

    case ERROR_INT:
      return ERROR;

    case WARNING_INT:
      return WARNING;

    case NOTICE_INT:
      return NOTICE;
      
    case INFO_INT:
      return INFO;

    case DEBUG_INT:
      return DEBUG;

    default:
      return defaultLevel;
    }
  }

  public static Level toLevel(int val) {
    return toLevel(val, CRITICAL);
  }

  public static List getAllPossibleLevels() {
  	ArrayList list=new ArrayList();
  	list.add(DEBUG);
  	list.add(INFO);
  	list.add(NOTICE);
  	list.add(WARNING);
  	list.add(ERROR);
  	list.add(CRITICAL);
  	list.add(ALERT);
  	list.add(EMERGENCY);
  	return list;
  }

  public static Level toLevel(String s) {
  	return toLevel(s, Level.DEBUG);
  }
  
  public static Level toLevel(String sArg, Level defaultLevel) {
    if (sArg == null) {
      return defaultLevel;
    }

    String s = sArg.toUpperCase();

    if (s.equals("EMERGENCY")) {
      return EMERGENCY;
    }

    if (s.equals("ALERT")) {
      return ALERT;
    }

    if (s.equals("CRITICAL")) {
      return CRITICAL;
    }

    if (s.equals("ERROR")) {
      return ERROR;
    }

    if (s.equals("WARNING")) {
      return WARNING;
    }

    if (s.equals("NOTICE")) {
      return NOTICE;
    }

    if (s.equals("INFO")) {
      return INFO;
    }

    if (s.equals("DEBUG")) {
      return DEBUG;
    }
    return defaultLevel;
  }

}

