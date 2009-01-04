#ifndef __SYSTEM_ENCODING__
#define __SYSTEM_ENCODING__

#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
#include <langinfo.h>
#endif

char* get_system_encoding();

#endif
