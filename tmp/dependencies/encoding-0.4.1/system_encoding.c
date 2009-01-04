#include "system_encoding.h"

char* get_system_encoding() {
#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
	return nl_langinfo(CODESET);
#else
	return "ASCII";
#endif
}
