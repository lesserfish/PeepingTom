#ifndef __HELPER
#define __HELPER
#include <limits.h>

int get_path_max() {
#ifdef PATH_MAX
    return PATH_MAX;
#else
    return 4028;
#endif
}
#endif
