#include <HsFFI.h>
#include <stdio.h>
#ifdef __GLASGOW_HASKELL__
#include "PeepingTom/Fast/Scan_stub.h"
#endif

#ifndef __ChunkReader_C
#define __ChunkReader_C

void scan (uint8_t (*filter)(char *, char*, size_t), HsStablePtr tableptr, uintptr_t base, uintptr_t range, char* data, char* reference, size_t size)
{
    for(uintptr_t offset = 0; offset <= range; offset++)
    {
        char *ptr = data + offset;
        uint8_t check = (*filter)(ptr, reference, size);
        if(check != 0)
        {
            appendMatch(tableptr, base + offset, check, ptr, size);
        }
    }
}



#endif
