#include <HsFFI.h>
#include <stdio.h>
#ifdef __GLASGOW_HASKELL__
#include "PeepingTom/Experimental/Fast/State_stub.h"
#endif

#ifndef __ChunkReader_C
#define __ChunkReader_C

void scan(HsStablePtr lstptr, char* base, size_t start, size_t end, bool (comparison)(char*, int ref_value), int ref_value)
{
    for(size_t offset = start; offset < end; offset++)
    {
        char* ptr = base + offset;
        bool check = (*comparison)(ptr, ref_value);
        if(check){
            appendAddr(lstptr, (long int)ptr);
        }
    }
}

bool int32_eq(char* ptr, int ref_value)
{
    return *((int32_t*) ptr) == (int32_t)ref_value;
}

#endif
