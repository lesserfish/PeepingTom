#include <HsFFI.h>
#include <stdio.h>
#ifdef __GLASGOW_HASKELL__
#include "PeepingTom/Experimental/Fast/State_stub.h"
#endif

#ifndef __ChunkReader_C
#define __ChunkReader_C

#define Void 0x0000
#define Int8 0x0001
#define Int16 0x0002
#define Int32 0x0004
#define Int64 0x0008
#define UInt8 0x0010
#define UInt16 0x0020
#define UInt32 0x0040
#define UInt64 0x0080
#define Flt 0x0100
#define Dbl 0x0200
#define Bytes 0x0400

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

// Tests for equality of Int8
uint8_t i8_eq(char *ptr, char* ref, size_t)
{
    return *((int8_t*)ptr) == *((int8_t*)ref) ? Int8 : Void;
}
// Tests for equality of Int16
uint8_t i16_eq(char *ptr, char* ref, size_t)
{
    return *((int16_t*)ptr) == *((int16_t*)ref) ? Int16 : Void;
}
// Tests for equality of Int32
uint8_t i32_eq(char *ptr, char* ref, size_t)
{
    return *((int32_t*)ptr) == *((int32_t*)ref) ? Int32 : Void;
}
// Tests for equality of Int64
uint8_t i64_eq(char *ptr, char* ref, size_t)
{
    return *((int64_t*)ptr) == *((int64_t*)ref) ? Int64 : Void;
}

// Tests for equality of all signed ints
uint8_t int_eq(char *ptr, char* ref, size_t value)
{
    if(i8_eq(ptr, ref, value) == 0)
        return Void;
    if(i16_eq(ptr, ref, value) == 0)
        return Int8;
    if(i32_eq(ptr, ref, value) == 0)
        return Int8 | Int16;
    if(i64_eq(ptr, ref, value) == 0)
        return Int8 | Int16 | Int32;
    return Int8 | Int16 | Int32 | Int64;
}

// Tests for equality of all signed ints with at least 2 bytes
uint8_t int16p_eq(char *ptr, char* ref, size_t value)
{
    if(i16_eq(ptr, ref, value) == 0)
        return Void;
    if(i32_eq(ptr, ref, value) == 0)
        return Int16;
    if(i64_eq(ptr, ref, value) == 0)
        return Int16 | Int32;
    return Int16 | Int32 | Int64;
}
// Tests for equality of all signed ints with at least 4 bytes
uint8_t int32p_eq(char *ptr, char* ref, size_t value)
{
    if(i32_eq(ptr, ref, value) == 0)
        return Void;
    if(i64_eq(ptr, ref, value) == 0)
        return Int32;
    return Int32 | Int64;
}
uint8_t voidf(char *, char*, size_t)
{
    return Void;
}


#endif
