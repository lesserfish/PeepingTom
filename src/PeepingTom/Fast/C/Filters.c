#ifndef __Filters_C
#define __Filters_C

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

#include <string.h>


uint16_t call (uint8_t (*filter)(char *, char*, size_t), char* data, char* reference, size_t size)
{
    return (*filter)(data, reference, size);
}

// Tests for equality of Int8
uint16_t i8_eq(char *ptr, char* ref, size_t)
{
    return *((int8_t*)ptr) == *((int8_t*)ref) ? Int8 : Void;
}
// Tests for equality of Int16
uint16_t i16_eq(char *ptr, char* ref, size_t)
{
    return *((int16_t*)ptr) == *((int16_t*)ref) ? Int16 : Void;
}
// Tests for equality of Int32
uint16_t i32_eq(char *ptr, char* ref, size_t)
{
    return *((int32_t*)ptr) == *((int32_t*)ref) ? Int32 : Void;
}
// Tests for equality of Int64
uint16_t i64_eq(char *ptr, char* ref, size_t)
{
    return *((int64_t*)ptr) == *((int64_t*)ref) ? Int64 : Void;
}

// Tests for equality of all signed ints
uint16_t int_eq(char *ptr, char* ref, size_t value)
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
uint16_t int16p_eq(char *ptr, char* ref, size_t value)
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
uint16_t int32p_eq(char *ptr, char* ref, size_t value)
{
    if(i32_eq(ptr, ref, value) == 0)
        return Void;
    if(i64_eq(ptr, ref, value) == 0)
        return Int32;
    return Int32 | Int64;
}
uint16_t voidf(char *, char*, size_t)
{
    return Void;
}

// Test for inequalities

// Tests for equality of Int8

// LT

uint16_t i8_lt(char *ptr, char* ref, size_t)
{
    return *((int8_t*)ptr) < *((int8_t*)ref) ? Int8 : Void;
}
uint16_t i16_lt(char *ptr, char* ref, size_t)
{
    return *((int16_t*)ptr) < *((int16_t*)ref) ? Int16 : Void;
}
uint16_t i32_lt(char *ptr, char* ref, size_t)
{
    return *((int32_t*)ptr) < *((int32_t*)ref) ? Int32 : Void;
}
uint16_t i64_lt(char *ptr, char* ref, size_t)
{
    return *((int64_t*)ptr) < *((int64_t*)ref) ? Int64 : Void;
}
uint16_t int_lt(char *ptr, char *ref, size_t size)
{
    return i8_lt(ptr, ref, size) || i16_lt(ptr, ref, size) || i32_lt(ptr, ref, size) || i64_lt(ptr, ref, size);
}
uint16_t int16p_lt(char *ptr, char *ref, size_t size)
{
    return i16_lt(ptr, ref, size) || i32_lt(ptr, ref, size) || i64_lt(ptr, ref, size);
}
uint16_t int32p_lt(char *ptr, char *ref, size_t size)
{
    return i32_lt(ptr, ref, size) || i64_lt(ptr, ref, size);
}

// LEQ

uint16_t i8_leq(char *ptr, char* ref, size_t)
{
    return *((int8_t*)ptr) <= *((int8_t*)ref) ? Int8 : Void;
}
uint16_t i16_leq(char *ptr, char* ref, size_t)
{
    return *((int16_t*)ptr) <= *((int16_t*)ref) ? Int16 : Void;
}
uint16_t i32_leq(char *ptr, char* ref, size_t)
{
    return *((int32_t*)ptr) <= *((int32_t*)ref) ? Int32 : Void;
}
uint16_t i64_leq(char *ptr, char* ref, size_t)
{
    return *((int64_t*)ptr) <= *((int64_t*)ref) ? Int64 : Void;
}
uint16_t int_leq(char *ptr, char *ref, size_t size)
{
    return i8_leq(ptr, ref, size) || i16_leq(ptr, ref, size) || i32_leq(ptr, ref, size) || i64_leq(ptr, ref, size);
}
uint16_t int16p_leq(char *ptr, char *ref, size_t size)
{
    return i16_leq(ptr, ref, size) || i32_leq(ptr, ref, size) || i64_leq(ptr, ref, size);
}
uint16_t int32p_leq(char *ptr, char *ref, size_t size)
{
    return i32_leq(ptr, ref, size) || i64_leq(ptr, ref, size);
}

// GT

uint16_t i8_gt(char *ptr, char* ref, size_t)
{
    return *((int8_t*)ptr) > *((int8_t*)ref) ? Int8 : Void;
}
uint16_t i16_gt(char *ptr, char* ref, size_t)
{
    return *((int16_t*)ptr) > *((int16_t*)ref) ? Int16 : Void;
}
uint16_t i32_gt(char *ptr, char* ref, size_t)
{
    return *((int32_t*)ptr) > *((int32_t*)ref) ? Int32 : Void;
}
uint16_t i64_gt(char *ptr, char* ref, size_t)
{
    return *((int64_t*)ptr) > *((int64_t*)ref) ? Int64 : Void;
}
uint16_t int_gt(char *ptr, char *ref, size_t size)
{
    return i8_gt(ptr, ref, size) || i16_gt(ptr, ref, size) || i32_gt(ptr, ref, size) || i64_gt(ptr, ref, size);
}
uint16_t int16p_gt(char *ptr, char *ref, size_t size)
{
    return i16_gt(ptr, ref, size) || i32_gt(ptr, ref, size) || i64_gt(ptr, ref, size);
}
uint16_t int32p_gt(char *ptr, char *ref, size_t size)
{
    return i32_gt(ptr, ref, size) || i64_gt(ptr, ref, size);
}

// GEQ

uint16_t i8_geq(char *ptr, char* ref, size_t)
{
    return *((int8_t*)ptr) >= *((int8_t*)ref) ? Int8 : Void;
}
uint16_t i16_geq(char *ptr, char* ref, size_t)
{
    return *((int16_t*)ptr) >= *((int16_t*)ref) ? Int16 : Void;
}
uint16_t i32_geq(char *ptr, char* ref, size_t)
{
    return *((int32_t*)ptr) >= *((int32_t*)ref) ? Int32 : Void;
}
uint16_t i64_geq(char *ptr, char* ref, size_t)
{
    return *((int64_t*)ptr) >= *((int64_t*)ref) ? Int64 : Void;
}
uint16_t int_geq(char *ptr, char *ref, size_t size)
{
    return i8_geq(ptr, ref, size) || i16_geq(ptr, ref, size) || i32_geq(ptr, ref, size) || i64_geq(ptr, ref, size);
}
uint16_t int16p_geq(char *ptr, char *ref, size_t size)
{
    return i16_geq(ptr, ref, size) || i32_geq(ptr, ref, size) || i64_geq(ptr, ref, size);
}
uint16_t int32p_geq(char *ptr, char *ref, size_t size)
{
    return i32_geq(ptr, ref, size) || i64_geq(ptr, ref, size);
}

// NEQ

uint16_t i8_neq(char *ptr, char* ref, size_t)
{
    return *((int8_t*)ptr) != *((int8_t*)ref) ? Int8 : Void;
}
uint16_t i16_neq(char *ptr, char* ref, size_t)
{
    return *((int16_t*)ptr) != *((int16_t*)ref) ? Int16 : Void;
}
uint16_t i32_neq(char *ptr, char* ref, size_t)
{
    return *((int32_t*)ptr) != *((int32_t*)ref) ? Int32 : Void;
}
uint16_t i64_neq(char *ptr, char* ref, size_t)
{
    return *((int64_t*)ptr) != *((int64_t*)ref) ? Int64 : Void;
}
uint16_t int_neq(char *ptr, char *ref, size_t size)
{
    return i8_neq(ptr, ref, size) || i16_neq(ptr, ref, size) || i32_neq(ptr, ref, size) || i64_neq(ptr, ref, size);
}
uint16_t int16p_neq(char *ptr, char *ref, size_t size)
{
    return i16_neq(ptr, ref, size) || i32_neq(ptr, ref, size) || i64_neq(ptr, ref, size);
}
uint16_t int32p_neq(char *ptr, char *ref, size_t size)
{
    return i32_neq(ptr, ref, size) || i64_neq(ptr, ref, size);
}

// ByteString
uint16_t bs_eq(char *ptr, char *ref, size_t size)
{
    if(memcmp(ptr, ref, size) == 0)
        return Bytes;
    return Void;
}
#endif
