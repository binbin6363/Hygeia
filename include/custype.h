/**
 * @filedesc: 
 * 
 * @author: 
 *  bbwang
 * @date: 
 *  
 * @modify:
 *
**/

#ifndef __CUSTYPE_H__
#define __CUSTYPE_H__

#include <limits.h>

#ifdef WIN32
#include <windows.h>
#endif /* WIN32 */

#ifdef _MSC_VER /* WIN32 */

/* from http://code.google.com/p/msinttypes/ */
#if _MSC_VER > 1000
#pragma once
#endif

#if (_MSC_VER < 1300)
typedef signed char       int8_t;
typedef signed short      int16_t;
typedef signed int        int32_t;
typedef unsigned char     uint8_t;
typedef unsigned short    uint16_t;
typedef unsigned int      uint32_t;
#else
typedef signed __int8     int8_t;
typedef signed __int16    int16_t;
typedef signed __int32    int32_t;
typedef unsigned __int8   uint8_t;
typedef unsigned __int16  uint16_t;
typedef unsigned __int32  uint32_t;
#endif
typedef signed __int64       int64_t;
typedef unsigned __int64     uint64_t;

/********************************/

#else /* *nix */

/* C99 compiler has this head file */
#include <stdint.h>
/* do not nee this */
#if 0
/* part of stdint.h from GNU C Library */
/* __WORDSIZE defined in <bits/wordsize.h> */
#ifndef __int8_t_defined
# define __int8_t_defined
typedef signed char		int8_t;
typedef short int		int16_t;
typedef int			int32_t;
# if __WORDSIZE == 64
typedef long int		int64_t;
# else
__extension__
typedef long long int		int64_t;
# endif
#endif

typedef unsigned char		uint8_t;
typedef unsigned short int	uint16_t;
#ifndef __uint32_t_defined
typedef unsigned int		uint32_t;
# define __uint32_t_defined
#endif
#if __WORDSIZE == 64
typedef unsigned long int	uint64_t;
#else
__extension__
typedef unsigned long long int	uint64_t;
#endif

#endif  /* #if 0 */

#endif /* _MSC_VER */

/********************************/
#if 0
typedef int8_t      int8;
typedef int8_t      INT8;
typedef uint8_t     uint8;
typedef uint8_t     u8;
typedef uint8_t     UINT8;

typedef int16_t     int16;
typedef int16_t     INT16;
typedef uint16_t    uint16;
typedef uint16_t    u16;
typedef uint16_t    UINT16;
typedef uint16_t    WORD16;

typedef int32_t     int32;
typedef int32_t     INT32;
typedef uint32_t    uint32;
typedef uint32_t    u32;
typedef uint32_t    UINT32;
typedef uint32_t    WORD32;

typedef int64_t     int64;
typedef int64_t     INT64;
typedef uint64_t    uint64;
typedef uint64_t    u64;
typedef uint64_t    UINT64;
typedef uint64_t    ULONG64;
typedef uint64_t    WORD64;
typedef uint64_t    DWORD64;
#endif


#endif //__CUSTYPE_H__