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
#ifndef __COMMONH_H__
#define __COMMONH_H__
#include <stdio.h>

// type header
#ifdef WIN32	// for windows
#include <stddef.h>				// for size_t
typedef __int8					int8_t;
typedef __int16					int16_t;
typedef __int32					int32_t;
typedef __int64					int64_t;

typedef unsigned __int8			uint8_t;
typedef unsigned __int16		uint16_t;
typedef unsigned __int32		uint32_t;
typedef unsigned __int64		uint64_t;

typedef int						ssize_t;
typedef int						socklen_t;
typedef uint32_t				in_addr_t;
typedef uint16_t				in_port_t;

#else			// for linux
#include <stdint.h>
#include <stddef.h>				// for size_t
#include <unistd.h>				// for ssize_t and socklen_t
#include <netinet/in.h>			// for in_addr_t and in_port_t

#endif


// net header
#ifdef WIN32 // for windows


#define HYGEIA_SOCKET SOCKET
#define close closesocket
#else        // for linux
#include <sys/socket.h>
#include <sys/types.h>

#define HYGEIA_SOCKET int

#endif

#include "event.h"
#include "inet_addr.h"

#define PORT        25341
#define BACKLOG     5
#define MEM_SIZE    (1024 * 4)

#ifdef WIN32
#define bzero(buf, len) memset(buf, 0, len)
#endif
#endif //__COMMONH_H__