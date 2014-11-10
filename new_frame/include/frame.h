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

#ifndef __FRAME_H__
#define __FRAME_H__
#include "config.h"
#include "inet_addr.h"

// interface
#ifdef __cplusplus
extern "C" {
#endif

// ����tcp����ˣ�ͬʱע��ص�
CEXPORT_API int create_tcp_server(evutil_socket_t &sock, const INET_Addr& local_addr);

// ����tcp�ͻ��ˣ�ͬʱע��ص�
CEXPORT_API int create_tcp_client(evutil_socket_t &sock, const INET_Addr& local_addr);

// �������
CEXPORT_API int start_frame(int argc, char *argv[]);

// ֹͣ���
CEXPORT_API int stop_frame();

#ifdef __cplusplus
}
#endif

#endif//__FRAME_H__