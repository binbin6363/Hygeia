/**
 * @filedesc: 
 *  ���������ļ�
 * @author: 
 *  bbwang
 * @date: 
 *  
 * @modify:
 *
**/

#ifndef __CONFFIG_H__
#define __CONFFIG_H__
#include <stdio.h>

#ifdef _LIB
// ������̬��ĺ�
#define CEXPORT_API 
#define CEXPORT_CLASS __declspec(dllexport)
#else
// ���붯̬��ĺ�
#define CEXPORT_API
#define CEXPORT_CLASS __declspec(dllimport)
#endif
#define INTERNAL_API static


// ��֤���볤��
#define PASSWD_LENGTH 16
// ����豸
#define DEVICE_OUT_PUT 1
#define DEVICE_STD stdout
#define DEVICE_ERR stderr
//#define DEVICE_FILE OUTFILE

#include "singleton.h"
// ��������
class Config : public utils::Singleton<Config>
{
public:

protected:
    Config()
        : MAX_NET_SIZE(4096)
        , buf_cnt(20)
        , per_size(4096)
    {

    }

public:
    uint32_t                      MAX_NET_SIZE;
    uint32_t                      buf_cnt;
    uint32_t                      per_size;
    uint32_t                      backlog;
    std::vector<ServerInfo>       server_infos;
};

class auto_buf_mngr;
auto_buf_mngr *buf_mgr;
#endif//__CONFFIG_H__