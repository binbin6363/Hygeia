/**
 * @filedesc: 
 *  工程配置文件
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
// 导出动态库的宏
#define CEXPORT_API 
#define CEXPORT_CLASS __declspec(dllexport)
#else
// 导入动态库的宏
#define CEXPORT_API
#define CEXPORT_CLASS __declspec(dllimport)
#endif
#define INTERNAL_API static


// 认证密码长度
#define PASSWD_LENGTH 16
// 输出设备
#define DEVICE_OUT_PUT 1
#define DEVICE_STD stdout
#define DEVICE_ERR stderr
//#define DEVICE_FILE OUTFILE

#include "singleton.h"
// 配置属性
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