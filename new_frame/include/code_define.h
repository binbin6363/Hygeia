/**
 * @filedesc: 
 *  ���ֳ����Ķ��壬�����룬Э���
 * @author: 
 *  bbwang
 * @date: 
 *  
 * @modify:
 *
**/
#ifndef __CODE_DEFINE_H__
#define __CODE_DEFINE_H__

// error code
const uint32_t BASE_ERROR_CODE = 0X01;
const uint32_t MAX_ERROR_CODE = 0X0000FFFF;
enum ERROR_CODE
{
    SUCCEED = 0,
    EVENT_BASE_NEW_FAILED = BASE_ERROR_CODE,

    SESSION_CREATE_FAILED = BASE_ERROR_CODE + 0X0010,

    EVENT_BASE_SET_FAILED = BASE_ERROR_CODE + 0X0020,
    EVENT_ADD_FAILED      = BASE_ERROR_CODE + 0X0021,

    // NET CODE
    SOCKET_CREATE_FAILED  = BASE_ERROR_CODE + 0X00FF,
    SOCKET_BIND_FAILED    = BASE_ERROR_CODE + 0X0100,
    SOCKET_LISTEN_FAILED  = BASE_ERROR_CODE + 0X0101,
    SOCKET_CONNECT_FAILED = BASE_ERROR_CODE + 0X0102,
    SOCKET_PEER_CLOSE     = BASE_ERROR_CODE + 0X0103,
    SOCKET_EXCEPTION      = BASE_ERROR_CODE + 0X0104,
    NET_PKG_OK            = BASE_ERROR_CODE + 0X0105,
    NET_PKG_NOT_COMPLETE  = BASE_ERROR_CODE + 0X0106,
    NET_PKG_UNKNOW        = BASE_ERROR_CODE + 0X0107,

    UNDEFINED_ERROR = MAX_ERROR_CODE,
};


// protocol code
const uint32_t BASE_PROTOCOL_CODE = 0X01;             // 1
const uint32_t MAX_PROTOCOL_CODE  = 0X00FFFFFF;       // 16777215
enum PROTOCOL_CODE
{
    HEART_BEAT = 0,

    UNDEFINED_PROTOCOL = MAX_PROTOCOL_CODE,
};

#endif //__CODE_DEFINE_H__
