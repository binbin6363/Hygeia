/**
 * @filedesc: 
 * 会话的基类
 * @author: 
 *  bbwang
 * @date: 
 *  
 * @modify:
 *
**/
#ifndef __NET_SESSION_H__
#define __NET_SESSION_H__
#include "commonh.h"

class ProtoBufMessage;

namespace hygeia {

class NetSession
{
public:
    NetSession();
    ~NetSession();
    int send_msg(uint32_t session_id, const char *data, uint32_t len);
    int send_msg(uint32_t session_id, const ProtoBufMessage &msg);
    int on_receive_msg(const ProtoBufMessage &msg);
    int on_receive_msg(const char *data, uint32_t len);
    int on_open(void *arg, const INET_Addr &remote_addr);
    int on_timeout(int id,void *userData);
    int on_close(uint32_t handle);
    int on_send_msg(const ProtoBufMessage &msg);
    int on_send_msg(const char *data, uint32_t len);
    int on_signal();
    const ServerInfo &server_info();
    void server_info(const ServerInfo &si);
    const uint32_t session_id();
    const uint32_t handle();

protected:
    void msg_report(int err_code);
    void msg_log(const char *data, uint32_t len);

private:
    uint32_t   session_id_;
    uint32_t   sockfd_;
    ServerInfo si_;
    uint32_t   time_out_;

};

}
#endif //__NET_SESSION_H__
