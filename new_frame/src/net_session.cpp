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

#include "net_session.h"
#include "utils/utils.h"

namespace hygeia {


const ServerInfo &NetSession::server_info()
{
    return si_;
}

void NetSession::server_info(const ServerInfo &si)
{
    si_ = si;
}



const uint32_t NetSession::session_id()
{
    return session_id_;
}

const uint32_t NetSession::handle()
{
    return sockfd_;
}



}