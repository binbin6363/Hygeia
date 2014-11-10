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

#include "config.h"
#include "event.h"
#include "commonh.h"
#include "frame.h"
#include "code_define.h"
#include "macros_define.h"
#include "utils/log.h"
#include "client_session.h"
#include "server_session.h"


using namespace hygeia;
using namespace utils;
#ifdef WIN32
#pragma comment(lib, "ws2_32.lib")
class WSAInit
{
public:
    WSAInit()
    {
        WSADATA swaData;
        WSAStartup(MAKEWORD(2, 2), &swaData);
    }

    ~WSAInit()
    {
        WSACleanup();
    }
};
#endif

// 日志初始化标注
static int LOG_INITED = 0;
static char *g_buffer = NULL;

#ifdef WIN32
INTERNAL_API WSAInit WSInit;
#endif

struct event_base* base;
struct sock_ev {
    struct event* read_ev;
    struct event* write_ev;
    char* buffer;
};

INTERNAL_API void release_sock_event(struct sock_ev* ev)
{
    event_del(ev->read_ev);
    free(ev->read_ev);
    free(ev->write_ev);
    free(ev->buffer);
    free(ev);
}

INTERNAL_API void on_write(evutil_socket_t sock, short event, void* arg)
{
    LOG(ERROR)("[FRAME write event not implements yet. SESSION ID=%u]", sock);
    fprintf(DEVICE_ERR, "[FRAME write event not implements yet. SESSION ID=%u]", sock);
}


INTERNAL_API void on_read(evutil_socket_t sock, short event, void* arg)
{
    struct event* write_ev;
    int size;
    struct sock_ev* ev = (struct sock_ev*)arg;
    ev->buffer = (char*)malloc(MEM_SIZE);
    bzero(ev->buffer, MEM_SIZE);
    size = recv(sock, ev->buffer, MEM_SIZE, 0);
    if (size == -1) {
        LOG(INFO)("[FRAME SESSION CLOSE, SESSION ID=%u]", sock);
        release_sock_event(ev);
        close(sock);
        return;
    }
    LOG(DEBUG)("[FRAME RECV DATA: SIZE=%d, SESSION ID=%u]", size, sock);
    LOG_HEX(ev->buffer, size, utils::L_DEBUG);
}

INTERNAL_API void sock_err_report(int code)
{
    LOG(ERROR)("[FRAME SOCK FATAL ERROR, ERROR CODE=%d]", code);
}


//INTERNAL_API void on_accept(evutil_socket_t sock, short event, void* arg)
//{
//    struct sockaddr_in cli_addr;
//    int newfd, sin_size;
//    struct sock_ev* ev = (struct sock_ev*)malloc(sizeof(struct sock_ev));
//    ev->read_ev  = (struct event*)malloc(sizeof(struct event));
//    ev->write_ev = (struct event*)malloc(sizeof(struct event));
//    sin_size = sizeof(struct sockaddr_in);
//    newfd = accept(sock, (struct sockaddr*)&cli_addr, &sin_size);
//    LOG(INFO)("[FRAME SESSION CONNECT, SESSION ID=%u]", newfd);
//    event_set(ev->read_ev, newfd, EV_READ|EV_PERSIST, on_read, ev);
//    event_base_set(base, ev->read_ev);
//    event_add(ev->read_ev, NULL);
//    event_set(ev->write_ev, sock, EV_WRITE, on_write, ev->buffer);
//    event_base_set(base, ev->write_ev);
//    event_add(ev->write_ev, NULL);
//}

// 分包，没有校验，会有问题，一个包错误，后面全错
int split_net_pkg(const char *data, uint32_t len, uint32_t &pkg_begin, uint32_t &pkg_len)
{
    int ret = NET_PKG_OK;
    uint32_t data_len = ntohl((uint32_t *)data);
    ONCE_LOOP_ENTER
    if (0 == len || NULL == data)
    {
        pkg_begin = 0;
        pkg_len = 0;
        break;
    }
    // 包不完整，下次再分隔
    if (data_len > Config::inst().MAX_NET_SIZE)
    {
        ret = NET_PKG_UNKNOW;
        pkg_len = data_len;
        break;
    }
    // 
    else if (data_len > len)
    {
        ret = NET_PKG_NOT_COMPLETE;
        break;
    }
    // 包完整
    else if (data_len <= len)
    {
        pkg_begin = 0；
        pkg_len = data_len;
        break;
    }
    ONCE_LOOP_LEAVE
    return ret;
}

int read_data(evutil_socket_t sock, const NetSession *session)
{
    int ret = SUCCEED;
    ONCE_LOOP_ENTER
    auto_buf<> au_buffer;
    const uint32_t &max_size = Config::inst().MAX_NET_SIZE;
    uint32_t data_pos = 0;
    while (true)
    {
        int rc = recv(sock, g_buffer, max_size, 0);
        // rc > 0表示读到数据
        if (rc > 0)
        {
            au_buffer.append(g_buffer, rc);
            int packet_begin = 0;
            int packet_len = 0;

            // 循环拆包
            while (true)
            {
                packet_len = 0;
                int split_result = split_net_pkg(au_buffer.data() + data_pos, au_buffer.size() - data_pos, packet_begin, packet_len);
                if (NET_PKG_OK == split_result && 0 != packet_len)
                {
                    // got a packet, notify business
                    session->on_receive_msg(au_buffer.data() + data_pos, packet_len);
                    data_pos += packet_len;
                }
                else if (NET_PKG_NOT_COMPLETE == split_result)
                {
                    // 包不完整，下次自动扩充buf
                    break;
                }
                else
                {
                    // 协议错，可以断连接
                    LOG(ERROR)("[FRAME INPUT PKG ERROR, SPLIT RET:%d, CLOSE SESSION.]", split_result);
                    ret = SOCKET_EXCEPTION;
                    au_buffer.reset();
                    session->on_close(0);
                    return ret;
                }
            }

            if (1)
            {
                //m_recv_buffer.recycle();
                au_buffer.reset();
                data_pos = 0;
            }
        }
        // rc == 0表示对端关闭
        else if (rc == 0)
        {
            // close
            LOG(DEBUG)("[FRAME SOCKET CLOSE BY PEER NORMALLY]");
            ret =  SOCKET_PEER_CLOSE;
            session->on_close(0);
            break;
        }
        // 否则就是网络出错
        else
        {
            // 缓冲区已满
            if (EAGAIN == error_no()) 
            {
                LOG(WARN)("[FRAME SOCKET BUFFER FULL]");
                ret = SUCCEED;
                break;
            }
            // 异常
            else 
            {
                // exception
                LOG(WARN)("[FRAME DATA RECV EXCEPTION, ERRNO:%d]", error_no());
                session->on_close(0);
                ret = SOCKET_EXCEPTION;
                break;
            }
        }
    }
    ONCE_LOOP_LEAVE
    return ret;
}


int write_data(evutil_socket_t sock, auto_buf<> &au_buffer)
{
    if (m_current_send_task != NULL)
    {
        int rc = send(sock, m_current_send_task->packet->ptr() + m_current_send_length, m_current_send_task->packet->length() - m_current_send_length, 0);
        if (rc > 0) {
            // 发送成功
            m_current_send_length += rc;

            if (m_current_send_length == m_current_send_task->packet->length()) {
                // 当前包发送完毕
                delete m_current_send_task;
                m_current_send_task = NULL;
                m_current_send_length = 0;
            }
            else {
                // 短写
                return -3;
            }
        }
        else if (rc == 0) {
            // close
            LOG(WARN)("SOCK_Stream::handle_output, socket close by peer send curr, send m_current_send_task %d for net:%d needlen:%d return rc:%d",
                m_current_send_task->packet->length() - m_current_send_length,
                get_id(), m_current_send_task->packet->length(), rc);
            return -1;
        }
        else {
            if (EAGAIN == error_no()) {
                LOG(DEBUG)("SOCK_Stream::handle_output, send return EAGAIN, send m_current_send_task %d for net:%d needlen:%d return rc:%d",
                    m_current_send_task->packet->length() - m_current_send_length,
                    get_id(), m_current_send_task->packet->length(), rc);
                return -3;	//数据未写完，需要等待后续写入
            }
            else {
                LOG(WARN)("SOCK_Stream::handle_output, error, send error, errno:%d, send m_current_send_task %d for net:%d needlen:%d return rc:%d",
                    error_no(), m_current_send_task->packet->length() - m_current_send_length,
                    get_id(), m_current_send_task->packet->length(), rc);
                return -2;
            }
        }
    }

    assert(m_current_send_task == NULL);
    assert(m_current_send_length == 0);

    while (true)
    {
        bool brc = m_send_task_queue.read(m_current_send_task);
        if (false == brc) {
            // 发送队列已空
            break;
        }

        int rc = send(m_socket, m_current_send_task->packet->ptr(), m_current_send_task->packet->length(), 0);
        if (rc > 0) {
            if (rc == m_current_send_task->packet->length()) {
                // 当前包发送完毕
                delete m_current_send_task;
                m_current_send_task = NULL;
            }
            else {
                // 短写
                m_current_send_length = rc;
                LOG(DEBUG)("SOCK_Stream::handle_output, send m_send_task_queue %d for net:%d needlen:%d return rc:%d",
                    m_current_send_task->packet->length() - m_current_send_length,
                    get_id(), m_current_send_task->packet->length(), rc);
                return -3;
            }
        }
        else if (rc == 0) {
            // close
            LOG(WARN)("SOCK_Stream::handle_output, socket close by peer, send m_send_task_queue %d for net:%d needlen:%d return rc:%d",
                m_current_send_task->packet->length() - m_current_send_length,
                get_id(), m_current_send_task->packet->length(), rc);
            return -1;
        }
        else {
            if (EAGAIN == error_no()) {
                return -3;	//数据未写完，需要等待后续写入
            }
            else {
                LOG(WARN)("SOCK_Stream::handle_outputerror, send error, errno:%d, socket close by peer, send m_send_task_queue %d for net:%d needlen:%d return rc:%d",
                    error_no(), m_current_send_task->packet->length() - m_current_send_length,
                    get_id(), m_current_send_task->packet->length(), rc);
                return -2;
            }
        }
    }

    assert(m_current_send_task == NULL);
    assert(m_current_send_length == 0);

    int rc = reactor()->remove_handler(this, MASK_WRITE);
    if (0 != rc) {
        // 设置reactor失败,认为socket异常
        LOG(ERROR)("SOCK_Stream::handle_output error, remove_handler error");
        return -2;
    }

}

// 数据层收到消息，应该要将该消息推送到业务层
INTERNAL_API void on_msg(evutil_socket_t sock, short event, void* arg)
{
    ONCE_LOOP_ENTER
    if (INVALID_SOCKET == sock)
    {
        LOG(ERROR)("[FRAME INVALID SOCKET, TYPE=%d]", event);
        break;
    }
    if (NULL == arg)
    {
        LOG(ERROR)("[FRAME SESSION IS NULL, event=%u, SOCK=%u]", event, sock);
        break;
    }
    NetSession *session = dynamic_cast<NetSession *>(arg);
    if (NULL == session)
    {
        LOG(ERROR)("[FRAME SESSION CAST FAILED]");
        break;
    }
    //EV_TIMEOUT|EV_READ|EV_WRITE|EV_SIGNAL|EV_PERSIST
    switch(event)
    {
    case EV_TIMEOUT:
        session->on_timeout();
        break;
    case EV_READ:
        read_data(sock, session);
        // parse msg here and call on_receive_msg(msg)
        break;
    case EV_WRITE:
        write_data(sock, session);
        break;
    case EV_SIGNAL:
        session->on_signal();
        break;
    case EV_PERSIST:
        break;
    default:
        LOG(ERROR)("[FRAME UNSUPPORTED EVENT TYPE]");
        break;
    }
    ONCE_LOOP_LEAVE
    return;
}
// handle connect event, new a client session
INTERNAL_API void on_accept(evutil_socket_t sock, short event, void* arg)
{
    struct sockaddr_in cli_addr;
    int newfd, sin_size;
    struct event* net_event = shared_ptr<struct event*>(new struct event);

    sin_size = sizeof(struct sockaddr_in);
    newfd = accept(sock, (struct sockaddr*)&cli_addr, &sin_size);
    LOG(INFO)("[FRAME SESSION CONNECT, SESSION ID=%u]", newfd);
    event_new(net_event, newfd, EV_TIMEOUT|EV_READ|EV_WRITE|EV_SIGNAL|EV_PERSIST, on_msg, cli_session);
    event_base_set(base, net_event);
    event_add(net_event, NULL);
}

CEXPORT_API int create_tcp_server(evutil_socket_t &sock, const INET_Addr& local_addr)
{
    int ret = SUCCEED;
    ONCE_LOOP_ENTER
    struct sockaddr_in my_addr;

    sock = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    if (INVALID_SOCKET == sock) 
    {
        LOG(ERROR)("[FRAME BIND SOCKET FAILED, SOCKET ERROR, ERRNO:%d]", error_no());
        sock = INVALID_SOCKET;
        ret = SOCKET_CREATE_FAILED;
        return ret;
    }
    // none blocking
    set_socket_nonblocking(sock);
    bzero(&my_addr, sizeof(my_addr));
    my_addr.sin_family = AF_INET;
    my_addr.sin_port = htons(local_addr.get_port());
    my_addr.sin_addr.s_addr = local_addr.get_addr();
    ret = bind(sock, (const struct sockaddr*)&my_addr, sizeof(struct sockaddr));
    if (SUCCEED != ret)
    {
        LOG(ERROR)("[FRAME BIND SOCKET FAILED, ERRNO:%d]", error_no());
        sock = INVALID_SOCKET;
        ret = SOCKET_BIND_FAILED;
        break;
    }
    ret = listen(sock, Config::inst().backlog);
    if (SUCCEED != ret)
    {
        LOG(ERROR)("[FRAME LISTEN SOCKET FAILED, ERRNO:%d]", error_no());
        sock = INVALID_SOCKET;
        ret = SOCKET_LISTEN_FAILED;
        break;
    }
    ONCE_LOOP_LEAVE
    return ret;
}

CEXPORT_API int create_tcp_client(evutil_socket_t &sock, const INET_Addr& server_addr)
{
    int ret = SUCCEED;
    ONCE_LOOP_ENTER
    struct sockaddr_in my_addr;

    sock = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    if (INVALID_SOCKET == sock) 
    {
        LOG(ERROR)("[FRAME BIND SOCKET FAILED, SOCKET ERROR, ERRNO:%d]", error_no());
        sock = INVALID_SOCKET;
        ret = SOCKET_CREATE_FAILED;
        return ret;
    }
    // none blocking
    set_socket_nonblocking(sock);

    ret = connect(socket, (sockaddr*)&server_addr, sizeof(server_addr));
    if (SUCCEED != ret)
    {
        LOG(ERROR)("[FRAME CONNECT SOCKET FAILED, ERRNO:%d]", error_no());
        sock = INVALID_SOCKET;
        ret = SOCKET_CONNECT_FAILED;
        break;
    }
    ONCE_LOOP_LEAVE
    return ret;
}


//CEXPORT_API int create_tcp_client(const INET_Addr& local_addr)
//{
//    return 0;
//}

INTERNAL_API int attach_libevent(const evutil_socket_t &sock, const ServerInfo &si)
{
    int ret = SUCCEED;
    ONCE_LOOP_ENTER
    NetSession *session = new ClientSession;
    if (NULL == session)
    {
        LOG(ERROR)("[FRAME SESSION CREATE FAILED]");
        ret = SESSION_CREATE_FAILED;
        break;
    }
    session->server_info(si);
    struct event *listen_ev = event_new(base, sock, EV_TIMEOUT|EV_READ|EV_WRITE|EV_SIGNAL|EV_PERSIST, on_msg, session);
    struct timeval time_out;
    // handle heart beat timeout
    time_out.tv_sec = si.heart_beat_interval;
    ret = event_base_set(base, listen_ev);
    if (SUCCEED == ret)
    {
        LOG(ERROR)("[FRAME EVENT BASE SET FAILED]");
        ret = EVENT_BASE_SET_FAILED;
        break;
    }
    ret = event_add(listen_ev, &time_out);
    if (SUCCEED == ret)
    {
        LOG(ERROR)("[FRAME EVENT ADD FAILED]");
        ret = EVENT_ADD_FAILED;
        break;
    }
    ONCE_LOOP_LEAVE
    LOG(INFO)("[FRAME ATTACH EVENT DONE, CODE=%d]", ret);
    fprintf(DEVICE_STD, "[FRAME ATTACH EVENT DONE, CODE=%d]", ret);
    return ret;
}

CEXPORT_API int start_frame(int argc, char* argv[])
{
    int ret = SUCCEED;
    // set log
    if (!LOG_INITED)
    {
        LOG_INIT("hygeia", "DEBUG");
        LOG_OPEN();
        utils::Log::global_log.set_max_level("DEBUG");
        LOG_INITED = 1;
    }
    ONCE_LOOP_ENTER
    LOG(INFO)("[FRAME START...]");

    // read config
    LOG(INFO)("[FRAME READING CONFIG]");
    // buf use common
    g_buffer = new char[Config::inst().MAX_NET_SIZE];

    // init libevent
    base = event_base_new();
    if (NULL == base)
    {
        LOG(ERROR)("[FRAME NEW EVENT BASE FAILED]");
        ret = EVENT_BASE_NEW_FAILED;
        break;
    }
    event_set_fatal_callback(sock_err_report);

    // create server
    LOG(INFO)("[FRAME STARTING SELF SERVER]");
    for (int i = 0; i < Config::inst().server_infos.size(); ++i)
    {
        INET_Addr addr;
        ServerInfo &si = Config::inst().server_infos[i];
        string strAddr = si.server_ip+":"+si.server_port;
        FromStringToAddr(strAddr.c_str(), addr);
        evutil_socket_t sock = INVALID_SOCKET;
        ret = create_tcp_server(sock, addr);
        attach_libevent(sock, si);
    }

    // create client
    LOG(INFO)("[FRAME CONNECTING PEER SERVER]");

    // start autobuf, default init 20 buf, per buf 4096 size
    buf_mgr = new auto_buf_mngr(Config::inst().buf_cnt, Config::inst().per_size);

    event_base_dispatch(base);
    event_free(listen_ev);
    event_base_free(base);

    ONCE_LOOP_LEAVE
    LOG(INFO)("[FRAME START RET CODE=%d]", ret);;
    fprintf(DEVICE_STD, "[FRAME START RET CODE=%d]", ret);
    return ret;
}


CEXPORT_API int stop_frame ()
{
    LOG(INFO)("[FRAME STOP...]");
    return 0;
}
