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
#include "event.h"
#include "commonh.h"
#include "frame.h"
#include "code_define.h"
#include "macros_define.h"
#include "utils/log.h"

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


INTERNAL_API void on_accept(evutil_socket_t sock, short event, void* arg)
{
    struct sockaddr_in cli_addr;
    int newfd, sin_size;
    struct sock_ev* ev = (struct sock_ev*)malloc(sizeof(struct sock_ev));
    ev->read_ev  = (struct event*)malloc(sizeof(struct event));
    ev->write_ev = (struct event*)malloc(sizeof(struct event));
    sin_size = sizeof(struct sockaddr_in);
    newfd = accept(sock, (struct sockaddr*)&cli_addr, &sin_size);
    LOG(INFO)("[FRAME SESSION CONNECT, SESSION ID=%u]", newfd);
    event_set(ev->read_ev, newfd, EV_READ|EV_PERSIST, on_read, ev);
    event_base_set(base, ev->read_ev);
    event_add(ev->read_ev, NULL);
    event_set(ev->write_ev, sock, EV_WRITE, on_write, ev->buffer);
    event_base_set(base, ev->write_ev);
    event_add(ev->write_ev, NULL);
}


INTERNAL_API void on_msg(evutil_socket_t sock, short event, void* arg)
{
    if (INVALID_SOCKET == sock)
    {
        LOG(ERROR)("[FRAME INVALID SOCKET, TYPE=%d]", event);
        return;
    }
    switch(event)
    {
    case EV_READ:
        break;
    case EV_WRITE:
        break;
    default:
        LOG(ERROR)("[FRAME UNSUPPORTED EVENT TYPE]");
        break;
    }
}
// define
INTERNAL_API void on_accept(evutil_socket_t sock, short event, void* arg)
{
    struct sockaddr_in cli_addr;
    int newfd, sin_size;
    struct event* net_event = shared_ptr<struct event*>(new struct event);

    //struct sock_ev* ev = (struct sock_ev*)malloc(sizeof(struct sock_ev));
    //ev->read_ev  = (struct event*)malloc(sizeof(struct event));
    //ev->write_ev = (struct event*)malloc(sizeof(struct event));
    sin_size = sizeof(struct sockaddr_in);
    newfd = accept(sock, (struct sockaddr*)&cli_addr, &sin_size);
    LOG(INFO)("[FRAME SESSION CONNECT, SESSION ID=%u]", newfd);
    event_new(net_event, newfd, EV_TIMEOUT|EV_READ|EV_WRITE|EV_SIGNAL|EV_PERSIST, on_msg, arg);
    event_base_set(base, net_event);
    event_add(net_event, NULL);
}
//
//CEXPORT_API int create_tcp_server(const INET_Addr& local_addr)
//{
//    return 0;
//}
//
//CEXPORT_API int create_tcp_client(const INET_Addr& local_addr)
//{
//    return 0;
//}

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
    LOG(INFO)("[FRAME START...]");;
    struct sockaddr_in my_addr;
    evutil_socket_t sock;

    sock = socket(AF_INET, SOCK_STREAM, 0);
    int yes = 1;
    setsockopt(sock, SOL_SOCKET, SO_REUSEADDR, (const char*)&yes, sizeof(int));
    bzero(&my_addr, sizeof(my_addr));
    my_addr.sin_family = AF_INET;
    my_addr.sin_port = htons(PORT);
    my_addr.sin_addr.s_addr = INADDR_ANY;
    bind(sock, (const struct sockaddr*)&my_addr, sizeof(struct sockaddr));
    if (INVALID_SOCKET == sock)
    {
        LOG(ERROR)("[FRAME BIND SOCKET FAILED]");
        ret = SOCKET_BIND_FAILED;
        break;
    }
    listen(sock, BACKLOG);

    struct event listen_ev;
    event_set_fatal_callback(sock_err_report);
    base = event_base_new();
    if (NULL == base)
    {
        LOG(ERROR)("[FRAME NEW EVENT BASE FAILED]");
        ret = EVENT_BASE_NEW_FAILED;
        break;
    }
    event_set(&listen_ev, sock, EV_READ|EV_PERSIST, on_accept, NULL);
    event_base_set(base, &listen_ev);
    event_add(&listen_ev, NULL);
    event_base_dispatch(base);

    ONCE_LOOP_LEAVE
    LOG(INFO)("[FRAME START RET CODE:%d]", ret);;
    return ret;
}


CEXPORT_API int stop_frame ()
{
    LOG(INFO)("[FRAME STOP...]");
    return 0;
}
