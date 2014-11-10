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


#define TEST_LIBEVENT
#ifdef TEST_LIBEVENT
#include "frame.h"
#pragma comment(lib,"libevent.lib")
#pragma comment(lib,"new_frame.lib")

int main(int argc, char *argv[])
{
    start_frame(argc, argv);
    stop_frame();
    return 0;
}






//////////////////////////////////////////////////////////////////////////////
#else
#include <Windows.h>
#include <iostream>

#pragma comment(lib, "ws2_32.lib")
using namespace std;

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
}WSInit;

const int port = 25341;

int main()
{
    int i = 0;
    //cin >> i;
    cout << "client start, connecting..." << endl;
    if(i)
    {
        SOCKET client_socket = INVALID_SOCKET;
        SOCKET server_socket = socket(AF_INET, SOCK_STREAM, 0);
        if(server_socket == INVALID_SOCKET)
        {
            cout << "create server socket error" << endl;
            exit(0);
        }
        sockaddr_in serveraddr = {0};
        sockaddr_in clientaddr = {0};

        serveraddr.sin_family = AF_INET;
        serveraddr.sin_addr.S_un.S_addr = htonl(INADDR_ANY);
        serveraddr.sin_port = htons(port);

        if(bind(server_socket, (sockaddr*)&serveraddr, sizeof(sockaddr_in)) == SOCKET_ERROR)
        {
            cout << "bind error" << endl;
            exit(0);
        }

        if(listen(server_socket, 5) == SOCKET_ERROR)
        {
            cout << "listen error" << endl;
            exit(0);
        }
        cout << "listen socket at " << port << endl;

        int addr_len = sizeof(sockaddr_in);
        if( (client_socket = accept(server_socket, (sockaddr*)&clientaddr, &addr_len)) == SOCKET_ERROR)
        {
            cout << "accept connect error" << endl;
            exit(0);
        }
        cout << "accept a connection from " << inet_ntoa(clientaddr.sin_addr) << ":" << ntohs(clientaddr.sin_port) << endl;

        char recvbuffer[1024] = {0};
        while(true)
        {
            memset(recvbuffer, 0, sizeof(recvbuffer));
            int ret = recv(client_socket, recvbuffer, sizeof(recvbuffer), 0);
            if(ret == 0 || ret == SOCKET_ERROR)
            {
                cout << "client socket closed!" << endl;
                break;
            }
            cout << "recv: " << recvbuffer << endl;
        }
        closesocket(server_socket);
        closesocket(client_socket);

    }
    else
    {
        // client 

        SOCKET client_socket = socket(AF_INET, SOCK_STREAM, 0);
        if(client_socket == INVALID_SOCKET)
        {
            cout << "create client socket error" << endl;
            exit(0);
        }

        sockaddr_in serveraddr = {0};
        serveraddr.sin_family = AF_INET;
        serveraddr.sin_addr.S_un.S_addr = inet_addr("127.0.0.1");
        serveraddr.sin_port= htons(port);
        if(connect(client_socket, (sockaddr*)&serveraddr, sizeof(sockaddr_in)) == SOCKET_ERROR)
        {
            cout << "connect failed" << endl;
            exit(0);
        }
        cout << "connect successfully" << endl;
        char sendbuffer[1024] = {0};
        while(true)
        {
            cin >> sendbuffer;
            int ret = send(client_socket, sendbuffer, strlen(sendbuffer), 0);
            if(ret == 0 || ret == SOCKET_ERROR)
            {
                cout << "server closed" << endl;
                break;
            }
        }
        closesocket(client_socket);
    }

    system("pause");
    return 0;
}

#endif // TEST_LIBEVENT