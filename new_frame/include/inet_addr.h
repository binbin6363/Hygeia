//! @file inet_addr.h


#ifndef __INET_ADDR_H__
#define __INET_ADDR_H__


#include "commonh.h"


//! @class INET_Addr
//! @brief Internet域地址的封装类
class INET_Addr
{
public:
	INET_Addr();
	~INET_Addr();

	//! 拷贝构造
	INET_Addr(const INET_Addr& rhs);

	//! 赋值操作符
	INET_Addr& operator=(const INET_Addr& rhs);

public:
	//! 设置网络地址
	void set_addr(in_addr_t addr);

	//! 取网络地址
	in_addr_t get_addr() const;

	//! 设置端口(网络字节)
	void set_port(in_port_t port);

	//! 取端口(网络字节)
	in_port_t get_port() const;

	//! 清除地址
	void clear();

private:
	//! 网络地址
	in_addr_t m_addr;

	//! 端口(网络字节)
	in_port_t m_port;
};


#endif // __INET_ADDR_H__
