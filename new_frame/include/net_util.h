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

#ifndef __NET_UTIL_H__
#define __NET_UTIL_H__
#include "commonh.h"

namespace utils{

class netSpliter{

public:
    int split(const char *in_data, int in_len, char &*out_data, int &out_len);



private:
    HYGEIA_SOCKET sock_fd;

};


}// namespace utils
#endif //__NET_UTIL_H__
