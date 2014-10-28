/**
 * @filedesc: 
 *  client_session.h
 * 
 * @author: 
 *  bbwang
 * @date: 
 *  2014/10/26 12:44
 *  
 * @modify:
 *
**/

#include "comdef.h"

namespace hygeia{
namespace user{

DECLEAR_STR_IDENTIFY(IDENFITY);

class ClientSession
{
public:
    ClientSession();
    ~ClientSession();


private:
    uint32_t uid_;
};


}// namespace user
} // namespace hygeia

