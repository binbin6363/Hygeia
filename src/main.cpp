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
#include "client_session.h"
#include "gen/head.pb.h"


using namespace hygeia;
using namespace user;
using namespace gen;

int main(int argc, char *argv[])
{
    int ret = 0;
    ClientSession cs;
    MessageHead head;
    head.set_cmd(1002);
    head.set_imei("sdaseds");
    head.set_uid(78234);

    return ret;
}



