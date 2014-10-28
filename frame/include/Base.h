/**
 * @filedesc: 
 *  client_session.h
 *  compile .dll or .so
 * @author: 
 *  bbwang
 * @date: 
 *  2014/10/26 12:44
 *  
 * @modify:
 *
**/

#ifndef __BASE_H__
#define __BASE_H__
namespace hygeia{

static const char *IDENFITY = "hygeia::BaseClass\0";

struct BaseClass
{
    virtual on_message(const char *data, uint32_t len) = 0;
    virtual handle(const char *data, uint32_t len) = 0;
    virtual on_send(const char *data, uint32_t len) = 0;
};


}




#endif//__BASE_H__