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
#include "ClassFactory.h"

using namespace hygeia;

int run(const string &signature)
{
    BaseClass *cli = ClassFactory::shared_factory.getClassBySignature(signature);
    cli->on_message("hello", 6);
    cli->on_send("hello", 6);
}