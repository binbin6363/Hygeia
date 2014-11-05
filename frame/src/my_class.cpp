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
#include "custype.h"
namespace hygeia{
namespace myclass{

uint32_t global_id_ = 1;

MyClass::MyClass()
: id_(global_id_++)
{
    string inst_signature = IDENFITY + "_" + id_;
    ClassFactory::shared_factory().registClass(inst_signature);
}

MyClass::~MyClass()
{
    string inst_signature = IDENFITY + "_" + id_;
    ClassFactory::shared_factory().unRegistClass(inst_signature);
    id_ = 0;
}



//IMPLEMENT_CLASS(MyClass)

}
}
