/**
 * @filedesc: 
 *  ���ڼ��ض�̬��Ĺ��߿�
 * @author: 
 *  bbwang
 * @date: 
 *  2014��11��6
 * @modify:
 *
**/
#ifndef __FUN_CONTAINER_H__
#define __FUN_CONTAINER_H__

#include <string>
#include <map>
//#include <memory>

class shared_ptr;
using namespace std;

namespace utils{

// ������ֺ���ָ�룬�����¼�
typedef struct fun_con_
{
    int (*create_inst)(uint32_t &handle);
    int (*on_open)(const uint32_t handle, void *data, uint32_t length);
    int (*on_message)(const uint32_t handle, void *data, uint32_t length);
    int (*on_close)(const uint32_t handle, void *data, uint32_t length);
    int (*destory_inst)(const uint32_t handle);
}fun_container;

// ����ָ��
typedef shared_ptr<fun_container> fun_container_ptr;
// ���������ռ���غ���
typedef map<string, fun_container_ptr> space_fun;

}
#endif //__FUN_CONTAINER_H__
