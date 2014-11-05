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

#ifndef __CONFFIG_H__
#define __CONFFIG_H__

#ifndef CEXPORT_API
// 导出动态库的宏
#define CEXPORT_API extern "C" __declspec(dllexport)
#else
// 导入动态库的宏
#define CEXPORT_API __declspec(dllimport)
#endif

// 认证密码长度
#define PASSWD_LENGTH 16
// 输出设备
#define DEVICE_STD stdout
#define DEVICE_ERR stderr
//#define DEVICE_FILE OUTFILE


#endif//__CONFFIG_H__