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
// ������̬��ĺ�
#define CEXPORT_API extern "C" __declspec(dllexport)
#else
// ���붯̬��ĺ�
#define CEXPORT_API __declspec(dllimport)
#endif

// ��֤���볤��
#define PASSWD_LENGTH 16
// ����豸
#define DEVICE_STD stdout
#define DEVICE_ERR stderr
//#define DEVICE_FILE OUTFILE


#endif//__CONFFIG_H__