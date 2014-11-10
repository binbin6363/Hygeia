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

#ifndef __CLASSFACTORY_H__
#define __CLASSFACTORY_H__

#include "config.h"
#include <string>
#include <map>

using namespace std;

class BaseClass;

typedef map<string, BaseClass*> ClassMap;
typedef map<string, string> SignatureMap;

class ClassFactory
{
public:
    static ClassFactory &shared_factory();
    // ʵ��ǩ����������ǩ������id
    void* getClassBySignature(const string &inst_signature);
    int registClass(const string &inst_signature, BaseClass *class_inst);
    int unRegistClass(const string &inst_signature);
    const char *identify() const;
    int load_lib(const char *lib_path);
private:
    int generate_identify();
    string fetchSignature(const string &inst_signature);
    int signatureCertification(const string &signature);


    ClassMap m_classMap;
    // ϵͳ����ʱ�������ļ����أ������¼���
    SignatureMap m_signatureMap;
    // ������ɣ�ÿ��������������һ��
    char IDENTIFY[PASSWD_LENGTH];
    space_fun spaceFun;

    ClassFactory(void);
    ~ClassFactory(void);
};


#define DECLARE_CLASS(name) \
    protected: \
    static ClassInfo ms_classinfo; \
    public:  \
    virtual ClassInfo* GetClassInfo() const; \
    static Object* CreateObject();

#define IMPLEMENT_CLASS_COMMON(name,func) \
    ClassInfo name::ms_classinfo((#name), \
    (ObjectConstructorFn) func); \
    \
    ClassInfo *name::GetClassInfo() const \
{return &name::ms_classinfo;}

#define IMPLEMENT_CLASS(name)                       \
    IMPLEMENT_CLASS_COMMON(name,name::CreateObject) \
    hygeia::BaseClass* ClassFactory::shared_factory().registClass(signature)         \
{ return new name;}

//���º����������ʵ���ļ�����ӣ������ൽdll
// ��֤ 
// ����ʵ��
// ����ʵ��
#define EXPORT_CLASS(name)              \
    CEXPORT_API int Authenticate()      \
{\
return !strcmp(name::IDENTIFY, ClassFactory::IDENTIFY)\
}\
    CEXPORT_API name* CreateExportObj() \
{\
    return new name;\
}\
    CEXPORT_API int DestroyExportObj(name* pExportClass) \
{\
    delete name;\
}






#endif //__CLASSFACTORY_H__