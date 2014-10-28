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
class BaseClass;

typedef map<string, BaseClass*> ClassMap;
typedef map<string, string> SignatureMap;

class ClassFactory
{
public:
    static ClassFactory &shared_factory();
    // 实例签名，就是类签名加上id
    void* getClassBySignature(const string &inst_signature);
    int registClass(const string &inst_signature, BaseClass *class_inst);
    int unRegistClass(const string &inst_signature);

private:
    string fetchSignature(const string &inst_signature);
    int signatureCertification(const string &signature);


    ClassMap m_classMap;
    // 系统启动时从配置文件加载，可重新加载
    SignatureMap m_signatureMap;

    ClassFactory(void);
    ~ClassFactory(void);
};

/*
#define DECLARE_CLASS(className)\  
string className##Name ;        \  
static CKDynamicClass* m_className##dc ;  

#define IMPLEMENT_CLASS(className)  \  
CKDynamicClass* className::m_className##dc = \  
new CKDynamicClass(#className, className::createInstance) ;  
*/
#endif //__CLASSFACTORY_H__