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
    // ʵ��ǩ����������ǩ������id
    void* getClassBySignature(const string &inst_signature);
    int registClass(const string &inst_signature, BaseClass *class_inst);
    int unRegistClass(const string &inst_signature);

private:
    string fetchSignature(const string &inst_signature);
    int signatureCertification(const string &signature);


    ClassMap m_classMap;
    // ϵͳ����ʱ�������ļ����أ������¼���
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