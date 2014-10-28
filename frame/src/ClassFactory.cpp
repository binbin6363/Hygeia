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

ClassFactory::ClassFactory(void)
{
}

ClassFactory::~ClassFactory(void)
{
}

static ClassFactory &ClassFactory::shared_factory()
{
    static ClassFactory shared_fascory_;
    return shared_fascory_;
}

void* ClassFactory::getClassBySignature(const string &inst_signature)
{
    ClassMap::iterator iter = m_classMap.find(inst_signature);
    if (m_classMap.end() != iter)
    {
        return m_classMap[inst_signature];
    }
    else
    {
        return NULL;
    }

}

string ClassFactory::fetchSignature(const string &inst_signature)
{
    string signature = inst_signature;
    uint32_t pos = signature.rfind("_");
    signature = signature.substr(0, pos);
    return signature;
}

// 反射创建实例，由子类自己调用
int ClassFactory::registClass(const string &inst_signature, BaseClass *class_inst)
{
// 1.签名认证
    string signature = fetchSignature(inst_signature);
    int ret = signatureCertification(signature);
    if (!ret)
    {
        // 认证失败，返回
        return ret;
    }

// 2.创建实例，添加到实例表中
}

int ClassFactory::unRegistClass(const string &inst_signature)
{


}
// 认证签名的前缀，后缀表示实例id。成功返回0，失败返回1
int ClassFactory::signatureCertification(const string &signature)
{
    SignatureMap::const_iterator iter = m_signatureMap.find(signature);
    if (m_signatureMap.end() == iter)
    {
        return 1;
    }
    else
    {
        return 0;
    }
}


