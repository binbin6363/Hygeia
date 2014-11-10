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
#include "custype.h"
#include "utils/sutils.h"
#include "utils/sulib.h"
using namespace sp;

char ClassFactory::IDENTIFY[PASSWD_LENGTH];

ClassFactory::ClassFactory(void)
{
    memset(ClassFactory::IDENTIFY, 0, PASSWD_LENGTH);
    generate_identify();
}

ClassFactory::~ClassFactory(void)
{
    memset(IDENTIFY, 0, PASSWD_LENGTH);
}

int ClassFactory::generate_identify()
{
    srand(time(NULL));
    for (int i = 0; i < (int)PASSWD_LENGTH; ++i)
    {
        // 生成可打印的明文[33-126]
        IDENTIFY[i] = (rand() % 94) + 33;
    }
    fprintf(DEVICE_STD, "class factory generate identify:%s\n", IDENTIFY);
}

// TODO:获取该值需要身份认证
const char *ClassFactory::identify() const
{
    // 1.身份确认
    // 2.身份合法返回，反之返回NULL
    return IDENTIFY;
}

static ClassFactory &ClassFactory::shared_factory()
{
    static ClassFactory shared_fascory_;
    return shared_fascory_;
}

void* ClassFactory::getClassBySignature(const string &signature)
{
    ClassMap::iterator iter = m_classMap.find(signature);
    if (m_classMap.end() != iter)
    {
        return m_classMap[signature];
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



// 加载lib，提取函数，将函数存储于fun_container中，存于map
int ClassFactory::load_lib(const char *lib_path)
{
    int ret = 0;
    string name = pathname_to_noextname(lib_path);
    lib_loader libLoader;
    fun_container fcon;
    libLoader.open(lib_path);
    libLoader.attach_fun(name, fcon);

    // 
    spaceFun.insert(make_pair(name, fcon));
    return ret;
}

