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
        // ���ɿɴ�ӡ������[33-126]
        IDENTIFY[i] = (rand() % 94) + 33;
    }
    fprintf(DEVICE_STD, "class factory generate identify:%s\n", IDENTIFY);
}

// TODO:��ȡ��ֵ��Ҫ�����֤
const char *ClassFactory::identify() const
{
    // 1.���ȷ��
    // 2.��ݺϷ����أ���֮����NULL
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

// ���䴴��ʵ�����������Լ�����
int ClassFactory::registClass(const string &inst_signature, BaseClass *class_inst)
{
// 1.ǩ����֤
    string signature = fetchSignature(inst_signature);
    int ret = signatureCertification(signature);
    if (!ret)
    {
        // ��֤ʧ�ܣ�����
        return ret;
    }

// 2.����ʵ������ӵ�ʵ������
}

int ClassFactory::unRegistClass(const string &inst_signature)
{


}
// ��֤ǩ����ǰ׺����׺��ʾʵ��id���ɹ�����0��ʧ�ܷ���1
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



// ����lib����ȡ�������������洢��fun_container�У�����map
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

