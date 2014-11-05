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

#define MATUREAPPROACH_EXPORTS

#ifdef MATUREAPPROACH_EXPORTS
#define MATUREAPPROACH_API __declspec(dllexport)
#else
#define MATUREAPPROACH_API __declspec(dllimport)
#endif

class IExport
{
public:
    virtual void Hi() = 0;
    virtual void Test() = 0;
    virtual void Release() = 0;
};

class MyExport : public IExport
{
public:
    virtual void Hi();
    virtual void Test();
    virtual void Release();
};
extern "C" MATUREAPPROACH_API IExport*  CreateExportObj();
extern "C" MATUREAPPROACH_API void  DestroyExportObj(IExport* pExport);