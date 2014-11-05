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
#include "dllLoader.h"
#include <vector>
#include <algorithm>
#include "tt.h"
using namespace sp;
using namespace std;

class t
{
public:
    t(int i)
    {
        a = i;
    }
    int a;
    bool operator ==(const t &o)
    {
        return this->a == o.a;
    }
};

int main(int argc, char *argv[])
{
    vector<t> ij;
    string str("sf.\0\0\0\0",7);
    int l = str.length();
    t as(3);
    ij.push_back(as);
    as.a = 8;
    ij.push_back(as);
    as.a = 80;
    ij.push_back(90);
    as.a = 8;
    vector<t>::iterator i = std::find(ij.begin(), ij.end(), as);
    dllLoader libloader;
    libloader.open("D:\\bbwang\\Hygeia\\vs2008\\tt\\Debug\\tt.dll");
    MyExport *ep = dynamic_cast<MyExport *>(libloader.CreateExportObj()());
    ep->Hi();
    ep->Test();
    ep->Release();
    libloader.DestroyExportObj()(ep);
    return 0;
}