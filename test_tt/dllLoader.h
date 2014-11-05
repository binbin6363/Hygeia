#pragma once
#include "utils/sulib.h"
#include "tt.h"

using namespace sp;

class dllLoader : public su_lib<>
{
public:
    dllLoader(void);
    ~dllLoader(void);
protected:
    virtual int load_procs(void);

private:
    su_proc_decl(CreateExportObj)
    su_proc_decl(DestroyExportObj)
};
