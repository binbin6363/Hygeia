#include "dllLoader.h"

dllLoader::dllLoader(void)
{
}

dllLoader::~dllLoader(void)
{
}


int dllLoader::load_procs(void)
{
    su_proc_load(CreateExportObj)
    su_proc_load(DestroyExportObj)
    return 0;
}