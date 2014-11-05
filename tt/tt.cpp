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
#include <stdio.h>
#include "tt.h"

void MyExport::Hi()
{
    printf("hi\n");
}
void MyExport::Test()
{
    printf("Test\n");

}
void MyExport::Release()
{
    printf("Release\n");

}


IExport * CreateExportObj()
{
    return new MyExport;
}


void  DestroyExportObj(IExport* pExport)
{
    delete pExport;
    pExport = NULL;
}