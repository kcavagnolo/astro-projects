//
// Code auto-generated by initpackage (XSPEC12 local model package 
// code generator).  Do not edit
// Package: clmass Created :
// Initializer: clmass.cxx

#include <XSUser/UserInterface/xstcl.h>

#include  "clmassFunctionMap.h"
#include  <XSModelFunction.h>

extern "C" int Clmass_Init(Tcl_Interp* tclInterp);
extern "C" int Clmass_SafeInit(Tcl_Interp* tclInterp);

int Clmass_Init(Tcl_Interp* tclInterp)
{
        return Clmass_SafeInit(tclInterp);
}

int Clmass_SafeInit(Tcl_Interp* tclInterp)
{

        char PACKAGE[] = "clmass";
        char VERSION[] = "1.0";
        Tcl_PkgProvide(tclInterp, PACKAGE, VERSION);
        createclmassFunctionMap();
        XSModelFunction::updateComponentList
              ("/home/cavagnolo/research/smcs/analysis/modmodel/model.dat");
        return TCL_OK;

}

