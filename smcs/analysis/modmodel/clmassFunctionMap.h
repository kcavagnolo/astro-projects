//
// Code auto-generated by initpackage (XSPEC12 local model package 
// code generator).  Do not edit
// Package: clmass Created :
// Function header: clmassFunctionMap.h
#ifndef    FUNCTIONMAP_H
#define    FUNCTIONMAP_H

#include    <funcType.h>
#include <cstdio>
#include <iterator> 

class    XSModelFunction;

extern    ModelFunctionMap  XSFunctionMap;

void  createclmassFunctionMap();

extern "C"  {


	XSMixCCall   hydrostatic;
	XSMixCCall   nfwhydrostatic;
	XSMixCCall   monohydrostatic;
}


#endif
