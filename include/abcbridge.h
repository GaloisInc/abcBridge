#define ABC_LIB
#define WIN32_NO_DLL

#include <stdio.h>
#include "abc.h"
#include "gia.h"
#include "aig.h"
#include "abc_global.h"

int AbcBridge_Gia_ManAppendCi( Gia_Man_t * p );
int AbcBridge_Gia_ManAppendAnd( Gia_Man_t * p, int iLit0, int iLit1 );
int AbcBridge_Gia_ManAppendCo( Gia_Man_t * p, int iLit0 );

int AbcBridge_Gia_DupLit(Gia_Man_t* pNew, Gia_Man_t* p, int iLit);

Gia_Obj_t * AbcBridge_Gia_ManCi( Gia_Man_t * p, int v );
Gia_Obj_t * AbcBridge_Gia_ManCo( Gia_Man_t * p, int v );
int AbcBridge_Gia_ObjToLit( Gia_Man_t * p, Gia_Obj_t * pObj );
Gia_Obj_t * AbcBridge_Gia_ObjFromLit( Gia_Man_t * p, int l );

int AbcBridge_NtkQbf( Abc_Ntk_t * pNtk,
                       int nPars,
                       int nItersMax,
                       Vec_Int_t* vPiValues);

int AbcBridge_Aig_ManNObj(Aig_Man_t* p, Aig_Type_t tp);
Aig_Obj_t * AbcBridge_Aig_ManCi( Aig_Man_t * p, int v );
Aig_Obj_t * AbcBridge_Aig_ManCo( Aig_Man_t * p, int v );
int AbcBridge_Aig_ObjId( Aig_Obj_t * pObj );

void AbcBridge_Gia_ClearGiaObj( Gia_Obj_t* pObj );
