/**
 * Wrapper functions for static inline functions (when reimplementing it
 * ourselves in Haskell would be error-prone).
 */

#include "abcbridge.h"

Abc_Obj_t * AbcBridge_Abc_ObjNot( Abc_Obj_t * p ) { return Abc_ObjNot(p); }
Abc_Obj_t * AbcBridge_Abc_ObjRegular( Abc_Obj_t * p ) { return Abc_ObjRegular(p); }
int AbcBridge_Abc_ObjIsComplement( Abc_Obj_t * p ) { return Abc_ObjIsComplement(p); }
void AbcBridge_Abc_ObjSetFaninC( Abc_Obj_t * p, int i ) { return Abc_ObjSetFaninC(p, i); }
void AbcBridge_Abc_ObjXorFaninC( Abc_Obj_t * p, int i ) { return Abc_ObjXorFaninC(p, i); }
Abc_Obj_t * AbcBridge_Abc_ObjFanin( Abc_Obj_t * p, int i ) { return Abc_ObjFanin(p, i); }
int AbcBridge_Abc_ObjFaninNum( Abc_Obj_t * p ) { return Abc_ObjFaninNum(p); }
int AbcBridge_Abc_ObjFanoutNum( Abc_Obj_t * p ) { return Abc_ObjFanoutNum(p); }
int AbcBridge_Abc_ObjFaninC( Abc_Obj_t * p, int i ) { return Abc_ObjFaninC(p, i); }
Abc_Obj_t * AbcBridge_Abc_ObjCopy( Abc_Obj_t * p ) { return Abc_ObjCopy(p); }
void AbcBridge_Abc_ObjSetCopy( Abc_Obj_t * p, Abc_Obj_t * pCopy ) { return Abc_ObjSetCopy(p, pCopy); }

int AbcBridge_Gia_ManAppendCi( Gia_Man_t * p ) { return Gia_ManAppendCi(p); }
int AbcBridge_Gia_ManAppendAnd( Gia_Man_t * p, int iLit0, int iLit1 ) { return Gia_ManAppendAnd(p, iLit0, iLit1); }
int AbcBridge_Gia_ManAppendCo( Gia_Man_t * p, int iLit0 ) { return Gia_ManAppendCo(p, iLit0); }
Gia_Obj_t * AbcBridge_Gia_ManCi( Gia_Man_t * p, int v ) { return Gia_ManCi(p, v); }
Gia_Obj_t * AbcBridge_Gia_ManCo( Gia_Man_t * p, int v ) { return Gia_ManCo(p, v); }

int AbcBridge_Gia_ObjToLit( Gia_Man_t * p, Gia_Obj_t * pObj ) { return Gia_ObjToLit(p, pObj); }
Gia_Obj_t * AbcBridge_Gia_ObjFromLit( Gia_Man_t * p, int l ) { return Gia_ObjFromLit(p, l); }

size_t AbcBridge_Gia_ObjSize() { return sizeof(Gia_Obj_t); }
int AbcBridge_Gia_ObjIsTerm( Gia_Obj_t * pObj ) { return pObj->fTerm; }
unsigned AbcBridge_Gia_ObjDiff0( Gia_Obj_t * pObj ) { return pObj->iDiff0; }
unsigned AbcBridge_Gia_ObjDiff1( Gia_Obj_t * pObj ) { return pObj->iDiff1; }
int AbcBridge_Gia_ObjFaninC0( Gia_Obj_t * pObj ) { return pObj->fCompl0; }
int AbcBridge_Gia_ObjFaninC1( Gia_Obj_t * pObj ) { return pObj->fCompl1; }

int AbcBridge_Gia_Mark0(Gia_Obj_t* p) { return p->fMark0; }
void AbcBridge_Gia_SetMark0(Gia_Obj_t* p, int v) { p->fMark0 = v; }

int AbcBridge_Gia_Mark1(Gia_Obj_t* p) { return p->fMark1; }
void AbcBridge_Gia_SetMark1(Gia_Obj_t* p, int v) { p->fMark1 = v; }

int AbcBridge_Aig_ManNObj(Aig_Man_t* p, Aig_Type_t tp) { return p->nObjs[tp]; }

Aig_Obj_t * AbcBridge_Aig_ManCi( Aig_Man_t * p, int v ) { return Aig_ManCi(p, v); }
Aig_Obj_t * AbcBridge_Aig_ManCo( Aig_Man_t * p, int v ) { return Aig_ManCo(p, v); }
int AbcBridge_Aig_ObjId( Aig_Obj_t * pObj ) { return Aig_ObjId(pObj);  }

int AbcBridge_Abc_NtkObjCount( Abc_Ntk_t * p, Abc_ObjType_t t) {
    return p->nObjCounts[t];
}

/**
 * Helper functions for foreign pointers.
 */

void AbcBridge_NtkPtrDelete( Abc_Ntk_t **pp ) {
    if (pp == NULL) {
        // This shouldn't happen: it indicates that we created a foreign
        // pointer to NULL.
        assert(0);
        return;
    }
    if (*pp == NULL) {
        // Some ABC code must have punted it prematurely.  Note that
        // this is not really a safe state of affairs, since most of the
        // Haskell code assumes that the inner pointer is not NULL.
        assert(0);
        return;
    }
    Abc_NtkDelete(*pp);
}

void AbcBridge_Gia_MmFixedStop( Gia_MmFixed_t *p ) {
    Gia_MmFixedStop(p, 0);
}

void AbcBridge_Gia_MmFlexStop( Gia_MmFlex_t *p ) {
    Gia_MmFlexStop(p, 0);
}

void AbcBridge_Gia_MmStepStop( Gia_MmStep_t *p ) {
    Gia_MmStepStop(p, 0);
}

/**
 * Make the linker happy.
 */

int Abc_NtkIsSeq( Abc_Ntk_t *p ) {
    assert(0);
    return 0;
}

int Seq_ObjFaninL0( Abc_Obj_t *p ) {
    assert(0);
    return 0;
}

int Seq_ObjFaninL1( Abc_Obj_t *p ) {
    assert(0);
    return 0;
}
