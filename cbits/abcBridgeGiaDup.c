#include "aig.h"
#include "gia.h"

/**
 * This copies the given object into a new GIA, using the value field of objects
 * for mappings.  It is similar to Gia_ManDupDfs_rec, but supports objects that
 * are uncopied inputs by allocating new inputs on demand.
 *
 * This function is written in C so that we get efficient access to inline functions
 * that manipulate bitfields.
 */
static
int Gia_CopyGiaObj(Gia_Man_t* pNew, Gia_Obj_t* pObj)
{
  if (pObj->Value != -1) return pObj->Value;
  if (Gia_ObjIsAnd(pObj)) {
    Gia_CopyGiaObj(pNew, Gia_ObjFanin0(pObj));
    Gia_CopyGiaObj(pNew, Gia_ObjFanin1(pObj));
    return pObj->Value = Gia_ManAppendAnd(pNew, Gia_ObjFanin0Copy(pObj),
                                                Gia_ObjFanin1Copy(pObj));

  } else {
    assert(Gia_ObjIsCi(pObj));
    return pObj->Value = Gia_ManAppendCi(pNew);
  }
}

int AbcBridge_Gia_DupLit(Gia_Man_t* pNew, Gia_Man_t* p, int iLit)
{
  int var = Gia_CopyGiaObj(pNew, Gia_ManObj(p, Abc_Lit2Var(iLit)));
  return Abc_LitNotCond(var, Abc_LitIsCompl(iLit));
}

/**
 * Returns lit in most recently duplicated new manager or -1 if lit has not
 * been copied.
 */
int Gia_LitCopy(Gia_Man_t* p, int iLit)
{
  Gia_Obj_t* obj = Gia_ManObj(p, Abc_Lit2Var(iLit));
  return (obj->Value == -1)
         ? -1
         : Abc_LitNotCond(obj->Value, Abc_LitIsCompl(iLit));
}

/**
 * Pointer to an array where for each GIA object o, the element
 * at index Gia_ObjId(_,o) stores the AIG object equivalent to o.
 */
typedef Aig_Obj_t* GiaAigMap;

static
Aig_Obj_t* Aig_CopyGiaObj(Aig_Man_t* pNew, GiaAigMap* m, Gia_Man_t* p, Gia_Obj_t* pObj)
{
  int id = Gia_ObjId(p, pObj);
  if (m[id]) return m[id]; // Return previous object if pObj has already been copied.

  if (Gia_ObjIsAnd(pObj)) {
    Aig_Obj_t* obj0 = Aig_NotCond(Aig_CopyGiaObj(pNew, m, p, Gia_ObjFanin0(pObj)),
                                  Gia_ObjFaninC0(pObj));
    Aig_Obj_t* obj1 = Aig_NotCond(Aig_CopyGiaObj(pNew, m, p, Gia_ObjFanin1(pObj)),
                                  Gia_ObjFaninC1(pObj));
    return m[id] = Aig_And(pNew, obj0, obj1);
  } else {
    assert(Gia_ObjIsCi(pObj));
    return m[id] = Aig_ObjCreateCi(pNew);
  }
}

/**
 * Copies a GIA literal to an AIG, and adds it to the list of combinational outputs.
 */
Aig_Obj_t* AbcBridge_Aig_DupGiaLit(Aig_Man_t* pNew, GiaAigMap* m, Gia_Man_t* p, int iLit)
{
  // Copy underlying var to new AIG.
  Aig_Obj_t* obj = Aig_CopyGiaObj(pNew, m, p, Gia_ManObj(p, Abc_Lit2Var(iLit)));
  // Negate obj if necessary.
  return Aig_NotCond(obj, Abc_LitIsCompl(iLit));
}
