#include "abcbridge.h"

extern int Abc_NtkDSat( Abc_Ntk_t * pNtk, ABC_INT64_T nConfLimit, ABC_INT64_T nInsLimit, int nLearnedStart, int nLearnedDelta, int nLearnedPerce, int fAlignPol, int fAndOuts, int fNewSolver, int fVerbose );

/**Function*************************************************************

  Synopsis    [Translates model into the vector of values.]

  Description []
               
  SideEffects []

  SeeAlso     []

***********************************************************************/
static
void AbcBridge_NtkModelToVector( Abc_Ntk_t * pNtk, Vec_Int_t * vPiValues )
{
    int * pModel, i;
    pModel = pNtk->pModel;
    for ( i = 0; i < Abc_NtkPiNum(pNtk); i++ )
        Vec_IntWriteEntry( vPiValues, i, pModel[i] );
}

/**Function*************************************************************

  Synopsis    [Clears parameters.]

  Description []
               
  SideEffects []

  SeeAlso     []

***********************************************************************/
static
void AbcBridge_NtkVectorClearPars( Vec_Int_t * vPiValues, int nPars )
{
    int i;
    for ( i = 0; i < nPars; i++ )
        Vec_IntWriteEntry( vPiValues, i, -1 );
}

/**Function*************************************************************

  Synopsis    [Clears variables.]

  Description []
               
  SideEffects []

  SeeAlso     []

***********************************************************************/
static
void AbcBridge_NtkVectorClearVars( Abc_Ntk_t * pNtk, Vec_Int_t * vPiValues, int nPars )
{
    int i;
    for ( i = nPars; i < Abc_NtkPiNum(pNtk); i++ )
        Vec_IntWriteEntry( vPiValues, i, -1 );
}

/**Function*************************************************************

  Synopsis    [Solve the QBF problem EpAx[M(p,x)].]

  Description [The network should be a Boolean network where, the variables
               p go first, followed by variables x.
               The number of parameters is nPars.
               The number of iterations to try is nItersMax.
               The inputs to try are in vPiValues, and it will store the
               results if a model is found.
               The return value is 1 if the problem is false, 0 if the problem is
               true (and an assignment to p returned via vPiValeus), -1 if the
               iteration limit reached, and -2 if the sat solver times out. ]

  SideEffects []

  SeeAlso     []

***********************************************************************/
int AbcBridge_NtkQbf( Abc_Ntk_t * pNtk,
                       int nPars,
                       int nItersMax,
                       Vec_Int_t* vPiValues)
{
    Abc_Ntk_t * pNtkVer, * pNtkSyn, * pNtkSyn2, * pNtkTemp;
    int nIters, nInputs, RetValue, fFound = 0;

    assert( Abc_NtkIsStrash(pNtk) );
    assert( Abc_NtkIsComb(pNtk) );
    assert( Abc_NtkPoNum(pNtk) == 1 );
    assert( nPars > 0 && nPars < Abc_NtkPiNum(pNtk) );
//    assert( Abc_NtkPiNum(pNtk)-nPars < 32 );
    nInputs = Abc_NtkPiNum(pNtk) - nPars;

    assert(Vec_IntSize(vPiValues) == Abc_NtkPiNum(pNtk));

    AbcBridge_NtkVectorClearPars( vPiValues, nPars );
    pNtkSyn = Abc_NtkMiterCofactor( pNtk, vPiValues );

    // iteratively solve
    for ( nIters = 0; nIters < nItersMax; nIters++ )
    {
        // solve the synthesis instance
//        RetValue = Abc_NtkMiterSat( pNtkSyn, 0, 0, 0, NULL, NULL );
        RetValue = Abc_NtkDSat( pNtkSyn, (ABC_INT64_T)0, (ABC_INT64_T)0, 0, 0, 0, 1, 0, 0, 0 );
        if ( RetValue == 0 )
            AbcBridge_NtkModelToVector( pNtkSyn, vPiValues );

        // Formula is unsat when forall variables replaced with concrete inputs, and
        // thus unsat in general.
        if ( RetValue == 1 )
        {
          Abc_NtkDelete(pNtkSyn);
          return 1; // Return UNSAT
        }

        // Synthesis timed out.
        if (RetValue == -1) {
          Abc_NtkDelete(pNtkSyn);
          return -2; 
        }
        // there is a counter-example

        // construct the verification instance
        AbcBridge_NtkVectorClearVars( pNtk, vPiValues, nPars );

        pNtkVer = Abc_NtkMiterCofactor( pNtk, vPiValues );
        // complement the output
        Abc_ObjXorFaninC( Abc_NtkPo(pNtkVer,0), 0 );

        // solve the verification instance
        RetValue = Abc_NtkMiterSat( pNtkVer, 0, 0, 0, NULL, NULL );

        if ( RetValue == 0 )
            AbcBridge_NtkModelToVector( pNtkVer, vPiValues );
        Abc_NtkDelete( pNtkVer );

        if ( RetValue == 1 )
        {
          Abc_NtkDelete( pNtkSyn );
          return 0; // Return sat
        }

        // If verification timed out.
        if ( RetValue == -1 ) {
          Abc_NtkDelete(pNtkSyn);
          return -2;
        }

        // there is a counter-example

        // create a new synthesis network
        AbcBridge_NtkVectorClearPars( vPiValues, nPars );
        pNtkSyn2 = Abc_NtkMiterCofactor( pNtk, vPiValues );
        // add to the synthesis instance
        pNtkSyn = Abc_NtkMiterAnd( pNtkTemp = pNtkSyn, pNtkSyn2, 0, 0 );
        Abc_NtkDelete( pNtkSyn2 );
        Abc_NtkDelete( pNtkTemp );
    }

    Abc_NtkDelete( pNtkSyn );

    // Limit reached.
    return -1;
}
