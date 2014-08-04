#include "cnf.h"

static inline int Cnf_Lit2Var( int Lit ) {
    return (Lit & 1)? -(Lit >> 1)-1 : (Lit >> 1)+1;
}

static inline int Cnf_Lit2Var2( int Lit ) {
    return (Lit & 1)? -(Lit >> 1)   : (Lit >> 1);
}

int Cnf_DataWriteIntoFileWithHeader( Cnf_Dat_t * p, char * pFileName, char * header, int fReadable )
{
    FILE * pFile;
    int * pLit, * pStop, i;
    pFile = fopen( pFileName, "w" );
    if ( pFile == NULL )
    {
        return 1;
    }
    fprintf( pFile, "c Result of efficient AIG-to-CNF conversion using package CNF\n" );
    fprintf( pFile, "p cnf %d %d\n", p->nVars, p->nClauses );
    fprintf( pFile, "%s", header);
    for ( i = 0; i < p->nClauses; i++ )
    {
        for ( pLit = p->pClauses[i], pStop = p->pClauses[i+1]; pLit < pStop; pLit++ )
            fprintf( pFile, "%d ", fReadable? Cnf_Lit2Var2(*pLit) : Cnf_Lit2Var(*pLit) );
        fprintf( pFile, "0\n" );
    }
    fprintf( pFile, "\n" );
    fclose( pFile );
    return 0;
}
