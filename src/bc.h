// #ifndef _BCCPLEX_H_
// #define _BCCPLEX_H_

// #include <R.h>
// #include <Rinternals.h>
// #include <math.h>
// #include <ctype.h>
// #include <stdlib.h>
// #include <string.h>
// #include <stdio.h>
// #include <assert.h>
// #include "mmhcfast.h"
// #include "eocb.h"
// #include <ilcplex/cplex.h>

// SEXP bnsl_bc(SEXP cplexenv, SEXP cplexprob, SEXP num_nodes, SEXP cpcs, SEXP combinations,
//                             SEXP cpcs_vars, SEXP edge_vars,
//                             SEXP start_cpcs_vars, SEXP start_edge_vars,
//                             SEXP num_vars, SEXP num_cpcs_vars, SEXP cpcs_scores);

// static int CPXPUBLIC 
//    usersolve      (CPXCENVptr env, void *cbdata, int wherefrom,
//                    void *cbhandle, int *useraction_p);
// static int CPXPUBLIC
//    usersetbranch  (CPXCENVptr env, void *cbdata, int wherefrom,
//                    void *cbhandle, int brtype, int sos, int nodes,
//                    int bdcnt, const double *nodeest, const int *nodebeg,
//                    const int *indices, const char *lu, const int *bd,
//                    int *useraction_p);
// static int CPXPUBLIC 
//    userselectnode (CPXCENVptr env, void *cbdata, int wherefrom,
//                    void *cbhandle, int *nodeid_p,
//                    int *useraction_p);

// static void
// free_and_null (char **ptr)
// {
//    if ( *ptr != NULL ) {
//       free (*ptr);
//       *ptr = NULL;
//    }
// }

// #endif