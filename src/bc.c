// #include "bc.h"

// SEXP bnsl_bc(SEXP cplexenv, SEXP cplexprob, SEXP num_nodes, SEXP cpcs, SEXP combinations,
//                             SEXP cpcs_vars, SEXP edge_vars,
//                             SEXP start_cpcs_vars, SEXP start_edge_vars,
//                             SEXP num_vars, SEXP num_cpcs_vars, SEXP cpcs_scores) {

//     int status, i;

//     SEXP out = R_NilValue;

//     CPXENVptr env = R_ExternalPtrAddr(cplexenv);
//     CPXLPptr prob = R_ExternalPtrAddr(cplexprob);

//     cplex_callback_params ccp;
//     ccp.num_nodes       = *INTEGER(num_nodes);
//     ccp.num_vars        = *INTEGER(num_vars);
//     ccp.num_cpcs_vars   = *INTEGER(num_cpcs_vars);
//     ccp.start_cpcs_vars =  INTEGER(start_cpcs_vars);
//     ccp.start_edge_vars = (*INTEGER(start_edge_vars))-1;
//     ccp.prob = prob;
//     ccp.x    = NULL;
//     ccp.beg  = NULL;
//     ccp.ind  = NULL;
//     ccp.val  = NULL;
//     ccp.sen  = NULL;
//     ccp.rhs  = NULL;

//     status = CPXsetlazyconstraintcallbackfunc(env, cpx_callback_bnsl, &ccp);
//     status = CPXsetnodecallbackfunc   (env, userselectnode, NULL)  ||
//              CPXsetbranchcallbackfunc (env, usersetbranch, NULL)   ||
//              CPXsetsolvecallbackfunc  (env, usersolve, NULL);

//     status = CPXmipopt(env, prob);
//     if (status) {
//     fprintf(stderr, "Fatal error in bnsl_bc:\n"
//                     "CPXmipopt : %d\n", status);
//     exit(status);
//     }

//     ccp.x = malloc(ccp.num_vars * sizeof(double));
  
//     status = CPXgetx(env, prob, ccp.x, 0, ccp.num_vars-1);
//     if (status) {
//     fprintf(stderr, "Fatal error in bnsl_bc:\n"
//                     "CPXgetx : %d\n", status);
//     exit(status);
//     }

//     PROTECT(out = allocVector(REALSXP, ccp.num_vars));
//     double *output = REAL(out);
//     for (i = 0 ; i < ccp.num_vars ; i++) {
//     output[i] = ccp.x[i];
//     }
//     UNPROTECT(1);
//     return(out);

// }

// static int CPXPUBLIC 
// usersolve (CPXCENVptr env,
//            void       *cbdata,
//            int        wherefrom,
//            void       *cbhandle,
//            int        *useraction_p)
// {
//    int      status = 0;
//    int      nodecount;
//    CPXLPptr nodelp;

//    *useraction_p = CPX_CALLBACK_DEFAULT;

//    /* Get pointer to LP subproblem */

//    status = CPXgetcallbacknodelp (env, cbdata, wherefrom, &nodelp);
//    if ( status )  goto TERMINATE;

//    /* Find out what node is being processed */

//    status = CPXgetcallbackinfo (env, cbdata, wherefrom,
//                                 CPX_CALLBACK_INFO_NODE_COUNT,
//                                 &nodecount);
//    if ( status )  goto TERMINATE;

//    /* Solve initial node with primal, others with dual */

//    if ( nodecount < 1 )  status = CPXprimopt (env, nodelp); //primopt
//    else                  status = CPXdualopt (env, nodelp);
  
//   /*int cols = CPXgetnumcols (env, nodelp);
//    if ( cols <= 0 ) {
//       fprintf (stdout, "Can't get number of columns.\n");
//       status = CPXERR_CALLBACK;
//       goto TERMINATE;
//    }
//   double *x = malloc(cols*sizeof(double));
  
//         status = CPXgetcallbacknodex (env, cbdata, wherefrom, x, 0,
//                                  cols-1);
//   // status = CPXgetx(env, nodelp, x, 0, cols-1);
//   if (status) {
//     fprintf(stderr, "Fatal error in cpx_callback_bnsl_solve:\n"
//                     "CPXgetcallbacknodex : %d\n", status);
//     exit(status);
//   }
  
  
//   int i, j,nfrac = 0;
//   for (i = 0 ; i < cols ; i++) {
//     if (x[i] > 0.9999) {
//       printf("INT :: %d: %f\n", i, fabs(x[i]));
//     } else if (x[i] > 0.0001) {
//       printf("FRACT :: %d: %f\n", i, fabs(x[i]));
//       nfrac++;
//     }
//   }*/

//    /* If the solve was OK, set return to say optimization has
//       been done in callback, otherwise return the CPLEX error
//       code */

//    if ( !status )  *useraction_p = CPX_CALLBACK_SET;

// TERMINATE:

//    return (status);

// } /* END usersolve */


// static int CPXPUBLIC
// usersetbranch (CPXCENVptr   env,
//                void         *cbdata,
//                int          wherefrom,
//                void         *cbhandle,
//                int          brtype,
//                int          sos,
//                int          nodecnt,
//                int          bdcnt,
//                const double *nodeest,
//                const int    *nodebeg,
//                const int    *indices,
//                const char   *lu,
//                const int    *bd,
//                int          *useraction_p)   
// {
//    int status = 0;

//    cplex_callback_params *ccp = (cplex_callback_params *)cbdata;
 
//    int      j, bestj = -1;
//    int      cols;
//    double   maxobj = -CPX_INFBOUND;
//    double   maxinf = -CPX_INFBOUND;
//    double   xj_inf;
//    int      xj_lo;
//    double   objval;
//    double   *x   = NULL;
//    double   *obj = NULL;
//    int      *feas = NULL;
 
//    char     varlu[1];
//    int      varbd[1];
//    int      seqnum1, seqnum2;
 
//    CPXCLPptr lp;
 
//    /* Initialize useraction to indicate no user action taken */
 
//    *useraction_p = CPX_CALLBACK_DEFAULT;
 
//    /* If CPLEX is choosing an SOS branch, take it */
 
//    if ( sos >= 0 )  return (status);
 
//    /* Get pointer to the problem */
 
//    status = CPXgetcallbacklp (env, cbdata, wherefrom, &lp);
//    if ( status ) {
//       fprintf (stdout, "Can't get LP pointer.\n");
//       goto TERMINATE;
//    }
 
//    cols = CPXgetnumcols (env, lp);
//    if ( cols <= 0 ) {
//       fprintf (stdout, "Can't get number of columns.\n");
//       status = CPXERR_CALLBACK;
//       goto TERMINATE;
//    }
 
//    /* Get solution values and objective coefficients */
 
//    x    = (double *) malloc (cols * sizeof (double));
//    obj  = (double *) malloc (cols * sizeof (double));
//    feas = (int *)    malloc (cols * sizeof (int));
//    if ( x     == NULL ||
//         obj   == NULL ||
//         feas  == NULL   ) {
//       fprintf (stdout, "Out of memory.");
//       status = CPXERR_CALLBACK;
//       goto TERMINATE;
//    }
 
//    status = CPXgetcallbacknodex (env, cbdata, wherefrom, x, 0,
//                                  cols-1);
//    if ( status ) {
//       fprintf (stdout, "Can't get node solution.");
//       goto TERMINATE;
//    }
 
//    status = CPXgetcallbacknodeobjval (env, cbdata, wherefrom,
//                                       &objval);
//    if ( status ) {
//       fprintf (stdout, "Can't get node objective value.");
//       goto TERMINATE;
//    }
 
//    status = CPXgetobj (env, lp, obj, 0, cols-1);
//    if ( status ) {
//       fprintf (stdout, "Can't get obj.");
//       goto TERMINATE;
//    }
 
//    status = CPXgetcallbacknodeintfeas (env, cbdata, wherefrom, feas,
//                                        0, cols-1);
//    if ( status ) {
//       fprintf (stdout,
//                "Can't get variable feasible status for node.");
//       goto TERMINATE;
//    }
 
//    /* Branch on var with largest objective coefficient among those
//       with largest infeasibility */
 
//    for (j = 0; j < cols; j++) {
//       if ( feas[j] == CPX_INTEGER_INFEASIBLE ) {
//          xj_inf = x[j] - floor (x[j]);
//          if ( xj_inf > 0.5 )  xj_inf = 1.0 - xj_inf;
 
//          if ( xj_inf >= maxinf                            &&
//               (xj_inf > maxinf || fabs (obj[j]) >= maxobj)  ) {
//             bestj  = j;
//             maxinf = xj_inf; 
//             maxobj = fabs (obj[j]);
//          }
//       }
//    }
 
//    /* If there weren't any eligible variables, take default branch */
 
//    if ( bestj < 0 ) {
//       goto TERMINATE;
//    }
 
//    /* Now set up node descriptions */
 
//    xj_lo = (int) floor (x[bestj]);
 
//    /* Up node */
 
//    varlu[0] = 'L';
//    varbd[0] = xj_lo + 1;
//    status = CPXbranchcallbackbranchbds (env, cbdata, wherefrom,
//                                         objval, 1, &bestj, varlu, varbd,
//                                         NULL, &seqnum1);
//    if ( status )  goto TERMINATE;
 
//    /* Down node */
 
//    varlu[0] = 'U';
//    varbd[0] = xj_lo;
 
//    status = CPXbranchcallbackbranchbds (env, cbdata, wherefrom,
//                                         objval, 1, &bestj, varlu, varbd,
//                                         NULL, &seqnum2);
//    if ( status )  goto TERMINATE;
 
//    /* Set useraction to indicate a user-specified branch */
 
//    *useraction_p = CPX_CALLBACK_SET;
 
// TERMINATE:
 
//    free_and_null ((char **) &x);
//    free_and_null ((char **) &obj);
//    free_and_null ((char **) &feas); 
//    return (status);
 
// } /* END usersetbranch */


// static int CPXPUBLIC 
// userselectnode (CPXCENVptr env,
//                 void       *cbdata,
//                 int         wherefrom,
//                 void       *cbhandle,
//                 int        *nodenum_p,
//                 int        *useraction_p)
// {
//     int status = 0;

//     // cplex_callback_params *ccp = (cplex_callback_params *)cbdata;

//     int    thisnode;
//     int    nodesleft;
//     int    bestnode = 0;
//     int    depth;
//     int    maxdepth = -1;
//     double siinf;
//     double maxsiinf = 0.0;

//     /* Initialize useraction to indicate no user node selection */

//     *useraction_p = CPX_CALLBACK_DEFAULT;

//     /* Choose the node with the largest sum of infeasibilities among
//       those at the greatest depth */

//     status = CPXgetcallbackinfo (env, cbdata, wherefrom,
//                                 CPX_CALLBACK_INFO_NODES_LEFT,
//                                 &nodesleft);
//     if ( status )  goto TERMINATE;

//     for (thisnode = 0; thisnode < nodesleft; thisnode++) {
//       status = CPXgetcallbacknodeinfo (env, cbdata, wherefrom,
//                                        thisnode,
//                                        CPX_CALLBACK_INFO_NODE_DEPTH,
//                                        &depth);
//       if ( !status ) {
//          status = CPXgetcallbacknodeinfo (env, cbdata, wherefrom,
//                                         thisnode,
//                                         CPX_CALLBACK_INFO_NODE_SIINF,
//                                         &siinf);
//       }
//       if ( status )  break;

//       if ( (depth >= maxdepth)                   &&
//            (depth > maxdepth || siinf > maxsiinf)  ) {
//          bestnode = thisnode;
//          maxdepth = depth;
//          maxsiinf = siinf;
//       }
//     }

//     *nodenum_p = bestnode;
//     *useraction_p = CPX_CALLBACK_SET;

// TERMINATE:

//     return (status);

// } /* END userselectnode */