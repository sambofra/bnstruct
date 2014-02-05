#include "eocb.h"

SEXP getListElement(SEXP list, const char *str) {
  SEXP elmt  = R_NilValue,
       names = getAttrib(list, R_NamesSymbol);
  for (int i = 0; i < length(list); i++)
    if(strcmp(CHAR(STRING_ELT(names, i)), str) == 0) {
      elmt = VECTOR_ELT(list, i);
      break;
    }
  return elmt;
}


SEXP solve_BNSL_with_callbacks(SEXP cplexenv, SEXP cplexprob, SEXP num_nodes, SEXP cpcs, SEXP combinations,
                               SEXP cpcs_vars, SEXP edge_vars,
                               SEXP start_cpcs_vars, SEXP start_edge_vars,
                               SEXP num_vars, SEXP num_cpcs_vars, SEXP cpcs_scores) {
  SEXP out = R_NilValue;
  int i, j, k,
      status;
      
  CPXENVptr env = R_ExternalPtrAddr(cplexenv);
  CPXLPptr prob = R_ExternalPtrAddr(cplexprob);
  
  cplex_callback_params ccp;
  ccp.num_nodes       = *INTEGER(num_nodes);
  ccp.num_vars        = *INTEGER(num_vars);
  ccp.num_cpcs_vars   = *INTEGER(num_cpcs_vars);
  ccp.start_cpcs_vars = INTEGER(start_cpcs_vars);
  ccp.start_edge_vars = (*INTEGER(start_edge_vars))-1;
  ccp.prob = prob;
  ccp.x    = NULL;
  ccp.beg  = NULL;
  ccp.ind  = NULL;
  ccp.val  = NULL;
  ccp.sen  = NULL;
  ccp.rhs  = NULL;
  
  // CPLEX dynamic search gets deactivated when callbacks are on
  status = CPXsetintparam (env, CPX_PARAM_MIPSEARCH, CPX_MIPSEARCH_TRADITIONAL);
  if (status) {
    fprintf(stderr, "Fatal error in solve_BNSL_with_callbacks:\n"
                    "CPXsetinparam (CPX_PARAM_MIPSEARCH) : %d\n", status);
    exit(status);
  }
  
  status = CPXsetintparam(env, CPX_PARAM_PRELINEAR, 0);
  
  // don't remember... on, off? should try
  status = CPXsetintparam (env, CPX_PARAM_MIPCBREDLP, CPX_OFF);
  if (status) {
    fprintf(stderr, "Fatal error in solve_BNSL_with_callbacks:\n"
                    "CPXsetinparam (CPX_PARAM_MIPCBREDLP) : %d\n", status);
    exit(status);
  }  
  
  // upper/lower bound, just in case
  /** /status = CPXsetdblparam(env, CPX_PARAM_CUTUP, ub);
  if (status) {
    fprintf(stderr, "Fatal error in solve_BNSL_with_callbacks:\n"
                    "CPXsetdblparam (CPX_PARAM_CUTUP) : %d\n", status);
    exit(status);
  }/*

  status = CPXsetdblparam(env, CPX_PARAM_CUTLO, lb);
  if (status) {
    fprintf(stderr, "Fatal error in solve_BNSL_with_callbacks:\n"
                    "CPXsetdblparam (CPX_PARAM_CUTLO) : %d\n", status);
    exit(status);
  }*/
  
  status = CPXsetlazyconstraintcallbackfunc(env, cpx_callback_bnsl, &ccp);
  // status = CPXsetusercutcallbackfunc(env, cpx_callback_bnsl, &ccp);
  if (status) {
    fprintf(stderr, "Fatal error in solve_BNSL_with_callbacks:\n"
                    "CPXsetlazyconstraintcallbackfunc : %d\n", status);
    exit(status);
  }/* */
 /* */ status = CPXsetnodecallbackfunc   (env, cpx_callback_bnsl_selectnode, NULL) ||
                CPXsetbranchcallbackfunc (env, cpx_callback_bnsl_branch, NULL)     ||
                CPXsetsolvecallbackfunc  (env, cpx_callback_bnsl_solve, NULL);
/*  */
  /* * /
  status = CPXpresolve(env, prob, CPX_ALG_NONE);
  if (status) {
    fprintf(stderr, "Fatal error in solve_BNSL_with_callbacks:\n"
                    "CPXpresolve : %d\n", status);
    exit(status);
  } / * */

    /* status = CPXsetnodecallbackfunc (env, userselectnode, NULL)  ||
            CPXsetbranchcallbackfunc (env, usersetbranch, NULL) ||
            CPXsetsolvecallbackfunc (env, usersolve, NULL);*/

  status = CPXmipopt(env, prob);
  if (status) {
    fprintf(stderr, "Fatal error in solve_BNSL_with_callbacks:\n"
                    "CPXmipopt : %d\n", status);
    exit(status);
  }
  
  /**/
  status = CPXfreepresolve(env, prob);
  if (status) {
    fprintf(stderr, "Fatal error in solve_BNSL_with_callbacks:\n"
                    "CPXfreepresolve : %d\n", status);
    exit(status);
  }/**/
  
  return(out);
}

static int CPXPUBLIC cpx_callback_bnsl_solve(CPXCENVptr env,
                                             void      *cbdata,
                                             int        wherefrom,
                                             void      *cbhandle,
                                             int       *useraction_p) {
  int      status = 0,
           nodecount,
           cols;
  
  double *x = NULL,
         *prex;
  
  CPXCLPptr nodelp;
  
  cplex_callback_params *ccp = (cplex_callback_params *)cbhandle;
  
  printf("usersolve\n");
  
  *useraction_p = CPX_CALLBACK_DEFAULT;
  
  // get pointer to LP problem
  status = CPXgetcallbacklp(env, cbdata, wherefrom, &nodelp);
  if (status) {
    fprintf(stderr, "Fatal error in cpx_callback_bnsl_solve:\n"
                    "CPXgetcallbacklp : %d\n", status);
    exit(status);
  }
  
  // here goes the cycle finding
  cols = CPXgetnumcols(env, nodelp);
  if (cols <= 0) {
    status = CPXERR_CALLBACK;
    fprintf(stderr, "Fatal error in cpx_callback_bnsl_solve:\n"
                    "CPXgetnumcols : %d\n", status);
    exit(status);
  }
  
  printf("# cols = %d\n", cols);
  
  /*status = CPXmipopt(env, nodelp);
  if (status) {
    fprintf(stderr, "Fatal error in cpx_callback_bnsl_solve:\n"
                    "CPXmipopt : %d\n", status);
    exit(status);
  }*/
  
  x    = malloc(cols * sizeof(double));
  prex = malloc(cols * sizeof(double));
  
  if (x == NULL) printf("no eh\n");
  
  /* */status = CPXuncrushx(env, nodelp, x, prex);
  if (status) {
    fprintf(stderr, "Fatal error in cpx_callback_bnsl:\n"
                    "CPXgetuncrushx : %d\n", status);
    exit(status);
  } /* */
  
  printf("CPX_CALLBACK_MIP: %d\n", CPX_CALLBACK_MIP);
  printf("CPX_CALLBACK_MIP_BRANCH: %d\n", CPX_CALLBACK_MIP_BRANCH);
  //printf("CPX_CALLBACK_MIP_INCUMBENT: %d\n", CPX_CALLBACK_MIP_INCUMBENT);
  printf("CPX_CALLBACK_MIP_NODE: %d\n", CPX_CALLBACK_MIP_NODE);
  printf("CPX_CALLBACK_MIP_HEURISTIC: %d\n", CPX_CALLBACK_MIP_HEURISTIC);
  //printf("CPX_CALLBACK_MIP_CUT: %d\n", CPX_CALLBACK_MIP_CUT);
  printf("wherefrom: %d\n", wherefrom);
  status = CPXgetcallbacknodex (env, cbdata, wherefrom, x, 0,
                                cols-1);
  //status = CPXgetx(env, nodelp, x, 0, cols-1);
  if (status) {
    fprintf(stderr, "Fatal error in cpx_callback_bnsl_solve:\n"
                    "CPXgetcallbacknodex : %d\n", status);
    exit(status);
  }
  
  /** /int i, j,nfrac = 0;
  for (i = 0 ; i < cols ; i++) {
    if (x[i] > 0.9999) {
      printf("INT :: %d: %f\n", i, fabs(x[i]));
    } else if (x[i] > 0.0001) {
      printf("FRACT :: %d: %f\n", i, fabs(x[i]));
      nfrac++;
    }
  }/ **/
  
  if (!status) {
    *useraction_p = CPX_CALLBACK_SET;
  }
  
  return status;
}

static int CPXPUBLIC cpx_callback_bnsl_branch(CPXCENVptr    env,
                                              void         *cbdata,
                                              int           wherefrom,
                                              void         *cbhandle,
                                              int           brtype,
                                              int           sos,
                                              int           nodecnt,
                                              int           bdcnt,
                                              const double *nodeest,
                                              const int    *nodebeg,
                                              const int    *indices,
                                              const char   *lu,
                                              const char   *bd,
                                              int          *useraction_p) {
  int status   = 0,
      j, bestj = -1,
      cols,
      xj_lo = 0,
      seqnum1, seqnum2;
      
  double  maxobj = -CPX_INFBOUND,
          maxinf = -CPX_INFBOUND,
          objval,
          xj_inf;
  
  double *x    = NULL,
         *obj  = NULL,
         *feas = NULL;
         
  char varlu[1];
  int  varbd[1];
  
  CPXCLPptr lp;
  
  printf("branch\n");
  
  *useraction_p = CPX_CALLBACK_DEFAULT;
  
  // take SOS (special ordered set) branch, if any
  if (sos > 0) return(status);
  
  // get pointer to LP problem
  status = CPXgetcallbacklp(env, cbdata, wherefrom, &lp);
  if (status) {
    fprintf(stderr, "Fatal error in cpx_callback_bnsl_branch:\n"
                    "CPXgetcallbacklp : %d\n", status);
    exit(status);
  }
  
  cols = CPXgetnumcols(env, lp);
  if (cols <= 0) {
    status = CPXERR_CALLBACK;
    fprintf(stderr, "Fatal error in cpx_callback_bnsl_branch:\n"
                    "CPXgetnumcols : %d\n", status);
    exit(status);
  }
  
  x    = malloc(cols * sizeof(double));
  obj  = malloc(cols * sizeof(double));
  feas = malloc(cols * sizeof(int));
  // ci fidiamo?
  
  status = CPXgetcallbacknodex(env, cbdata, wherefrom, x, 0, cols-1);
  if (status) {
    fprintf(stderr, "Fatal error in cpx_callback_bnsl_branch:\n"
                    "CPXgetcallbacknodex : %d\n", status);
    exit(status);
  }
  
  status = CPXgetcallbacknodeobjval(env, cbdata, wherefrom, &objval);
  if (status) {
    fprintf(stderr, "Fatal error in cpx_callback_bnsl_branch:\n"
                    "CPXgetcallbacknodeobjval : %d\n", status);
    exit(status);
  }
  
  status = CPXgetobj(env, lp, obj, 0, cols-1);
  if (status) {
    fprintf(stderr, "Fatal error in cpx_callback_bnsl_branch:\n"
                    "CPXgetobj : %d\n", status);
    exit(status);
  }
  
  // look for fractional variables I can branch on
  // take the one with largest value
  for (j = 0 ; j < cols ; j++) {
    if (feas[j] == CPX_INTEGER_INFEASIBLE) {
      xj_inf = x[j] - floor(x[j]);
      if (xj_inf > 0.5) xj_inf = 1.0 - xj_inf;
      if ( xj_inf >= maxinf                          &&
          (xj_inf >  maxinf || fabs(obj[j]) >= maxobj)  ) {
            bestj  = j;
            maxinf = xj_inf;
            maxobj = fabs(obj[j]);
      }
    }
  }
  
  if (bestj < 0) goto TERMINATE; // IBM does this, why can't I?
  
  // branch!
  // first branch, set variable to 1: set lower bound to 1
  varlu[0] = 'U';
  varbd[0] = 0;
  status = CPXbranchcallbackbranchbds(env, cbdata, wherefrom, objval, 1,
                                      &bestj, varlu, varbd, NULL, &seqnum1);
  if (status) goto TERMINATE;
  
  // second branch, set variable to 1: set lower bound to 1
  varlu[0] = 'L';
  varbd[0] = 1;
  status = CPXbranchcallbackbranchbds(env, cbdata, wherefrom, objval, 1,
                                      &bestj, varlu, varbd, NULL, &seqnum2);
  if (status) goto TERMINATE;
  
  *useraction_p = CPX_CALLBACK_SET;
  
TERMINATE:  
  free(x);
  free(obj);
  free(feas);
  return(status);
}

static int CPXPUBLIC cpx_callback_bnsl_selectnode(CPXCENVptr env,
                                                  void      *cbdata,
                                                  int        wherefrom,
                                                  void      *cbhandle,
                                                  int       *nodenum_p,
                                                  int       *useraction_p) {
  int status = 0,
      thisnode,
      nodesleft,
      bestnode = 0,
      depth,
      maxdepth = -1;

  double siinf,
         maxsiinf = 0.0;
         
  printf("select node\n");
  
  *useraction_p = CPX_CALLBACK_DEFAULT;
  
  /* Choose the node with the largest sum of infeasibilities among
      those at the greatest depth THIS PROBABLY NEEDS TO BE CHANGED */

   status = CPXgetcallbackinfo (env, cbdata, wherefrom,
                                CPX_CALLBACK_INFO_NODES_LEFT,
                                &nodesleft);
  if (status) return (status);
  
  for (thisnode = 0 ; thisnode < nodesleft ; thisnode++) {
    status = CPXgetcallbacknodeinfo(env, cbdata, wherefrom, thisnode,
                                    CPX_CALLBACK_INFO_NODE_DEPTH, &depth);
    if (!status)  {
      status = CPXgetcallbacknodeinfo(env, cbdata, wherefrom, thisnode,
                                      CPX_CALLBACK_INFO_NODE_SIINF, &siinf);
    }
    if (status) break;
    if (depth >= maxdepth && (depth > maxdepth || siinf >= maxsiinf)) {
      bestnode = thisnode;
      maxdepth = depth;
      maxsiinf = siinf;
    }
  }
  
  *nodenum_p    = bestnode;
  *useraction_p = CPX_CALLBACK_SET;
  
  return (status);
}

/*****/

static int CPXPUBLIC cpx_callback_bnsl(CPXENVptr env,
                                       void     *cbdata,
                                       int       wherefrom,
                                       void     *cbhandle,
                                       int      *useraction_p) {
  printf("I'm a callback\n");
  
  int status = 0;
  *useraction_p = CPX_CALLBACK_DEFAULT;
  cplex_callback_params *ccp = (cplex_callback_params *)cbhandle; 

  int prenumcols, numcols;
  CPXCLPptr prelp;

  status = CPXgetcallbacklp(env, cbdata, wherefrom, &prelp);
  if (status) {
    fprintf(stderr, "Fatal error in cpx_callback_bnsl:\n"
                    "CPXgetcallbacklp : %d\n", status);
    exit(status);
  }
  
  numcols    = CPXgetnumcols(env, ccp->prob);
  prenumcols = CPXgetnumcols(env, prelp);
  
  printf("Hey\n%d %d\n",numcols, prenumcols);

  double *prex = calloc(prenumcols, sizeof(double)),
         *x    = calloc(prenumcols, sizeof(double));
         
  printf("before getcallbacknodex\n");
  
  status = CPXgetcallbacknodex(env, cbdata, wherefrom, prex, 0, prenumcols-1);
  if (status) {
    fprintf(stderr, "Fatal error in cpx_callback_bnsl:\n"
                    "CPXgetcallbacknodex : %d\n", status);
    exit(status);
  }
  
  printf("before uncrushx\n");
  
  int prenzcnt;
  double preoffset;

  // revert problem from presolved to original form
  // status = CPXuncrushx(env, ccp->prob, x, prex);
  if (status) {
    fprintf(stderr, "Fatal error in cpx_callback_bnsl:\n"
                    "CPXgetuncrushx : %d\n", status);
    exit(status);
  }
  
  printf("after uncrushx\n");
  
  int i, j,nfrac = 0;
  for (i = 0 ; i < prenumcols ; i++) {
    if (prex[i] > 0.9999) {
      printf("INT :: %d: %f\n", i, fabs(prex[i]));
    } else if (prex[i] > 0.0001) {
      printf("FRACT :: %d: %f\n", i, fabs(prex[i]));
      nfrac++;
    }
  }
  
  printf("----\n%d\n",ccp->num_nodes);
  // getchar();
  
  for (i = 0 ; i < ccp->num_nodes ; i++) {
    printf("%d ",ccp->start_cpcs_vars[i]);
  }
  printf("\n%d\n",ccp->start_edge_vars);
  
  // look for cycles
  
  // create constraints
  
  // add constraints with CPXaddrows
  
  
  int nzcnt;
  int *rmatind;
  double *rmatval;
  int *pind;
  double *pval;
  
  int *graph = malloc((ccp->num_nodes)*(ccp->num_nodes)*sizeof(int));
  // memcpy(graph, &prex[ccp->start_edge_vars], (ccp->num_nodes)*(ccp->num_nodes));
  for (i = 0 ; i < ccp->num_nodes*ccp->num_nodes ; i++) {
    if (prex[i + ccp->start_edge_vars] > 0.999) {
      graph[i] = 1;
    } else {
      graph[i] = 0;
    }
  }
  
  printf("AAAAAAAAAA\n");
  
  if (!is_acyclic_c(graph, ccp->num_nodes)) {
    digraph_cycles *dcs = find_cycles_in_digraph(graph, ccp->num_nodes);
    int    *ind = NULL,
            sen;
    double *val = NULL,
            rhs;
    
    digraph_cycle *dc = NULL;
    
    int cols = CPXgetnumcols(env, prelp);
    if ( cols <= 0 ) {
      fprintf (stderr, "Fatal error in build_add_secs : \n"
                       "Can't get number of columns.\n");
      status = CPXERR_CALLBACK;
      exit (status);
    }
    
    dc = dcs->cycles;
    while (dc != NULL) {
      ind = malloc(dc->length * sizeof(int));
      val = malloc(dc->length * sizeof(double));
      
      rhs = dc->length - 1;
      sen = 'L';
      
      for (j = 0 ; j < dc->length ; j++) {
        ind[j] = ccp->start_edge_vars + dc->nodes[j] * ccp->num_nodes + dc->nodes[(j+1) % ccp->num_nodes];
        val[j] = 1.0;
        
        //ind[j + dc->length] = ccp->start_edge_vars + graph[ dc->nodes[(j-1) % ccp->num_nodes] * ccp->num_nodes + dc->nodes[j] ];
        //val[j + dc->length] = 1.0;
      }
      
      status = CPXcutcallbackadd(env, cbdata, wherefrom, dc->length, rhs, sen, ind, val, CPX_USECUT_PURGE); //FORCE?
      
      free(ind);
      free(val);
      
      dc = dc->next;
    }
  } else {
    int *toint = calloc(nfrac, sizeof(int));
    double *ub = calloc(nfrac, sizeof(double));
    double *lb = calloc(nfrac, sizeof(double));
    char *ls = malloc(nfrac*sizeof(char));
    char *us = malloc(nfrac*sizeof(char));
    j = 0;
    for (i = 0 ; i < prenumcols ; i++) {
      if (prex[i] > 0.0001 && prex[i] < 0.9999) {
        toint[j++] = i;
      }
    }
    
    printf("BBBBBBBBBBBBBBB\n");
    
    char *ct = malloc(nfrac*sizeof(char));
    for (i = 0 ; i < nfrac ; i++) {
      ct[i] = 'B';
      lb[i] = 0.0;
      ub[i] = 1.0;
      ls[i] = 'L';
      us[i] = 'U';
    }
    // status = CPXcutcallbackadd(env, cbdata, wherefrom, j, presec.rhs, presec.sense, presec.rmatind, presec.rmatval, CPX_USECUT_PURGE);
    printf("ciao\n");
    //status = CPXchgctype(env, ccp->prob, nfrac, toint, ct);
    //status = CPXtightenbds(env, ccp->prob, nfrac, toint, ls, lb);
    //status = CPXtightenbds(env, ccp->prob, nfrac, toint, us, ub);
    double objval;
    status = CPXgetcallbacknodeobjval(env, cbdata, wherefrom, &objval);
    /*char varlu = 'L';
    double varbd = 1.0;
    int bestj;
    int seqnum1;
    status = CPXbranchcallbackbranchbds (env, cbdata, wherefrom,
                                        objval, 1, &bestj, &varlu, &varbd,
                                        NULL, &seqnum1);*/
    printf("akshd\n");
    if (status) {
      fprintf(stderr, "Fatal error in cpx_callback_bnsl:\n"
                      "CPXchgctype : %d\n", status);
      exit(status);
    }
  }
  
  
  // status = CPXcrushform(env, ccp->prob, nzcnt, rmatind, rmatval, &prenzcnt, &preoffset, pind, pval);
  free(graph);
  free(x);
  free(prex);
  printf("oh, really?\n");
  *useraction_p = CPX_CALLBACK_SET;
  return status;
}


/**********************************************************************************/
/**********************************************************************************/
/**********************************************************************************/
/**********************************************************************************/
/**********************************************************************************/


digraph_cycle *digraph_search(int *graph, int num_nodes, int start) {
  int i, j, k, steps = 0, node, length = 0;
  int *pred  = calloc(num_nodes, sizeof(int));
  for(k = 0 ; k < num_nodes ; k++) pred[k] = -1;
  int *queue = calloc(num_nodes, sizeof(int));
  int qs = 0,
      qe = 0;
  digraph_cycle *dc = NULL;
  
  printf("%d\n",start);
  for(k = 0 ; k < num_nodes ; k++) printf("%d ", pred[k]);
  printf("\n");
  for(i = 0 ; i < num_nodes ; i++) printf("%d ", queue[i]);
  printf("\n");
  // pred[start] = start;
  queue[qe++] = start;
  do {
    for(k = 0 ; k < num_nodes ; k++) printf("%d ", pred[k]);
    printf("\n");
    for(k = 0 ; k < num_nodes ; k++) printf("%d ", queue[k]);
    printf("\n qs = %d ; qe = %d\n", qs, qe);
    printf("steps: %d\nstart = %d\n", steps, start);
    i = queue[qs++];
    printf("i: %d ; qs = %d ; qe = %d\n",i, qs, qe);
    
    if (i == start && steps > 1) {
      // compute length of cycle (it's a BFS, so it's likely to be shorter that steps)
      node = pred[i];
      length = 1;
      while (node != start) {
        length++;
        node = pred[node];
      }
      
      // make up cycle
      dc = malloc(sizeof(dc));
      dc->length = length;
      dc->nodes  = malloc(length*sizeof(int));
      node = pred[i];
      printf("ciclo %d ",node);
      for (k = length-1 ; k >= 0 ; k--) {
        dc->nodes[k] = node;
        node = pred[node];
        printf("%d ",node);
      }
      printf("\n");
      printf("node==start? %d %d\n", node, start);
      //assert(node == start); why do this assert does not work?
      dc->next   = NULL;
      goto TERMINATE; // IBM does this, why can't I?
    } else {
      for (j = 0 ; j < num_nodes ; j++) {
        if (graph[i*num_nodes+j] == 1 && pred[j] == -1) {
          if (steps <= 1 && j == start) continue;
          pred[j] = i;
          queue[qe++] = j;
        }
      }
    }
    steps++;
  } while(qs < qe);

TERMINATE:
  free(pred);
  free(queue);
  return dc;
}

digraph_cycles *find_cycles_in_digraph(int *orig_graph, int num_nodes) {
  // we will find each cycle |cycle| times, so after a cycle is found
  // we remove an edge to break it, in order to prevent the following
  // rounds to find it again and break the browns. If, by this way, we
  // delete a different valid cycle, it will be found in the following
  // iterations, in case it will still be there. Lazy constraints already
  // provide an efficient management for this, but...
  int i,j;
  int *graph = malloc(num_nodes * num_nodes * sizeof(int));
  printf("graph:\n");
  for (i = 0 ; i < num_nodes*num_nodes ; i++) {
    graph[i] = orig_graph[i];
    printf("%d ",graph[i]);
  }
  printf("\n");
  int num_cycles = 0;
  digraph_cycles *dcs = malloc(sizeof(digraph_cycles));
  digraph_cycle *tail = NULL, *dc;
  dcs->num_cycles = 0;
  dcs->cycles = NULL;
  for (i = 0 ; i < num_nodes ; i++) {
    printf("ok, now let's call from node %d\n",i);
    dc = digraph_search(graph, num_nodes, i);
    if (dc != NULL) {
      if (dcs->cycles == NULL) {
        dcs->cycles = dc;
        tail = dc;
      } else {
        tail->next = dc;
        tail = tail->next;
      }
      dcs->num_cycles += 1;
      graph[dc->nodes[0]*num_nodes+dc->nodes[1]] = 0;
    }
    // getchar();
  }
  
  printf("print cycles\n");
  digraph_cycle *first = dcs->cycles;
  for (i = 0 ; i < dcs->num_cycles ; i++){
    for (j =0 ; j < first->length ; j++){
      printf("%d ", first->nodes[j]);
    }
    printf("\n");
    first = first->next;
  }
  // getchar();
  return(dcs);
}


int is_acyclic_c(int *graph, int n_nodes) {
  int *rem    = calloc(n_nodes, sizeof(int));
  int *leaves = calloc(n_nodes, sizeof(int));
  int  rem_count = 0;
  int  i, j, flag, aleaf;
  /*printf("graph:\n");
  for(i = 0 ; i < n_nodes*n_nodes ; i++) printf("%d ",graph[i]);
  printf("\n");*/
  
  int test;
  
  int *gtemp = malloc(n_nodes * n_nodes * sizeof(int)); 
  for (i = 0; i < n_nodes * n_nodes; i++)
    gtemp[i] = graph[i];
  
  while( rem_count < n_nodes ) // still some edges to remove
  {
    aleaf = 0;
    // find leaves among non-removed nodes
    for( i = 0; i < n_nodes; i++ )
      if( !rem[i] )
      {
        flag = gtemp[i];
        for( j = 1; j < n_nodes; j++ )
          flag |= gtemp[j*n_nodes + i];
        if( !flag ) // a leaf
        {
          // printf("Leaf: %d\n",i);
          aleaf = 1;
          leaves[i] = 1;
          rem[i] = 1;
          rem_count++;
        }
      }
  
    // printf("test for leaves\n");
    // test for leaves
    if( !aleaf )
    {
      test = 0;
      // printf("test %d\n", test);
      return test ;
    }
    else // remove edges incident on leaves
    {
      // printf("remove\n");
      for(j = 0; j < n_nodes; j++)
        if( leaves[j] )
        {
          leaves[j] = 0; // reset for next iteration
          for( i = 0; i < n_nodes; i++ )
            gtemp[j * n_nodes + i] = 0;
        }
    }
    // printf("Next\n");
  }
  
  free(rem);
  free(leaves);
  free(gtemp);
  
  // acyclic
  test = 1;
  //printf("test %d\n", test);
  return test ;
}


static int build_add_sscs(CPXENVptr              env,
                          CPXCLPptr              lp,
                          int                   *graph,
                          cplex_callback_params *ccp,
                          digraph_cycles        *dcs) {
  int i, j, con,
      status = 0,
      cols,
      M = ccp->num_nodes + 1;
  
  int    *beg = NULL,
         *ind = NULL;
  double *val = NULL,
         *rhs = NULL,
         *obj = NULL,
         *ub  = NULL,
         *lb  = NULL;
  char   *sen = NULL,
         *cty = NULL;
  
  digraph_cycle *dc = NULL;
  
  cols = CPXgetnumcols(env, lp);
  if ( cols <= 0 ) {
    fprintf (stderr, "Fatal error in build_add_secs : \n"
                     "Can't get number of columns.\n");
    status = CPXERR_CALLBACK;
    goto TERMINATE;
  }
  
  dc = dcs->cycles;
  while (dc != NULL) {
    /*lb  = malloc(2 * dc->length * sizeof(double));
    ub  = malloc(2 * dc->length * sizeof(double));
    obj = malloc(2 * dc->length * sizeof(double));
    cty = malloc(2 * dc->length * sizeof(char));
    for (i = 0 ; i < 2 * dc->length ; i++) {
      lb[i]  = 0.0;
      ub[i]  = 1.0;
      obj[i] = 0.0;
      cty[i] = 'B';
    }
    status = CPXnewcols(env, lp, 2 * dc->length, obj, lb, ub, cty, NULL);
    free(lb);
    free(ub);
    free(obj);
    free(cty);
    
    // 6.14, 6.15, 6.16, 6.17
    beg = malloc ((3 + 2 * dc->length) * sizeof(int));
    rhs = malloc ((3 + 2 * dc->length) * sizeof(double));
    sen = malloc ((3 + 2 * dc->length) * sizeof(char));
    ind = malloc ((4 + 2 * dc->length) * dc->length * sizeof(int));
    val = malloc ((4 + 2 * dc->length) * dc->length * sizeof(double));
    
    i   = 0;
    con = 0;
    
    sen[0] = 'G';
    sen[1] = 'G';
    sen[2] = 'E';
    rhs[0] = 1.0;
    rhs[1] = 1.0;
    rhs[2] = 0.0;
    beg[0] = 0;
    beg[1] = dc->length;
    beg[2] = 2 * dc->length;
    
    for (j  = 0 ; j < dc->length ; j++) { // 6.14, 6.15, 6.16
      val[j]                      =  1.0;
      val[j + dc->length]         =  1.0;
      val[2*j + 2*dc->length]     =  1.0;
      val[2*j + 2*dc->length + 1] = -1.0;
      ind[j]                      =  j + cols;
      ind[j + dc->length]         =  j + dc->length + cols ;
      val[2*j + 2*dc->length]     =  2 * j + 2 * dc->length + cols;
      val[2*j + 2*dc->length + 1] =  2 * j + 2 * dc->length + cols + 1;
    }
    
    for (con = 3 ; con < (3 + 2*dc->length) ; con++) { // 6.17
      sen[con] = 'L';
      beg[con] = 4*dc->length + 2 * (con - 3);
      rhs[con] = 1.0;
      ind[4 * dc->length + 2 * (con-3)]     = cols + (con - 3);
      ind[4 * dc->length + 2 * (con-3) + 1] = cols + (con - 3) + dc->length;
      val[4 * dc->length + 2 * (con-3)]     = 1.0;
      val[4 * dc->length + 2 * (con-3) + 1] = 1.0;
    }
    
    // add to cplex
    /*status = CPXcutcallbackadd (env, cbdata, wherefrom,
                                cutnz, rhs[i], 'L',
                                cutind, cutval, 1);*/
    // 6.18 (a,b), 6.19 (a,b)
    beg = malloc(2 * sizeof(int));
    rhs = malloc(2 * sizeof(double));
    sen = malloc(2 * sizeof(char));
    ind = malloc(2 * dc->length * sizeof(int));
    val = malloc(2 * dc->length * sizeof(double));
    
    beg[0] = 0;
    beg[1] = dc->length;
    rhs[0] = dc->length - 1;
    rhs[1] = dc->length - 1;
    sen[0] = 'L';
    sen[1] = 'L';
    
    /** /for (j = 0 ; j < dc->length ; j++) {
      ind[j] = ccp->start_edge_vars + graph[ dc->nodes[j] * ccp->num_nodes + dc->nodes[(j+1) % ccp->num_nodes] ];
      val[j] = 1.0;
      
      ind[j + dc->length] = ccp->start_edge_vars + graph[ dc->nodes[(j-1) % ccp->num_nodes] * ccp->num_nodes + dc->nodes[j] ];
      val[j + dc->length] = 1.0;
    }/ **/
    
    //status = CPXcutcallbackadd(env, cbdata, wherefrom, 2 * dc->length, rhs, sen, ind, val, CPX_USECUT_PURGE); //FORCE?
    
    free(beg);
    free(rhs);
    free(sen);
    free(ind);
    free(val);
    
    dc = dc->next;
  }
  
TERMINATE:

  return (status);
}


static void
free_and_null (char **ptr)
{
   if ( *ptr != NULL ) {
      free (*ptr);
      *ptr = NULL;
   }
} /* END free_and_null */ 


static void
usage (char *progname)
{
   fprintf (stderr,
    "Usage: %s [-r] filename\n", progname);
   fprintf (stderr,
    "  filename   Name of a file, with .mps, .lp, or .sav\n");
   fprintf (stderr,
    "             extension, and a possible, additional .gz\n"); 
   fprintf (stderr,
    "             extension\n");
   fprintf (stderr,
    "  -r         Indicates that callbacks will refer to the\n");
   fprintf (stderr,
    "             presolved model\n");
} /* END usage */


static int CPXPUBLIC 
usersolve (CPXCENVptr env,
           void       *cbdata,
           int        wherefrom,
           void       *cbhandle,
           int        *useraction_p)
{
   int      status = 0;
   int      nodecount;
   CPXLPptr nodelp;

   *useraction_p = CPX_CALLBACK_DEFAULT;

   /* Get pointer to LP subproblem */

   status = CPXgetcallbacknodelp (env, cbdata, wherefrom, &nodelp);
   if ( status )  goto TERMINATE;

   /* Find out what node is being processed */

   status = CPXgetcallbackinfo (env, cbdata, wherefrom,
                                CPX_CALLBACK_INFO_NODE_COUNT,
                                &nodecount);
   if ( status )  goto TERMINATE;

   /* Solve initial node with primal, others with dual */

   if ( nodecount < 1 )  status = CPXprimopt (env, nodelp);
   else                  status = CPXdualopt (env, nodelp);
  
  /*int cols = CPXgetnumcols (env, nodelp);
   if ( cols <= 0 ) {
      fprintf (stdout, "Can't get number of columns.\n");
      status = CPXERR_CALLBACK;
      goto TERMINATE;
   }
  double *x = malloc(cols*sizeof(double));
  
        status = CPXgetcallbacknodex (env, cbdata, wherefrom, x, 0,
                                 cols-1);
  // status = CPXgetx(env, nodelp, x, 0, cols-1);
  if (status) {
    fprintf(stderr, "Fatal error in cpx_callback_bnsl_solve:\n"
                    "CPXgetcallbacknodex : %d\n", status);
    exit(status);
  }
  
  
  int i, j,nfrac = 0;
  for (i = 0 ; i < cols ; i++) {
    if (x[i] > 0.9999) {
      printf("INT :: %d: %f\n", i, fabs(x[i]));
    } else if (x[i] > 0.0001) {
      printf("FRACT :: %d: %f\n", i, fabs(x[i]));
      nfrac++;
    }
  }*/

   /* If the solve was OK, set return to say optimization has
      been done in callback, otherwise return the CPLEX error
      code */

   if ( !status )  *useraction_p = CPX_CALLBACK_SET;

TERMINATE:

   return (status);

} /* END usersolve */


static int CPXPUBLIC
usersetbranch (CPXCENVptr   env,
               void         *cbdata,
               int          wherefrom,
               void         *cbhandle,
               int          brtype,
               int          sos,
               int          nodecnt,
               int          bdcnt,
               const double *nodeest,
               const int    *nodebeg,
               const int    *indices,
               const char   *lu,
               const int    *bd,
               int          *useraction_p)   
{
   int status = 0;
 
   int      j, bestj = -1;
   int      cols;
   double   maxobj = -CPX_INFBOUND;
   double   maxinf = -CPX_INFBOUND;
   double   xj_inf;
   int      xj_lo;
   double   objval;
   double   *x   = NULL;
   double   *obj = NULL;
   int      *feas = NULL;
 
   char     varlu[1];
   int      varbd[1];
   int      seqnum1, seqnum2;
 
   CPXCLPptr lp;
 
   /* Initialize useraction to indicate no user action taken */
 
   *useraction_p = CPX_CALLBACK_DEFAULT;
 
   /* If CPLEX is choosing an SOS branch, take it */
 
   if ( sos >= 0 )  return (status);
 
   /* Get pointer to the problem */
 
   status = CPXgetcallbacklp (env, cbdata, wherefrom, &lp);
   if ( status ) {
      fprintf (stdout, "Can't get LP pointer.\n");
      goto TERMINATE;
   }
 
   cols = CPXgetnumcols (env, lp);
   if ( cols <= 0 ) {
      fprintf (stdout, "Can't get number of columns.\n");
      status = CPXERR_CALLBACK;
      goto TERMINATE;
   }
 
   /* Get solution values and objective coefficients */
 
   x    = (double *) malloc (cols * sizeof (double));
   obj  = (double *) malloc (cols * sizeof (double));
   feas = (int *)    malloc (cols * sizeof (int));
   if ( x     == NULL ||
        obj   == NULL ||
        feas  == NULL   ) {
      fprintf (stdout, "Out of memory.");
      status = CPXERR_CALLBACK;
      goto TERMINATE;
   }
 
   status = CPXgetcallbacknodex (env, cbdata, wherefrom, x, 0,
                                 cols-1);
   if ( status ) {
      fprintf (stdout, "Can't get node solution.");
      goto TERMINATE;
   }
 
   status = CPXgetcallbacknodeobjval (env, cbdata, wherefrom,
                                      &objval);
   if ( status ) {
      fprintf (stdout, "Can't get node objective value.");
      goto TERMINATE;
   }
 
   status = CPXgetobj (env, lp, obj, 0, cols-1);
   if ( status ) {
      fprintf (stdout, "Can't get obj.");
      goto TERMINATE;
   }
 
   status = CPXgetcallbacknodeintfeas (env, cbdata, wherefrom, feas,
                                       0, cols-1);
   if ( status ) {
      fprintf (stdout,
               "Can't get variable feasible status for node.");
      goto TERMINATE;
   }
 
   /* Branch on var with largest objective coefficient among those
      with largest infeasibility */
 
   for (j = 0; j < cols; j++) {
      if ( feas[j] == CPX_INTEGER_INFEASIBLE ) {
         xj_inf = x[j] - floor (x[j]);
         if ( xj_inf > 0.5 )  xj_inf = 1.0 - xj_inf;
 
         if ( xj_inf >= maxinf                            &&
              (xj_inf > maxinf || fabs (obj[j]) >= maxobj)  ) {
            bestj  = j;
            maxinf = xj_inf; 
            maxobj = fabs (obj[j]);
         }
      }
   }
 
   /* If there weren't any eligible variables, take default branch */
 
   if ( bestj < 0 ) {
      goto TERMINATE;
   }
 
   /* Now set up node descriptions */
 
   xj_lo = (int) floor (x[bestj]);
 
   /* Up node */
 
   varlu[0] = 'L';
   varbd[0] = xj_lo + 1;
   status = CPXbranchcallbackbranchbds (env, cbdata, wherefrom,
                                        objval, 1, &bestj, varlu, varbd,
                                        NULL, &seqnum1);
   if ( status )  goto TERMINATE;
 
   /* Down node */
 
   varlu[0] = 'U';
   varbd[0] = xj_lo;
 
   status = CPXbranchcallbackbranchbds (env, cbdata, wherefrom,
                                        objval, 1, &bestj, varlu, varbd,
                                        NULL, &seqnum2);
   if ( status )  goto TERMINATE;
 
   /* Set useraction to indicate a user-specified branch */
 
   *useraction_p = CPX_CALLBACK_SET;
 
TERMINATE:
 
   free_and_null ((char **) &x);
   free_and_null ((char **) &obj);
   free_and_null ((char **) &feas); 
   return (status);
 
} /* END usersetbranch */


static int CPXPUBLIC 
userselectnode (CPXCENVptr env,
                void       *cbdata,
                int        wherefrom,
                void       *cbhandle,
                int        *nodenum_p,
                int        *useraction_p)
{
   int status = 0;

   int    thisnode;
   int    nodesleft;
   int    bestnode = 0;
   int    depth;
   int    maxdepth = -1;
   double siinf;
   double maxsiinf = 0.0;

   /* Initialize useraction to indicate no user node selection */

   *useraction_p = CPX_CALLBACK_DEFAULT;

   /* Choose the node with the largest sum of infeasibilities among
      those at the greatest depth */

   status = CPXgetcallbackinfo (env, cbdata, wherefrom,
                                CPX_CALLBACK_INFO_NODES_LEFT,
                                &nodesleft);
   if ( status )  goto TERMINATE;

   for (thisnode = 0; thisnode < nodesleft; thisnode++) {
      status = CPXgetcallbacknodeinfo (env, cbdata, wherefrom,
                                       thisnode,
                                       CPX_CALLBACK_INFO_NODE_DEPTH,
                                       &depth);
      if ( !status ) {
         status = CPXgetcallbacknodeinfo (env, cbdata, wherefrom,
                                        thisnode,
                                        CPX_CALLBACK_INFO_NODE_SIINF,
                                        &siinf);
      }
      if ( status )  break;

      if ( (depth >= maxdepth)                   &&
           (depth > maxdepth || siinf > maxsiinf)  ) {
         bestnode = thisnode;
         maxdepth = depth;
         maxsiinf = siinf;
      }
   }

   *nodenum_p = bestnode;
   *useraction_p = CPX_CALLBACK_SET;

TERMINATE:

   return (status);

} /* END userselectnode */