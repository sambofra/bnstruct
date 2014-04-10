#include "eocb.h"

SEXP nextLexicalBitComb(SEXP starting_num) {
  unsigned long int v = *INTEGER(starting_num), w;
  SEXP out;
  PROTECT( out = allocVector(REALSXP, 1) );
  // from http://stackoverflow.com/q/8281951/571569
  unsigned long int t = (v | (v - 1)) + 1;  
  w = t | ((((t & -t) / (v & -v)) >> 1) - 1);
  *REAL(out) = (double)w;
  UNPROTECT(1);
  return out;
}

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
  
  // disable presolve
  status = CPXsetintparam(env, CPX_PARAM_PRELINEAR, 0);
  status = CPXsetintparam (env, CPX_PARAM_MIPCBREDLP, CPX_OFF);
  if (status) {
    fprintf(stderr, "Fatal error in solve_BNSL_with_callbacks:\n"
                    "CPXsetinparam (CPX_PARAM_MIPCBREDLP) : %d\n", status);
    exit(status);
  }  
  
  // set callback function cpx_callback_bnsl() to be executed whenever
  // an integer solution for the relaxated proble is found
  status = CPXsetlazyconstraintcallbackfunc(env, cpx_callback_bnsl, &ccp);
  if (status) {
    fprintf(stderr, "Fatal error in solve_BNSL_with_callbacks:\n"
                    "CPXsetlazyconstraintcallbackfunc : %d\n", status);
    exit(status);
  }

  // optimize (= solve\dots)
  // Cutting planes are inserted here
  status = CPXmipopt(env, prob);
  if (status) {
    fprintf(stderr, "Fatal error in solve_BNSL_with_callbacks:\n"
                    "CPXmipopt : %d\n", status);
    exit(status);
  }
  
  // now problem is solved to the global optimum
  // retrieve final solution
  ccp.x = malloc(ccp.num_vars * sizeof(double));
  status = CPXgetx(env, prob, ccp.x, 0, ccp.num_vars-1);
  if (status) {
    fprintf(stderr, "Fatal error in cpx_callback_bnsl_solve:\n"
                    "CPXgetx : %d\n", status);
    exit(status);
  }
  
  // return solution
  PROTECT(out = allocVector(REALSXP, ccp.num_vars));
  double *output = REAL(out);
  for (i = 0 ; i < ccp.num_vars ; i++) {
    output[i] = ccp.x[i];
  }
  UNPROTECT(1);
  
  // get objective value
  double finalcost;
  status = CPXgetobjval(env, prob, &finalcost);
  printf("----\nfinal cost: %f\n-----\n", finalcost);
  
  return(out);
}

// callback
static int CPXPUBLIC cpx_callback_bnsl(CPXENVptr env,
                                       void     *cbdata,
                                       int       wherefrom,
                                       void     *cbhandle,
                                       int      *useraction_p)
{
  int status = 0;
  // tell cplex what to do
  *useraction_p = CPX_CALLBACK_DEFAULT;
  // get parameters from mipopt() execution
  cplex_callback_params *ccp = (cplex_callback_params *)cbhandle; 

  int prenumcols, numcols;
  CPXCLPptr prelp;

  // get pointer to LP problem
  status = CPXgetcallbacklp(env, cbdata, wherefrom, &prelp);
  if (status) {
    fprintf(stderr, "Fatal error in cpx_callback_bnsl:\n"
                    "CPXgetcallbacklp : %d\n", status);
    exit(status);
  }
  

  numcols    = CPXgetnumcols(env, ccp->prob);
  prenumcols = CPXgetnumcols(env, prelp);
  
  double *prex = calloc(prenumcols, sizeof(double)),
         *x    = calloc(prenumcols, sizeof(double));
  
  // get solution computed in the node that got an integral solution
  status = CPXgetcallbacknodex(env, cbdata, wherefrom, prex, 0, prenumcols-1);
  if (status) {
    fprintf(stderr, "Fatal error in cpx_callback_bnsl:\n"
                    "CPXgetcallbacknodex : %d\n", status);
    exit(status);
  }
  
  int prenzcnt;
  double preoffset;

  // revert problem from presolved to original form
  int i, j,nfrac = 0;
  for (i = 0 ; i < prenumcols ; i++) {
    if (prex[i] > 0.9999) { // manual control of integrality of variables
    } else if (prex[i] > 0.0001) {
      nfrac++;
    }
  }
  
  int     nzcnt;
  int    *rmatind;
  double *rmatval;
  int    *pind;
  double *pval;
  
  // create adjacency matrix from variables (= graph computed in LP solution)
  int *graph = malloc((ccp->num_nodes)*(ccp->num_nodes)*sizeof(int));
  for (i = 0 ; i < ccp->num_nodes*ccp->num_nodes ; i++) {
    if (prex[i + ccp->start_edge_vars] > 0.999) {
      graph[i] = 1;
    } else {
      graph[i] = 0;
    }
  }
  
  if (!is_acyclic_c(graph, ccp->num_nodes)) {
    // look for violated constraints
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
    
    // build constraints to add
    dc = dcs->cycles;
    while (dc != NULL) {
      ind = malloc(dc->length * sizeof(int));
      val = malloc(dc->length * sizeof(double));
      
      rhs = dc->length - 1;
      sen = 'L';
      
      for (j = 0 ; j < dc->length ; j++) {
        ind[j] = ccp->start_edge_vars + dc->nodes[j] * ccp->num_nodes + dc->nodes[(j+1) % dc->length];
        val[j] = 1.0;
      }
      
      // insert constraints into the model
      status = CPXcutcallbackadd(env, cbdata, wherefrom, dc->length, rhs, sen, ind, val, CPX_USECUT_PURGE); //FORCE?
      if (status) {
        fprintf(stderr, "Fatal error in cpx_callback_bnsl:\n"
                        "CPXcutcallbackadd : %d\n", status);
        exit(status);
      }

      free(ind);
      free(val);
      
      dc = dc->next;
    }
  } else {
    // acyclic graph!
    int    *toint = calloc(nfrac, sizeof(int));
    double *ub    = calloc(nfrac, sizeof(double));
    double *lb    = calloc(nfrac, sizeof(double));
    char   *ls    = malloc(nfrac*sizeof(char));
    char   *us    = malloc(nfrac*sizeof(char));
    j = 0;
    for (i = 0 ; i < prenumcols ; i++) {
      if (prex[i] > 0.0001 && prex[i] < 0.9999) {
        toint[j++] = i;
      }
    }
    
    printf("graph is acyclic: got a feasible solution in hand\n");
    printf("TODO: save it...\n");

    double objval;
    status = CPXgetcallbacknodeobjval(env, cbdata, wherefrom, &objval);
    if (status) {
      fprintf(stderr, "Fatal error in cpx_callback_bnsl:\n"
                      "CPXgetcallbacknodeobjval : %d\n", status);
      exit(status);
    }
  }
  
  free(graph);
  free(x);
  free(prex);
  *useraction_p = CPX_CALLBACK_SET;
  return status;
}

/**********************************************************************************/

// starting from node `start`, look for a directed cycle (can we return to `start`?)
digraph_cycle *digraph_search(int *graph, int num_nodes, int start) {
  int i, j, k, steps = 0, node, length = 0;
  int *pred  = calloc(num_nodes, sizeof(int));
  for(k = 0 ; k < num_nodes ; k++) pred[k] = -1;
  int *queue = calloc(num_nodes, sizeof(int));
  int qs = 0,
      qe = 0;
  digraph_cycle *dc = NULL;
  
  queue[qe++] = start;
  do {
    i = queue[qs++];
  
    if (i == start && steps > 1) {
      // compute length of cycle (it's a BFS, so it's likely to be shorter that steps)
      node = pred[i];
      length = 1;
      while (node != start) {
        length++;
        node = pred[node];
      }
      
      // build up cycle
      dc = malloc(sizeof(dc));
      dc->length = length;
      dc->nodes  = malloc(length*sizeof(int));
      node = pred[i];
      for (k = length-1 ; k >= 0 ; k--) {
        dc->nodes[k] = node;
        node = pred[node];
      }
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
  for (i = 0 ; i < num_nodes*num_nodes ; i++) {
    graph[i] = orig_graph[i];
  }
  int num_cycles = 0;
  digraph_cycles *dcs = malloc(sizeof(digraph_cycles));
  digraph_cycle *tail = NULL, *dc;
  dcs->num_cycles = 0;
  dcs->cycles = NULL;
  for (i = 0 ; i < num_nodes ; i++) {
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
      // remove one edge in order to disallow to rediscover the same cycle multiple times
      graph[dc->nodes[0]*num_nodes+dc->nodes[1]] = 0;
    }
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
  return(dcs);
}

// copied also here for the different parameters
int is_acyclic_c(int *graph, int n_nodes) {
  int *rem    = calloc(n_nodes, sizeof(int));
  int *leaves = calloc(n_nodes, sizeof(int));
  int  rem_count = 0;
  int  i, j, flag, aleaf;
  
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
          aleaf = 1;
          leaves[i] = 1;
          rem[i] = 1;
          rem_count++;
        }
      }
  
    // test for leaves
    if( !aleaf )
    {
      test = 0;
      return test ;
    }
    else // remove edges incident on leaves
    {
      for(j = 0; j < n_nodes; j++)
        if( leaves[j] )
        {
          leaves[j] = 0; // reset for next iteration
          for( i = 0; i < n_nodes; i++ )
            gtemp[j * n_nodes + i] = 0;
        }
    }
  }
  
  free(rem);
  free(leaves);
  free(gtemp);
  
  // acyclic
  test = 1;
  return test ;
}
