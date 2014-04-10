#ifndef _BNCPLEX_H_
#define _BNCPLEX_H_

#include <R.h>
#include <Rinternals.h>
#include <math.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <assert.h>
#include "mmhcfast.h"
#include <ilcplex/cplex.h>

typedef struct _cplex_callback_params {
  int      num_nodes;
  int     *start_cpcs_vars;
  int      start_edge_vars;
  int      numcols;
  int      num_vars;
  int      num_cpcs_vars;
  int      num;
  double  *x;
  int     *beg;
  int     *ind; 
  double  *val;
  double  *rhs;
  char    *sen;
  CPXLPptr prob;
} cplex_callback_params;

typedef struct _digraph_cycle {
  int  length;
  int *nodes;
  struct _digraph_cycle *next;
} digraph_cycle;

typedef struct _digraph_cycles {
  int            num_cycles;
  digraph_cycle *cycles;
} digraph_cycles;


// generate bit combination following the one given as parameter in lexical order
SEXP nextLexicalBitComb(SEXP starting_num);

/*
iterates through a list looking for *str
Use as:
a <- list(f = 1, g = 2, h = 3)
...
double g;
g = REAL(getListElement(a, "g"))[0];
*/
SEXP getListElement(SEXP list, const char *str);

// solve by using cplex callbacks
SEXP solve_BNSL_with_callbacks(SEXP cplexenv, SEXP cplexprob, SEXP num_nodes, SEXP cpcs, SEXP combinations,
                               SEXP cpcs_vars, SEXP edge_vars,
                               SEXP start_cpcs_vars, SEXP start_edge_vars,
                               SEXP num_vars, SEXP num_cpcs_vars, SEXP cpcs_scores);
// callback for cplex
static int CPXPUBLIC cpx_callback_bnsl(CPXENVptr env,
                                       void     *cbdata,
                                       int       wherefrom,
                                       void     *cbhandle,
                                       int      *useraction_p);

// is_acyclic for graphs passed by c not R
int is_acyclic_c(int *graph, int n_nodes);

// find cycles in directed graphs
digraph_cycle *digraph_search(int *graph, int num_nodes, int start);
digraph_cycles *find_cycles_in_digraph(int *orig_graph, int num_nodes);

#endif