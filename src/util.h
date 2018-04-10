#ifndef _UTIL_H_
#define _UTIL_H_

#include <R.h>
#include <Rinternals.h>
#include <math.h>

/* compute the BDEu score of node _ni_, with parents _pars_, from dataset
   _data_ and with equivalent sample size _ess_
*/
SEXP bnstruct_score_node( SEXP data, SEXP node_sizes, SEXP ni, SEXP pars, SEXP func, SEXP ess );

// compute score for a node as above, but called from c, not from R
double score_node_1( int* data, int ncols_data, int nrows_data, int* node_sizes, unsigned int ni, int* pars, int length_pars, int func, double ess );

/* test wether the graph _g_ is acyclic
*/
SEXP bnstruct_is_acyclic( SEXP g );

/* computes contingency table, excluding NAs: 
*/
SEXP bnstruct_compute_counts_nas( SEXP data, SEXP node_sizes );

/* computes contingency table, no NAs assumed */
SEXP bnstruct_compute_counts( SEXP data, SEXP node_sizes );

/* computes log likelihood for the family (ni,pa), accounting for NAs
*/
double bdeu_score( unsigned int * d, unsigned int n_nodes, unsigned int n_cases, unsigned int * ns, 
	unsigned int ni, unsigned int * pa, unsigned int n_pa, double alpha );
  
/**
 * log likelihood
 */
double log_likelihood( unsigned int * d, unsigned int n_nodes, unsigned int n_cases, unsigned int * ns, 
  unsigned int ni, unsigned int * pa, unsigned int n_pa, double penalty );
	
#endif
