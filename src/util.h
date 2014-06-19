#ifndef _UTIL_H_
#define _UTIL_H_

#include <R.h>
#include <Rinternals.h>
#include <math.h>

/* compute the BDEu score of node _ni_, with parents _pars_, from dataset
   _data_ and with equivalent sample size _ess_
*/
SEXP score_node( SEXP data, SEXP node_sizes, SEXP ni, SEXP pars, SEXP ess );

/* test wether the graph _g_ is acyclic
*/
SEXP is_acyclic( SEXP g );

/* computes contingency table, excluding NAs: 
*/
SEXP compute_counts_nas( SEXP data, SEXP node_sizes );

/* computes contingency table, no NAs assumed */
SEXP compute_counts( SEXP data, SEXP node_sizes );

/* computes log likelihood for the family (ni,pa), accounting for NAs
*/
double log_lik( unsigned int * d, unsigned int n_nodes, unsigned int n_cases, unsigned int * ns, 
	unsigned int ni, unsigned int * pa, unsigned int n_pa, double alpha );
	
#endif