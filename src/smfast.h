#ifndef _SMFAST_H_
#define _SMFAST_H_

#include <R.h>
#include <Rinternals.h>
#include <math.h>

/* find best parents */
SEXP fbp( SEXP aflml );

/* find best sinks */
SEXP fbs( SEXP bps, SEXP aflml );

/* compute the number of rows with at least one NA
 	(integer input)
*/
SEXP na_rows_int( SEXP mat );

/* fast upwards moebius transform */
SEXP fumt_mask( SEXP n_elements, SEXP pattern );

/* computes contingency table, excluding NAs */
SEXP compute_counts_nas( SEXP data, SEXP node_sizes, SEXP na_rows );

/* computes contingency table, no NAs assumed */
SEXP compute_counts( SEXP data, SEXP node_sizes );

/* computes log marginal likelihood for all possible families */
SEXP all_fam_log_marg_lik( SEXP data, SEXP node_sizes, SEXP imp_fam_mask, SEXP iss );

/* 
	very internal functions 
*/

/* fill _bits_ with the indices of ones in _word_ (which can be at most _size_)
	and return the number of ones */
unsigned int get_bits( unsigned int word, unsigned int * bits, unsigned int size ); 

/* computes log likelihood for the family (ni,pa), accounting for NAs
*/
double log_lik( unsigned int * d, unsigned int n_nodes, unsigned int n_cases, unsigned int * ns, 
	unsigned int ni, unsigned int * pa, unsigned int n_pa, double alpha );
  
#endif