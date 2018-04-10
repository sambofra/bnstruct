#ifndef _SMFAST_H_
#define _SMFAST_H_

#include <R.h>
#include <Rinternals.h>
#include <math.h>
#include "util.h"

/* find best parents */
SEXP bnstruct_fbp( SEXP aflml );

/* find best sinks */
SEXP bnstruct_fbs( SEXP bps, SEXP aflml );

/* fast upwards moebius transform */
SEXP bnstruct_fumt_mask( SEXP n_elements, SEXP pattern );

/* computes log marginal likelihood for all possible families */
SEXP bnstruct_all_fam_log_marg_lik( SEXP data, SEXP node_sizes, SEXP imp_fam_mask, SEXP iss, SEXP func );

/* 
	very internal function
*/

/* fill _bits_ with the indices of ones in _word_ (which can be at most _size_)
	and return the number of ones */
unsigned int get_bits( unsigned int word, unsigned int * bits, unsigned int size ); 
  
#endif
