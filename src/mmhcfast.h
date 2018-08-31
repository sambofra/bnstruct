#ifndef _MMHCFAST_H_
#define _MMHCFAST_H_

#include <R.h>
#include <Rinternals.h>
#include <math.h>
#include "smfast.h"
#include "util.h"

/* generate the next combinations of length(sexp_comb) indices drawn
   from 1:sexp_n, or 0^m if no next combination 
*/
SEXP bnstruct_next_comb( SEXP sexp_comb, SEXP sexp_n );

/* compute the g2 statistic for conditional independence of
   data[,1] from data[,2] conditioned on data[,3:end]
*/
SEXP bnstruct_g2_stat( SEXP data, SEXP node_sizes );

/* check if matrix _mat_ is in tabu list
*/
SEXP bnstruct_in_tabu( SEXP mat, SEXP tabu );

#endif
