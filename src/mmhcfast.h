#ifndef _MMHCFAST_H_
#define _MMHCFAST_H_

#include <R.h>
#include <Rinternals.h>
#include <math.h>
#include "smfast.h"

/* generate the next combinations of length(sexp_comb) indices drawn
   from 1:sexp_n, or 0^m if no next combination 
*/
SEXP next_comb( SEXP sexp_comb, SEXP sexp_n );

/* compute the g2 statistic for conditional independence of
   data[,1] from data[,2] conditioned on data[,3:end]
*/
SEXP g2_stat( SEXP data, SEXP node_sizes );

/* compute the BDEu score of node _ni_, with parents _pars_, from dataset
   _data_ and with equivalent sample size _ess_
*/
SEXP score_node( SEXP data, SEXP node_sizes, SEXP ni, SEXP pars, SEXP ess );

/* check if matrix _mat_ is in tabu list
*/
SEXP in_tabu( SEXP mat, SEXP tabu );

/* test wether the graph _g_ is acyclic
*/
SEXP is_acyclic( SEXP g );

#endif