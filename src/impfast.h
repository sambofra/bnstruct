#include <R.h>
#include <Rinternals.h>
#include <math.h>

/* Compute the Heterogeneous Euclidean Overlap Metric between vector vec and each row of matrix 
   mat
*/
SEXP bnstruct_heom_dist( SEXP sexp_vec, SEXP sexp_mat, SEXP sexp_num_var, SEXP sexp_num_var_range );

