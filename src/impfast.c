#include "impfast.h"

SEXP bnstruct_heom_dist( SEXP sexp_vec, SEXP sexp_mat, SEXP sexp_num_var, SEXP sexp_num_var_range )
{
	// inputs
	int i,j;
	int nvar = ncols(sexp_mat);
	int nrow = nrows(sexp_mat);
	double * vec = REAL(sexp_vec);
	double * mat = REAL(sexp_mat);
	int * num_var = INTEGER(sexp_num_var);
	double * num_var_range = REAL(sexp_num_var_range);
		
	// allocate output and copy input
	SEXP result;
	PROTECT( result = allocVector(REALSXP, nrow) );
	double * res = REAL(result);
	for( i = 0; i < nrow; i++ )
		res[i] = 0;
		
	// internal structure
	int num_var_ind[nvar];
	double num_var_range_ind[nvar];
	for( i = 0; i < nvar; i++ )
	{
		num_var_ind[i] = 0;
		num_var_range_ind[i] = 0;
	}

	for( i = 0; i < length(sexp_num_var); i++ )
	{
		num_var_ind[ num_var[i] - 1 ] = 1;
		num_var_range_ind[ num_var[i] - 1 ] = num_var_range[i];
	}
	
	// compute distances
	for( i = 0; i < nvar; i++ )
	{
		if( ISNA(vec[i]) )
			for( j = 0; j < nrow; j++ )
				res[j] += 1;
		
		else if( num_var_ind[i] )
			for( j = 0; j < nrow; j++ )
				if( ISNA(mat[j + i*nrow]) )
					res[j] += 1;
				else
					res[j] += pow( (vec[i] - mat[j + i*nrow]) / num_var_range_ind[i], 2 );
		
		else
			for( j = 0; j < nrow; j++ )
				if( ISNA(mat[j + i*nrow]) )
					res[j] += 1;
				else
					res[j] += ( vec[i] != mat[j + i*nrow] );
	}

	for( i = 0; i < nrow; i++ )
		res[i] = sqrt(res[i]);
	
	UNPROTECT(1);
	return( result );
}
