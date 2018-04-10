#include "mmhcfast.h"

SEXP bnstruct_next_comb( SEXP sexp_comb, SEXP sexp_n )
{
	R_len_t m, i, j, finished;
	m = length(sexp_comb);
	int *comb = INTEGER(sexp_comb);
	int *n = INTEGER(sexp_n);
	
	// allocate output and copy input
	SEXP result;
	PROTECT( result = allocVector(INTSXP, m) );
	int * ncomb = INTEGER(result);
	for ( i = 0; i < m; i++ )
		ncomb[i] = comb[i];
	
	finished = 1;
	for (i = m - 1; i >= 0; i--) 
	{
		if (ncomb[i] < *n - m + i + 1) 
		{
			ncomb[i]++;
			for (j = i + 1; j < m; j++) 
				ncomb[j] = ncomb[i] - i + j;
	      finished = 0;
	      break;
		}
	}
	
	if( finished )
		// to distinguish the end
		for( i = 0; i < m; i++ )
			ncomb[i] = 0;
			
	UNPROTECT(1);
	return( result );
}

SEXP bnstruct_g2_stat( SEXP data, SEXP node_sizes )
{
	int i,j,k,index,sx,sy,sz,sxy,val,zx,zy,zz,zc,df,elmt;
	double stat = 0;
	// inputs
	int * d = INTEGER(data);
	int n_nodes = ncols(data);
	int n_cases = nrows(data);
	int * ns = INTEGER(node_sizes);
	sx = ns[0];
	sy = ns[1];
	sz = 1;
	for( i = 2; i < length(node_sizes); i++ )
		sz *= ns[i];
	sxy = sx * sy;
	
	// internal structures
	int cum_prod_sizes[n_nodes+1]; 
	cum_prod_sizes[0] = 1;
	for( i = 1; i < n_nodes + 1; i++ )
		cum_prod_sizes[i] = cum_prod_sizes[i-1] * ns[i-1];
	
	int * counts = (int*) calloc( cum_prod_sizes[n_nodes], sizeof(int) );
	double px[ sy ];
	double py[ sx ];
	double pz;
	
	// compute counts, skipping NAs
	for( i = 0; i < n_cases; i++ )
	{
		index = 0;
		for( j = 0; j < n_nodes; j++ )
		{
			elmt = d[ i + j*n_cases ];
			if( elmt == NA_INTEGER )
				break;
			index += (elmt - 1) * cum_prod_sizes[j];
		}
		// check if NA encountered
		if( j < n_nodes )
			continue;
			
		counts[index]++;			
	}	
	
	// compute stat
	zx = 0; zy = 0; zz = 0; zc = 0;
	for( i = 0; i < sz; i++ )
	{
		// compute px, py and pz
		for( j = 0; j < sy; j++ )
			px[j] = 0;
		for( j = 0; j < sx; j++ )
			py[j] = 0;
		pz = 0;
		
		for( j = 0; j < sy; j++ )
			for( k = 0; k < sx; k++ )
			{
				val = counts[ i*sxy + j*sx + k ];
				px[j] += val; 
				py[k] +=	val;
				pz += val;
			}
			
		// check for zeros
		for( j = 0; j < sx; j++ )
			if( py[j] == 0 )
				zy++;
		for( j = 0; j < sy; j++ )
			if( px[j] == 0 )
				zx++;
		if( pz == 0 )
			zz++;
		
		// accumulate on stat
		for( j = 0; j < sy; j++ )
			for( k = 0; k < sx; k++ )
			{
				val = counts[ i*sxy + j*sx + k ];
				if( val != 0 )
					stat += val * log( (val * pz) / (px[j] * py[k]) );
				else
					zc++;
			}
	}
	
	free( counts );
	
	df = (sz*sxy - zc) + (sz - zz) - (sz*sx - zx) - (sz*sy - zy);
	
	// allocate output
	SEXP result;
	PROTECT( result = allocVector(REALSXP, 2) );
	REAL(result)[0] = 2*stat;
	REAL(result)[1] = df;
	
	UNPROTECT(1);
	return result;
}

SEXP bnstruct_in_tabu( SEXP mat, SEXP tabu )
{
  int n_nodes = ncols(mat);
  int t_tenure = INTEGER(getAttrib(tabu, R_DimSymbol))[2]; // dim[3]
  int sq_nodes = n_nodes * n_nodes;
  int stride, i, j;
  int * m = INTEGER(mat);
  int * t = INTEGER(tabu);
  
  // allocate and protect output
  SEXP test;
  PROTECT( test = allocVector(INTSXP, 1) );
  
  *INTEGER(test) = 0;
  
  for( i = 0; i < t_tenure; i++ )
  {
    stride = i*sq_nodes;
    for( j = 0; j < sq_nodes; j++ )
      if( m[j] != t[stride + j] )
        break;
    if( j == sq_nodes ) // not out from the break
    {
      *INTEGER(test) = 1;
      break;
    }
  }
  
  UNPROTECT(1);
  return test ;
}
