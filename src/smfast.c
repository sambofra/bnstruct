#include "smfast.h"

SEXP fbp( SEXP aflml )
{
	R_len_t ni, si, bi, pos, pos2; 
	R_len_t n_nodes = nrows(aflml); 
	R_len_t pow_nodes = ncols(aflml);
	R_len_t mask = 1; 
	int *bps; // force to 32bit integer
	double *ml = REAL(aflml), *bss;
	SEXP res;
	
	// allocate output
	PROTECT( res = allocMatrix(INTSXP, n_nodes, pow_nodes) );
	bps = INTEGER(res);
	memset( bps, 0, sizeof(int) * n_nodes * pow_nodes );
	
	// allocate internal structures 
	bss = (double *) R_alloc( n_nodes * pow_nodes, sizeof(double) );
	memset( bss, 0, sizeof(double) * n_nodes * pow_nodes );
	
	for( ni = 0; ni < n_nodes; ni++ )
		for( si = 0; si < pow_nodes; si++ )
		{
			/* a node can not be parent of itself */
			if( si & (mask<<ni) )
				continue;
			
			pos = si*n_nodes + ni;
			bps[pos] = si+1;
			bss[pos] = ml[pos];
			
			/* cycle through all parents and remove one in turn */
			for( bi = 0; bi < n_nodes; bi++ )
				if( si & (mask<<bi) )
				{
					pos2 = ( si ^ (mask<<bi) )*n_nodes + ni; /* remove bi-th parent */
					if( bss[pos2] > bss[pos] )
					{
						bps[pos] = bps[pos2];
						bss[pos] = bss[pos2];
					}
				}
			}
	
	UNPROTECT(1);
	return( res );
}

SEXP fbs( SEXP bps, SEXP aflml )
{
	R_len_t si, sink, nop, pos;
	R_len_t n_nodes = nrows(aflml); 
	R_len_t pow_nodes = ncols(aflml);
	R_len_t mask = 1;
	double skore;
	int *sinks; // force to 32bit integer
	int *ps = INTEGER(bps);
	double *ml = REAL(aflml), *scores;
	SEXP res;
	
	// allocate output
	PROTECT( res = allocVector(INTSXP, pow_nodes) );
	sinks = INTEGER(res);
	for( si = 0; si < pow_nodes; si++ ) sinks[si] = -1;
		
	// allocate internal structures 
	scores = (double *) R_alloc( pow_nodes, sizeof(double) );
	memset( scores, 0, sizeof(double) * pow_nodes );
	
	for( si = 0; si < pow_nodes; si++ )
		for( sink = 0; sink < n_nodes; sink++ )
			if( si & (mask<<sink) )
			{
				nop = si ^ (mask<<sink);
				skore = scores[nop] + ml[(ps[nop*n_nodes+sink]-1)*n_nodes+sink];
				
				if( sinks[si] == -1 || skore > scores[si] )
				{
					scores[si] = skore;
					sinks[si] = sink+1;
				}
			}
			
	UNPROTECT(1);
	return( res );
}

SEXP na_rows_int( SEXP mat )
{
	int i,j,stride, *res;
	int n_rows = nrows( mat );
	int n_cols = ncols( mat );
	int * m = INTEGER( mat );
	SEXP(result);
	
	// allocate oputput
	PROTECT(result = allocVector(INTSXP, n_rows));
	res = INTEGER(result);
	memset( res, 0, sizeof(int) * n_rows );
	
	for( j = 0; j < n_cols; j++ )
	{
		stride = n_rows * j;
		for( i = 0; i < n_rows; i++ )
			res[i] |= (int)(NA_INTEGER == (m[i+stride]));
	}
	
	UNPROTECT(1);
	return( result );
}

SEXP fumt_mask( SEXP n_elements, SEXP pattern )
{
	int i, times, ind, stride, start;
	int bitmask = 1;
	int n_el = asInteger(n_elements);
	PROTECT( pattern = coerceVector(pattern,INTSXP) );
	int * pat = INTEGER(pattern);
	int l_pat = LENGTH(pattern);
	int pow_el = bitmask<<n_el; // faster than power for int
	SEXP result;
	
	// allocate output
	PROTECT(result = allocVector(INTSXP, pow_el));
	int * mask = INTEGER(result);
	memset( mask, 0, sizeof(int) * pow_el );
	
	// assign pattern
	for( i = 0; i < l_pat; i++ )
		mask[ bitmask << ((int)pat[i]-1) ] = 1;
	
	// compute fumt
	for( i = 0; i < n_el; i++ )
	{
		stride = bitmask<<i;
		for( times = 0; times < (bitmask<<(n_el-i-1)); times++ )
		{
			start = times * (bitmask<<(i+1));
			for( ind = 0; ind < stride; ind++ )
				mask[ start + ind + stride ] += mask[ start + ind ];
		}
	}
	
	UNPROTECT(2);
	return(result);
}

SEXP compute_counts_nas( SEXP data, SEXP node_sizes, SEXP na_rows )
{
	int i,j,index,elmt;
	// inputs
	int * d = INTEGER(data);
	int n_nodes = ncols(data);
	int n_cases = nrows(data);
	int * ns = INTEGER(node_sizes);
	int * nas = INTEGER(na_rows);
	
	// internal structure
	int cum_prod_sizes[n_nodes+1]; 
	cum_prod_sizes[0] = 1;
	for( i = 1; i < n_nodes + 1; i++ )
		cum_prod_sizes[i] = cum_prod_sizes[i-1] * ns[i-1];
	
	// allocate output
	SEXP result;
	PROTECT( result = allocVector(REALSXP, cum_prod_sizes[n_nodes]) );
	double * counts = REAL(result);
	memset( counts, 0, sizeof(double) * cum_prod_sizes[n_nodes] );
	setAttrib(result, R_DimSymbol, node_sizes);
	
	// compute counts
	for( i = 0; i < n_cases; i++ )
		if( !nas[i] )
		{
			index = 0;
			for( j = 0; j < n_nodes; j++ )
				index += (d[ i + j*n_cases ] - 1) * cum_prod_sizes[j];
			counts[index] += 1;			
		}
	
	UNPROTECT(1);
	return result;
}

SEXP compute_counts( SEXP data, SEXP node_sizes )
{
  int i,j,index,elmt;
	// inputs
	int * d = INTEGER(data);
	int n_nodes = ncols(data);
	int n_cases = nrows(data);
	int * ns = INTEGER(node_sizes);
	
	// internal structure
	int cum_prod_sizes[n_nodes+1]; 
	cum_prod_sizes[0] = 1;
	for( i = 1; i < n_nodes + 1; i++ )
		cum_prod_sizes[i] = cum_prod_sizes[i-1] * ns[i-1];
	
	// allocate output
	SEXP result;
	PROTECT( result = allocVector(REALSXP, cum_prod_sizes[n_nodes]) );
	double * counts = REAL(result);
	memset( counts, 0, sizeof(double) * cum_prod_sizes[n_nodes] );
	setAttrib(result, R_DimSymbol, node_sizes);
	
	// compute counts
	for( i = 0; i < n_cases; i++ )
	{
		index = 0;
		for( j = 0; j < n_nodes; j++ )
			index += (d[ i + j*n_cases ] - 1) * cum_prod_sizes[j];
		counts[index] += 1;			
	}
	
	UNPROTECT(1);
	return result;
}


SEXP all_fam_log_marg_lik( SEXP data, SEXP node_sizes, SEXP imp_fam_mask, SEXP iss )
{
	unsigned int i,j,n_pa,pos;
	
	// get inputs
	unsigned int * d = INTEGER(data);
	unsigned int n_nodes = ncols(data);
	unsigned int n_ex = nrows(data);	 
	unsigned int * ns = INTEGER(node_sizes);
	unsigned int * ifm = INTEGER(imp_fam_mask);
	unsigned int pow_nodes = ncols(imp_fam_mask);
	double alpha = *(REAL(iss));
	unsigned int * pa = (unsigned int *) R_alloc( n_nodes, sizeof(unsigned int) );
	
	// allocate and initialize output
	SEXP result;
	PROTECT( result = allocMatrix(REALSXP, n_nodes, pow_nodes) );
	double * aflml = REAL(result);
	for( i = 0; i < n_nodes*pow_nodes; i++ )
		aflml[i] = R_NegInf;
	
	// compute log likelihood
	for( i = 0; i < n_nodes; i++ )
		for( j = 0; j < pow_nodes; j++ )
		{
			pos = j*n_nodes + i;
			if( ifm[pos] )
			{
				// Rprintf("get bits\n");
				n_pa = get_bits( j, pa, n_nodes );
				// Rprintf("log lik, node %d, n parents %d\n",i,n_pa);
				aflml[pos] = log_lik( d, n_nodes, n_ex, ns, i, pa, n_pa, alpha );
				// Rprintf("end\n");
			}
		}
	
	UNPROTECT(1);
	return result;	
}

unsigned int get_bits( unsigned int word, unsigned int * bits, unsigned int size )
{
	unsigned int i, count = 0, bitmask = 1;
	for( i = 0; i < size; i++ )
		if( word & bitmask<<i )
			bits[count++] = i;
	
	return count;
}

double log_lik( unsigned int * d, unsigned int n_nodes, unsigned int n_cases, unsigned int * ns, 
	unsigned int ni, unsigned int * pa, unsigned int n_pa, double alpha )
{
	int i, j, index, elmt, stride;
	double acc, ll;
	
	// utility vectors
	int cum_prod_sizes[n_pa+2]; 
	cum_prod_sizes[0] = 1;
	cum_prod_sizes[1] = ns[ni];
	for( i = 0; i < n_pa; i++ )
		cum_prod_sizes[i+2] = cum_prod_sizes[i+1] * ns[pa[i]];
	
	int strides[n_pa + 1];
	strides[0] = ni * n_cases;
	for( i = 0; i < n_pa; i++ )
		strides[i+1] = pa[i] * n_cases;
	
	// utility variables
	int prod_sizes = cum_prod_sizes[n_pa+1];
	int prod_sizes_pa = prod_sizes / ns[ni];
	// int n_na = 0;
	double prior = alpha / prod_sizes;
	double alpha_ij = alpha / prod_sizes_pa;
	
	// vector holding counts (LARGE)
	double * counts = calloc( prod_sizes, sizeof(double) ); 
	
	// compute counts, skipping NAs
	for( i = 0; i < n_cases; i++ )
	{
		index = 0;
		// sum using strides
		for( j = 0; j < n_pa + 1; j++ )
		{
			elmt = d[ i + strides[j] ];
			//if( elmt == NA_INTEGER )
			//{
			//	n_na++;
			//	break;
			//}
			index += (elmt - 1) * cum_prod_sizes[j];
		}
		// check if NA encountered
		//if( j < n_pa + 1 )
		//	continue;
    
		counts[index] += 1;			
	}
	
	// correct for NAs
	//for( i = 0; i < prod_sizes; i++ )
	//	counts[i] += n_na * (counts[i] + prior) / ( n_cases - n_na + alpha );
		
	// compute log likelihood
	ll = prod_sizes_pa*lgamma(alpha_ij) - prod_sizes*lgamma(prior);
	for( i = 0; i < prod_sizes; i++ )
		ll += lgamma(prior + counts[i]);
	
	// accumulate parent counts, by summing over ni
	for( i = 0; i < prod_sizes_pa; i++ )
	{
		stride = i * ns[ni];
		acc = counts[stride];
		for( j = 1; j < ns[ni]; j++ )
			acc += counts[stride + j];
		ll -= lgamma(alpha_ij + acc);
	}
  
  free( counts );
	
	return ll;
}

/*
SEXP llna( SEXP parents, int node, SEXP node_sizes, double iss, SEXP data )
{
	int i, j, index, flag, elmt;
	int * pa = INTEGER(parents);
	int * ns = INTEGER(node_sizes);
	int * d = INTEGER(data);
	int n_cases = nrows(data);
	int n_pa = length(pa);
	
	// convenience variables
	long cum_size[ n_pa+1 ];
	long sum_cum_size_pa = 1;
	cum_size[0] = 1;
	for( i = 1; i < n_pa; i++ )
	{
		cum_size[i] = cum_size[i-1] * node_sizes[pa[i-1]-1];
		sum_cum_size_pa += cum_size[i];
	}
	cum_size[n_pa] = cum_size[n_pa-1] * node_sizes[pa[n_pa]-1];
	sum_cum_size = sum_cum_size_pa + cum_size[n_pa];
	long prod_size_pa = cum_size[n_pa];
	long prod_size = prod_size_pa * node_sizes[node-1]; 
	
	// allocate space for storing counts
	counts = (int *) R_alloc( prod_size, sizeof(int) );
	memset( counts, 0, sizeof(int) * prod_size );
	counts_pa = (int *) R_alloc( prod_size_pa, sizeof(int) );
	memset( counts_pa, 0, sizeof(int) * prod_size_pa );
	
	// compute counts
	for( i = 0; i < n_cases; i++ )
	{
		index = 0;
		flag = 0;
		for( j = 0; j < n_pa; j++ )
		{
			elmt = d[ i + n_cases*(pa[j]-1) ];
			if( NA_INTEGER(elmt) ) // if one elmt is NA, skip case
			{
				flag = 1;
				break;
			}
			index += elmt * cum_size[i];
		}
		if( flag ) continue;

		elmt = d[ i + n_cases*(node-1) ];
		if( !NA_INTEGER(elmt) )
		{
			counts_pa[ index - sum_cum_size_pa ]++;
			counts[ index*elmt - sum_cum_size ]++;
		}
	}
	
}
*/

