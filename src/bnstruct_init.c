#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP bnstruct_all_fam_log_marg_lik(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP bnstruct_compute_counts(SEXP, SEXP);
extern SEXP bnstruct_compute_counts_nas(SEXP, SEXP);
extern SEXP bnstruct_fbp(SEXP);
extern SEXP bnstruct_fbs(SEXP, SEXP);
extern SEXP bnstruct_fumt_mask(SEXP, SEXP);
extern SEXP bnstruct_g2_stat(SEXP, SEXP);
extern SEXP bnstruct_heom_dist(SEXP, SEXP, SEXP, SEXP);
extern SEXP bnstruct_in_tabu(SEXP, SEXP);
extern SEXP bnstruct_is_acyclic(SEXP);
extern SEXP bnstruct_next_comb(SEXP, SEXP);
extern SEXP bnstruct_score_node(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"bnstruct_all_fam_log_marg_lik", (DL_FUNC) &bnstruct_all_fam_log_marg_lik, 5},
    {"bnstruct_compute_counts",       (DL_FUNC) &bnstruct_compute_counts,       2},
    {"bnstruct_compute_counts_nas",   (DL_FUNC) &bnstruct_compute_counts_nas,   2},
    {"bnstruct_fbp",                  (DL_FUNC) &bnstruct_fbp,                  1},
    {"bnstruct_fbs",                  (DL_FUNC) &bnstruct_fbs,                  2},
    {"bnstruct_fumt_mask",            (DL_FUNC) &bnstruct_fumt_mask,            2},
    {"bnstruct_g2_stat",              (DL_FUNC) &bnstruct_g2_stat,              2},
    {"bnstruct_heom_dist",            (DL_FUNC) &bnstruct_heom_dist,            4},
    {"bnstruct_in_tabu",              (DL_FUNC) &bnstruct_in_tabu,              2},
    {"bnstruct_is_acyclic",           (DL_FUNC) &bnstruct_is_acyclic,           1},
    {"bnstruct_next_comb",            (DL_FUNC) &bnstruct_next_comb,            2},
    {"bnstruct_score_node",           (DL_FUNC) &bnstruct_score_node,           6},
    {NULL, NULL, 0}
};

void R_init_bnstruct(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}

