#------------------------------------------------------------------------------#
#                     R Interface to C API of IBM ILOG CPLEX                   #
#------------------------------------------------------------------------------#

#  cplexConst.R
#  R Interface to C API of IBM ILOG CPLEX Version 12.1, 12.2, 12.3, 12.4, 12.5.
#
#  Copyright (C) 2011-2013 Gabriel Gelius-Dietrich, Dpt. for Bioinformatics,
#  Institute for Informatics, Heinrich-Heine-University, Duesseldorf, Germany.
#  All right reserved.
#  Email: geliudie@uni-duesseldorf.de
#
#  This file is part of cplexAPI.
#
#  CplexAPI is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.
#
#  CplexAPI is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with cplexAPI.  If not, see <http://www.gnu.org/licenses/>.


#------------------------------------------------------------------------------#
#              global variables (from cpxconst.h [12.5.0.0])                   #
#------------------------------------------------------------------------------#

# CPX_INFBOUND:  Any bound bigger than this is treated as infinity
#' @export
CPX_INFBOUND      <- 1.0E+20

#' @export
CPX_STR_PARAM_MAX <- 512


# Types of parameters
#' @export
CPX_PARAMTYPE_NONE   <- 0L
#' @export
CPX_PARAMTYPE_INT    <- 1L
#' @export
CPX_PARAMTYPE_DOUBLE <- 2L
#' @export
CPX_PARAMTYPE_STRING <- 3L
#' @export
CPX_PARAMTYPE_LONG   <- 4L


#------------------------------------------------------------------------------#
# Values returned for 'stat' by solution
#' @export
CPX_STAT_OPTIMAL                <-  1L
#' @export
CPX_STAT_UNBOUNDED              <-  2L
#' @export
CPX_STAT_INFEASIBLE             <-  3L
#' @export
CPX_STAT_INForUNBD              <-  4L
#' @export
CPX_STAT_OPTIMAL_INFEAS         <-  5L
#' @export
CPX_STAT_NUM_BEST               <-  6L
#' @export
CPX_STAT_ABORT_IT_LIM           <- 10L
#' @export
CPX_STAT_ABORT_TIME_LIM         <- 11L
#' @export
CPX_STAT_ABORT_OBJ_LIM          <- 12L
#' @export
CPX_STAT_ABORT_USER             <- 13L
#' @export
CPX_STAT_FEASIBLE_RELAXED_SUM   <- 14L
#' @export
CPX_STAT_OPTIMAL_RELAXED_SUM    <- 15L
#' @export
CPX_STAT_FEASIBLE_RELAXED_INF   <- 16L
#' @export
CPX_STAT_OPTIMAL_RELAXED_INF    <- 17L
#' @export
CPX_STAT_FEASIBLE_RELAXED_QUAD  <- 18L
#' @export
CPX_STAT_OPTIMAL_RELAXED_QUAD   <- 19L
#' @export
CPX_STAT_FEASIBLE               <- 23L
#' @export
CPX_STAT_ABORT_DETTIME_LIM      <- 25L


#------------------------------------------------------------------------------#
# Solution type return values from CPXsolninfo
#' @export
CPX_NO_SOLN       <- 0L
#' @export
CPX_BASIC_SOLN    <- 1L
#' @export
CPX_NONBASIC_SOLN <- 2L
#' @export
CPX_PRIMAL_SOLN   <- 3L


#------------------------------------------------------------------------------#
# Values of presolve 'stats' for columns and rows
#' @export
CPX_PRECOL_LOW   <- -1L # fixed to original lb
#' @export
CPX_PRECOL_UP    <- -2L # fixed to original ub
#' @export
CPX_PRECOL_FIX   <- -3L # fixed to some other value
#' @export
CPX_PRECOL_AGG   <- -4L # aggregated y = a*x + b
#' @export
CPX_PRECOL_OTHER <- -5L # cannot be expressed by a linear combination
#' @export
                                   # of active variables in the presolved model
#' @export
                                   #  -> crushing will fail if it has to touch
#' @export
                                   #  such a variable

#' @export
CPX_PREROW_RED   <- -1L # redundant row removed in presolved model
#' @export
CPX_PREROW_AGG   <- -2L # used to aggregate a variable
#' @export
CPX_PREROW_OTHER <- -3L # other, for example merge two inequalities
#' @export
                                   # into a single equation

#------------------------------------------------------------------------------#
# Generic constants
#' @export
CPX_ON  <-  1L
#' @export
CPX_OFF <-  0L
#' @export
CPX_MAX <- -1L
#' @export
CPX_MIN <-  1L


#------------------------------------------------------------------------------#
# Primal simplex pricing algorithm
#' @export
CPX_PPRIIND_PARTIAL     <- -1L
#' @export
CPX_PPRIIND_AUTO        <-  0L
#' @export
CPX_PPRIIND_DEVEX       <-  1L
#' @export
CPX_PPRIIND_STEEP       <-  2L
#' @export
CPX_PPRIIND_STEEPQSTART <-  3L
#' @export
CPX_PPRIIND_FULL        <-  4L


#------------------------------------------------------------------------------#
# Dual simplex pricing algorithm
#' @export
CPX_DPRIIND_AUTO        <- 0L
#' @export
CPX_DPRIIND_FULL        <- 1L
#' @export
CPX_DPRIIND_STEEP       <- 2L
#' @export
CPX_DPRIIND_FULL_STEEP  <- 3L
#' @export
CPX_DPRIIND_STEEPQSTART <- 4L
#' @export
CPX_DPRIIND_DEVEX       <- 5L


#------------------------------------------------------------------------------#
# PARALLELMODE values
#' @export
CPX_PARALLEL_DETERMINISTIC <-  1L
#' @export
CPX_PARALLEL_AUTO          <-  0L
#' @export
CPX_PARALLEL_OPPORTUNISTIC <- -1L


#------------------------------------------------------------------------------#
# Values for CPX_PARAM_WRITELEVEL
#' @export
CPX_WRITELEVEL_AUTO                 <- 0L
#' @export
CPX_WRITELEVEL_ALLVARS              <- 1L
#' @export
CPX_WRITELEVEL_DISCRETEVARS         <- 2L
#' @export
CPX_WRITELEVEL_NONZEROVARS          <- 3L
#' @export
CPX_WRITELEVEL_NONZERODISCRETEVARS  <- 4L


#------------------------------------------------------------------------------#
# Values for CPX_PARAM_SOLUTIONTARGET
#' @export
CPX_SOLUTIONTARGET_AUTO          <- 0L
#' @export
CPX_SOLUTIONTARGET_OPTIMALCONVEX <- 1L
#' @export
CPX_SOLUTIONTARGET_FIRSTORDER    <- 2L


#------------------------------------------------------------------------------#
# LP/QP solution algorithms, used as possible values for
# CPX_PARAM_LPMETHOD/CPX_PARAM_QPMETHOD/CPX_PARAM_BARCROSSALG/
# CPXgetmethod/...
#' @export
CPX_ALG_NONE       <- -1L
#' @export
CPX_ALG_AUTOMATIC  <-  0L
#' @export
CPX_ALG_PRIMAL     <-  1L
#' @export
CPX_ALG_DUAL       <-  2L
#' @export
CPX_ALG_NET        <-  3L
#' @export
CPX_ALG_BARRIER    <-  4L
#' @export
CPX_ALG_SIFTING    <-  5L
#' @export
CPX_ALG_CONCURRENT <-  6L
#' @export
CPX_ALG_BAROPT     <-  7L
#' @export
CPX_ALG_PIVOTIN    <-  8L
#' @export
CPX_ALG_PIVOTOUT   <-  9L
#' @export
CPX_ALG_PIVOT      <- 10L
#' @export
CPX_ALG_FEASOPT    <- 11L
#' @export
CPX_ALG_MIP        <- 12L
#' @export
CPX_ALG_ROBUST     <- 13L


#------------------------------------------------------------------------------#
# Basis status values
#' @export
CPX_AT_LOWER   <- 0L
#' @export
CPX_BASIC      <- 1L
#' @export
CPX_AT_UPPER   <- 2L
#' @export
CPX_FREE_SUPER <- 3L


#------------------------------------------------------------------------------#
# Variable types for ctype array
#' @export
CPX_CONTINUOUS <- "C"
#' @export
CPX_BINARY     <- "B"
#' @export
CPX_INTEGER    <- "I"
#' @export
CPX_SEMICONT   <- "S"
#' @export
CPX_SEMIINT    <- "N"


#------------------------------------------------------------------------------#
# PREREDUCE settings
#' @export
CPX_PREREDUCE_PRIMALANDDUAL  <- 3L
#' @export
CPX_PREREDUCE_DUALONLY       <- 2L
#' @export
CPX_PREREDUCE_PRIMALONLY     <- 1L
#' @export
CPX_PREREDUCE_NOPRIMALORDUAL <- 0L


#------------------------------------------------------------------------------#
# Conflict statuses
#' @export
CPX_STAT_CONFLICT_FEASIBLE            <- 30L
#' @export
CPX_STAT_CONFLICT_MINIMAL             <- 31L
#' @export
CPX_STAT_CONFLICT_ABORT_CONTRADICTION <- 32L
#' @export
CPX_STAT_CONFLICT_ABORT_TIME_LIM      <- 33L
#' @export
CPX_STAT_CONFLICT_ABORT_IT_LIM        <- 34L
#' @export
CPX_STAT_CONFLICT_ABORT_NODE_LIM      <- 35L
#' @export
CPX_STAT_CONFLICT_ABORT_OBJ_LIM       <- 36L
#' @export
CPX_STAT_CONFLICT_ABORT_MEM_LIM       <- 37L
#' @export
CPX_STAT_CONFLICT_ABORT_USER          <- 38L
#' @export
CPX_STAT_CONFLICT_ABORT_DETTIME_LIM   <- 39L


#------------------------------------------------------------------------------#
# Conflict status values
#' @export
CPX_CONFLICT_EXCLUDED        <- -1L
#' @export
CPX_CONFLICT_POSSIBLE_MEMBER <-  0L
#' @export
CPX_CONFLICT_POSSIBLE_LB     <-  1L
#' @export
CPX_CONFLICT_POSSIBLE_UB     <-  2L
#' @export
CPX_CONFLICT_MEMBER          <-  3L
#' @export
CPX_CONFLICT_LB              <-  4L
#' @export
CPX_CONFLICT_UB              <-  5L


#------------------------------------------------------------------------------#
# Problem Types
# Types 4, 9, and 12 are internal, the others are for users
#' @export
CPXPROB_LP                    <-  0L
#' @export
CPXPROB_MILP                  <-  1L
#' @export
CPXPROB_FIXEDMILP             <-  3L
#' @export
CPXPROB_NODELP                <-  4L
#' @export
CPXPROB_QP                    <-  5L
#' @export
CPXPROB_MIQP                  <-  7L
#' @export
CPXPROB_FIXEDMIQP             <-  8L
#' @export
CPXPROB_NODEQP                <-  9L
#' @export
CPXPROB_QCP                   <- 10L
#' @export
CPXPROB_MIQCP                 <- 11L
#' @export
CPXPROB_NODEQCP               <- 12L


#------------------------------------------------------------------------------#
# CPLEX Parameter numbers
#' @export
CPX_PARAM_ADVIND              <- 1001L
#' @export
CPX_PARAM_AGGFILL             <- 1002L
#' @export
CPX_PARAM_AGGIND              <- 1003L
#' @export
CPX_PARAM_BASINTERVAL         <- 1004L
#' @export
CPX_PARAM_CFILEMUL            <- 1005L
#' @export
CPX_PARAM_CLOCKTYPE           <- 1006L
#' @export
CPX_PARAM_CRAIND              <- 1007L
#' @export
CPX_PARAM_DEPIND              <- 1008L
#' @export
CPX_PARAM_DPRIIND             <- 1009L
#' @export
CPX_PARAM_PRICELIM            <- 1010L
#' @export
CPX_PARAM_EPMRK               <- 1013L
#' @export
CPX_PARAM_EPOPT               <- 1014L
#' @export
CPX_PARAM_EPPER               <- 1015L
#' @export
CPX_PARAM_EPRHS               <- 1016L
#' @export
CPX_PARAM_FASTMIP             <- 1017L
#' @export
CPX_PARAM_SIMDISPLAY          <- 1019L
#' @export
CPX_PARAM_ITLIM               <- 1020L
#' @export
CPX_PARAM_ROWREADLIM          <- 1021L
#' @export
CPX_PARAM_NETFIND             <- 1022L
#' @export
CPX_PARAM_COLREADLIM          <- 1023L
#' @export
CPX_PARAM_NZREADLIM           <- 1024L
#' @export
CPX_PARAM_OBJLLIM             <- 1025L
#' @export
CPX_PARAM_OBJULIM             <- 1026L
#' @export
CPX_PARAM_PERIND              <- 1027L
#' @export
CPX_PARAM_PERLIM              <- 1028L
#' @export
CPX_PARAM_PPRIIND             <- 1029L
#' @export
CPX_PARAM_PREIND              <- 1030L
#' @export
CPX_PARAM_REINV               <- 1031L
#' @export
CPX_PARAM_REVERSEIND          <- 1032L
#' @export
CPX_PARAM_RFILEMUL            <- 1033L
#' @export
CPX_PARAM_SCAIND              <- 1034L
#' @export
CPX_PARAM_SCRIND              <- 1035L
#' @export
CPX_PARAM_SINGLIM             <- 1037L
#' @export
CPX_PARAM_SINGTOL             <- 1038L
#' @export
CPX_PARAM_TILIM               <- 1039L
#' @export
CPX_PARAM_XXXIND              <- 1041L
#' @export
CPX_PARAM_PREDUAL             <- 1044L
#' @export
CPX_PARAM_EPOPT_H             <- 1049L
#' @export
CPX_PARAM_EPRHS_H             <- 1050L
#' @export
CPX_PARAM_PREPASS             <- 1052L
#' @export
CPX_PARAM_DATACHECK           <- 1056L
#' @export
CPX_PARAM_REDUCE              <- 1057L
#' @export
CPX_PARAM_PRELINEAR           <- 1058L
#' @export
CPX_PARAM_LPMETHOD            <- 1062L
#' @export
CPX_PARAM_QPMETHOD            <- 1063L
#' @export
CPX_PARAM_WORKDIR             <- 1064L
#' @export
CPX_PARAM_WORKMEM             <- 1065L
#' @export
CPX_PARAM_THREADS             <- 1067L
#' @export
CPX_PARAM_CONFLICTDISPLAY     <- 1074L
#' @export
CPX_PARAM_SIFTDISPLAY         <- 1076L
#' @export
CPX_PARAM_SIFTALG             <- 1077L
#' @export
CPX_PARAM_SIFTITLIM           <- 1078L
#' @export
CPX_PARAM_MPSLONGNUM          <- 1081L
#' @export
CPX_PARAM_MEMORYEMPHASIS      <- 1082L
#' @export
CPX_PARAM_NUMERICALEMPHASIS   <- 1083L
#' @export
CPX_PARAM_FEASOPTMODE         <- 1084L
#' @export
CPX_PARAM_PARALLELMODE        <- 1109L
#' @export
CPX_PARAM_TUNINGMEASURE       <- 1110L
#' @export
CPX_PARAM_TUNINGREPEAT        <- 1111L
#' @export
CPX_PARAM_TUNINGTILIM         <- 1112L
#' @export
CPX_PARAM_TUNINGDISPLAY       <- 1113L
#' @export
CPX_PARAM_WRITELEVEL          <- 1114L
#' @export
CPX_PARAM_RANDOMSEED          <- 1124L
#' @export
CPX_PARAM_DETTILIM            <- 1127L
#' @export
CPX_PARAM_FILEENCODING        <- 1129L
#' @export
CPX_PARAM_APIENCODING         <- 1130L
#' @export
CPX_PARAM_SOLUTIONTARGET      <- 1131L
#' @export
CPX_PARAM_CLONELOG            <- 1132L
#' @export
CPX_PARAM_TUNINGDETTILIM      <- 1139L

# Barrier is in bardefs.h, MIP is in mipdefs.h, QP is in qpdefs.h
#' @export
CPX_PARAM_ALL_MIN             <- 1000L
#' @export
CPX_PARAM_ALL_MAX             <- 6000L


#------------------------------------------------------------------------------#
# Values for CPX_PARAM_TUNINGMEASURE
#' @export
CPX_TUNE_AVERAGE <- 1L
#' @export
CPX_TUNE_MINMAX  <- 2L


#------------------------------------------------------------------------------#
# Values for incomplete tuning
#' @export
CPX_TUNE_ABORT     <- 1L
#' @export
CPX_TUNE_TILIM     <- 2L
#' @export
CPX_TUNE_DETTILIM  <- 3L


#------------------------------------------------------------------------------#
# Quality query identifiers
#' @export
CPX_MAX_PRIMAL_INFEAS          <-  1L
#' @export
CPX_MAX_SCALED_PRIMAL_INFEAS   <-  2L
#' @export
CPX_SUM_PRIMAL_INFEAS          <-  3L
#' @export
CPX_SUM_SCALED_PRIMAL_INFEAS   <-  4L
#' @export
CPX_MAX_DUAL_INFEAS            <-  5L
#' @export
CPX_MAX_SCALED_DUAL_INFEAS     <-  6L
#' @export
CPX_SUM_DUAL_INFEAS            <-  7L
#' @export
CPX_SUM_SCALED_DUAL_INFEAS     <-  8L
#' @export
CPX_MAX_INT_INFEAS             <-  9L
#' @export
CPX_SUM_INT_INFEAS             <- 10L
#' @export
CPX_MAX_PRIMAL_RESIDUAL        <- 11L
#' @export
CPX_MAX_SCALED_PRIMAL_RESIDUAL <- 12L
#' @export
CPX_SUM_PRIMAL_RESIDUAL        <- 13L
#' @export
CPX_SUM_SCALED_PRIMAL_RESIDUAL <- 14L
#' @export
CPX_MAX_DUAL_RESIDUAL          <- 15L
#' @export
CPX_MAX_SCALED_DUAL_RESIDUAL   <- 16L
#' @export
CPX_SUM_DUAL_RESIDUAL          <- 17L
#' @export
CPX_SUM_SCALED_DUAL_RESIDUAL   <- 18L
#' @export
CPX_MAX_COMP_SLACK             <- 19L
#' @export
CPX_SUM_COMP_SLACK             <- 21L
#' @export
CPX_MAX_X                      <- 23L
#' @export
CPX_MAX_SCALED_X               <- 24L
#' @export
CPX_MAX_PI                     <- 25L
#' @export
CPX_MAX_SCALED_PI              <- 26L
#' @export
CPX_MAX_SLACK                  <- 27L
#' @export
CPX_MAX_SCALED_SLACK           <- 28L
#' @export
CPX_MAX_RED_COST               <- 29L
#' @export
CPX_MAX_SCALED_RED_COST        <- 30L
#' @export
CPX_SUM_X                      <- 31L
#' @export
CPX_SUM_SCALED_X               <- 32L
#' @export
CPX_SUM_PI                     <- 33L
#' @export
CPX_SUM_SCALED_PI              <- 34L
#' @export
CPX_SUM_SLACK                  <- 35L
#' @export
CPX_SUM_SCALED_SLACK           <- 36L
#' @export
CPX_SUM_RED_COST               <- 37L
#' @export
CPX_SUM_SCALED_RED_COST        <- 38L
#' @export
CPX_KAPPA                      <- 39L
#' @export
CPX_OBJ_GAP                    <- 40L
#' @export
CPX_DUAL_OBJ                   <- 41L
#' @export
CPX_PRIMAL_OBJ                 <- 42L
#' @export
CPX_MAX_QCPRIMAL_RESIDUAL      <- 43L
#' @export
CPX_SUM_QCPRIMAL_RESIDUAL      <- 44L
#' @export
CPX_MAX_QCSLACK_INFEAS         <- 45L
#' @export
CPX_SUM_QCSLACK_INFEAS         <- 46L
#' @export
CPX_MAX_QCSLACK                <- 47L
#' @export
CPX_SUM_QCSLACK                <- 48L
#' @export
CPX_MAX_INDSLACK_INFEAS        <- 49L
#' @export
CPX_SUM_INDSLACK_INFEAS        <- 50L
#' @export
CPX_EXACT_KAPPA                <- 51L
#' @export
CPX_KAPPA_STABLE               <- 52L
#' @export
CPX_KAPPA_SUSPICIOUS           <- 53L
#' @export
CPX_KAPPA_UNSTABLE             <- 54L
#' @export
CPX_KAPPA_ILLPOSED             <- 55L
#' @export
CPX_KAPPA_MAX                  <- 56L
#' @export
CPX_KAPPA_ATTENTION            <- 57L


#------------------------------------------------------------------------------#
# feasopt options
#' @export
CPX_FEASOPT_MIN_SUM  <- 0L
#' @export
CPX_FEASOPT_OPT_SUM  <- 1L
#' @export
CPX_FEASOPT_MIN_INF  <- 2L
#' @export
CPX_FEASOPT_OPT_INF  <- 3L
#' @export
CPX_FEASOPT_MIN_QUAD <- 4L
#' @export
CPX_FEASOPT_OPT_QUAD <- 5L


#------------------------------------------------------------------------------#
# File: barconst.h
# Version 12.3

#' @export
CPX_STAT_OPTIMAL_FACE_UNBOUNDED <- 20L
#' @export
CPX_STAT_ABORT_PRIM_OBJ_LIM     <- 21L
#' @export
CPX_STAT_ABORT_DUAL_OBJ_LIM     <- 22L
#' @export
CPX_STAT_FIRSTORDER             <- 24L

# Barrier parameters
#' @export
CPX_PARAM_BARDSTART           <- 3001L
#' @export
CPX_PARAM_BAREPCOMP           <- 3002L
#' @export
CPX_PARAM_BARGROWTH           <- 3003L
#' @export
CPX_PARAM_BAROBJRNG           <- 3004L
#' @export
CPX_PARAM_BARPSTART           <- 3005L
#' @export
CPX_PARAM_BARALG              <- 3007L
#' @export
CPX_PARAM_BARCOLNZ            <- 3009L
#' @export
CPX_PARAM_BARDISPLAY          <- 3010L
#' @export
CPX_PARAM_BARITLIM            <- 3012L
#' @export
CPX_PARAM_BARMAXCOR           <- 3013L
#' @export
CPX_PARAM_BARORDER            <- 3014L
#' @export
CPX_PARAM_BARSTARTALG         <- 3017L
#' @export
CPX_PARAM_BARCROSSALG         <- 3018L
#' @export
CPX_PARAM_BARQCPEPCOMP        <- 3020L

# Optimizing Problems
#' @export
CPX_BARORDER_AUTO <- 0L
#' @export
CPX_BARORDER_AMD  <- 1L
#' @export
CPX_BARORDER_AMF  <- 2L
#' @export
CPX_BARORDER_ND   <- 3L


#------------------------------------------------------------------------------#
# File: mipconst.h
# Version 12.3

# MIP emphasis settings
#' @export
CPX_MIPEMPHASIS_BALANCED     <- 0L
#' @export
CPX_MIPEMPHASIS_FEASIBILITY  <- 1L
#' @export
CPX_MIPEMPHASIS_OPTIMALITY   <- 2L
#' @export
CPX_MIPEMPHASIS_BESTBOUND    <- 3L
#' @export
CPX_MIPEMPHASIS_HIDDENFEAS   <- 4L

# Values for sostype and branch type
#' @export
CPX_TYPE_VAR                 <- "0"
#' @export
CPX_TYPE_SOS1                <- "1"
#' @export
CPX_TYPE_SOS2                <- "2"
#' @export
CPX_TYPE_USER                <- "X"
#' @export
CPX_TYPE_ANY                 <- "A"

# Variable selection values
#' @export
CPX_VARSEL_MININFEAS      <- -1L
#' @export
CPX_VARSEL_DEFAULT        <-  0L
#' @export
CPX_VARSEL_MAXINFEAS      <-  1L
#' @export
CPX_VARSEL_PSEUDO         <-  2L
#' @export
CPX_VARSEL_STRONG         <-  3L
#' @export
CPX_VARSEL_PSEUDOREDUCED  <-  4L

# Node selection values
#' @export
CPX_NODESEL_DFS           <- 0L
#' @export
CPX_NODESEL_BESTBOUND     <- 1L
#' @export
CPX_NODESEL_BESTEST       <- 2L
#' @export
CPX_NODESEL_BESTEST_ALT   <- 3L

# Values for generated priority order
#' @export
CPX_MIPORDER_COST                <- 1L
#' @export
CPX_MIPORDER_BOUNDS              <- 2L
#' @export
CPX_MIPORDER_SCALEDCOST          <- 3L

# Values for direction array
#' @export
CPX_BRANCH_GLOBAL                <-  0L
#' @export
CPX_BRANCH_DOWN                  <- -1L
#' @export
CPX_BRANCH_UP                    <-  1L

# Values for CPX_PARAM_BRDIR
#' @export
CPX_BRDIR_DOWN                   <- -1L
#' @export
CPX_BRDIR_AUTO                   <-  0L
#' @export
CPX_BRDIR_UP                     <-  1L

# Values for CPX_PARAM_MIPSEARCH
#' @export
CPX_MIPSEARCH_AUTO         <- 0L
#' @export
CPX_MIPSEARCH_TRADITIONAL  <- 1L
#' @export
CPX_MIPSEARCH_DYNAMIC      <- 2L

# Values for CPX_PARAM_MIPKAPPASTATS
#' @export
CPX_MIPKAPPA_OFF     <- -1L
#' @export
CPX_MIPKAPPA_AUTO    <-  0L
#' @export
CPX_MIPKAPPA_SAMPLE  <-  1L
#' @export
CPX_MIPKAPPA_FULL    <-  2L

# Effort levels for MIP starts
#' @export
CPX_MIPSTART_AUTO          <- 0L
#' @export
CPX_MIPSTART_CHECKFEAS     <- 1L
#' @export
CPX_MIPSTART_SOLVEFIXED    <- 2L
#' @export
CPX_MIPSTART_SOLVEMIP      <- 3L
#' @export
CPX_MIPSTART_REPAIR        <- 4L

# MIP Problem status codes
#' @export
CPXMIP_OPTIMAL               <- 101L
#' @export
CPXMIP_OPTIMAL_TOL           <- 102L
#' @export
CPXMIP_INFEASIBLE            <- 103L
#' @export
CPXMIP_SOL_LIM               <- 104L
#' @export
CPXMIP_NODE_LIM_FEAS         <- 105L
#' @export
CPXMIP_NODE_LIM_INFEAS       <- 106L
#' @export
CPXMIP_TIME_LIM_FEAS         <- 107L
#' @export
CPXMIP_TIME_LIM_INFEAS       <- 108L
#' @export
CPXMIP_FAIL_FEAS             <- 109L
#' @export
CPXMIP_FAIL_INFEAS           <- 110L
#' @export
CPXMIP_MEM_LIM_FEAS          <- 111L
#' @export
CPXMIP_MEM_LIM_INFEAS        <- 112L
#' @export
CPXMIP_ABORT_FEAS            <- 113L
#' @export
CPXMIP_ABORT_INFEAS          <- 114L
#' @export
CPXMIP_OPTIMAL_INFEAS        <- 115L
#' @export
CPXMIP_FAIL_FEAS_NO_TREE     <- 116L
#' @export
CPXMIP_FAIL_INFEAS_NO_TREE   <- 117L
#' @export
CPXMIP_UNBOUNDED             <- 118L
#' @export
CPXMIP_INForUNBD             <- 119L
#' @export
CPXMIP_FEASIBLE_RELAXED_SUM  <- 120L
#' @export
CPXMIP_OPTIMAL_RELAXED_SUM   <- 121L
#' @export
CPXMIP_FEASIBLE_RELAXED_INF  <- 122L
#' @export
CPXMIP_OPTIMAL_RELAXED_INF   <- 123L
#' @export
CPXMIP_FEASIBLE_RELAXED_QUAD <- 124L
#' @export
CPXMIP_OPTIMAL_RELAXED_QUAD  <- 125L
#' @export
CPXMIP_ABORT_RELAXED         <- 126L
#' @export
CPXMIP_FEASIBLE              <- 127L
#' @export
CPXMIP_POPULATESOL_LIM       <- 128L
#' @export
CPXMIP_OPTIMAL_POPULATED     <- 129L
#' @export
CPXMIP_OPTIMAL_POPULATED_TOL <- 130L
#' @export
CPXMIP_DETTIME_LIM_FEAS      <- 131L
#' @export
CPXMIP_DETTIME_LIM_INFEAS    <- 132L

# Valid purgeable values for adding usercuts and lazyconstraints
#' @export
CPX_USECUT_FORCE             <- 0L
#' @export
CPX_USECUT_PURGE             <- 1L
#' @export
CPX_USECUT_FILTER            <- 2L

# For CPXgetnodeintfeas
#' @export
CPX_INTEGER_FEASIBLE         <- 0L
#' @export
CPX_INTEGER_INFEASIBLE       <- 1L
#' @export
CPX_IMPLIED_INTEGER_FEASIBLE <- 2L

# MIP Parameter numbers
#' @export
CPX_PARAM_BRDIR               <- 2001L
#' @export
CPX_PARAM_BTTOL               <- 2002L
#' @export
CPX_PARAM_CLIQUES             <- 2003L
#' @export
CPX_PARAM_COEREDIND           <- 2004L
#' @export
CPX_PARAM_COVERS              <- 2005L
#' @export
CPX_PARAM_CUTLO               <- 2006L
#' @export
CPX_PARAM_CUTUP               <- 2007L
#' @export
CPX_PARAM_EPAGAP              <- 2008L
#' @export
CPX_PARAM_EPGAP               <- 2009L
#' @export
CPX_PARAM_EPINT               <- 2010L
#' @export
CPX_PARAM_MIPDISPLAY          <- 2012L
#' @export
CPX_PARAM_MIPINTERVAL         <- 2013L
#' @export
CPX_PARAM_INTSOLLIM           <- 2015L
#' @export
CPX_PARAM_NODEFILEIND         <- 2016L
#' @export
CPX_PARAM_NODELIM             <- 2017L
#' @export
CPX_PARAM_NODESEL             <- 2018L
#' @export
CPX_PARAM_OBJDIF              <- 2019L
#' @export
CPX_PARAM_MIPORDIND           <- 2020L
#' @export
CPX_PARAM_RELOBJDIF           <- 2022L
#' @export
CPX_PARAM_STARTALG            <- 2025L
#' @export
CPX_PARAM_SUBALG              <- 2026L
#' @export
CPX_PARAM_TRELIM              <- 2027L
#' @export
CPX_PARAM_VARSEL              <- 2028L
#' @export
CPX_PARAM_BNDSTRENIND         <- 2029L
#' @export
CPX_PARAM_HEURFREQ            <- 2031L
#' @export
CPX_PARAM_MIPORDTYPE          <- 2032L
#' @export
CPX_PARAM_CUTSFACTOR          <- 2033L
#' @export
CPX_PARAM_RELAXPREIND         <- 2034L
#' @export
CPX_PARAM_PRESLVND            <- 2037L
#' @export
CPX_PARAM_BBINTERVAL          <- 2039L
#' @export
CPX_PARAM_FLOWCOVERS          <- 2040L
#' @export
CPX_PARAM_IMPLBD              <- 2041L
#' @export
CPX_PARAM_PROBE               <- 2042L
#' @export
CPX_PARAM_GUBCOVERS           <- 2044L
#' @export
CPX_PARAM_STRONGCANDLIM       <- 2045L
#' @export
CPX_PARAM_STRONGITLIM         <- 2046L
#' @export
CPX_PARAM_FRACCAND            <- 2048L
#' @export
CPX_PARAM_FRACCUTS            <- 2049L
#' @export
CPX_PARAM_FRACPASS            <- 2050L
#' @export
CPX_PARAM_FLOWPATHS           <- 2051L
#' @export
CPX_PARAM_MIRCUTS             <- 2052L
#' @export
CPX_PARAM_DISJCUTS            <- 2053L
#' @export
CPX_PARAM_AGGCUTLIM           <- 2054L
#' @export
CPX_PARAM_MIPCBREDLP          <- 2055L
#' @export
CPX_PARAM_CUTPASS             <- 2056L
#' @export
CPX_PARAM_MIPEMPHASIS         <- 2058L
#' @export
CPX_PARAM_SYMMETRY            <- 2059L
#' @export
CPX_PARAM_DIVETYPE            <- 2060L
#' @export
CPX_PARAM_RINSHEUR            <- 2061L
#' @export
CPX_PARAM_SUBMIPNODELIM       <- 2062L
#' @export
CPX_PARAM_LBHEUR              <- 2063L
#' @export
CPX_PARAM_REPEATPRESOLVE      <- 2064L
#' @export
CPX_PARAM_PROBETIME           <- 2065L
#' @export
CPX_PARAM_POLISHTIME          <- 2066L
#' @export
CPX_PARAM_REPAIRTRIES         <- 2067L
#' @export
CPX_PARAM_EPLIN               <- 2068L
#' @export
CPX_PARAM_EPRELAX             <- 2073L
#' @export
CPX_PARAM_FPHEUR              <- 2098L
#' @export
CPX_PARAM_EACHCUTLIM          <- 2102L
#' @export
CPX_PARAM_SOLNPOOLCAPACITY    <- 2103L
#' @export
CPX_PARAM_SOLNPOOLREPLACE     <- 2104L
#' @export
CPX_PARAM_SOLNPOOLGAP         <- 2105L
#' @export
CPX_PARAM_SOLNPOOLAGAP        <- 2106L
#' @export
CPX_PARAM_SOLNPOOLINTENSITY   <- 2107L
#' @export
CPX_PARAM_POPULATELIM         <- 2108L
#' @export
CPX_PARAM_MIPSEARCH           <- 2109L
#' @export
CPX_PARAM_MIQCPSTRAT          <- 2110L
#' @export
CPX_PARAM_ZEROHALFCUTS        <- 2111L
#' @export
CPX_PARAM_POLISHAFTEREPAGAP   <- 2126L
#' @export
CPX_PARAM_POLISHAFTEREPGAP    <- 2127L
#' @export
CPX_PARAM_POLISHAFTERNODE     <- 2128L
#' @export
CPX_PARAM_POLISHAFTERINTSOL   <- 2129L
#' @export
CPX_PARAM_POLISHAFTERTIME     <- 2130L
#' @export
CPX_PARAM_MCFCUTS             <- 2134L
#' @export
CPX_PARAM_MIPKAPPASTATS       <- 2137L
#' @export
CPX_PARAM_AUXROOTTHREADS      <- 2139L
#' @export
CPX_PARAM_INTSOLFILEPREFIX    <- 2143L
#' @export
CPX_PARAM_PROBEDETTIME        <- 2150L
#' @export
CPX_PARAM_POLISHAFTERDETTIME  <- 2151L

# Values for CPX_PARAM_SOLNPOOLREPLACE
#' @export
CPX_SOLNPOOL_FIFO    <- 0L
#' @export
CPX_SOLNPOOL_OBJ     <- 1L
#' @export
CPX_SOLNPOOL_DIV     <- 2L

#' @export
CPX_SOLNPOOL_FILTER_DIVERSITY   <- 1L
#' @export
CPX_SOLNPOOL_FILTER_RANGE       <- 2L


#------------------------------------------------------------------------------#
# File: gcconst.h
# Version 12.3

#' @export
CPX_CON_LOWER_BOUND          <-  1L
#' @export
CPX_CON_UPPER_BOUND          <-  2L
#' @export
CPX_CON_LINEAR               <-  3L
#' @export
CPX_CON_QUADRATIC            <-  4L
#' @export
CPX_CON_SOS                  <-  5L
#' @export
CPX_CON_INDICATOR            <-  6L

# internal types
#' @export
CPX_CON_MINEXPR              <-  7L
#' @export
CPX_CON_MAXEXPR              <-  8L
#' @export
CPX_CON_PWL                  <-  9L
#' @export
CPX_CON_ABS                  <-  9L  # same as PWL since using it
#' @export
CPX_CON_DISJCST              <- 10L
#' @export
CPX_CON_INDDISJCST           <- 11L
#' @export
CPX_CON_SETVAR               <- 12L
#' @export
CPX_CON_SETVARMEMBER         <- 13L
#' @export
CPX_CON_SETVARCARD           <- 14L
#' @export
CPX_CON_SETVARSUM            <- 15L
#' @export
CPX_CON_SETVARMIN            <- 16L
#' @export
CPX_CON_SETVARMAX            <- 17L
#' @export
CPX_CON_SETVARSUBSET         <- 18L
#' @export
CPX_CON_SETVARDOMAIN         <- 19L
#' @export
CPX_CON_SETVARUNION          <- 20L
#' @export
CPX_CON_SETVARINTERSECTION   <- 21L
#' @export
CPX_CON_SETVARNULLINTERSECT  <- 22L
#' @export
CPX_CON_SETVARINTERSECT      <- 23L
#' @export
CPX_CON_SETVAREQ             <- 24L
#' @export
CPX_CON_SETVARNEQ            <- 25L
#' @export
CPX_CON_SETVARNEQCST         <- 26L
#' @export
CPX_CON_LAST_CONTYPE         <- 27L


#------------------------------------------------------------------------------#
# File: netconst.h
# Version 12.3

# Network parameters
#' @export
CPX_PARAM_NETITLIM            <- 5001L
#' @export
CPX_PARAM_NETEPOPT            <- 5002L
#' @export
CPX_PARAM_NETEPRHS            <- 5003L
#' @export
CPX_PARAM_NETPPRIIND          <- 5004L
#' @export
CPX_PARAM_NETDISPLAY          <- 5005L

# NETOPT display values
#' @export
CPXNET_NO_DISPLAY_OBJECTIVE <- 0L
#' @export
CPXNET_TRUE_OBJECTIVE       <- 1L
#' @export
CPXNET_PENALIZED_OBJECTIVE  <- 2L

# NETOPT pricing parameters
#' @export
CPXNET_PRICE_AUTO           <- 0L
#' @export
CPXNET_PRICE_PARTIAL        <- 1L
#' @export
CPXNET_PRICE_MULT_PART      <- 2L
#' @export
CPXNET_PRICE_SORT_MULT_PART <- 3L


#------------------------------------------------------------------------------#
# File: qpconst.h
# Version 12.3

# Copying data
#' @export
CPX_PARAM_QPNZREADLIM         <- 4001L

# presolve
#' @export
CPX_PARAM_QPMAKEPSDIND        <- 4010L


#------------------------------------------------------------------------------#
# Error codes

# Callable library miscellaneous routines
#' @export
CPXERR_NEGATIVE_SURPLUS       <- 1207L
#' @export
CPXERR_NO_SENSIT              <- 1260L

